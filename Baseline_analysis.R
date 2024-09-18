###################################
####Load required packages#########
#################################
library(data.table);library(lubridate);library(dplyr);library(dlnm);library(splines)
library(tsModel);library(forestplot);library(reshape2);library(mvmeta);library(metafor);
library(ggthemes);library(ggsci);library(tidyr)

#preparation stage
cause_name<-c("Non-accidental","CVD","CHD",
              "Stroke","Haemorrhagic Stroke","Ischaemic Stroke","Respiratory Disease","COPD")
hwtype<-c("Day","Night","Com")
xlag  <- 0:100/10
xvar <- seq(0,1,by=1)
logknots <- logknots(0:10,df=3);arglag=list(fun="ns",knots=logknots)

plots <- data.frame()

#################################

###################################
####First and Second stage analysis#########
#################################
datahot<-readRDS("D:/xxx/city_era5_hw(2013_2015).rds")#exposure and mortality
city_id<-unique(datahot$code_city)
dlist <- lapply(city_id,function(x) datahot[datahot$code_city==x,])
names(dlist) <- as.character(city_id)

metada<-fread("D:/xxx/city_infomation.csv")#used for meta-analysis
#########
#for city-specific analysis
yall1<-yall2<-yall3<-matrix(NA,nrow(metada),3,dimnames=list(city_id,paste("b",seq(3),sep="")))
Sall1 <-Sall2 <-Sall3 <- vector("list",nrow(metada))
est_fit<-est_se<-array(NA,dim=c(3,8,nrow(metada)),
                                dimnames = list(hwtype,
                                                cause_name,
                                                city_id));

#BLUP city-specific estimation for third-stage
bp_rr<-array(NA,dim=c(3,3,8,nrow(metada)),
                                dimnames = list(hwtype,
                                                c("rr","rrl","rrh"),
                                                cause_name,
                                                city_id));


for (k in 1:3) {#three heatwave types
  for (t in 1:8) { #eight diseases
    for(i in 1:length(dlist)){#cities

      sub<-as.data.frame(dlist[[i]]);
      causes<-as.data.frame(sub[,c(25,26,28,32:35,37)])
      
      HW.basis1  <- crossbasis(sub$daywave,lag=c(0,10),argvar=list("lin"),arglag=arglag)
      HW.basis2  <- crossbasis(sub$nightwave,lag=c(0,10),argvar=list("lin"),arglag=arglag)
      HW.basis3  <- crossbasis(sub$comwave,lag=c(0,10),argvar=list("lin"),arglag=arglag)
      
      #city-specific analysis
      mfirst    <- gam(causes[,t] ~HW.basis1+HW.basis2+HW.basis3+ns(day,6)+ns(rhu,3)+as.factor(year)+as.factor(dow)+as.factor(holi),family=quasipoisson,data=sub,na.action="na.exclude")

      eval(parse(text = paste0("crall<-crossreduce(HW.basis",k,",mfirst,type='var',value=1)")))
      eval(parse(text = paste0("yall",k,"[",i,",]  <- coef(crall)")))
      eval(parse(text = paste0("Sall",k,"[[",i,"]] <- vcov(crall)")))
      
      eval(parse(text = paste0("crhw<-crossreduce(HW.basis",k,",mfirst,at=1,lag=c(0,6))")))
      est_fit[k,t,i]<-coef(crhw)
      est_se[k,t,i]<-vcov(crhw)
    }

    #meta-analysis for lag-response
    eval(parse(text = paste0("mvall<- mvmeta(yall",k,"~1,
                             Sall",k,",method='reml')")))
    
    eval(parse(text = paste0("blag <- do.call('onebasis',c(list(x=xlag),attr(HW.basis",k,",'arglag')))")))
    cplag1 <- crosspred(blag,coef=coef(mvall),vcov=vcov(mvall),model.link="log",at=0:100/10)
    
    plot_data <- data.frame(lag = 0:100/10, RR = cplag1$matRRfit[,1], ymin = cplag1$matRRlow[,1], ymax = cplag1$matRRhigh[,1], type = hwtype[k], factor = cause_name[t])
    plots <- rbind(plots, plot_data)# for Figure 1
    
    #meta-analysis for mortality risks
    mvhw<-mvmeta(est_fit[k,t,]~as.numeric(gdp)+as.numeric(urban)+as.numeric(rainfall)+
                   as.numeric(meantemp)+as.numeric(tempsd),est_se[k,t,],data=metada,method='reml')


    #preparation for future prediction
    if(k%in%c(1:3)&t==1){
      eval(parse(text = paste0("modelvar",k,"<-mvhw")))
    }
    
    blupx <- blup.mvmeta(mvhw,vcov=T)
    blupx$RR <- exp(blupx$blup)
    blupx$Lower <- exp(blupx$blup - 1.96 * blupx$vcov)
    blupx$Upper <- exp(blupx$blup + 1.96 * blupx$vcov)
    
    bp_rr[k,1,t,]<-blupx$RR
    bp_rr[k,2,t,]<-blupx$Lower
    bp_rr[k,3,t,]<-blupx$Upper
    
  }
  
}

saveRDS(bp_rr, "D:/xxx/RR.rds")
# FUNCTION FOR COMPUTING THE P-VALUE from WALD TEST
fwald <- function(model,var) {
  ind <- grep(var,names(coef(model)))
  coef <- coef(model)[ind]
  vcov <- vcov(model)[ind,ind]
  waldstat <- coef%*%solve(vcov)%*%coef
  df <- length(coef)
  return(1-pchisq(waldstat,df))
}

fwald(modelvar3,"gdp")
fwald(modelvar3,"urban")
fwald(modelvar3,"rainfall")
fwald(modelvar3,"meantemp")
fwald(modelvar3,"tempsd")

##########for third stage
city_pop<-fread("D:/xxx/city_anual_pop.csv")#used for population
city_pop$allmean<-apply(city_pop[,c(2:4)],1,mean)
city_pop <- city_pop %>%
  arrange(city_id, citycode)
MR<-array(NA,dim = c(3,8,272),
          dimnames = list(hwtype,cause_name,city_id))

sub<-datahot
for (i in city_id) {
  dd<-as.data.frame(subset(sub,daywave==0))
  dd<-dd[dd$code_city==i,c("total","cvd","chd","stroke","hstroke","istroke","res","copd")]
  
  nn<-as.data.frame(subset(sub,nightwave==0))
  nn<-nn[nn$code_city==i,c("total","cvd","chd","stroke","hstroke","istroke","res","copd")]
  
  cc<-as.data.frame(subset(sub,comwave==0))
  cc<-cc[cc$code_city==i,c("total","cvd","chd","stroke","hstroke","istroke","res","copd")]
  
  for (k in 1:8) {
    MR_region[1,k,i]<-sum(dd[,k])/city_pop[i,5]
    MR_region[2,k,i]<-sum(nn[,k])/city_pop[i,5]
    MR_region[3,k,i]<-sum(cc[,k])/city_pop[i,5]
    
  }
}
saveRDS(MR_region,"D:/XX/MR_new.rds")

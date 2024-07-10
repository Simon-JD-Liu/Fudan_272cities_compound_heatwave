###################################
####Load required packages#########
#################################
library(data.table);library(lubridate);library(dplyr);library(dlnm);library(splines)
library(tsModel);library(forestplot);library(reshape2);library(mvmeta);library(mixmeta)
library(metafor);library(ggthemes);library("ggsci");library(tidyr)
#################################

###################################
####Historical analysis#########
#################################
datahot<-readRDS("D:/xxx/city_era5_hw(2013_2015).rds")#exposure and mortality
city_id<-unique(datahot$code_city)
dlist <- lapply(city_id,function(x) datahot[datahot$code_city==x,])
names(dlist) <- city_id

metada<-fread("D:/xxx/city_infomation.csv")#used for meta-analysis
#########
logknots <- logknots(0:10,df=3);arglag=list(fun="ns",knots=logknots)
yall1<-yall2<-yall3<-matrix(NA,nrow(metada),1,dimnames=list(city_id))
Sall1 <-Sall2 <-Sall3 <- vector("list",nrow(metada))

#pooled estimation
COEFF<-SD<-matrix(NA,3,8,dimnames=list(c("day","night","com"),c("Non-accidental death","CVD","CHD","Stroke","Haemorrhagic stroke",
                                                            "Ischaemic stroke","Respiratory disease","COPD")))

#BLUP city-specific estimation
est_rr<-est_rrl<-est_rrh<-array(NA,dim=c(3,8,nrow(metada)),
                                   dimnames = list(c("day","night","com"),
                                                   c("Non-accidental death","CVD","CHD","Stroke","Haemorrhagic stroke",
                                                     "Ischaemic stroke","Respiratory disease","COPD"),
                                                   city_id));


I2<-matrix(NA,3,8,dimnames=list(c("day","night","com"),c("Non-accidental death","CVD","CHD","Stroke","Haemorrhagic stroke",
                                                                "Ischaemic stroke","Respiratory disease","COPD")))

for (k in 1:3) {#three heatwave types
 for (t in 1:8) { #eight diseases
  for(i in 1:length(dlist)){#cities
     sub<-as.data.frame(dlist[[i]]); 
     causes<-as.data.frame(sub[,c(25,26,28,32:35,37)])

     HW.basis1  <- crossbasis(sub$daywave,lag=c(0,10),argvar=list("lin"),arglag=arglag)
     HW.basis2  <- crossbasis(sub$nightwave,lag=c(0,10),argvar=list("lin"),arglag=arglag)
     HW.basis3  <- crossbasis(sub$comwave,lag=c(0,10),argvar=list("lin"),arglag=arglag)
    
     #city-specific analysis
     mfirst    <- glm(causes[,t] ~HW.basis1+HW.basis2+HW.basis3+ns(day,6)+ns(rhu,3)+as.factor(year)+as.factor(dow)+as.factor(holi),family=quasipoisson,data=sub,,na.action="na.exclude")

      eval(parse(text = paste0("crall<-crossreduce(HW.basis",k,",mfirst,type='overall',lag=c(0,6))")))
      eval(parse(text = paste0("yall",k,"[",i,",]  <- coef(crall)")))
      eval(parse(text = paste0("Sall",k,"[[",i,"]] <- vcov(crall)")))
     }

    #meta-analysis
    eval(parse(text = paste0("mvall<- mixmeta(yall",k,"~as.numeric(gdp)+as.numeric(urban)+as.numeric(rainfall)+
                             as.numeric(meantemp)+as.numeric(tempsd),
                             Sall",k,"data=metada,method='reml')")))
    if(k==3&i==1){model_analysis<-mvall}
    COEFF[k,t]<-predict(mvall,vcov = T)[1,1]
    SD[k,t]<-predict(mvall,vcov = T)[1,2]
    I2[k,t]<-mvall$I2
    
    #preparation for future prediction
    blupx <- as.data.frame(blup(mvall,vcov=T))
    blupx$RR <- exp(blupx$blup)
    blupx$Lower <- exp(blupx$blup - 1.96 * blupx$vcov)
    blupx$Upper <- exp(blupx$blup + 1.96 * blupx$vcov)
    est_rr[k,t,]<-blupx$RR
    est_rrl[k,t,]<-blupx$Lower
    est_rrh[k,t,]<-blupx$Upper

 }
  
}

# FUNCTION FOR COMPUTING THE P-VALUE from WALD TEST
fwald <- function(model,var) {
  ind <- grep(var,names(coef(model)))
  coef <- coef(model)[ind]
  vcov <- vcov(model)[ind,ind]
  waldstat <- coef%*%solve(vcov)%*%coef
  df <- length(coef)
  return(1-pchisq(waldstat,df))
}

fwald(model,"gdp")
fwald(model,"urban")
fwald(model,"rainfall")
fwald(model,"meantemp")
fwald(model,"tempsd")


###################################
#######for heatwave characteristic##
###################################
##Idntify heatwaves' characteristics
character_define<-function(x,HW,thr,metrix){
  tempData<-x;  
  thr<-thr
  ## Tos:Time of hot season;Dos:Day of hot season
  ##HD1 used in CSNtab only
  tempData$Tos<-tempData$Dos<-tempData$nHW<-tempData$HI<-tempData$HD<-tempData$HD1<-tempData$HT<-0 
  tempData$hotSeason<-as.numeric(tempData$month%in%c(6,7,8))
  vec.hotseason<-rle(tempData$hotSeason);val.season<-vec.hotseason$values;pos.season<-vec.hotseason$lengths
  nseason<-sum(val.season)
  for(i in seq(nseason)){
    start<-ifelse(which(val.season==1)[i]==1,0,sum(pos.season[1:(which(val.season==1)[i]-1)]))
    Dur<-pos.season[which(val.season==1)[i]]
    tempData$Tos[(start+1):(start+Dur)]<-i;tempData$Dos[(start+1):(start+Dur)]<-rep(c(1:92),length(unique(tempData$year)))
  } 
  nHW.per.season<-0  
  for(g in seq(nseason)){
    season_data<-subset(tempData,Tos==g);
    eval(parse(text=paste("vec.code<-rle(season_data$",HW,")",sep=""))); 
    val<-vec.code$values;pos<-vec.code$lengths
    if(length(val)>1){
      freq.hw<-sum(val)
      for(i in seq(freq.hw)){ 
        start<-ifelse(which(val==1)[i]==1,0,sum(pos[1:(which(val==1)[i]-1)]));Dur<-pos[which(val==1)[i]]
        season_data$nHW[(start+1):(start+Dur)]<-rep((sum(nHW.per.season)+i),Dur)        #The number of HW
        season_data$HD[(start+1):(start+Dur)]<-1:Dur;season_data$HD1[(start+1):(start+Dur)]<-rep((Dur-1),Dur)       #Duration
        season_data$HI[(start+1):(start+Dur)]<--thr[(start+1):(start+Dur)]+eval(parse(text=paste0("season_data$",metric)))[(start+1):(start+Dur)] #Intensity
        season_data$HT[(start+1):(start+Dur)]<-(season_data$Dos[(start+1):(start+Dur)]-1)/10;          #Time of occurrence
      } 
      nHW.per.season<-c(nHW.per.season,freq.hw)
    }
    tempData[which(tempData$Tos==g),]<-season_data
  }
  return(tempData)
}

####
dlist1<-dlist
for (i in seq(dlist1)) {
  sub<-dlist[[i]]
  
  xx<-character_define(sub,"daywave",sub$max,"rowmax")
  xx<-xx[,c(9,73,75,76)]
  colnames(xx)[2:4]<-c("DT","DD","DI")
  
  xx1<-character_define(sub,"nightwave",sub$min,"rowmin")
  xx1<-xx1[,c(9,73,75,76)]
  colnames(xx1)[2:4]<-c("NT","ND","NI")
  xx<-left_join(xx,xx1,by="date")
  
  xx2<-character_define(sub,"comwave",sub$mean,"rowmean")
  xx2<-xx2[,c(9,73,75,76)]
  colnames(xx2)[2:4]<-c("CT","CD","CI")
  xx<-left_join(xx,xx2,by="date")
  
  sub<-left_join(sub,xx,by="date")
  
  dlist1[[i]]<-sub
}
per_day <- as.data.frame(lapply(dlist1, function(df) df[["DI"]]))
tday<-quantile(as.matrix(per_day)[which(as.matrix(per_day)!=0)],0.75)-quantile(as.matrix(per_day)[which(as.matrix(per_day)!=0)],0.25)
range(as.matrix(per_day)[which(as.matrix(per_day)!=0)])

per_night <- as.data.frame(lapply(dlist1, function(df) df[["NI"]]))
tnig<-quantile(as.matrix(per_night)[which(as.matrix(per_night)!=0)],0.25)-quantile(as.matrix(per_night)[which(as.matrix(per_night)!=0)],0.25)
range(as.matrix(per_night)[which(as.matrix(per_night)!=0)])

per_com <- as.data.frame(lapply(dlist1, function(df) df[["CI"]]))
tcom<-quantile(as.matrix(per_com)[which(as.matrix(per_com)!=0)],0.75)-quantile(as.matrix(per_com)[which(as.matrix(per_com)!=0)],0.25)
range(as.matrix(per_com)[which(as.matrix(per_com)!=0)])

##########################
HW_types <- c("Day heatwave", "Night heatwave", "Compound heatwave")
chaname <- c("Intensity", "Duration", "Timing")

for (l in 1:3) {    
 t<-1  
    yall <- matrix(NA, gg[t], 3, dimnames = list(1:gg[t], paste("b", seq(3), sep = "")))
    Sall <- vector("list", gg[t])
    
    for (i in 1:length(dlist1)) {   
      sub <- dlist1[[i]]
      causes <- as.data.frame(sub[, c(25)])
      
   
      DIn <- crossbasis(sub$DI, lag = c(0, 10), argvar = list("lin"), arglag = arglag)
      DDu <- crossbasis(sub$DD, lag = c(0, 10), argvar = list("lin"), arglag = arglag)
      NIn <- crossbasis(sub$NI, lag = c(0, 10), argvar = list("lin"), arglag = arglag)
      NDu <- crossbasis(sub$ND, lag = c(0, 10), argvar = list("lin"), arglag = arglag)
      CIn <- crossbasis(sub$CI, lag = c(0, 10), argvar = list("lin"), arglag = arglag)
      CDu <- crossbasis(sub$CD, lag = c(0, 10), argvar = list("lin"), arglag = arglag)
      
      
      mfirst <- glm(causes[, t] ~ DIn + DDu + NIn + NDu + CIn + CDu +
                      ns(day, df = 6) + ns(rhu, 3) +
                      as.factor(year) + as.factor(dow) + as.factor(holi),
                    family = quasipoisson, data = sub)
      
      if (l == 1) {
        crall <- crossreduce(DIn, mfirst)
      } else if (l == 2) {
        crall <- crossreduce(NIn, mfirst)
      } else if (l == 3) {
        crall <- crossreduce(CIn, mfirst)
      }
      
      yall[i, ] <- coef(crall)
      Sall[[i]] <- vcov(crall)
    }

    mvall <- mvmeta(yall~as.numeric(gdp)+as.numeric(urban)+as.numeric(rainfall)+
                             as.numeric(meantemp)+as.numeric(tempsd), Sall,data=metada, method = "reml")
    
    xlag <- 0:100 / 10
    blag <- do.call("onebasis", c(list(x = xlag),arglag))
    cplag <- crosspred(blag, coef = coef(mvall), vcov = vcov(mvall), model.link = "log", at = 0:100 / 10)
    plot(cplag, xlim = c(0, 10), main = chaname[l], ylab = "Relative Risk", xlab = "Lag(days)", cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
    lines(cplag, col = 2, lwd = 2)
  }
}

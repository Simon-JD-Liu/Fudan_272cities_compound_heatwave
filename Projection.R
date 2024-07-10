# colors recommended by nature
len <- 9
colors_nature <- pal_npg("nrc", alpha=0.9)(len)
barplot(rep(1:9),col=colors_nature)


gcms<-c('BCC-CSM2-MR','CNRM-CM6-1','CanESM5','HadGEM3-GC31-LL',
        'IPSL-CM6A-LR', 'MIROC6','MRI-ESM2-0','NorESM2-LM')



type<-c("historical","ssp245","ssp585")


for(j in 1:272){
  eval(parse(text=paste0("city",j,"<-data.frame()")))
  for(i in seq(gcms)){
    eval(parse(text=paste("xx<-fread('/d2/home/user19/272cities_ljd/future_data/min/historical/",gcms[i],"/",j,".csv')",sep = "")))
    xx$type<-rep("historical",length(xx$tasmin))
    
    eval(parse(text=paste0("xx2<-fread('/d2/home/user19/272cities_ljd/future_data/min/ssp245/",gcms[i],"/",j,".csv')")))
    xx2$type<-rep("ssp245",length(xx2$tasmin))
    
    eval(parse(text=paste0("xx3<-fread('/d2/home/user19/272cities_ljd/future_data/min/ssp585/",gcms[i],"/",j,".csv')")))
    xx3$type<-rep("ssp585",length(xx3$tasmin))
    
    xx4<-rbind(xx,xx2,xx3);
    eval(parse(text=paste0("xx4$gcm<-rep(gcms[",i,"],length(xx4$time))")))
    xx4$time<-as.Date(xx4$time)
    eval(parse(text=paste0("city",j,"<-rbind(city",j,",xx4)")))
    
    
  }
  
  eval(parse(text=paste0("city",j,"$year<-year(city",j,"$time)")))
  eval(parse(text=paste0("city",j,"$month<-month(city",j,"$time)")))
  eval(parse(text=paste0("city",j,"$days<-day(city",j,"$time)")))
  eval(parse(text=paste0("city",j,"<-subset(city",j,",year >=1986 &! (month==2 &days ==29))")))
  
  
  eval(parse(text=paste0("need_31 <- city",j,"$gcm == 'HadGEM3-GC31-LL' & city",j,"$month %in% c(1, 3, 5, 7, 8, 10, 12) & city",j,"$days == 30")))
  
  eval(parse(text=paste0("add_31 <- city",j,"[need_31, ]")))
  add_31$days <- 31
  add_31$time <- as.Date(paste(add_31$year, add_31$month, add_31$days, sep = "-"))
  eval(parse(text=paste0("city",j,"<-rbind(city",j,",add_31)")))
  cat(j)
}


for(i in 1:272){
  #eval(parse(text=paste("P1_",i,"<-subset(city",i,",type=='historical' | type=='ssp126')",sep="")))
  #eval(parse(text=paste0("P1_",i,"<-dcast(P1_",i,"[,c(1,2,4)],time~gcm,value.var='tasmin')")))
  
  eval(parse(text=paste("P2_",i,"<-subset(city",i,",type=='historical' | type=='ssp245')",sep="")))
  eval(parse(text=paste0("P2_",i,"<-dcast(P2_",i,"[,c(1,2,4)],time~gcm,value.var='tasmin')")))
  
  eval(parse(text=paste("P5_",i,"<-subset(city",i,",type=='historical' | type=='ssp585')",sep="")))
  eval(parse(text=paste0("P5_",i,"<-dcast(P5_",i,"[,c(1,2,4)],time~gcm,value.var='tasmin')")))
}


for(i in 1:272){
  #eval(parse(text=paste0("P1_",i,"$year<-year(P1_",i,"$time)")))
  #eval(parse(text=paste0("P1_",i,"<-subset(P1_",i,",year %in% c(2010:2100))")))
  
  eval(parse(text=paste0("P2_",i,"<-as.data.frame(P2_",i,")")))
  #eval(parse(text=paste0("P2_",i,"<-subset(P2_",i,",year %in% c(2011:2020,2031:2040,2051:2060,2091:2100))")))
  
  eval(parse(text=paste0("P5_",i,"<-as.data.frame(P5_",i,")")))
  #eval(parse(text=paste0("P5_",i,"<-subset(P5_",i,",year %in% c(2011:2020,2031:2040,2051:2060,2091:2100))")))
}

#for SSP119 ONLY
gcms<-c('CanESM5','IPSL-CM6A-LR', 'MIROC6','MRI-ESM2-0')
type<-c("historical","ssp119")


for(j in 1:272){
  eval(parse(text=paste0("city",j,"<-data.frame()")))
  for(i in seq(gcms)){
    eval(parse(text=paste("xx<-fread('/d2/home/user19/272cities_ljd/future_data/min/historical/",gcms[i],"/",j,".csv')",sep = "")))
    xx$type<-rep("historical",length(xx$tasmin))
    
    eval(parse(text=paste0("xx2<-fread('/d2/public/CMIP6/China_for_daily_tmin_and_tmax/ssp119min/",gcms[i],"/",j,".csv')")))
    xx2$type<-rep("ssp119",length(xx2$tasmin))
    
    xx4<-rbind(xx,xx2);
    eval(parse(text=paste0("xx4$gcm<-rep(gcms[",i,"],length(xx4$time))")))
    xx4$time<-as.Date(xx4$time)
    eval(parse(text=paste0("city",j,"<-rbind(city",j,",xx4)")))
    
    
  }
 
  eval(parse(text=paste0("city",j,"$year<-year(city",j,"$time)")))
  eval(parse(text=paste0("city",j,"$month<-month(city",j,"$time)")))
  eval(parse(text=paste0("city",j,"$days<-day(city",j,"$time)")))
  eval(parse(text=paste0("city",j,"<-subset(city",j,",year >=1986 &! (month==2 &days ==29))")))
  
  cat(j)
}

for(i in 1:272){
  eval(parse(text=paste("P1_",i,"<-subset(city",i,",type=='historical' | type=='ssp119')",sep="")))
  eval(parse(text=paste0("P1_",i,"<-dcast(P1_",i,"[,c(1,2,4)],time~gcm,value.var='tasmin')")))
  eval(parse(text=paste0("P1_",i,"$year<-year(P1_",i,"$time)")))
}

#########################
hismap<-readRDS("/d2/home/user19/272cities_ljd/historical_era5_temp/citiess_era5_1985-2015.rds")
hismap$cities<-as.character(hismap$cities)
hismapmin<-reshape2::dcast(hismap[,c(1,8,9)],date~cities)
hismapmin$date<-as.Date(hismapmin$date)

for(i in 1:272){
  #eval(parse(text=paste0("PC1_",i,"<-fhempel(obs=his_loc[,c(1,",i+1,")],mod=P1_",i,",add=TRUE,mult=TRUE,output='series')")))
  eval(parse(text=paste0("PC1_",i,"<-fhempel(obs=hismapmin[,c(1,",i+1,")],mod=P1_",i,",add=TRUE,mult=TRUE,output='series')")))
  eval(parse(text=paste0("PC2_",i,"<-fhempel(obs=hismapmin[,c(1,",i+1,")],mod=P2_",i,",add=TRUE,mult=TRUE,output='series')")))
  eval(parse(text=paste0("PC5_",i,"<-fhempel(obs=hismapmin[,c(1,",i+1,")],mod=P5_",i,",add=TRUE,mult=TRUE,output='series')")))
  cat(i)
}

#################
######################
#for ssp2-5
for (k in c(2,5)) {
  for (t in 1:272) {
    eval(parse(text = paste0("threshold_non",k,"_",t,"<-subset(total",k,"_",t,",year%in% c(1986:2015))")))
    eval(parse(text = paste0("threshold_non_max",k,"_",t,"<-data.table(month=c(rep(6,30),rep(7,31),rep(8,31)),day=c(1:30,1:31,1:31))")))
    eval(parse(text = paste0("threshold_non_min",k,"_",t,"<-data.table(month=c(rep(6,30),rep(7,31),rep(8,31)),day=c(1:30,1:31,1:31))")))
    
    for (j in 1:8) {
      ggmax<-ggmin<-gg1<-gg2<-vector()
      eval(parse(text = paste0("xx<-total",k,"_",t,"[,c(",j,"+1,",j,"+9,18:20,1)]")))
      z<-0
      for (m in 6:8) {
        for (d in 1:272) {
          if(m==6 & d==31){next}
          z<-z+1 
          gg1<-subset(xx,year%in% 1986:2015 & month==m &days==d)
          row_numbers <- which(rownames(xx) %in% rownames(gg1))
          row_range <- sapply(row_numbers, function(x) (x-7):(x+7))
          gg2 <- xx[unique(unlist(row_range)),]
          ggmin[z]<-quantile(gg2[,1],0.9)
          ggmax[z]<-quantile(gg2[,2],0.9)
        }
        
      }
      eval(parse(text = paste0("threshold_non_max",k,"_",t,"<-cbind(threshold_non_max",k,"_",t,",ggmax)")))
      
      eval(parse(text = paste0("threshold_non_min",k,"_",t,"<-cbind(threshold_non_min",k,"_",t,",ggmin)")))
      
    }
    
    print(t)
  }
  
}



heatwave_non <- data.frame(matrix(ncol=27, nrow=0))
colnames(heatwave_non) <- c("time","year","month",paste0(rep(c("D","N","C"),8), rep(1:8, each=3)))
heatwave_non$time<-as.Date(heatwave_non$time)


days<-2 ;periodx<-c(2010:2019,2030:2039,2050:2059,2090:2099)
for (k in c(2,5)) {
  for (t in 1:272) {
    eval(parse(text = paste0("heatwave_non",k,"_",t,"<-heatwave_non")))
    for (g in 0:3) { 
      eval(parse(text = paste0("hw",k,"_",t,"<-as.data.frame(subset(min2_1[,c(1,10,11)],year %in% periodx[1:10+10*",g,"]& month %in% c(6:8)) )")))
      
      
      for (j in 1:8) {
        eval(parse(text = paste0("thr<-cbind(threshold_non_min",k,"_",t,"[,c(1,2,",j,"+2)],threshold_non_max",k,"_",t,"[,c(",j,"+2)])")))
        #try<-total2_1[,c(j+1,j+9,18:20,1)]
        colnames(thr)[c(3:4)]<-c("min","max")
        eval(parse(text = paste0("try<-total",k,"_",t,"[,c(1,18,19,20,",j,"+1,",j,"+9)]")))
        colnames(try)[c(5:6)]<-c("rowmin","rowmax")
        eval(parse(text = paste0("try<-subset(try,year %in% periodx[1:10+",10*g,"] & month %in% c(6:8) )")))
        
        try<-left_join(try,thr,by=c("month","days"="day"))
        
     
        v1<-v2<-rep(0,length(try$max))
        for (i in 1:length(try$max)) {
          if(try$rowmax[i]>=try$max[i]) v1[i]<-1
          if(try$rowmin[i]>=try$min[i]) v2[i]<-1
        }
        
        
        v10<-v20<-v3<-rep(0,length(try$max))
        for (i in 1:length(try$max)) {
          if(v1[i]==1&v2[i]==1) v3[i]<-1
          
          if(v1[i]==1&v2[i]==0) v10[i]<-1
          
          if(v1[i]==0&v2[i]==1) v20[i]<-1
        }
        
        try$hotday<-v10
        try$hotnight<-v20
        try$mixhot<-v3
        
        s.hw.lag<-Lag(v10,c(0:(days-1)))
        s.hw.sum<-apply(s.hw.lag,1,sum)
        s.hw.sum[s.hw.sum<days]<-0
        for (i in (days:length(s.hw.sum))){
          if (s.hw.sum[i]==days){
            s.hw.sum[c((i-days+1):(i-1))]<-1
          }
        }
        s.hw.sum[s.hw.sum>0]<-1
        v11<-s.hw.sum
        v11[is.na(v11)]<-0
        try$daywave<-v11
        
        s.hw.lag<-Lag(v20,c(0:(days-1)))
        s.hw.sum<-apply(s.hw.lag,1,sum)
        s.hw.sum[s.hw.sum<days]<-0
        for (i in (days:length(s.hw.sum))){
          if (s.hw.sum[i]==days){
            s.hw.sum[c((i-days+1):(i-1))]<-1
          }
        }
        s.hw.sum[s.hw.sum>0]<-1
        v21<-s.hw.sum
        v21[is.na(v21)]<-0
        try$nightwave<-v21
        
        s.hw.lag<-Lag(v3,c(0:(days-1)))
        s.hw.sum<-apply(s.hw.lag,1,sum)
        s.hw.sum[s.hw.sum<days]<-0
        for (i in (days:length(s.hw.sum))){
          if (s.hw.sum[i]==days){
            s.hw.sum[c((i-days+1):(i-1))]<-1
          }
        }
        s.hw.sum[s.hw.sum>0]<-1
        v31<-s.hw.sum
        v31[is.na(v31)]<-0
        try$comwave<-v31
        
        eval(parse(text = paste0("colnames(try)[12:14]<-c('D",j,"','N",j,"','C",j,"')")))
        #colnames(try)[9:11]<-c('D1','N1','C1')
        
        eval(parse(text = paste0("hw",k,"_",t,"<-dplyr::bind_cols(hw",k,"_",t,",try[,c(12:14)])")))
        #hw2_1<-rowr::rbind.fill(hw2_1,try[,c(9:11)])
        
      }
      eval(parse(text = paste0("heatwave_non",k,"_",t,"<-plyr::rbind.fill(heatwave_non",k,"_",t,",hw",k,"_",t,")")))
      # heatwave_non2_1<-plyr::cbind.fill(heatwave_non2_1,hw2_1[,-1])
      
    }
    
  }
  print(k)
}

######

for (k in c(2,5)) {
  for (t in 1:272) {
    for (g in 0:3) {
      eval(parse(text = paste0("heatwave_non",k,"_",t,"_fur",g,"<-subset(heatwave_non",k,"_",t,", year %in% periodx[1:10+10*",g,"])")))
      
    }
    
  }
}

hwno_fur0_a<-hwno_fur0_b<-array(NA,dim=c(3,length(heatwave_non2_1_fur0$time),8,272),
                                dimnames=list(paste0("HW",1:3),heatwave_non2_1_fur0$year,paste0("T",1:8),paste0("city",1:272)))
hwno_fur1_a<-hwno_fur1_b<-array(NA,dim=c(3,length(heatwave_non2_1_fur1$time),8,272),
                                dimnames=list(paste0("HW",1:3),heatwave_non2_1_fur1$year,paste0("T",1:8),paste0("city",1:272)))
hwno_fur2_a<-hwno_fur2_b<-array(NA,dim=c(3,length(heatwave_non2_1_fur2$time),8,272),
                                dimnames=list(paste0("HW",1:3),heatwave_non2_1_fur2$year,paste0("T",1:8),paste0("city",1:272)))
hwno_fur3_a<-hwno_fur3_b<-array(NA,dim=c(3,length(heatwave_non2_1_fur3$time),8,272),
                                dimnames=list(paste0("HW",1:3),heatwave_non2_1_fur3$year,paste0("T",1:8),paste0("city",1:272)))

for (t in 1:272) {
  for (g in 0:3) {
    for (k in 1:8) {
      eval(parse(text = paste0("hwno_fur",g,"_a[1,,",k,",",t,"]<-heatwave_non2_",t,"_fur",g,"[,",3*k+1,"]")))
      eval(parse(text = paste0("hwno_fur",g,"_a[2,,",k,",",t,"]<-heatwave_non2_",t,"_fur",g,"[,",3*k+2,"]")))
      eval(parse(text = paste0("hwno_fur",g,"_a[3,,",k,",",t,"]<-heatwave_non2_",t,"_fur",g,"[,",3*k+3,"]")))
      
      eval(parse(text = paste0("hwno_fur",g,"_b[1,,",k,",",t,"]<-heatwave_non5_",t,"_fur",g,"[,",3*k+1,"]")))
      eval(parse(text = paste0("hwno_fur",g,"_b[2,,",k,",",t,"]<-heatwave_non5_",t,"_fur",g,"[,",3*k+2,"]")))
      eval(parse(text = paste0("hwno_fur",g,"_b[3,,",k,",",t,"]<-heatwave_non5_",t,"_fur",g,"[,",3*k+3,"]")))
      
    }
    
  }
  
}


durno_fur0_a<-durno_fur0_b<-array(NA,dim=c(3,10,8,272),dimnames = list(paste0("HW",1:3),paste0("201",0:9),paste0("T",1:8),paste0("city",1:272)))
durno_fur1_a<-durno_fur1_b<-array(NA,dim=c(3,10,8,272),dimnames = list(paste0("HW",1:3),paste0("203",0:9),paste0("T",1:8),paste0("city",1:272)))
durno_fur2_a<-durno_fur2_b<-array(NA,dim=c(3,10,8,272),dimnames = list(paste0("HW",1:3),paste0("205",0:9),paste0("T",1:8),paste0("city",1:272)))
durno_fur3_a<-durno_fur3_b<-array(NA,dim=c(3,10,8,272),dimnames = list(paste0("HW",1:3),paste0("209",0:9),paste0("T",1:8),paste0("city",1:272)))


for (k in 1:272) {
  for (i in 1:8) {
    for (j in 1:3) {
      durno_fur0_a[j,,i,k]<-tapply(hwno_fur0_a[j,,i,k],heatwave_non2_1_fur0$year,sum)
      durno_fur0_b[j,,i,k]<-tapply(hwno_fur0_b[j,,i,k],heatwave_non2_1_fur0$year,sum)
      
      durno_fur1_a[j,,i,k]<-tapply(hwno_fur1_a[j,,i,k],heatwave_non2_1_fur1$year,sum)
      durno_fur1_b[j,,i,k]<-tapply(hwno_fur1_b[j,,i,k],heatwave_non5_1_fur1$year,sum)
      
      durno_fur2_a[j,,i,k]<-tapply(hwno_fur2_a[j,,i,k],heatwave_non2_1_fur2$year,sum)
      durno_fur2_b[j,,i,k]<-tapply(hwno_fur2_b[j,,i,k],heatwave_non5_1_fur2$year,sum)
      
      durno_fur3_a[j,,i,k]<-tapply(hwno_fur3_a[j,,i,k],heatwave_non2_1_fur3$year,sum)
      durno_fur3_b[j,,i,k]<-tapply(hwno_fur3_b[j,,i,k],heatwave_non5_1_fur3$year,sum)
    }
  }
  
}


####for ssp1

for (k in c(1)) { 
  for (t in 1:272) {
    eval(parse(text = paste0("threshold_non",k,"_",t,"<-subset(total",k,"_",t,",year%in% c(1986:2015))")))
    eval(parse(text = paste0("threshold_non_max",k,"_",t,"<-data.table(month=c(rep(6,30),rep(7,31),rep(8,31)),day=c(1:30,1:31,1:31))")))
    eval(parse(text = paste0("threshold_non_min",k,"_",t,"<-data.table(month=c(rep(6,30),rep(7,31),rep(8,31)),day=c(1:30,1:31,1:31))")))
    
    for (j in 1:4) {
      ggmax<-ggmin<-gg1<-gg2<-vector()
      eval(parse(text = paste0("xx<-total",k,"_",t,"[,c(",j,"+1,",j,"+5,10:12,1)]")))
      xx$days<-day(xx$time)
      z<-0
      for (m in 6:8) {
        for (d in 1:272) {
          if(m==6 & d==31){next}
          z<-z+1 
          gg1<-subset(xx,year%in% 1986:2015 & month==m &days==d)
          row_numbers <- which(rownames(xx) %in% rownames(gg1))
          row_range <- sapply(row_numbers, function(x) (x-7):(x+7))
          gg2 <- xx[unique(unlist(row_range)),]
          ggmin[z]<-quantile(gg2[,1],0.9)
          ggmax[z]<-quantile(gg2[,2],0.9)
        }
        
      }
      eval(parse(text = paste0("threshold_non_max",k,"_",t,"<-cbind(threshold_non_max",k,"_",t,",ggmax)")))
      
      eval(parse(text = paste0("threshold_non_min",k,"_",t,"<-cbind(threshold_non_min",k,"_",t,",ggmin)")))
      
    }
    
    print(t)
  }
  
}



heatwave_non <- data.frame(matrix(ncol=15, nrow=0))
colnames(heatwave_non) <- c("time","year","month",paste0(rep(c("D","N","C"),4), rep(1:4, each=3)))
heatwave_non$time<-as.Date(heatwave_non$time)


days<-2 ;periodx<-c(2010:2019,2030:2039,2050:2059,2090:2099)
for (k in c(1)) {
  for (t in 1:272) {
    eval(parse(text = paste0("heatwave_non",k,"_",t,"<-heatwave_non")))
    for (g in 0:3) { 
      eval(parse(text = paste0("hw",k,"_",t,"<-as.data.frame(subset(min2_1[,c(1,10,11)],year %in% periodx[1:10+10*",g,"]& month %in% c(6:8)) )")))
      
      
      for (j in 1:4) {
        eval(parse(text = paste0("thr<-cbind(threshold_non_min",k,"_",t,"[,c(1,2,",j,"+2)],threshold_non_max",k,"_",t,"[,c(",j,"+2)])")))
        #try<-total2_1[,c(j+1,j+9,18:20,1)]
        colnames(thr)[c(3:4)]<-c("min","max")
        eval(parse(text = paste0("try<-total",k,"_",t,"[,c(1,10,11,",j,"+1,",j,"+5)]")))
        colnames(try)[c(4:5)]<-c("rowmin","rowmax")
        try$days<-day(try$time)
        eval(parse(text = paste0("try<-subset(try,year %in% periodx[1:10+",10*g,"] & month %in% c(6:8) )")))
        
        try<-left_join(try,thr,by=c("month","days"="day"))
        
        
        v1<-v2<-rep(0,length(try$max))
        for (i in 1:length(try$max)) {
          if(try$rowmax[i]>=try$max[i]) v1[i]<-1
          if(try$rowmin[i]>=try$min[i]) v2[i]<-1
        }
        
        
        v10<-v20<-v3<-rep(0,length(try$max))
        for (i in 1:length(try$max)) {
          if(v1[i]==1&v2[i]==1) v3[i]<-1
          
          if(v1[i]==1&v2[i]==0) v10[i]<-1
          
          if(v1[i]==0&v2[i]==1) v20[i]<-1
        }
        
        try$hotday<-v10
        try$hotnight<-v20
        try$mixhot<-v3
        
        s.hw.lag<-Lag(v10,c(0:(days-1)))
        s.hw.sum<-apply(s.hw.lag,1,sum)
        s.hw.sum[s.hw.sum<days]<-0
        for (i in (days:length(s.hw.sum))){
          if (s.hw.sum[i]==days){
            s.hw.sum[c((i-days+1):(i-1))]<-1
          }
        }
        s.hw.sum[s.hw.sum>0]<-1
        v11<-s.hw.sum
        v11[is.na(v11)]<-0
        try$daywave<-v11
        
        s.hw.lag<-Lag(v20,c(0:(days-1)))
        s.hw.sum<-apply(s.hw.lag,1,sum)
        s.hw.sum[s.hw.sum<days]<-0
        for (i in (days:length(s.hw.sum))){
          if (s.hw.sum[i]==days){
            s.hw.sum[c((i-days+1):(i-1))]<-1
          }
        }
        s.hw.sum[s.hw.sum>0]<-1
        v21<-s.hw.sum
        v21[is.na(v21)]<-0
        try$nightwave<-v21
        
        s.hw.lag<-Lag(v3,c(0:(days-1)))
        s.hw.sum<-apply(s.hw.lag,1,sum)
        s.hw.sum[s.hw.sum<days]<-0
        for (i in (days:length(s.hw.sum))){
          if (s.hw.sum[i]==days){
            s.hw.sum[c((i-days+1):(i-1))]<-1
          }
        }
        s.hw.sum[s.hw.sum>0]<-1
        v31<-s.hw.sum
        v31[is.na(v31)]<-0
        try$comwave<-v31
        
        eval(parse(text = paste0("colnames(try)[12:14]<-c('D",j,"','N",j,"','C",j,"')")))
        #colnames(try)[9:11]<-c('D1','N1','C1')
        
        eval(parse(text = paste0("hw",k,"_",t,"<-dplyr::bind_cols(hw",k,"_",t,",try[,c(12:14)])")))
        #hw2_1<-rowr::rbind.fill(hw2_1,try[,c(9:11)])
        
      }
      eval(parse(text = paste0("heatwave_non",k,"_",t,"<-plyr::rbind.fill(heatwave_non",k,"_",t,",hw",k,"_",t,")")))
      # heatwave_non1_1<-plyr::cbind.fill(heatwave_non1_1,hw2_1[,-1])
      
    }
    
  }
  print(k)
}

######

for (k in c(1)) {
  for (t in 1:272) {
    for (g in 0:3) {
      eval(parse(text = paste0("heatwave_non",k,"_",t,"_fur",g,"<-subset(heatwave_non",k,"_",t,", year %in% periodx[1:10+10*",g,"])")))
      
    }
    
  }
}


hwno_fur0_c<-array(NA,dim=c(3,length(heatwave_non1_1_fur0$time),4,272),
                   dimnames=list(paste0("HW",1:3),heatwave_non1_1_fur0$year,paste0("T",1:4),paste0("city",1:272)))
hwno_fur1_c<-array(NA,dim=c(3,length(heatwave_non1_1_fur1$time),4,272),
                   dimnames=list(paste0("HW",1:3),heatwave_non1_1_fur1$year,paste0("T",1:4),paste0("city",1:272)))
hwno_fur2_c<-array(NA,dim=c(3,length(heatwave_non1_1_fur2$time),4,272),
                   dimnames=list(paste0("HW",1:3),heatwave_non1_1_fur2$year,paste0("T",1:4),paste0("city",1:272)))
hwno_fur3_c<-array(NA,dim=c(3,length(heatwave_non1_1_fur3$time),4,272),
                   dimnames=list(paste0("HW",1:3),heatwave_non1_1_fur3$year,paste0("T",1:4),paste0("city",1:272)))

for (t in 1:272) {
  for (g in 0:3) {
    for (k in 1:4) {
      eval(parse(text = paste0("hwno_fur",g,"_c[1,,",k,",",t,"]<-heatwave_non1_",t,"_fur",g,"[,",3*k+1,"]")))
      eval(parse(text = paste0("hwno_fur",g,"_c[2,,",k,",",t,"]<-heatwave_non1_",t,"_fur",g,"[,",3*k+2,"]")))
      eval(parse(text = paste0("hwno_fur",g,"_c[3,,",k,",",t,"]<-heatwave_non1_",t,"_fur",g,"[,",3*k+3,"]")))
      
    }
    
  }
  
}


durno_fur0_c<-array(NA,dim=c(3,10,4,272),dimnames = list(paste0("HW",1:3),paste0("201",0:9),paste0("T",1:4),paste0("city",1:272)))
durno_fur1_c<-array(NA,dim=c(3,10,4,272),dimnames = list(paste0("HW",1:3),paste0("203",0:9),paste0("T",1:4),paste0("city",1:272)))
durno_fur2_c<-array(NA,dim=c(3,10,4,272),dimnames = list(paste0("HW",1:3),paste0("205",0:9),paste0("T",1:4),paste0("city",1:272)))
durno_fur3_c<-array(NA,dim=c(3,10,4,272),dimnames = list(paste0("HW",1:3),paste0("209",0:9),paste0("T",1:4),paste0("city",1:272)))



for (k in 1:272) {
  for (i in 1:4) {
    for (j in 1:3) {
      durno_fur0_c[j,,i,k]<-tapply(hwno_fur0_c[j,,i,k],heatwave_non1_1_fur0$year,sum)
      
      durno_fur1_c[j,,i,k]<-tapply(hwno_fur1_c[j,,i,k],heatwave_non1_1_fur1$year,sum)
      
      durno_fur2_c[j,,i,k]<-tapply(hwno_fur2_c[j,,i,k],heatwave_non1_1_fur2$year,sum)
      
      durno_fur3_c[j,,i,k]<-tapply(hwno_fur3_c[j,,i,k],heatwave_non1_1_fur3$year,sum)
      
    }
  }
  
}

##############################################
#Prediction
#POP
pop<-read.csv("/d2/home/user19/272cities_ljd/POP/total_city.csv")
pop<-pop[,-1]
POP<-array(NA,dim=c(3,length(2010:2100),272),
           dimnames = list(paste0("SSP",1:3),as.character(2010:2100),paste0("city",1:272)))

for (i in 1:272) {
  POP[1,,i]<-subset(pop,ssp=="SSP2"& city1==i)$value
  POP[2,,i]<-subset(pop,ssp=="SSP5"& city1==i)$value
  POP[3,,i]<-subset(pop,ssp=="SSP1"& city1==i)$value
}

#MR   MR[I,K,L] I=heatwave type;  K=disease;  L=city
MR<-readRDS("/d2/home/user19/272cities_ljd/MR_new.rds")

for(i in 1:3){
  for (k in 1:8) {
    for (l in 1:272) {
      MR[i,k,l]<-MR[i,k,l]/((sum(POP[1,4:6,l])/3+sum(POP[2,4:6,l])/3)/2)
    }
    
  }
}


#RR RR[I,J,K,L] I=heatwave type; J=mean low high; K=disease;  L=city
RR<-readRDS("/d2/home/user19/272cities_ljd/RR_new.rds")

excess_death_no_now<-
  excess_death_no_fur1<-
  excess_death_no_fur2<-
  excess_death_no_fur3<-array(NA,
                              dim = c(3,8,8,272,3,1001),
                              dimnames = list(paste0("HW",1:3),paste0("dise",1:8),
                                              paste0("T",1:8),paste0("city",1:272),
                                              c("ssp2","ssp5","ssp1"),c("est",paste0("sim",seq(1000)))))
#ssp2-5
for (i in 1:3) {#i is i*th heatwave
  for (l in 1:272) {#l is l*th city
    for (t in 1:8) {#t is t*th disease
      for (g in 1:8) {#g is g*th gcm
        excess_death_no_now[i,t,g,l,1,1]<-MR[i,t,l]*(RR[i,1,t,l]-1)*sum(durno_fur0_a[i,,g,l]*POP[1,as.character(2010:2019),l])
        excess_death_no_now[i,t,g,l,2,1]<-MR[i,t,l]*(RR[i,1,t,l]-1)*sum(durno_fur0_b[i,,g,l]*POP[2,as.character(2010:2019),l])
        
        excess_death_no_fur1[i,t,g,l,1,1]<-MR[i,t,l]* (RR[i,1,t,l]-1) *sum(durno_fur1_a[i,,g,l]*POP[1,as.character(2030:2039),l])
        excess_death_no_fur1[i,t,g,l,2,1]<-MR[i,t,l]* (RR[i,1,t,l]-1) *sum(durno_fur1_b[i,,g,l]*POP[2,as.character(2030:2039),l])
        
        excess_death_no_fur2[i,t,g,l,1,1]<-MR[i,t,l]* (RR[i,1,t,l]-1) *sum(durno_fur2_a[i,,g,l]*POP[1,as.character(2050:2059),l])
        excess_death_no_fur2[i,t,g,l,2,1]<-MR[i,t,l]* (RR[i,1,t,l]-1) *sum(durno_fur2_b[i,,g,l]*POP[2,as.character(2050:2059),l])
        
        excess_death_no_fur3[i,t,g,l,1,1]<-MR[i,t,l]* (RR[i,1,t,l]-1) *sum(durno_fur3_a[i,,g,l]*POP[1,as.character(2090:2099),l])
        excess_death_no_fur3[i,t,g,l,2,1]<-MR[i,t,l]* (RR[i,1,t,l]-1) *sum(durno_fur3_b[i,,g,l]*POP[2,as.character(2090:2099),l])
        
        set.seed(20230403+g)
        
        excess_death_no_now[i,t,g,l,1,2:1001]<-MR[i,t,l]*sum(durno_fur0_a[i,,g,l]*POP[1,as.character(2010:2019),l])* (exp(rnorm(1000,log(RR[i,1,t,l]),(log(RR[i,3,t,l])-log(RR[i,2,t,l]))/(1.96*2)))-1) 
        excess_death_no_now[i,t,g,l,2,2:1001]<-MR[i,t,l]*sum(durno_fur0_b[i,,g,l]*POP[2,as.character(2010:2019),l])* (exp(rnorm(1000,log(RR[i,1,t,l]),(log(RR[i,3,t,l])-log(RR[i,2,t,l]))/(1.96*2)))-1)  
        
        excess_death_no_fur1[i,t,g,l,1,2:1001]<-MR[i,t,l]*sum(durno_fur1_a[i,,g,l]*POP[1,as.character(2030:2039),l])* (exp(rnorm(1000,log(RR[i,1,t,l]),(log(RR[i,3,t,l])-log(RR[i,2,t,l]))/(1.96*2)))-1)  
        excess_death_no_fur1[i,t,g,l,2,2:1001]<-MR[i,t,l]*sum(durno_fur1_b[i,,g,l]*POP[2,as.character(2030:2039),l])* (exp(rnorm(1000,log(RR[i,1,t,l]),(log(RR[i,3,t,l])-log(RR[i,2,t,l]))/(1.96*2)))-1) 
        
        excess_death_no_fur2[i,t,g,l,1,2:1001]<-MR[i,t,l]*sum(durno_fur2_a[i,,g,l]*POP[1,as.character(2050:2059),l])* (exp(rnorm(1000,log(RR[i,1,t,l]),(log(RR[i,3,t,l])-log(RR[i,2,t,l]))/(1.96*2)))-1)  
        excess_death_no_fur2[i,t,g,l,2,2:1001]<-MR[i,t,l]*sum(durno_fur2_b[i,,g,l]*POP[2,as.character(2050:2059),l])* (exp(rnorm(1000,log(RR[i,1,t,l]),(log(RR[i,3,t,l])-log(RR[i,2,t,l]))/(1.96*2)))-1) 
        
        excess_death_no_fur3[i,t,g,l,1,2:1001]<-MR[i,t,l]*sum(durno_fur3_a[i,,g,l]*POP[1,as.character(2090:2099),l])* (exp(rnorm(1000,log(RR[i,1,t,l]),(log(RR[i,3,t,l])-log(RR[i,2,t,l]))/(1.96*2)))-1)  
        excess_death_no_fur3[i,t,g,l,2,2:1001]<-MR[i,t,l]*sum(durno_fur3_b[i,,g,l]*POP[2,as.character(2090:2099),l])* (exp(rnorm(1000,log(RR[i,1,t,l]),(log(RR[i,3,t,l])-log(RR[i,2,t,l]))/(1.96*2)))-1) 
        
        
      }
    }
    
  }
  print(i)
}

#ssp1
for (i in 1:3) {#i is i*th heatwave
  for (l in 1:272) {#l is l*th city
    for (t in 1:8) {#t is t*th disease
      for (g in 1:4) {#g is g*th gcm
        excess_death_no_now[i,t,g,l,3,1]<-MR[i,t,l]*(RR[i,1,t,l]-1)*sum(durno_fur0_c[i,,g,l]*POP[3,as.character(2010:2019),l])
        
        excess_death_no_fur1[i,t,g,l,3,1]<-MR[i,t,l]* (RR[i,1,t,l]-1) *sum(durno_fur1_c[i,,g,l]*POP[3,as.character(2030:2039),l])
        
        excess_death_no_fur2[i,t,g,l,3,1]<-MR[i,t,l]* (RR[i,1,t,l]-1) *sum(durno_fur2_c[i,,g,l]*POP[3,as.character(2050:2059),l])
        
        excess_death_no_fur3[i,t,g,l,3,1]<-MR[i,t,l]* (RR[i,1,t,l]-1) *sum(durno_fur3_c[i,,g,l]*POP[3,as.character(2090:2099),l])
        
        set.seed(20230403+g)
        
        excess_death_no_now[i,t,g,l,3,2:1001]<-MR[i,t,l]*sum(durno_fur0_c[i,,g,l]*POP[3,as.character(2010:2019),l])* (exp(rnorm(1000,log(RR[i,1,t,l]),(log(RR[i,3,t,l])-log(RR[i,2,t,l]))/(1.96*2)))-1) 
        
        excess_death_no_fur1[i,t,g,l,3,2:1001]<-MR[i,t,l]*sum(durno_fur1_c[i,,g,l]*POP[3,as.character(2030:2039),l])* (exp(rnorm(1000,log(RR[i,1,t,l]),(log(RR[i,3,t,l])-log(RR[i,2,t,l]))/(1.96*2)))-1)  
        
        excess_death_no_fur2[i,t,g,l,3,2:1001]<-MR[i,t,l]*sum(durno_fur2_c[i,,g,l]*POP[3,as.character(2050:2059),l])* (exp(rnorm(1000,log(RR[i,1,t,l]),(log(RR[i,3,t,l])-log(RR[i,2,t,l]))/(1.96*2)))-1)  
        
        excess_death_no_fur3[i,t,g,l,3,2:1001]<-MR[i,t,l]*sum(durno_fur3_c[i,,g,l]*POP[3,as.character(2090:2099),l])* (exp(rnorm(1000,log(RR[i,1,t,l]),(log(RR[i,3,t,l])-log(RR[i,2,t,l]))/(1.96*2)))-1)  
        
      }
    }
    
  }
  print(i)
}
############

city_death_no_now<-city_death_no_fur1<-city_death_no_fur2<-city_death_no_fur3<-array(NA,
                                                                                     dim = c(3,8,272,3,3),
                                                                                     dimnames = list(paste0("HW",1:3),paste0("dise",1:8),
                                                                                                     paste0("city",1:272),c("ssp2","ssp5","ssp1"),
                                                                                                     c("me","low","high")))
for (i in 1:3) {
  for (t in 1:8) {
    for (l in 1:272) {
      for (k in 1:3) {
        city_death_no_now[i,t,l,k,1]<-mean(excess_death_no_now[i,t,,l,k,1],na.rm=T)
        city_death_no_now[i,t,l,k,2]<-quantile(excess_death_no_now[i,t,,l,k,2:1001],0.025,na.rm=T)
        city_death_no_now[i,t,l,k,3]<-quantile(excess_death_no_now[i,t,,l,k,2:1001],0.975,na.rm=T)
        
        city_death_no_fur1[i,t,l,k,1]<-mean(excess_death_no_fur1[i,t,,l,k,1],na.rm=T)
        city_death_no_fur1[i,t,l,k,2]<-quantile(excess_death_no_fur1[i,t,,l,k,2:1001],0.025,na.rm=T)
        city_death_no_fur1[i,t,l,k,3]<-quantile(excess_death_no_fur1[i,t,,l,k,2:1001],0.975,na.rm=T)
        
        city_death_no_fur2[i,t,l,k,1]<-mean(excess_death_no_fur2[i,t,,l,k,1],na.rm=T)
        city_death_no_fur2[i,t,l,k,2]<-quantile(excess_death_no_fur2[i,t,,l,k,2:1001],0.025,na.rm=T)
        city_death_no_fur2[i,t,l,k,3]<-quantile(excess_death_no_fur2[i,t,,l,k,2:1001],0.975,na.rm=T)
        
        city_death_no_fur3[i,t,l,k,1]<-mean(excess_death_no_fur3[i,t,,l,k,1],na.rm=T)
        city_death_no_fur3[i,t,l,k,2]<-quantile(excess_death_no_fur3[i,t,,l,k,2:1001],0.025,na.rm=T)
        city_death_no_fur3[i,t,l,k,3]<-quantile(excess_death_no_fur3[i,t,,l,k,2:1001],0.975,na.rm=T)
        
      }
      
    }
    
  }
  
}


total_death_no_now<-total_death_no_fur1<-total_death_no_fur2<-total_death_no_fur3<-array(NA,
                                                                                         dim = c(3,8,3,3),
                                                                                         dimnames = list(paste0("HW",1:3),paste0("dise",1:8),
                                                                                                         c("ssp2","ssp5","ssp1"),c("me","low","high")))
for (i in 1:3) {
  for (t in 1:8) {
    for (k in 1:3) {
      for (g in 1:3) {
        total_death_no_now[i,t,k,g]<-sum(city_death_no_now[i,t,,k,g],na.rm = T)
        total_death_no_fur1[i,t,k,g]<-sum(city_death_no_fur1[i,t,,k,g],na.rm=T)
        total_death_no_fur2[i,t,k,g]<-sum(city_death_no_fur2[i,t,,k,g],na.rm = T)
        total_death_no_fur3[i,t,k,g]<-sum(city_death_no_fur3[i,t,,k,g],na.rm = T)
      }
      
    }
    
  }
  
}
total_death_no_now_melt <- reshape2::melt(total_death_no_now)
total_death_no_now_dcast <- reshape2::dcast(total_death_no_now_melt, Var1 + Var2 +Var3 ~  Var4)
colnames(total_death_no_now_dcast) <- c("HW", "disease", "ssp", "est", "low", "high")

total_death_no_fur1_melt <- reshape2::melt(total_death_no_fur1)
total_death_no_fur1_dcast <- reshape2::dcast(total_death_no_fur1_melt, Var1 + Var2 +Var3 ~  Var4)
colnames(total_death_no_fur1_dcast) <- c("HW", "disease", "ssp", "est", "low", "high")

total_death_no_fur2_melt <- reshape2::melt(total_death_no_fur2)
total_death_no_fur2_dcast <- reshape2::dcast(total_death_no_fur2_melt, Var1 + Var2 +Var3 ~  Var4)
colnames(total_death_no_fur2_dcast) <- c("HW", "disease", "ssp", "est", "low", "high")

total_death_no_fur3_melt <- reshape2::melt(total_death_no_fur3)
total_death_no_fur3_dcast <- reshape2::dcast(total_death_no_fur3_melt, Var1 + Var2 +Var3 ~  Var4)
colnames(total_death_no_fur3_dcast) <- c("HW", "disease", "ssp", "est", "low", "high")

total_death_no<-rbind(total_death_no_now_dcast,total_death_no_fur1_dcast,total_death_no_fur2_dcast,total_death_no_fur3_dcast)
total_death_no$time<-c(rep("2010s",72),rep("2030s",72),rep("2050s",72),rep("2090s",72))

disease<-c("All-causes","CVD","CHD","Stroke","Hstroke","Istroke","RD","COPD")
names(disease) <- unique(total_death_no$disease)
total_death_no$disease <- disease[total_death_no$disease]
total_death_no$disease <-factor(total_death_no$disease,levels=c("All-causes","CVD","CHD","Stroke","Hstroke","Istroke","RD","COPD"))
x_HW <- c(1:3);names(x_HW) <- c("DHW","NHW","CHW")
total_death_no$X <- x_HW[total_death_no$HW]
total_death_no$X[total_death_no$ssp == "ssp2"] <- total_death_no$X[total_death_no$ssp == "ssp1"] +3.5
total_death_no$X[total_death_no$ssp == "ssp5"] <- total_death_no$X[total_death_no$ssp == "ssp2"] +3.5

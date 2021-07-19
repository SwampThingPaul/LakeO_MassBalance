## 
## Lake Okeechobee Mass Balance
##
## Code was compiled by Paul Julian
## contact info: pjulian@sccf.org


## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape2)
library(openxlsx)

## Paths
wd="C:/Julian_LaCie/_Github/LakeO_MassBalance"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]


# -------------------------------------------------------------------------
WYs=seq(1979,2021,1)
# dates=date.fun(c("1978-05-01","2020-05-01"))
dates=date.fun(c("1978-05-01","2021-05-01"))


# stage -------------------------------------------------------------------
# lakeO.stg=DBHYDRO_daily(dates[1],dates[2],"00268")
# lakeO.stg$Date.EST=date.fun(lakeO.stg$Date)
# lakeO.stg$WY=WY(lakeO.stg$Date.EST)
# lakeO.stg$month=as.numeric(format(lakeO.stg$Date.EST,"%m"))
# lakeO.stg$CY=as.numeric(format(lakeO.stg$Date.EST,"%Y"))
# 
# plot(Data.Value~Date.EST,lakeO.stg)

# Discharge ---------------------------------------------------------------
flow.dbkeys=read.xlsx(paste0(data.path,"discharge/LakeO_DBKEYS_V3.xlsx"),sheet=1)

flow.dat=data.frame()
for(i in 1:nrow(flow.dbkeys)){
  tmp=DBHYDRO_daily(dates[1],dates[2],flow.dbkeys$DBKEY[i])
  tmp$DBKEY=as.character(flow.dbkeys$DBKEY[i])
  flow.dat=rbind(tmp,flow.dat)
  print(paste(i,": ",flow.dbkeys$DBKEY[i]))
}
# write.csv(flow.dat,paste0(data.path,"discharge/WY1979_2020_dailyQ.csv"),row.names=F)

flow.data=merge(flow.dat,flow.dbkeys[,c("DBKEY","STRUCT","ALIAS","Priority","Basin","Inflow_direction","Outflow","WQSite")],"DBKEY")
flow.data$WY=WY(flow.data$Date)

flow.xtab=data.frame(reshape::cast(flow.data,Date+WY+STRUCT+ALIAS+Inflow_direction+Outflow+Basin+WQSite~Priority,value="Data.Value",fun.aggregate=function(x) ifelse(sum(x,na.rm=T)==0,NA,sum(x,na.rm=T))))
flow.xtab$Date.EST=date.fun(flow.xtab$Date)

test=reshape2::dcast(flow.data,Date+WY+STRUCT+ALIAS+Inflow_direction+Outflow+Basin+WQSite~Priority,value.var="Data.Value",fun.aggregate=function(x) sum(x,na.rm=T))

flow.xtab$fflow.cfs=with(flow.xtab,ifelse(is.na(P1),P2,P1));#if only two priorities
flow.xtab$fflow.cfs=with(flow.xtab,fflow.cfs*Inflow_direction)#all inflows are positive and all negative values are outflow
flow.xtab$direct=with(flow.xtab,ifelse(fflow.cfs<0,"Outflow","Inflow"))
flow.xtab$month=as.numeric(format(flow.xtab$Date,"%m"))
flow.xtab$CY=as.numeric(format(flow.xtab$Date,"%Y"))

mon.seq=data.frame(Date.EST=date.fun(seq(dates[1],dates[2],"1 months")))
mon.seq$month=as.numeric(format(mon.seq$Date.EST,"%m"))
mon.seq$CY=as.numeric(format(mon.seq$Date.EST,"%Y"))
mon.seq$WY=WY(mon.seq$Date.EST)

# Sanity Check
test=reshape2::dcast(flow.xtab,STRUCT+ALIAS+Basin+WQSite+WY+CY+month~direct,value.var="fflow.cfs",fun.aggregate=function(x) N.obs(x))
subset(test,Outflow>31)
subset(test,Inflow>31)

flow.mon.sum=reshape2::dcast(flow.xtab,STRUCT+ALIAS+Basin+WQSite+WY+CY+month~direct,value.var="fflow.cfs",fun.aggregate=function(x) sum(abs(x),na.rm=T))
flow.mon.sum=flow.mon.sum[,c("STRUCT", "ALIAS", "Basin", "WQSite", "WY", "CY", "month","Inflow", "Outflow")]

# Estimate flow for L31E, HP7 & Inflow 1,2,3
# Uncertainty is high using this method due to Q monitoring at C41H78, 
# see _docs/LOOP_C41H78_Justification_Final.docx
L61E_HP7=data.frame()
for(i in 1:nrow(mon.seq)){
  tmp.dat=subset(flow.mon.sum,month==mon.seq$month[i]&CY==mon.seq$CY[i])
  C41H78=if(nrow(subset(tmp.dat,ALIAS=="C41H78_I"))==0){data.frame(Inflow=0)}else{subset(tmp.dat,ALIAS=="C41H78_I")}
  G76=if(nrow(subset(tmp.dat,ALIAS=="G76_C"))==0){data.frame(Inflow=0)}else{subset(tmp.dat,ALIAS=="G76_C")}
  G207=if(nrow(subset(tmp.dat,ALIAS=="G207"))==0){data.frame(Outflow=0)}else{subset(tmp.dat,ALIAS=="G207")}
  S71=if(nrow(subset(tmp.dat,ALIAS=="S71_S"))==0){data.frame(Inflow=0)}else{subset(tmp.dat,ALIAS=="S71_S")}
  L61E_HP7.Q=C41H78$Inflow-((G207$Outflow*-1)+G76$Inflow+S71$Inflow)
  L61E_HP7.Q=ifelse(L61E_HP7.Q<0,0,L61E_HP7.Q)
  final.dat=data.frame(STRUCT="L61E_HP7",ALIAS="L61E_HP7",Basin="N",WQSite="L61E_HP7",WY=mon.seq$WY[i],CY=mon.seq$CY[i],month=mon.seq$month[i],Inflow=L61E_HP7.Q,Outflow=0)
  L61E_HP7=rbind(L61E_HP7,final.dat)
  print(i)
}
plot(L61E_HP7$Inflow)

flow.mon.sum2=rbind(subset(flow.mon.sum,ALIAS!="C41H78_I"),L61E_HP7)

WY.Tflow=ddply(flow.mon.sum,"WY",summarise,inflow.Q.cfs=sum(Inflow,na.rm=T),outflow.Q.cfs=sum(Outflow,na.rm=T))

# Sanity Check
reshape2::dcast(flow.mon.sum,WY~STRUCT,value.var = "Inflow",fun.aggregate = function(x)N.obs(x))

flow.mon.sum1=reshape2::dcast(flow.xtab,STRUCT+ALIAS+Basin+WQSite+WY+CY+month~direct,value.var="fflow.cfs",fun.aggregate=function(x) sum(abs(cfs.to.acftd(x)),na.rm=T))
WY.Tflow.in=reshape2::dcast(flow.mon.sum1,WY~STRUCT,value.var = "Inflow",sum)
WY.Tflow.in$TFlow=rowSums(WY.Tflow.in[,-1],na.rm=T)
EAA.struct=c("S352","S2.S351","S3.S354","S351_TEMP","S352_TEMP","S354_TEMP","S4")
WY.Tflow.in$EAA_flow=rowSums(WY.Tflow.in[,EAA.struct],na.rm=T)
WY.Tflow.in$S308_per=with(WY.Tflow.in,S308/TFlow)*100
WY.Tflow.in$EAA_per=with(WY.Tflow.in,EAA_flow/TFlow)*100

# S308 Backflow percent of total inflow
mean(subset(WY.Tflow.in,WY%in%seq(2012,2021,1))$S308)
range(subset(WY.Tflow.in,WY%in%seq(2012,2021,1))$S308)
mean(subset(WY.Tflow.in,WY%in%seq(2012,2021,1))$S308_per)
plot(S308_per~WY,WY.Tflow.in)
# EAA Backflow Percent
mean(subset(WY.Tflow.in,WY%in%seq(2012,2021,1))$EAA_flow)
range(subset(WY.Tflow.in,WY%in%seq(2012,2021,1))$EAA_flow)
mean(subset(WY.Tflow.in,WY%in%seq(2012,2021,1))$EAA_per)
range(subset(WY.Tflow.in,WY%in%seq(2012,2021,1))$EAA_per)
plot(EAA_per~WY,WY.Tflow.in)

test=melt(subset(WY.Tflow.in,WY==2021)[,1:42],id.var="WY")
with(subset(test,value>0),pie(value,labels=variable))
# Water Quality -----------------------------------------------------------
wq.sites=ddply(flow.dbkeys,"WQSite",summarise,N.val=N.obs(ALIAS))

wq.param=data.frame(Test.Number=c(16,18,20,21,23,25,80,89,100),
                    Param=c("TSS","NOx","NH4","TKN","OP","TP","TN","DOC","TOC"))
wq.param=subset(wq.param,!(Param%in%c("TSS","NH4","DOC","TOC","OP")))
wq.dat=data.frame()
for(i in 1:nrow(wq.sites)){
  tmp=DBHYDRO_WQ(dates[1],dates[2],wq.sites$WQSite[i],wq.param$Test.Number)
  wq.dat=rbind(tmp,wq.dat)
  print(paste0(i,": ",wq.sites$WQSite[i]))
}

wq.dat=merge(wq.dat,wq.param,"Test.Number")
unique(wq.dat$Collection.Method)
wq.dat=subset(wq.dat,Collection.Method=="G") # Grab sample only 
wq.dat$month=as.numeric(format(wq.dat$Date.EST,"%m"))
wq.dat$CY=as.numeric(format(wq.dat$Date.EST,"%Y"))
wq.dat$WY=WY(wq.dat$Date.EST)
N.TP=ddply(subset(wq.dat,Param=="TP"),c("WY","Station.ID"),summarise,N.val=N.obs(HalfMDL),mean.val=mean(HalfMDL,na.rm=T))
N.TP

# checking for more than one sample per day
# test=cast(subset(wq.dat,Project.Code!="LAB"),Station.ID+Date.EST~Param,value="HalfMDL",fun.aggregate = function(x)N.obs(x))
# subset(test,TP>2)
# subset(wq.dat,Param=="TP"&Station.ID=="S65E"&Date.EST==date.fun("1990-06-18"))

# Daily mean WQ removing LAB project code.
wq.dat.xtab=dcast(subset(wq.dat,Project.Code!="LAB"),Station.ID+Date.EST~Param,value.var="HalfMDL",mean,na.rm=T)
wq.dat.xtab$TN=with(wq.dat.xtab,TN_Combine(NOx,TKN,TN))
head(wq.dat.xtab)


ALIAS.vals=ddply(flow.dbkeys,"ALIAS",summarise,N.val=N.obs(DBKEY))
date.fill.frame=expand.grid(Date.EST=date.fun(seq(dates[1],dates[2],"1 days")),
                            ALIAS=ALIAS.vals$ALIAS)
flow.vars=c("STRUCT","ALIAS","WQSite","Basin","Date.EST","WY","direct","fflow.cfs")
wq.vars=c("Date.EST","Station.ID","TP","TN")

flow.wq=merge(flow.xtab[,flow.vars],wq.dat.xtab[,wq.vars],by.x=c("Date.EST","WQSite"),by.y=c("Date.EST","Station.ID"),all.x=T)
head(flow.wq)
flow.wq=merge(date.fill.frame,flow.wq,c("Date.EST","ALIAS"),all.y=T)
flow.wq=flow.wq[order(flow.wq$ALIAS,flow.wq$Date.EST),]

# ddply(flow.wq,c("ALIAS","STRUCT",'WQSite'),summarise,N.val=N.obs(TP))
flow.wq$TP.int=with(flow.wq,ave(TP,ALIAS,FUN = function(x)dat.interp(x)))
flow.wq$TPLoad.kg=with(flow.wq,Load.Calc.kg(abs(fflow.cfs),TP.int))
flow.wq$TN.int=with(flow.wq,ave(TN,ALIAS,FUN = function(x)dat.interp(x)))
flow.wq$TNLoad.kg=with(flow.wq,Load.Calc.kg(abs(fflow.cfs),TN.int))

# plot(fflow.cfs~Date.EST,subset(flow.wq,STRUCT=="CU10"))
# plot(TPLoad.kg~Date.EST,subset(flow.wq,STRUCT=="CU10"))
# plot(TNLoad.kg~Date.EST,subset(flow.wq,STRUCT=="CU10"))

flow.wq$month=as.numeric(format(flow.wq$Date.EST,"%m"))
flow.wq$CY=as.numeric(format(flow.wq$Date.EST,"%Y"))

TPload.mon.sum=dcast(flow.wq,STRUCT+ALIAS+WQSite+Basin+WY+CY+month~direct,value.var="TPLoad.kg",fun.aggregate=function(x) sum(x,na.rm=T))
TPload.mon.sum=TPload.mon.sum[,c("STRUCT", "ALIAS","WQSite", "Basin", "WY", "CY", "month","Inflow", "Outflow")]

TNload.mon.sum=dcast(flow.wq,STRUCT+ALIAS+WQSite+Basin+WY+CY+month~direct,value.var="TNLoad.kg",fun.aggregate=function(x) sum(x,na.rm=T))
TNload.mon.sum=TNload.mon.sum[,c("STRUCT", "ALIAS","WQSite", "Basin", "WY", "CY", "month","Inflow", "Outflow")]

# Estimate load for L31E, HP7 & Inflow 1,2,3
# overly complicated and highly variable estimates of flow and load due to
# shallow open channel discharge (see _docs/LOOP_C41H78_Justification_Final.docx)
L61E_HP7.TPload=data.frame()
for(i in 1:nrow(mon.seq)){
  tmp.dat=subset(TPload.mon.sum,month==mon.seq$month[i]&CY==mon.seq$CY[i])
  C41H78=if(nrow(subset(tmp.dat,ALIAS=="C41H78_I"))==0){data.frame(Inflow=0)}else{subset(tmp.dat,ALIAS=="C41H78_I")}
  G76=if(nrow(subset(tmp.dat,ALIAS=="G76_C"))==0){data.frame(Inflow=0)}else{subset(tmp.dat,ALIAS=="G76_C")}
  G207=if(nrow(subset(tmp.dat,ALIAS=="G207"))==0){data.frame(Outflow=0)}else{subset(tmp.dat,ALIAS=="G207")}
  S71=if(nrow(subset(tmp.dat,ALIAS=="S71_S"))==0){data.frame(Inflow=0)}else{subset(tmp.dat,ALIAS=="S71_S")}
  L61E_HP7.load.val=C41H78$Inflow-((G207$Outflow*-1)+G76$Inflow+S71$Inflow)
  L61E_HP7.load.val=ifelse(L61E_HP7.load.val<0,0,L61E_HP7.load.val)
  final.dat=data.frame(STRUCT="L61E_HP7",ALIAS="L61E_HP7",WQSite=NA,Basin="N",WY=mon.seq$WY[i],CY=mon.seq$CY[i],month=mon.seq$month[i],Inflow=L61E_HP7.load.val,Outflow=0)
  L61E_HP7.TPload=rbind(L61E_HP7.TPload,final.dat)
  print(i)
}
plot(L61E_HP7.TPload$Inflow)
TPload.mon.sum2=rbind(subset(TPload.mon.sum,ALIAS!="C41H78_I"),L61E_HP7.TPload)

# WY.TNLoad=ddply(TNload.mon.sum,"WY",summarise,inflow.load=sum(Inflow,na.rm=T),outflow.load=sum(Outflow,na.rm=T))
WY.TPLoad=ddply(TPload.mon.sum,"WY",summarise,inflow.load=sum(Inflow,na.rm=T),outflow.load=sum(Outflow,na.rm=T))
WY.TPLoad=subset(WY.TPLoad,WY%in%WYs)

##
WY.TPLoad.in=reshape2::dcast(TPload.mon.sum,WY~STRUCT,value.var = "Inflow",sum)
WY.TPLoad.in$TPLoad=rowSums(WY.TPLoad.in[,-1],na.rm=T)
WY.TPLoad.in$EAA_TPLoad=rowSums(WY.TPLoad.in[,EAA.struct],na.rm=T)

WY.TPLoad.in$S308_TPper=with(WY.TPLoad.in,S308/TPLoad)*100
WY.TPLoad.in$EAA_TPper=with(WY.TPLoad.in,EAA_TPLoad/TPLoad)*100

# S308 Backflow percent of total inflow
kg.to.mt(mean(subset(WY.TPLoad.in,WY%in%seq(2012,2021,1))$S308))
kg.to.mt(range(subset(WY.TPLoad.in,WY%in%seq(2012,2021,1))$S308))
mean(subset(WY.TPLoad.in,WY%in%seq(2012,2021,1))$S308_TPper)
range(subset(WY.TPLoad.in,WY%in%seq(2012,2021,1))$S308_TPper)
plot(S308_TPper~WY,WY.TPLoad.in)
# EAA Backflow Percent
kg.to.mt(mean(subset(WY.TPLoad.in,WY%in%seq(2012,2021,1))$EAA_TPLoad))
kg.to.mt(range(subset(WY.TPLoad.in,WY%in%seq(2012,2021,1))$EAA_TPLoad))
mean(subset(WY.TPLoad.in,WY%in%seq(2012,2021,1))$EAA_TPper)
range(subset(WY.TPLoad.in,WY%in%seq(2012,2021,1))$EAA_TPper)
plot(EAA_TPper~WY,WY.TPLoad.in)


##
WY.TNLoad.in=reshape2::dcast(TNload.mon.sum,WY~STRUCT,value.var = "Inflow",sum)
WY.TNLoad.in$TNLoad=rowSums(WY.TNLoad.in[,-1],na.rm=T)
WY.TNLoad.in$EAA_TNLoad=rowSums(WY.TNLoad.in[,EAA.struct],na.rm=T)

WY.TNLoad.in$S308_TNper=with(WY.TNLoad.in,S308/TNLoad)*100
WY.TNLoad.in$EAA_TNper=with(WY.TNLoad.in,EAA_TNLoad/TNLoad)*100

# S308 Backflow percent of total inflow
kg.to.mt(mean(subset(WY.TNLoad.in,WY%in%seq(2012,2021,1))$S308))
kg.to.mt(range(subset(WY.TNLoad.in,WY%in%seq(2012,2021,1))$S308))
mean(subset(WY.TNLoad.in,WY%in%seq(2012,2021,1))$S308_TNper)
range(subset(WY.TNLoad.in,WY%in%seq(2012,2021,1))$S308_TNper)
plot(S308_TNper~WY,WY.TNLoad.in)
# EAA Backflow Percent
kg.to.mt(mean(subset(WY.TNLoad.in,WY%in%seq(2012,2021,1))$EAA_TNLoad))
kg.to.mt(range(subset(WY.TNLoad.in,WY%in%seq(2012,2021,1))$EAA_TNLoad))
mean(subset(WY.TNLoad.in,WY%in%seq(2012,2021,1))$EAA_TNper)
range(subset(WY.TNLoad.in,WY%in%seq(2012,2021,1))$EAA_TNper)
plot(EAA_TNper~WY,WY.TNLoad.in)

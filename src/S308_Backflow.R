## 
## S308 Backflow
##
##
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
library(zoo)

library(lubridate)

## Paths
wd="C:/Julian_LaCie/_Github/LakeO_MassBalance"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]


# -------------------------------------------------------------------------
dates=date.fun(c("1979-05-01","2021-07-30"))
dbkeys=data.frame(ALIAS=c("S308","S308"),SITE=c("S308DS","S308"),DBKEY=c("15626","DJ239"),Priority=c("P1","P2"))

S308.Q=data.frame()
for(i in 1:nrow(dbkeys)){
tmp=DBHYDRO_daily(dates[1],dates[2],dbkeys$DBKEY[i])
tmp$DBKEY=as.character(dbkeys$DBKEY[i])
S308.Q=rbind(tmp,S308.Q)
print(i)
}
S308.Q=merge(S308.Q,dbkeys,"DBKEY")
flow.xtab=reshape2::dcast(S308.Q,Date+ALIAS~Priority,value.var="Data.Value",mean)
flow.xtab$fflow.cfs=with(flow.xtab,ifelse(is.na(P1),P2,P1));#if only two priorities

flow.xtab$WY=WY(flow.xtab$Date)
flow.xtab$month=as.numeric(format(flow.xtab$Date,"%m"))
flow.xtab$CY=as.numeric(format(flow.xtab$Date,"%Y"))
flow.xtab$direct=with(flow.xtab,ifelse(fflow.cfs<0,"Backflow","Flow"))

S308.Q.xtab=reshape2::dcast(flow.xtab,CY+month~direct,value.var = "fflow.cfs",fun.aggregate = function(x) sum(cfs.to.acftd(abs(x)),na.rm=T))
S308.Q.xtab$perBack=with(S308.Q.xtab,ifelse(Backflow>0&Flow==0,100,ifelse(Backflow==0&Flow==0,0,(Backflow/Flow)*100)))
range(S308.Q.xtab$perBack)

S308.Q.xtab$perBack.cat=as.factor(findInterval(S308.Q.xtab$perBack,c(0,10,25,50,75,100,200,180000)))
S308.Q.xtab=merge(S308.Q.xtab,
                 data.frame(perBack.cat=c(NA,1:7),
                            perBack.cat.txt=c("<NA>","0 - 10","10-25","25 - 50","50 - 75","75 - 100","100 - 200",">200")),
                 "perBack.cat")
S308.Q.xtab=S308.Q.xtab[order(S308.Q.xtab$CY,S308.Q.xtab$month),]
tail(S308.Q.xtab)

sum(with(subset(S308.Q.xtab,CY%in%seq(2012,2021,1)),Backflow>Flow))/nrow(subset(S308.Q.xtab,CY%in%seq(2012,2021,1)))
sum(with(subset(S308.Q.xtab,CY%in%seq(2012,2021,1)),perBack>50))

# png(filename=paste0(plot.path,"S308Backflow/S308_BF_20182021.png"),width=6.5,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1,0.25,1),oma=c(2,2,2,0.25),lwd=0.5);

xlim.val=date.fun(c("2018-01-01","2021-07-01"));xmaj=seq(xlim.val[1],xlim.val[2],"1 years");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
ylim.val=c(-45e3,90e3);by.y=45e3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

x=barplot(subset(S308.Q.xtab,CY%in%seq(2018,2021,1))$Flow,ylim=ylim.val,xpd=F,space=0,col=NA,border=NA,axes=F,ann=F,xaxs="i")
abline(h=ymaj,v=x[seq(1,length(x),12)],lty=1,col=adjustcolor("grey",0.5))
barplot(subset(S308.Q.xtab,CY%in%seq(2018,2021,1))$Flow,ylim=ylim.val,xpd=F,add=T,space=0,xaxs="i",col=adjustcolor("dodgerblue1",0.5),axes=F,ann=F)
barplot(subset(S308.Q.xtab,CY%in%seq(2018,2021,1))$Backflow*-1,ylim=ylim.val,xpd=F,add=T,space=0,xaxs="i",col=adjustcolor("indianred1",0.5),axes=F,ann=F)
abline(h=0,lwd=1)
axis_fun(1,x[seq(1,length(x),12)],x,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj/1000);box(lwd=1)
mtext(side=1,line=1.5,"Date (Month-Year)")
mtext(side=2,line=2,"Discharge (x1000 Ac-Ft Month\u207B\u00B9)")
mtext(side=3,line=1,"S308 Monthly Discharge",adj=0)
mtext(side=3,"Negative Discharge is Backflow to Lake Okeechobee",adj=0,cex=0.75,col="grey50")
dev.off()

library(ggplot2)
cols.wes=rev(wesanderson::wes_palette("Zissou1",7,"continuous"))
cols.vir=rev(viridis::viridis(7))
cols=c("1"=cols.vir[1],"2"=cols.vir[2],"3"=cols.vir[3],"4"=cols.vir[4],"5"=cols.vir[5],"6"=cols.vir[6],"7"=cols.vir[7])

BF_POR=ggplot(S308.Q.xtab, aes(x = month, y = CY, fill = perBack.cat)) +
  geom_tile(aes(group = perBack.cat), colour = 'black')+
  scale_y_reverse(expand = c(0, 0), breaks = S308.Q.xtab$CY) +
  # scale_x_discrete(expand = c(0, 0), position = 'top') +
  scale_x_continuous(expand = c(0, 0), breaks = 1:12)+
  scale_fill_manual(values = cols,
                    name="Percent Backflow\n(Percent)",
                    breaks=1:7,
                    labels=c("< 10","10 - 25","25 - 50","50 - 75","75 - 100","100 - 200",">200")) +
  theme_bw() +
  theme(
    #legend.position = 'none',
    text=element_text(family="serif"),
    plot.title=element_text(size=12),
    plot.subtitle = element_text(color = "grey50",size=8),
    plot.caption = element_text(hjust = 0)
  )+
  labs(title = "S308 Backflow Summary",
       subtitle= "Percent of negative discharge compared to positive discharge at S308",
       caption = paste0("Produced: ",format(Sys.Date(),"%d %b %Y")),
       x="Month",
       y="Year")
BF_POR
# ggsave(paste0(plot.path,"S308Backflow/S308_BF_POR.png"),BF_POR,device="png",height =7,width=5,units="in")
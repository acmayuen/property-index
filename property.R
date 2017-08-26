library(zoo,quietly=T,warn.conflicts = F)     
library(readxl,quietly=T,warn.conflicts=F)
library(ggplot2,quietly=T,warn.conflicts=F)
library(tidyr,quietly=T,warn.conflicts=F)
library(dplyr,quietly=T,warn.conflicts=F)
library(lubridate,quietly=T,warn.conflicts=F)
library(data.table,quietly=T,warn.conflicts=F) 
library(viridis,quietly=T,warn.conflicts=F) 
library(scales,quietly=T,warn.conflicts=F)
library(animation,quietly = T,warn.conflicts = F)

### Load data and transform from wide to long data frame 
df<-read.csv("Documents/property/ppd2.csv")
df.state<-df %>% gather(geo,hpi,-date) %>% mutate(type="state")

df.state<-
  df.state %>% group_by(geo) %>% 
  mutate(hpa=hpi/shift(hpi,12)-1,
         hpilag12=shift(hpi,12,fill=NA),
         hpimax12=zoo::rollmax(hpi,13,align="right",fill=NA),
         hpimin12=-zoo::rollmax(-hpi,13,align="right",fill=NA)) %>% ungroup()

dlist<-unique(filter(df.state,year(date)>1999)$date)

### Plot function
myplot1 <- function (i) {
  g<-
    ggplot(data=filter(df.state,
                       date==dlist[i]),
           aes(x=hpi, y=geo, label=paste(" ",geo,""),
               color=hpa,hjust=ifelse(hpa>0,0,1)))+
    geom_text() +
    geom_point()+
    scale_x_log10(limits=c(50,750), breaks=c(75,100,150,200,250,350,400,600,800))+
    geom_segment(aes(xend=hpimin12,x=hpimax12,y=geo,yend=geo),alpha=0.7)+
    theme_minimal()  +
    theme(legend.position="bottom",
          axis.text.y=element_blank(),
          legend.key.width=unit(4,"line"),
          panel.grid.major.y=element_blank(),
          plot.caption=element_text(hjust=0),
          plot.title=element_text(face="bold",size=18))+
    scale_color_viridis(end=0.85,option="C",limits=c(-.8,.8),
                        name="12個月指數      \n百分比變化     \n",
                        label=percent)+
    labs(y="", x="售價指數 (1999年7月 =100)",
         title="香港私人住宅各類單位售價指數(各區)(自2000年起)",
         subtitle=as.character(dlist[i],format="%B, %Y"),
         caption="資料來源:差餉物業估價署網站平均售價資料 後期合併計算: Andrew Yuen@mlact.com with RStudio")
  return(g)
}

N<-length(dlist)
myplot1(N)

### Render GIF file
oopt = ani.options(interval = 0.25)
saveGIF({for (i in seq(1,N,1)) {
  g<-myplot1(i)
  print(g)
  print(paste(i,"out of",N))
  ani.pause()}
  for (i in 1:5)
  {print(g)
    print(i)}
},movie.name="HK HPI.gif",ani.width = 600, ani.height = 400)

### ALL1
df.state$year<-year(df.state$date)
df.state$month<-month(df.state$date)
df.state$date<-as.Date(df.state$date)
df.state<-as.data.table(df.state)

g2<-
  ggplot(data=df.state[year>1999], aes(x=date,y=hpi))+
  theme_minimal()+
  scale_x_date(labels= date_format("%y"),date_breaks="4 year",
               limits = as.Date(c('2000-01-01','2017-06-30'))) +
  scale_y_log10(limits=c(50,800), breaks=c(50,75,100,125,150,200,250,350,500,700))+
  geom_line(color="black")  +
  facet_wrap(~geo, ncol=5) +
  theme(plot.title=element_text(face="bold",size=12))+
  theme(plot.caption=element_text(hjust=0))+
  xlab("")+ylab("售價指數 log scale")+
  labs(caption="資料來源:差餉物業估價署網站平均售價資料 後期合併計算: Andrew Yuen@mlact.com with RStudio",
       subtitle="2000年1月至2017年6月",
       title="香港私人住宅各類單位售價指數(各區)(自2000年起 1999年7月 = 100)")
png(filename="Documents/property/HK all1.png",width = 1200,height = 800)
g2
dev.off()

####################################################
png(filename="Documents/property/HK all2.png",width = 1200,height = 800)

df.state$datemon <- month(ymd(df.state$date), label = TRUE, abbr = FALSE)
devData <- subset(df.state, geo=="HK.E", select=c(geo, year, datemon, hpi))
ggplot(df.state[year>1999],aes(year,hpi,color=geo)) +
  geom_point(data=devData,size=I(4),alpha=I(1)) + 
  theme_grey(base_size=15) +
  theme(legend.title = element_blank(), legend.position="none", axis.title.y=element_blank(),axis.text.x=element_blank()) + 
  ggtitle("香港私人住宅各類單位售價指數(各區)(1999年7月 = 100)") + facet_grid(. ~datemon) + 
  xlab(paste("年份: 2000 to", max(df.state$year))) +
  labs(caption="資料來源:差餉物業估價署網站平均售價資料 \n後期合併計算: Andrew Yuen@mlact.com with RStudio")

dev.off()
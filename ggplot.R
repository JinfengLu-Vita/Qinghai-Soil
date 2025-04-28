install.packages("ggbreak")
install.packages("ggthemes")
install.packages("gg.gap")
install.packages("viridisLite")
install.packages("colorspace")
install.packages("tidyverse")

library(ggplot2)
library(gg.gap)
library(ggthemes)
library(ggbreak)
library(ggsci)
library(RColorBrewer)
library(patchwork)
library(viridis)
library(viridisLite)
library(tidyverse)
#opar<-par(no.readonly = TRUE)#保存系统当前的环境
#par(c("usr","fin"))#查看当前参数值
#par("pin")
#par(fin=c(6,5))
#par(opar)#还原环境

c1=viridis(5,option = "C")
c1
c2=inferno(8)
c2
"#000004FF" "#56106EFF" "#BB3754FF" "#F98C0AFF" "#FCFFA4FF"
c3=plasma(8)
c3
"#0D0887FF" "#7E03A8FF" "#CC4678FF" "#F89441FF" "#F0F921FF""#FEBC2AFF" "#F0F921FF"
"#0D0887FF" "#5402A3FF" "#8B0AA5FF" "#B93289FF" "#DB5C68FF" "#F48849FF""#FEBC2AFF" "#F0F921FF"
c4=magma(8)
c4
"#000004FF" "#231151FF" "#5F187FFF" "#982D80FF" "#D3436EFF" "#F8765CFF""#FEBA80FF" "#FCFDBFFF"
#VADeaths[,c(1:17)]<-as.numeric(unlist(VADeaths[,c(1:17)]))
mydata
#mydata[,c(1:17)]<-as.numeric(unlist(mydata[,c(1:17)]))
#mydata<-within(mydata,{
  group<-factor(group,levels=c("CK3","CK4","CK55","CK66","BB3","BB4","BB55","BB66"))
})
mydata<-within(mydata,{
  pathway<-factor(pathway,levels = c("Denitrification","DNRA","Nitrification","ANRA"))
})
#options(repr.plot.width=1,repr.plot.height=3)
#help("theme_bw")
#theme(panel.grid.major =element_blank(),panel.grid.minor = element_blank(), panel.background = element_blank())
p1=ggplot(mydata,aes(group,abundance,fill=factor(day)))+
  geom_bar(stat="identity",position=position_dodge(0.8),width = 0.8)+
  #theme(axis.text = element_text(size=8))+ #注释横纵坐标名称和字号，axis.text.x 仅注释横坐标
  #scale_color_viridis_c(option = "C")+
  scale_x_discrete(limits=c("CK","BB"
                            ))+
  scale_fill_npg("nrc",alpha = 0.9)+
  #scale_fill_manual("#BC3C2999")
  #scale_color_brewer(palette = "Set1")+
  #scale_x_continuous(limits = c(0,1.5),expand = c(0,0))+
  #xlim (0,180)+
  #scale_x_continuous(limits=c(0,180),
                     #breaks = c(6,16,26,36,46,56,66,76,86,96,106,116,126,136,146,156,166,176),
                     #labels = c("sh","ubiD","ubiE","pht5","nidA","nidB","phdF","phdG","nidD","phdI","pht4","pht3","pht2","phtAa","phtB","phtC","phdE","phdk"))+
  facet_grid(cols = vars(pathway))+ #按照pathway分面
  scale_y_continuous(limits=c(0,0.6),expand = c(0,0))+ #去掉与x轴间隙
  geom_text(aes(label=label),size=1.5,vjust=-2,position = position_dodge(0.8))+
  geom_errorbar(aes(ymin=abundance-sd,ymax=abundance+sd),
                width=0.8,size=0.2,position = position_dodge(0.8))
p1=p1+theme_classic()
#p1=p1+scale_color_brewer(palette = "Set1")
#p1=p1+theme(axis.line.x.bottom = )
p1
  #horiz=T
  #theme_economist(base_size=14)+
  #scale_fill_economist()+
  #theme(axis.ticks.length=unit(0.5,'cm'))
  #guides(fill=guide_legend(title=NULL))+
  #ggtitle("The Financial Performance of Five Giant")+
  #theme(axis.title = element_blank(),legend.position='none')+ 
  #facet_grid(Year~.)+
#gg.gap(plot=p1,segments = list(c(50,100),c(600,1000)),rel_heights= c(0.25,0,0.1,0.1),tick_widh=c(1,1,1),ylim=c(0,2400))#gg.gap截断，不能操作横坐标
#p3=coord_flip(p2)
#p2<-p1+scale_x_cut(breaks=c(50,600),which=c(1,0.1),scales = c(1,0.1),space=0.05)
p2<-p1+
  scale_x_break(c(0.004,0.008),scales = 0.1)
  #scale_x_break(c(600,1000),scales =0.6)
p2
ggsave(filename = "gene1.pdf",
       width = 4,
       height = 5,
       limitsize = FALSE,
       dpi = 300)
#par(pin=c(4,1))

#pdf("gene.pdf",height = 6,width = 4)
#print("gene.pdf")
#dev.off()
#?par#查看全局变量


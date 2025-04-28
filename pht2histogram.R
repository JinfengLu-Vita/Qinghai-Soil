install.packages("ggbreak")
install.packages("ggthemes")
install.packages("gg.gap")
install.packages("ggsci")
install.packages("devtools")
install.packages("dplyr")
install.packages("forcats")
devtools::install_github("thomasp85/patchwork")
library(ggplot2)
library(gg.gap)
library(ggthemes)
library(ggbreak)
library(RColorBrewer)
library(patchwork)
library(ggsci)
library(dplyr)
library(forcats)  # 用于分组排序
#opar<-par(no.readonly = TRUE)#保存系统当前的环境
#par(c("usr","fin"))#查看当前参数值
#par("pin")
#par(fin=c(6,5))
#par(opar)#还原环境
mydata<-read.csv("D:/R upload/bubble/N2O Months.csv",sep=',')
#VADeaths[,c(1:17)]<-as.numeric(unlist(VADeaths[,c(1:17)]))
mydata
#mydata[,c(1:17)]<-as.numeric(unlist(mydata[,c(1:17)]))
mydata<-within(mydata,{
  month<-factor(month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"
  ))
})
#options(repr.plot.width=1,repr.plot.height=3)
#help("theme_bw")
#theme(panel.grid.major =element_blank(),panel.grid.minor = element_blank(), panel.background = element_blank())
 p1=ggplot(mydata,aes(month,abundance,fill=factor(site)))+
   geom_col(aes(fill=group),position = 'dodge',width = 0.6)+
   geom_bar(stat="identity",position=position_dodge(0.9),width = 0.9)+
   #theme(axis.text = element_text(size=8))+ #注释横纵坐标名称和字号，axis.text.x 仅注释横坐标
   #scale_fill_brewer(palette = "Paired")+ #RcolorBrewer配色
   #scale_y_discrete(limits=c('Polycyclic aromatic hydrocarbon degradation','Benzoate degradation','Phthalate degradation','Benzoyl-CoA degradation','Naphthalene degradation',
   #'Assimilatory nitrate reduction','Nitrogen fixation','Nitrification','Urea cycle'
   #))+
   #scale_y_continuous(limits = c(0,4),expand = c(0,0))+
   #xlim (0,180)+
   #scale_x_continuous(limits=c(0,180),
   #breaks = c(6,16,26,36,46,56,66,76,86,96,106,116,126,136,146,156,166,176),
   #labels = c("sh","ubiD","ubiE","pht5","nidA","nidB","phdF","phdG","nidD","phdI","pht4","pht3","pht2","phtAa","phtB","phtC","phdE","phdk"))+
   #scale_y_continuous(limits=c(0,12),expand = c(0,0)) #去掉与x轴间隙
   #geom_text(aes(label=label),size=2,hjust=-4,position = position_dodge(0.9))+
   #geom_errorbar(aes(ymin=abundance-sd,ymax=abundance+sd),
                # width=0.9,size=0.2,position = position_dodge(0.9))
 p1=p1+theme_classic()
 p1=p1+scale_fill_tron() #ggsci配色
#' #p1=p1+theme(axis.line.x.bottom = )
 p1


 mydata1<-read.csv("D:/R upload/bubble/CYP116 0412.csv",sep=',')
# mydata<-within(mydata1,{
#   group<-factor(group,levels=c('CK','BB','Ps','BP'
#   ))
# })
p2=ggplot(mydata1,aes(group,abundance,fill=factor(group)))+
  geom_bar(stat="identity",position=position_dodge(0.7),width = 0.7)+
  scale_x_discrete(limits=c('CK','BB','Ps','BP'))+
  scale_y_continuous(limits = c(0,12.5),expand = c(0,0))+
  geom_text(aes(label=label),size=2,vjust=-4,position = position_dodge(0.7))+
  geom_errorbar(aes(ymin=abundance-sd,ymax=abundance+sd),
                width=0.7,size=0.2,position = position_dodge(0.7))
p2=p2+theme_classic()
p2=p2+scale_fill_tron()
p2
p1 + p2
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

#facet_wrap将图分为不同的面板，每个基因一个面板
# 读取数据
data <- read.csv("D:/R upload/bubble/N2O Months.csv")

# 转换分组，确保按逻辑顺序排列
data$site <- factor(data$site, levels = unique(data$site))
# 确保 month 是一个有序因子，并按自然月份排序
data$month <- factor(data$month, levels=c("Jan","Feb","Mar","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
# 按基因和分组绘制直方图，并添加误差线

ggplot(data, aes(x = site, y = concentration, fill = site)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  #geom_errorbar(aes(ymin = abundance - sd, ymax = abundance + sd), 
                #width = 0.2, position = position_dodge(width = 0.9)) +
  #scale_y_continuous(limits = c(7.80000e-08, 9.60000e-08))+  # 设置 y 轴范围
  facet_wrap(~month, scales = "free_y") +
  labs(title = "N2O concentration in different months",
       x = "site",
       y = "concentration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #scale_y_continuous(limits = function(x) c(min(data$min_concentration[data$month == unique(x)]), NA))  # 动态设置每个面板的最小纵坐标值
  #scale_y_continuous(limits = c(5.7 e-09, 9.6e-08))  # 自动设置最小值
  



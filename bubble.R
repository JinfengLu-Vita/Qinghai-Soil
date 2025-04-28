library(reshape2)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(patchwork)
library(ggsci)
mydata<-read.csv("D:/R upload/bubble/C family.csv",sep=',')
head(mydata)
#mydata1<-melt(mydata)
#head(mydata1)
ggplot(mydata,aes(family,group,size= abundance, color=family))+
  geom_point()+
  scale_x_discrete(limits=c('unclassified_p__Acidobacteria','Nocardiaceae','Mycobacteriaceae','Nocardioidaceae','Pseudonocardiaceae',
                            'Micrococcaceae','Geodermatophilaceae','unclassified_p__Candidatus_Rokubacteria','unclassified_p__Chloroflexi','unclassified_o__Thermomicrobiales',
                            'unclassified_c__Dehalococcoidia','unclassified_p__Nitrospinae','unclassified_o__Hyphomicrobiales','Xanthobacteraceae','unclassified_c__Betaproteobacteria',
                            'unclassified_c__Alphaproteobacteria','Comamonadaceae','unclassified_c__Deltaproteobacteria','unclassified_o__Burkholderiales','Acetobacteraceae'

                            ))+
  scale_y_discrete(limits=c('CK1','BB1','Ps1','BP1','CK2','BB2','Ps2','BP2','CK3','BB3','Ps3','BP3','CK4','BB4','Ps4','BP4',
                            'CK5','BB5','Ps5','BP5','CK6','BB6','Ps6','BP6','CK7','BB7','Ps7','BP7','CK8','BB8','Ps8','BP8',
                            'CK9','BB9','Ps9','BP9','CK10','BB10','Ps10','BP10','CK11','BB11','Ps11','BP11','CK12','BB12','Ps12','BP12',
                            'CK13','BB13','Ps13','BP13','CK14','BB14','Ps14','BP14','CK15','BB15','Ps15','BP15','CK16','BB16','Ps16','BP16'
))+
  theme(panel.background = element_blank(),#初始的灰色背景变成透明
        panel.grid.major = element_line(colour = "gray"),#网格线条变成灰色
        panel.border = element_rect(colour = "black",fill = NA),#调整边框为黑色
        axis.ticks = element_blank())+ #隐去刻度线，只能同时隐去x轴和y轴
  #scale_radius(range = c(2.6))#设置点大小范围
  #coord_fixed(ratio = 3/2)+
  theme(axis.text = element_text(size=8),
        axis.text.x = element_text(angle = 90))
  #scale_fill_tron()
        #plot.margin=unit(rep(0.5,3),'cm')) #注释横纵坐标名称和字号，axis.text.x 仅注释横坐标
  #scale_fill_brewer(palette = "Blues")+
  #theme_bw()
ggsave(filename = "genus1.pdf",
       width = 4,
       height = 5,
       limitsize = FALSE,
       dpi = 300)
  
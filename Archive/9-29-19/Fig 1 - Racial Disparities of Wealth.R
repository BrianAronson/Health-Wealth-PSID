# install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device="win")


#0 - Set directory
    library(ggplot2)
    setwd("C:/Users/briarons/Desktop/Analysis - Data/PSID")
    # setwd("C:/Users/admin/Desktop/Sociology/PSID Data")

#1 - Read and subset data
    I<-readRDS("PreppedData2.rds")
    I$eq_wealth<-I$eq_wealth/1000
    I<-I[order(I$year,decreasing = T),]
    I<-I[!(duplicated(I$ind_id)),]
    I$ind_id<-NULL
    I3 <- I2
    tm<-as.data.frame(table(I3$ind_id[I3$year>2004 & I3$year<2012 & I3$already_died==0]))
    rec.ids<-tm$Var1[tm$Freq>1]
    I3<-I3[I3$ind_id%in% rec.ids,]
    I3<-I3[I3$year==2011,]
    
#2 - Further format data
    df<-I3[,c("eq_wealth","ego_black2")]
    df<-df[df$eq_wealth<500 & df$eq_wealth>-100,]
    df$ego_black2<-ifelse(df$ego_black2==0,"White   ","Black   ")
    df$ego_black2<-factor(df$ego_black2,levels = sort(unique(df$ego_black2),decreasing = F))

#3 - plot data
    windowsFonts(A = windowsFont("Times New Roman"))
    png(file="C:/Users/briarons/Desktop/Temp - Figures/Racial Disparities in Net Worth.png", height=4, width=7,units = "in",res = 300)
      ggplot()+
        geom_density(data = df, aes(x = eq_wealth,fill=ego_black2),alpha=1,color="white")+
        scale_fill_grey(start = 0, end = .8)+
        theme_bw()+
        geom_density(data = df[df$ego_black2=="Black   ",], aes(x = eq_wealth),alpha=1,fill="black")+
        geom_density(data = df[df$ego_black2=="White   ",], aes(x = eq_wealth),alpha=.6,fill="grey90")+
        labs(x="Wealth (Thousands USD)", y="Density",fill="")+
        ggtitle("Figure 1 - Racial Disparities in Net Worth")+
        scale_x_continuous(labels=scales::dollar_format(),breaks = c(0,100,200,300,400,500))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            legend.text=element_text(size=12),
            text=element_text(family="A"),
            legend.title=element_text(size=12,face="bold"),
            title = element_text(size=12,face="bold"),
            legend.position="bottom",
            axis.text.x = element_text(size=11.5),
             # axis.text.y = element_blank(),
            axis.text.y = element_text(size=11.5),
            axis.title.x= element_text(size=12,face="bold",margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.title.y= element_text(size=13.5,face="bold",margin = margin(t = 0, r = 15, b = 0, l = 0)),
            strip.text.x = element_text(size = 14))
      dev.off()



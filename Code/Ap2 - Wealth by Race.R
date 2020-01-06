# install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device="win")


#0 - Set directory
    library(ggplot2)
    setwd("C:/Users/bda13/Desktop/Sociology/Statistics/PSID/")
    # setwd("C:/Users/admin/Desktop/Sociology/PSID Data")

#1 - read data
    I<-readRDS("PreppedData2.rds")
    I$eq_wealth<-I$eq_wealth/1000

    I<-I[order(I$year,decreasing = T),]
    I<-I[!(duplicated(I$ind_id)),]
    I$ind_id<-NULL
    
    
    #a - Prep data
        df<-I[,c("eq_wealth","ego_black2")]
        df<-df[df$eq_wealth<500 & df$eq_wealth>-100,]
        df$ego_black2<-ifelse(df$ego_black2==0,"White   ","Black   ")
        df$ego_black2<-factor(df$ego_black2,levels = sort(unique(df$ego_black2),decreasing = F))

    #b - plot data
        windowsFonts(A = windowsFont("Times New Roman"))

# 1 - Health outcomes by race
    #a - create vars
          propfun<-function(x,debt=F){
            x<-x/I$eq_assets
            x<-ifelse(x<0,0,x)
            x<-ifelse(is.na(x),0,x)
            x<-ifelse(is.infinite(x),1,x)
            if(debt==T){
              x<-ifelse(x>2,0,x)
            }else{
              x<-ifelse(x>1,0,x)
            }
            return(x)
          }
          I$peq_home<-propfun(I$eq_home)
          I$peq_debt<-propfun(I$eq_debt,debt=T)
          I$peq_stock<-propfun(I$eq_stock)
          I$peq_savings<-propfun(I$eq_savings)
        
    #a - get descriptive statistics about both racial groups
            vars<-c("h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","h_distress","ego_black2","ego_age","ego_female","year2","just_died","already_died","inc_total","ego_dg_highschool","ego_dg_somecollege","ego_dg_bachelors","ego_dg_advanced","eq_wealth","eq_home_d", "peq_home","peq_savings","peq_stock","peq_debt")
            I2<-I
        #i - Create dataframe with most recent info
            I2<-I2[order(I2$year,decreasing = T),]
            I2<-I2[,c(vars)]
        #ii - subset data
            I3<-I2[I2$ego_black2==0,]
            I4<-I2[I2$ego_black2==1,]
        #iii - Table of all variables:
                sm1<-as.data.frame(matrix(0,ncol(I3),5))
                names(sm1)<-c("variable","mean","median","sd","max")
                for(i in 1:length(I3)){
                  try({
                    sm1$variable[i]<-names(I3)[i]
                    sm1$mean[i]<-mean(I3[,i],na.rm = T)
                    sm1$median[i]<-median(I3[,i],na.rm = T)
                    sm1$sd[i]<-sd(I3[,i],na.rm = T)
                    sm1$max[i]<-max(I2[,i],na.rm = T)
                  })
                }
                sm2<-as.data.frame(matrix(0,ncol(I4),5))
                names(sm2)<-c("variable","mean","median","sd","max")
                for(i in 1:length(I4)){
                  try({
                    sm2$variable[i]<-names(I4)[i]
                    sm2$mean[i]<-mean(I4[,i],na.rm = T)
                    sm2$median[i]<-median(I4[,i],na.rm = T)
                    sm2$sd[i]<-sd(I4[,i],na.rm = T)
                    sm2$max[i]<-max(I2[,i],na.rm = T)
                  })
                }
                sm<-rbind(sm1,sm2)


# 2 - Wealth Components by race

    #b - visualize
        #subset to stats of interest
            vars<-c("eq_home_d", "peq_home","peq_savings","peq_stock","peq_debt")
            df<-sm[sm$variable %in% vars,]
        #rename variables
            nvars<-c("Own Home","Home Equity","Savings","Stocks","Debt")
            df$variable<-rep(nvars,2)
        #add race
            df$Race<-rep(c("White   ","Black   "),each=5)
        #remove median
            df$median<-NULL
        #rename variables
            names(df)<-c("Wealth Component","Mean","SD","Max","Race")
        #keep wealth order
            df$`Wealth Component`<-factor(df$`Wealth Component`,levels = df$`Wealth Component`[1:5])
        #create graph
            png(file="C:/Users/bda13/Desktop/Racial Disparities in Wealth Components.png", height=4, width=7,units = "in",res = 300)
            ggplot(data = df, aes(x = `Wealth Component`, y = Mean))+
                  geom_bar(aes(fill = as.factor(Race)), position = "dodge", stat="identity",width = .75)+
                  scale_fill_grey(start = 0, end = .75)+
                  theme_bw()+
                  labs(x="Wealth Component", y="Mean",fill="")+
                  ggtitle("Racial Disparities in Wealth Components")+
                  scale_y_continuous(labels=scales::percent)+
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
            
# install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device="win")


#0 - Set directory
    library(ggplot2)
    setwd("C:/Users/bda13/Desktop/Sociology/Statistics/PSID/")
    # setwd("C:/Users/admin/Desktop/Sociology/PSID Data")

#1 - Prep
    #a - read data
        I<-readRDS("PreppedData2.rds")
        I$eq_wealth<-I$eq_wealth/1000
    #b - Prep data
        I<-I[order(I$year,decreasing = T),]
        I<-I[!(duplicated(I$ind_id)),]
        I$ind_id<-NULL
        df<-I[,c("eq_wealth","ego_black2")]
        df<-df[df$eq_wealth<500 & df$eq_wealth>-100,]
        df$ego_black2<-ifelse(df$ego_black2==0,"White   ","Black   ")
        df$ego_black2<-factor(df$ego_black2,levels = sort(unique(df$ego_black2),decreasing = F))
    #c - load font
        windowsFonts(A = windowsFont("Times New Roman"))

# 2 - Get descriptive stats
    #a - pick variables
        vars<-c("h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","h_distress","ego_black2","ego_age","ego_female","year2","just_died","already_died","inc_total","ego_dg_highschool","ego_dg_somecollege","ego_dg_bachelors","ego_dg_advanced","eq_wealth","eq_home_d", "peq_home","peq_savings","peq_stock","peq_debt")
        I2<-I
    #b - Create dataframe with most recent info
        I2<-I2[order(I2$year,decreasing = T),]
        I2<-I2[,c(vars)]
    #c - subset data
        I3<-I2[I2$ego_black2==0,]
        I4<-I2[I2$ego_black2==1,]
    #d - Table of all variables:
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

      I3<-I
# 3 - predict values:
  #a - prep models
      vars<-c("h_general","h_conditions","h_activities")
      results<-list()
      m1<-list()
      m2<-list()
      m3<-list()
      m4<-list()
      m5<-list()
      m6<-list()
      m7<-list()
      for(i in 1:length(vars)){
        #a - Select variable
          I3$y<-I3[,vars[i]]
        #b - Run models
          m1[[i]]<-lmer(form1,I3)
          m2[[i]]<-lmer(form2,I3)
          m3[[i]]<-lmer(form2b,I3)
          m4[[i]]<-lmer(form3,I3)
          m5[[i]]<-lmer(form3b,I3)
          m6[[i]]<-lmer(form4,I3)
          m7[[i]]<-lmer(form4b,I3)
          print(i)
      }
      
  #b - Create baseline data to predict
      #grab frist row of data
          temp<-I[1,]
      #subset to vars of interest
          corder<-c("ego_black2","ego_age","ego_age2","ego_female","ego_black2:ego_age","ego_black2:ego_age2","fam_region","year2","just_died","already_died","log_income","ego_dg_highschool","ego_dg_somecollege","ego_dg_bachelors","ego_dg_advanced","ego_black2:log_income", "ego_black2:ego_dg_highschool","ego_black2:ego_dg_somecollege","ego_black2:ego_dg_bachelors","ego_black2:ego_dg_advanced","log_wealth","ego_black2:log_wealth","eq_home_d", "peq_home","peq_savings","peq_stock","peq_debt","ego_black2:eq_home_d","ego_black2:peq_home", "ego_black2:peq_savings", "ego_black2:peq_stock","ego_black2:peq_debt","(Intercept)")         
          temp<-temp[,names(temp) %in% c(corder,"ind_id","fam_id_68")]
      #hope that ind_id and fam_id aren't outliers, and set means/intuitive values for everything else
          temp$ego_age<-0
          temp$ego_age2<-0
          temp$ego_female<-0
          temp$ego_black2<-0
          temp$year2<-1
          temp$just_died<-0
          temp$already_died<-0
          temp$log_income<-log10(60000)
          temp$log_wealth<-log10(250000)
          temp$ego_dg_highschool<-0
          temp$ego_dg_somecollege<-0
          temp$ego_dg_bachelors<-1
          temp$ego_dg_advanced<-0
          temp$eq_home_d<-1
          temp$peq_home<-mean(I$peq_home,na.rm = T)/2
          temp$peq_debt<-mean(I$peq_debt,na.rm = T)/2
          temp$peq_stock<-mean(I$peq_stock,na.rm = T)/2
          temp$peq_savings<-mean(I$peq_savings,na.rm = T)/2
      #determine variables to manipulate
          black<-c(0,1)
          ages<-c((30-27):(80-27))
      #duplicate rows by number of variables to predict
          lengths<-length(black)*length(ages)
          temp<-temp[rep(1,lengths),]
      #replace key values with tested values
          temp$ego_black2<-rep(black,each=length(ages))
          temp$ego_age<-rep(ages,length(black))
          temp$ego_age2<-temp$ego_age^2

  #c - create predictions for each model and outcome
    pds1<-predict(m3[[1]],temp,allow.new.levels=T)
    pds2<-predict(m5[[1]],temp,allow.new.levels=T)
    pds3<-predict(m7[[1]],temp,allow.new.levels=T)

    #merge results with new df
      df<-temp
    #duplicate rows by number of variables to predict
      df<-df[rep(1:nrow(df),3),]
      df$Model<-rep(c("Model 2b (educ)","Model 3b (wealth)","Model 4b (compo)"),each=(nrow(df)/3))
      df$`Predicted Values`<-c(pds1,pds2,pds3)
    #Format data
      df$Age<-df$ego_age+27
      df$Race<-ifelse(df$ego_black2==0,"White","Black")
    #Subset to fewer ages
      df2<-df[df$Age>=50,]
    #plot
      png(file="C:/Users/bda13/Desktop/Mixed Effects - Self-Rated Health.png", height=7, width=11,units = "in",res = 200)
        ggplot(data = df2, aes(x = Age, y = `Predicted Values`,color=Race,group=Race))+
          geom_smooth(size=3,se=F)+
          scale_color_grey(start = 0, end = .75)+
          theme_bw()+
          labs(x="Age", y="Self-Rated Health",color="",group="")+
          ggtitle("Mixed Effects - Self-Rated Health")+
          # scale_y_continuous(limits=c(2.8,4.5))+
          scale_x_continuous(limits=c(48,80))+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              legend.text=element_text(size=21),
              legend.title=element_text(size=24),
              title = element_text(size=30),
              legend.position="bottom",
              axis.text.x = element_text(size=21, hjust = 1),
              # axis.text.y = element_blank(),
              axis.text.y = element_text(size=21),
              axis.title.x= element_text(size=24,margin = margin(t = 20, r = 0, b = 0, l = 0)),
              axis.title.y= element_text(size=24,margin = margin(t = 0, r = 20, b = 0, l = 0)),
              strip.text.x = element_text(size = 24),
              panel.spacing = unit(1, "lines"))+
          facet_wrap(~Model)
      dev.off()
    #          
              
              
    #          
              
    #c - create predictions for each model and outcome
      pds1<-predict(m3[[2]],temp,allow.new.levels=T)
      pds2<-predict(m5[[2]],temp,allow.new.levels=T)
      pds3<-predict(m7[[2]],temp,allow.new.levels=T)
      #merge results with new df
        df<-temp
      #duplicate rows by number of variables to predict
        df<-df[rep(1:nrow(df),3),]
        df$Model<-rep(c("Model 2b (educ)","Model 3b (wealth)","Model 4b (compo)"),each=(nrow(df)/3))
        df$`Predicted Values`<-c(pds1,pds2,pds3)
      #Format data
        df$Age<-df$ego_age+27
        df$Race<-ifelse(df$ego_black2==0,"White","Black")
      #plot
        png(file="C:/Users/bda13/Desktop/Mixed Effects - Health Conditions.png", height=7, width=11,units = "in",res = 200)
          ggplot(data = df, aes(x = Age, y = `Predicted Values`,color=Race,group=Race))+
            geom_smooth(size=3,se=F)+
            scale_color_grey(start = 0, end = .75)+
            theme_bw()+
            labs(x="Age", y="Health Conditions",color="",group="")+
            ggtitle("Mixed Effects - Health Conditions")+
            # scale_y_continuous(limits=c(2.8,4.5))+
            scale_x_continuous(limits=c(28,80))+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"),
                legend.text=element_text(size=21),
                legend.title=element_text(size=24),
                title = element_text(size=30),
                legend.position="bottom",
                axis.text.x = element_text(size=21, hjust = 1),
                # axis.text.y = element_blank(),
                axis.text.y = element_text(size=21),
                axis.title.x= element_text(size=24,margin = margin(t = 20, r = 0, b = 0, l = 0)),
                axis.title.y= element_text(size=24,margin = margin(t = 0, r = 20, b = 0, l = 0)),
                strip.text.x = element_text(size = 24),
                panel.spacing = unit(1, "lines"))+
            facet_wrap(~Model)
        dev.off()

              
              
              
              
              
        #c - create predictions for each model and outcome
          pds1<-predict(m3[[3]],temp,allow.new.levels=T)
          pds2<-predict(m5[[3]],temp,allow.new.levels=T)
          pds3<-predict(m7[[3]],temp,allow.new.levels=T)
          #merge results with new df
            df<-temp
          #duplicate rows by number of variables to predict
            df<-df[rep(1:nrow(df),3),]
            df$Model<-rep(c("Model 2b (educ)","Model 3b (wealth)","Model 4b (compo)"),each=(nrow(df)/3))
            df$`Predicted Values`<-c(pds1,pds2,pds3)
          #Format data
            df$Age<-df$ego_age+27
            df$Race<-ifelse(df$ego_black2==0,"White","Black")
          #Subset to fewer ages
            df2<-df[df$Age>=45,]
          #plot
            png(file="C:/Users/bda13/Desktop/Mixed Effects - Health Limitations.png", height=7, width=11,units = "in",res = 200)
              ggplot(data = df2, aes(x = Age, y = `Predicted Values`,color=Race,group=Race))+
                geom_smooth(size=3,se=F)+
                scale_color_grey(start = 0, end = .75)+
                theme_bw()+
                labs(x="Age", y="Health Limitations",color="",group="")+
                ggtitle("Mixed Effects - Health Limitations")+
                # scale_y_continuous(limits=c(2.8,4.5))+
                scale_x_continuous(limits=c(43,80))+
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    legend.text=element_text(size=21),
                    legend.title=element_text(size=24),
                    title = element_text(size=30),
                    legend.position="bottom",
                    axis.text.x = element_text(size=21, hjust = 1),
                    # axis.text.y = element_blank(),
                    axis.text.y = element_text(size=21),
                    axis.title.x= element_text(size=24,margin = margin(t = 20, r = 0, b = 0, l = 0)),
                    axis.title.y= element_text(size=24,margin = margin(t = 0, r = 20, b = 0, l = 0)),
                    strip.text.x = element_text(size = 24),
                    panel.spacing = unit(1, "lines"))+
                facet_wrap(~Model)
            dev.off()
              
              
              
              
              
              
              
              
            
            
  #4 - health conditions by equity
      #b - Create baseline data to predict
          #grab frist row of data
              temp<-I[1,]
          #subset to vars of interest
              temp<-temp[,names(temp) %in% c(corder,"ind_id","fam_id_68")]
          #hope that ind_id and fam_id aren't outliers, and set means/intuitive values for everything else
              temp$ego_age<-50
              temp$ego_age2<-50^2
              temp$ego_female<-0
              temp$ego_black2<-0
              temp$year2<-max(I$year2,na.rm = T)
              temp$just_died<-0
              temp$already_died<-0
              temp$log_income<-log10(60000)
              temp$log_wealth<-log10(250000)
              temp$ego_dg_highschool<-0
              temp$ego_dg_somecollege<-0
              temp$ego_dg_bachelors<-1
              temp$ego_dg_advanced<-0
              temp$eq_home_d<-1
              temp$peq_home<-mean(I$peq_home,na.rm = T)/2
              temp$peq_debt<-mean(I$peq_debt,na.rm = T)/2
              temp$peq_stock<-mean(I$peq_stock,na.rm = T)/2
              temp$peq_savings<-mean(I$peq_savings,na.rm = T)/2
          #determine variables to manipulate
              black<-c(0,1)
              home<-c(0:100/100)
          #duplicate rows by number of variables to predict
              lengths<-length(black)*length(home)
              temp<-temp[rep(1,lengths),]
          #replace key values with tested values
              temp$ego_black2<-rep(black,each=length(home))
              temp$peq_home<-rep(home,length(black))
        #c - create predictions for each model and outcome
          pds1<-predict(m3[[2]],temp,allow.new.levels=T)
          pds2<-predict(m4[[2]],temp,allow.new.levels=T)
          pds3<-predict(m7[[2]],temp,allow.new.levels=T)
          #merge results with new df
            df<-temp
          #duplicate rows by number of variables to predict
            df<-df[rep(1:nrow(df),2),]
            df$Model<-rep(c("Model 3a (wealth)","Model 4b (compo)"),each=(nrow(df)/2))
            df$`Predicted Values`<-c(pds2,pds3)
          #Format data
            df$`Home Equity`<-df$peq_home
            df$Race<-ifelse(df$ego_black2==0,"White","Black")
          #plot
            png(file="C:/Users/bda13/Desktop/Mixed Effects - Health Conditions by Home Equity.png", height=7, width=11,units = "in",res = 200)
              ggplot(data = df, aes(x = `Home Equity`, y = `Predicted Values`,color=Race,group=Race))+
                geom_smooth(size=3,se=F)+
                scale_color_grey(start = 0, end = .75)+
                theme_bw()+
                labs(x="Home Equity", y="Health Conditions",color="",group="")+
                ggtitle("Mixed Effects - Health Conditions by Home")+
                scale_y_continuous(limits=c(1.5,2.5))+
                # scale_x_continuous(limits=c(28,80))+
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    legend.text=element_text(size=21),
                    legend.title=element_text(size=24),
                    title = element_text(size=30),
                    legend.position="bottom",
                    axis.text.x = element_text(size=21, hjust = 1),
                    # axis.text.y = element_blank(),
                    axis.text.y = element_text(size=21),
                    axis.title.x= element_text(size=24,margin = margin(t = 20, r = 0, b = 0, l = 0)),
                    axis.title.y= element_text(size=24,margin = margin(t = 0, r = 20, b = 0, l = 0)),
                    strip.text.x = element_text(size = 24),
                    panel.spacing = unit(1, "lines"))+
                facet_wrap(~Model)
            dev.off()

              

              
              
              


# png(file="C:/Users/bda13/Desktop/Predicted Model Outcomes by Race and Wealth Component.png", height=4, width=7,units = "in",res = 300)
# geom_bar(aes(fill = as.factor(Race)), position = "dodge", stat="identity",width = .75)+
#   scale_fill_grey(start = 0, end = .75)+
#   theme_bw()+
#   labs(x="Wealth Component", y="Mean",fill="")+
#   ggtitle("Racial Disparities in Wealth Components")+
#   scale_y_continuous(labels=scales::percent)+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"),
#         legend.text=element_text(size=12),
#         text=element_text(family="A"),
#         legend.title=element_text(size=12,face="bold"),
#         title = element_text(size=12,face="bold"),
#         legend.position="bottom",
#         axis.text.x = element_text(size=11.5),
#         # axis.text.y = element_blank(),
#         axis.text.y = element_text(size=11.5),
#         axis.title.x= element_text(size=12,face="bold",margin = margin(t = 10, r = 0, b = 0, l = 0)),
#         axis.title.y= element_text(size=13.5,face="bold",margin = margin(t = 0, r = 15, b = 0, l = 0)),
#         strip.text.x = element_text(size = 14))

            

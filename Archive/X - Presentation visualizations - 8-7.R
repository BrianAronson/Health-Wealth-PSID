# 0 - Wealth densities by race
    #a - Prep data
        df<-I[,c("eq_wealth","ego_black2")]
        df<-df[df$eq_wealth<500 & df$eq_wealth>-100,]
        df$ego_black2<-ifelse(df$ego_black2==0,"White   ","Black   ")
        df$ego_black2<-factor(df$ego_black2,levels = sort(unique(df$ego_black2),decreasing = F))
        
    #b - plot data
        png(file="C:/Users/admin/Desktop/Racial Disparities in Net Worth.png", height=8.5, width=11,units = "in",res = 200)
          ggplot()+  
            geom_density(data = df, aes(x = eq_wealth,fill=ego_black2),alpha=1,color="white")+
            scale_fill_grey(start = 0, end = .8)+
            theme_bw()+
            geom_density(data = df[df$ego_black2=="Black   ",], aes(x = eq_wealth),alpha=1,fill="black")+
            geom_density(data = df[df$ego_black2=="White   ",], aes(x = eq_wealth),alpha=.6,fill="grey80")+
            labs(x="Wealth (Thousands USD)", y="Density",fill="")+
            ggtitle("Racial Disparities in Net Worth")+
            scale_x_continuous(labels=scales::dollar_format(),breaks = c(0,100,200,300,400,500))+
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
                strip.text.x = element_text(size = 20))
          dev.off()
    
    

# 1 - Health outcomes by race
    #a - get descriptive statistics about both racial groups
            vars<-c("h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","h_distress","ego_black2","ego_age","ego_female","year2","just_died","already_died","inc_total","ego_dg_highschool","ego_dg_somecollege","ego_dg_bachelors","ego_dg_advanced","eq_wealth","eq_home_d", "peq_home","peq_savings","peq_stock","peq_debt")
            I2<-I
        #i - Create dataframe with most recent info
            I2<-I2[order(I2$year,decreasing = T),]
            I2<-I2[!(duplicated(I2$ind_id)),]
            I2$ind_id<-NULL
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

      #b - visualize
          #subset to stats of interest
              vars<-c("h_general","h_BMI","h_activities","h_conditions","h_hospital","h_lim_work","h_disabled")
              df<-sm[sm$variable%in% vars,]
          #rename variables
              nvars<-c("Self-Rated","BMI","Activities Limitations","# Conditions","Days Hospitalized","Work Limitations","Disability")
              df$variable<-rep(nvars,2)
          #add race
              df$Race<-rep(c("White   ","Black   "),each=7)
          #remove median
              df$median<-NULL
          #rename variables
              names(df)<-c("Health Outcome","Mean","SD","Max","Race")
          #rescale by max value
              df$Mean[df$`Health Outcome`=="BMI"]<-df$Mean[df$`Health Outcome`=="BMI"]/100
              df$Mean[df$`Health Outcome`=="Self-Rated"]<-df$Mean[df$`Health Outcome`=="Self-Rated"]/10
              df$`Health Outcome`[df$`Health Outcome`=="Self-Rated"]<-"Self-Rated / 10"
              df$`Health Outcome`[df$`Health Outcome`=="BMI"]<-"BMI / 100"
          #keep health order
              df$`Health Outcome`<-factor(df$`Health Outcome`,levels = df$`Health Outcome`[1:7])
          #crfeate graph
              png(file="C:/Users/admin/Desktop/Racial Disparities in Health Outcomes.png", height=8.5, width=11,units = "in",res = 200)
              ggplot(data = df, aes(x = `Health Outcome`, y = Mean))+
                  geom_bar(aes(fill = as.factor(Race)), position = "dodge", stat="identity",width = .75)+
                  scale_fill_grey(start = 0, end = .75)+
                  theme_bw()+
                  labs(x="", y="Mean",fill="")+
                  ggtitle("Racial Disparities in Health Outcomes")+
                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank(), axis.line = element_line(colour = "black"),
                      legend.text=element_text(size=21),
                      legend.title=element_text(size=24),
                      title = element_text(size=30),
                      legend.position="bottom",
                      axis.text.x = element_text(size=21,angle = 30, hjust = 1),
                      # axis.text.y = element_blank(),
                      axis.text.y = element_text(size=21),
                      axis.title.x= element_text(size=24,margin = margin(t = 20, r = 0, b = 0, l = 0)),
                      axis.title.y= element_text(size=24,margin = margin(t = 0, r = 20, b = 0, l = 0)),
                      strip.text.x = element_text(size = 20))
            dev.off()


# 2 - Wealth Components by race
      #a - run a) above
      #b - visualize
          #subset to stats of interest
              vars<-c("eq_home_d", "peq_home","peq_savings","peq_stock","peq_debt")
              df<-sm[sm$variable%in% vars,]
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
                png(file="C:/Users/admin/Desktop/Racial Disparities in Wealth Components.png", height=8.5, width=11,units = "in",res = 200)
                ggplot(data = df, aes(x = `Wealth Component`, y = Mean))+
                    geom_bar(aes(fill = as.factor(Race)), position = "dodge", stat="identity",width = .75)+
                    scale_fill_grey(start = 0, end = .75)+
                    theme_bw()+
                    labs(x="", y="Mean",fill="")+
                    ggtitle("Racial Disparities in Wealth Components")+
                    scale_y_continuous(labels=scales::percent)+
                    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.line = element_line(colour = "black"),
                        legend.text=element_text(size=21),
                        legend.title=element_text(size=24),
                        title = element_text(size=30),
                        legend.position="bottom",
                        axis.text.x = element_text(size=21,angle = 30, hjust = 1),
                        # axis.text.y = element_blank(),
                        axis.text.y = element_text(size=21),
                        axis.title.x= element_text(size=24,margin = margin(t = 20, r = 0, b = 0, l = 0)),
                        axis.title.y= element_text(size=24,margin = margin(t = 0, r = 20, b = 0, l = 0)),
                        strip.text.x = element_text(size = 20))
              dev.off()


            
            
# 3 - predict values:
  #a - prep models
      a<-lmer(form1,I3)
      vars<-c("h_general","h_conditions","h_lim_work")
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
          temp<-temp[,names(temp) %in% c(corder,"ind_id","fam_id_68")]
      #hope that ind_id and fam_id aren't outliers, and set means/intuitive values for everything else
          temp$ego_age<-0
          temp$ego_age2<-0
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
          temp$peq_home<-mean(I$peq_home,na.rm = T)
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
    pds1<-predict(m1[[1]],temp,allow.new.levels=T)
    pds2<-predict(m6[[1]],temp,allow.new.levels=T)
    #merge results with new df
      df<-temp
    #duplicate rows by number of variables to predict
      df<-df[rep(1:nrow(df),2),]
      df$Model<-rep(c("Model 1","Model 4a"),each=(nrow(df)/2))
      df$`Predicted Values`<-c(pds1,pds2)
    #Format data
      df$Age<-df$ego_age+27
      df$Race<-ifelse(df$ego_black2==0,"White","Black")
    #plot
      png(file="C:/Users/admin/Desktop/Mixed Effects - Self-Rated Health.png", height=7, width=11,units = "in",res = 200)
        ggplot(data = df, aes(x = Age, y = `Predicted Values`,color=Race,group=Race))+
          geom_smooth(size=3,se=F)+
          scale_color_grey(start = 0, end = .75)+
          theme_bw()+
          labs(x="Age", y="Self-Rated Health",color="",group="")+
          ggtitle("Mixed Effects - Self-Rated Health")+
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
    #          
              
              
    #          
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              

#Visualize key findings
    results<-readRDS("models.3-31.rds")
    #subset data to interesting IVs
        rs<-results[c(1,2,3,4,8,9,10)]
        vs<-vars[c(1,2,3,4,8,9,10)]
        vs<-c("Self-Rated",
             "BMI",
             "Activities",
             "Conditions",
             "Limitations",
             "Disabled",
             "Distress")

    #convert data to long format
        longrs<-rs[[1]][[1]][1,]
        longrs$DV=0
        longrs$wealth=0
        longrs<-longrs[0,]
        rs2<-for(i in 1:length(rs)){
          trs<-rs[[i]]
          for(j in 1:length(trs)){
            temp<-trs[[j]]
            temp$DV<-vs[i]
            temp$wealth=j
            longrs<-rbind(longrs,temp)
          }
        }

# 1) Black health conditions (bar graph across wealth groups; x=condition)
    #prep data
        longrssub<-longrs[longrs$var=="ego_black2",]
        names(longrssub)<-c("var","mod1","sig1","mod2","sig2","mod3","sig3","DV","Wealth")
        longrssub$mod1<-as.numeric(longrssub$mod1)
        longrssub$sig1<-ifelse(longrssub$sig1!="","*","")
        longrssub$mod1<-ifelse(longrssub$sig1=="",longrssub$mod1/5,longrssub$mod1)
    #rescale betas by DV, such that they maintain their signs, are at least .1 if positive, and are on the same scale (e.g. BMI on much higher scale than distress).
        range02<-function(x){sign(x) * ((abs(x)-min(abs(x)))/(max(abs(x))-min(abs(x)))*(1 - .1) + .1)}
        longrssub<-data.table(longrssub)
        longrssub[,smod1:=range02(mod1),by="DV"]
    #plot
        png("C:/Users/bda13/Desktop/Fig 1 - Racial disparities in health.png", width=1300, height=800)
        ggplot(longrssub, aes(y = smod1, x=Wealth, fill=Wealth,label=sig1))+
          geom_bar(stat = 'identity',position="dodge")+
          facet_wrap(~DV,nrow = 1,strip.position ="bottom")+
          geom_text(aes(y=smod1 + ifelse(smod1<0,smod1*.05,smod1*.01)), size=10)+
          geom_hline(yintercept = 0)+
          theme(axis.ticks=element_blank(),
                axis.text=element_blank(),
                strip.text = element_text(size=30),
                axis.title = element_text(size=30),
                plot.title=element_text(size=40)
                )+
          labs(x="Health Outcome",y="Standardized Beta")+
          guides(fill=FALSE)+
          ggtitle("Racial disparities in health")
        dev.off()
        
        
# 2) Race wealth attenuation (bar graph, x=condition, group=wealth group)
        #prep data
            longrssub<-longrs[longrs$var=="ego_black2",]
            names(longrssub)<-c("var","mod1","sig1","mod2","sig2","mod3","sig3","DV","Wealth")
            longrssub$mod1<-as.numeric(longrssub$mod1)
            longrssub$mod3<-as.numeric(longrssub$mod3)
            longrssub$mod4<-abs(longrssub$mod3)-abs(longrssub$mod1)
            longrssub$mod4<-ifelse(longrssub$sig1=="",longrssub$mod4/5,longrssub$mod4)
            longrssub$sig1<-ifelse(longrssub$sig1!="","*","")
        #rescale betas by DV, such that they maintain their signs, are at least .1 if positive, and are on the same scale (e.g. BMI on much higher scale than distress).
            range02<-function(x){sign(x) * ((abs(x)-min(abs(x)))/(max(abs(x))-min(abs(x)))*(1 - .1) + .1)}
            longrssub<-data.table(longrssub)
            longrssub[,smod1:=range02(mod4),by="DV"]
        #plot
            png("C:/Users/bda13/Desktop/Fig 2 - Influence of Wealth Controls on Unexplained Racial Health Disparities.png", width=1300, height=800)
            ggplot(longrssub, aes(y = smod1, x=Wealth, fill=Wealth,label=sig1))+
              geom_bar(stat = 'identity',position="dodge")+
              facet_wrap(~DV,nrow = 1,strip.position ="bottom")+
              geom_text(aes(y=smod1 + ifelse(smod1<0,smod1*.05,smod1*.01)), size=10)+
              geom_hline(yintercept = 0)+
              theme(axis.ticks=element_blank(),
                    axis.text=element_blank(),
                    strip.text = element_text(size=26),
                    axis.title = element_text(size=26),
                    plot.title=element_text(size=32)
                    )+
              labs(x="Health Outcome",y="Change in Absolute Beta")+
              guides(fill=FALSE)+
              ggtitle("Influence of Wealth Controls on Unexplained Racial Health Disparities")
            dev.off()



# 3) Net worth betas (bar graph, x=condition, group=wealth group)
    #prep data
        longrssub<-longrs[longrs$var=="eq_wealth",]
        names(longrssub)<-c("var","mod1","sig1","mod2","sig2","mod3","sig3","DV","Wealth")
        longrssub$mod2<-as.numeric(longrssub$mod2)
        #longrssub$mod2<-ifelse(longrssub$sig2=="",longrssub$mod2/5,longrssub$mod2)
        longrssub$sig2<-ifelse(longrssub$sig2!="","*","")
    #rescale betas by DV, such that they maintain their signs, are at least .1 if positive, and are on the same scale (e.g. BMI on much higher scale than distress).
        range02<-function(x){sign(x) * ((abs(x)-min(abs(x)))/(max(abs(x))-min(abs(x)))*(1 - .1) + .1)}
        longrssub<-data.table(longrssub)
        longrssub[,smod1:=range02(mod2),by="DV"]
    #plot
        png("C:/Users/bda13/Desktop/Fig 3 - Associations between Net Worth and Health.png", width=1300, height=800)
        ggplot(longrssub, aes(y = smod1, x=Wealth, fill=Wealth,label=sig2))+
          geom_bar(stat = 'identity',position="dodge")+
          facet_wrap(~DV,nrow = 1,strip.position ="bottom")+
          geom_text(aes(y=smod1 + ifelse(smod1<0,smod1*.05,smod1*.01)), size=10)+
          geom_hline(yintercept = 0)+
          theme(axis.ticks=element_blank(),
                axis.text=element_blank(),
                strip.text = element_text(size=26),
                axis.title = element_text(size=26),
                plot.title=element_text(size=32)
                )+
          labs(x="Health Outcome",y="Standardized Beta")+
          guides(fill=FALSE)+
          ggtitle("Associations between Net Worth and Health")
        dev.off()

# 4) Wealth components (4 separate graphs for each component, x=condition, group=wealth group)
    #debt
        #prep data
            longrssub<-longrs[longrs$var=="peq_debt",]
            names(longrssub)<-c("var","mod1","sig1","mod2","sig2","mod3","sig3","DV","Wealth")
            longrssub$mod3<-as.numeric(longrssub$mod3)
            longrssub$mod3<-ifelse(longrssub$sig3=="",longrssub$mod3/3,longrssub$mod3)
            longrssub$sig3<-ifelse(longrssub$sig3!="","*","")
        #rescale betas by DV, such that they maintain their signs, are at least .1 if positive, and are on the same scale (e.g. BMI on much higher scale than distress).
            range02<-function(x){sign(x) * ((abs(x)-min(abs(x)))/(max(abs(x))-min(abs(x)))*(1 - .1) + .1)}
            longrssub<-data.table(longrssub)
            longrssub[,smod1:=range02(mod3),by="DV"]
            longrssub$smod1<-ifelse(longrssub$sig3=="",longrssub$smod1/3,longrssub$smod1)
        #plot
            png("C:/Users/bda13/Desktop/Fig 4a - Associations between Debt and Health.png", width=1300, height=800)
            ggplot(longrssub, aes(y = smod1, x=Wealth, fill=Wealth,label=sig3))+
              geom_bar(stat = 'identity',position="dodge")+
              facet_wrap(~DV,nrow = 1,strip.position ="bottom")+
              geom_text(aes(y=smod1 + ifelse(smod1<0,smod1*.05,smod1*.01)), size=10)+
              geom_hline(yintercept = 0)+
              theme(axis.ticks=element_blank(),
                    axis.text=element_blank(),
                    strip.text = element_text(size=26),
                    axis.title = element_text(size=26),
                    plot.title=element_text(size=32)
                    )+
              labs(x="Health Outcome",y="Standardized Beta")+
              guides(fill=FALSE)+
              ggtitle("Associations between Debt and Health")
            dev.off()

    #Home equity
        #prep data
            longrssub<-longrs[longrs$var=="peq_home",]
            names(longrssub)<-c("var","mod1","sig1","mod2","sig2","mod3","sig3","DV","Wealth")
            longrssub$mod3<-as.numeric(longrssub$mod3)
            longrssub$mod3<-ifelse(longrssub$sig3=="",longrssub$mod3/3,longrssub$mod3)
            longrssub$sig3<-ifelse(longrssub$sig3!="","*","")
        #rescale betas by DV, such that they maintain their signs, are at least .1 if positive, and are on the same scale (e.g. BMI on much higher scale than distress).
            range02<-function(x){sign(x) * ((abs(x)-min(abs(x)))/(max(abs(x))-min(abs(x)))*(1 - .1) + .1)}
            longrssub<-data.table(longrssub)
            longrssub[,smod1:=range02(mod3),by="DV"]
            longrssub$smod1<-ifelse(longrssub$sig3=="",longrssub$smod1/3,longrssub$smod1)
        #plot
            png("C:/Users/bda13/Desktop/Fig 4b - Associations between Home Equity and Health.png", width=1300, height=800)
            ggplot(longrssub, aes(y = smod1, x=Wealth, fill=Wealth,label=sig3))+
              geom_bar(stat = 'identity',position="dodge")+
              facet_wrap(~DV,nrow = 1,strip.position ="bottom")+
              geom_text(aes(y=smod1 + ifelse(smod1<0,smod1*.05,smod1*.01)), size=10)+
              geom_hline(yintercept = 0)+
              theme(axis.ticks=element_blank(),
                    axis.text=element_blank(),
                    strip.text = element_text(size=26),
                    axis.title = element_text(size=26),
                    plot.title=element_text(size=32)
                    )+
              labs(x="Health Outcome",y="Standardized Beta")+
              guides(fill=FALSE)+
              ggtitle("Associations between Home Equity and Health")
            dev.off()
    #Stocks
        #prep data
            longrssub<-longrs[longrs$var=="peq_stock",]
            names(longrssub)<-c("var","mod1","sig1","mod2","sig2","mod3","sig3","DV","Wealth")
            longrssub$mod3<-as.numeric(longrssub$mod3)
            longrssub$sig3<-ifelse(longrssub$sig3!="","*","")
        #rescale betas by DV, such that they maintain their signs, are at least .1 if positive, and are on the same scale (e.g. BMI on much higher scale than distress).
            range02<-function(x){sign(x) * ((abs(x)-min(abs(x)))/(max(abs(x))-min(abs(x)))*(1 - .1) + .1)}
            longrssub<-data.table(longrssub)
            longrssub[,smod1:=range02(mod3),by="DV"]
            longrssub$smod1<-ifelse(longrssub$sig3=="",longrssub$smod1/3,longrssub$smod1)
        #plot
            png("C:/Users/bda13/Desktop/Fig 4c - Associations between Stocks and Health.png", width=1300, height=800)
            ggplot(longrssub, aes(y = smod1, x=Wealth, fill=Wealth,label=sig3))+
              geom_bar(stat = 'identity',position="dodge")+
              facet_wrap(~DV,nrow = 1,strip.position ="bottom")+
              geom_text(aes(y=smod1 + ifelse(smod1<0,smod1*.05,smod1*.01)), size=10)+
              geom_hline(yintercept = 0)+
              theme(axis.ticks=element_blank(),
                    axis.text=element_blank(),
                    strip.text = element_text(size=26),
                    axis.title = element_text(size=26),
                    plot.title=element_text(size=32)
                    )+
              labs(x="Health Outcome",y="Standardized Beta")+
              guides(fill=FALSE)+
              ggtitle("Associations between Stocks and Health")
            dev.off()
    #Savings
        #prep data
            longrssub<-longrs[longrs$var=="peq_savings",]
            names(longrssub)<-c("var","mod1","sig1","mod2","sig2","mod3","sig3","DV","Wealth")
            longrssub$mod3<-as.numeric(longrssub$mod3)
            longrssub$sig3<-ifelse(longrssub$sig3!="","*","")
        #rescale betas by DV, such that they maintain their signs, are at least .1 if positive, and are on the same scale (e.g. BMI on much higher scale than distress).
            range02<-function(x){sign(x) * ((abs(x)-min(abs(x)))/(max(abs(x))-min(abs(x)))*(1 - .1) + .1)}
            longrssub<-data.table(longrssub)
            longrssub[,smod1:=range02(mod3),by="DV"]
            longrssub$smod1<-ifelse(longrssub$sig3=="",longrssub$smod1/3,longrssub$smod1)

        #plot
            png("C:/Users/bda13/Desktop/Fig 4d - Associations between Savings and Health.png", width=1300, height=800)
            ggplot(longrssub, aes(y = smod1, x=Wealth, fill=Wealth,label=sig3))+
              geom_bar(stat = 'identity',position="dodge")+
              facet_wrap(~DV,nrow = 1,strip.position ="bottom")+
              geom_text(aes(y=smod1 + ifelse(smod1<0,smod1*.05,smod1*.01)), size=10)+
              geom_hline(yintercept = 0)+
              theme(axis.ticks=element_blank(),
                    axis.text=element_blank(),
                    strip.text = element_text(size=26),
                    axis.title = element_text(size=26),
                    plot.title=element_text(size=32)
                    )+
              labs(x="Health Outcome",y="Standardized Beta")+
              guides(fill=FALSE)+
              ggtitle("Associations between Savings and Health")
            dev.off()


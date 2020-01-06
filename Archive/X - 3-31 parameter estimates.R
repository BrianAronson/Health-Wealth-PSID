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



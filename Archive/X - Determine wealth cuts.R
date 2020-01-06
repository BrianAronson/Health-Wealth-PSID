library(ggplot2)
library(data.table)
library(plm)
library(pglm)
library(lme4)
library(nlme)
library(lmerTest)
library(cowplot)
options(scipen = 999)

#0 - Set directory
    setwd("C:/Users/bda13/Desktop/Sociology/Statistics/PSID/")

#1 - read data
    I<-readRDS("PreppedData2.rds")

#1.5 - Prep dataset
    #Reduce dataset to variables of interest
        I<-I[,c("h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","ego_age","ego_age2","ego_black2","ego_female","ego_dg_advanced","ego_dg_bachelors","ego_dg_highschool","ego_dg_somecollege","eq_home","eq_bus","eq_savings","eq_otr_estate","eq_stock","eq_debt","inc_total","eq_wealth","ind_id", "year","fam_id_68","fam_id","eq_home_d")] #"eq_vehicle","eq_otr_assets"
    #remove any rows with nas
        I <- I[rowSums(is.na(I))== 0,]

#2 - estimate where to make wealth cuts
  #function to extract coefficients of interest, put in df, and plot
      myfun<-function(x){
      #extract coefficients
          lmer.fit<-x
          Vcov <- vcov(lmer.fit, useScale = FALSE)
          betas <- fixef(lmer.fit)
          se <- sqrt(diag(Vcov))
          zval <- betas / se
          pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
      #put in df
          b<-as.data.frame(cbind(betas, se))
          names(b)<-c("betas", "se")
      #keep only coefficients of interest
          rows<-grep("eq",rownames(b))
          c<-b[rows,]
          c<-c[-1,]
      #convert rownames to sensible numeric
          g<-rownames(c)
          g<-strsplit(g,c('[[]'))
          g<-sapply(g,function(x) x[2])
          g<-strsplit(g,c('[,]'))
          g<-sapply(g,function(x) x[1])
          g<-as.numeric(g)
          if(g[1]<1){
            g[1]<-(-quantile(g,.5))
          }
          c$names<-g
          max<-round(max(c$names/1000)/10^(nchar(max(c$names/1000))-1))*10^(nchar(max(c$names/1000))-1)
          min<-round(min(c$names/1000)/10^(nchar(max(c$names/1000))-1))*10^(nchar(max(c$names/1000))-1)
          br<-round((max-min)/5)
          sp<-3/nrow(c)
      #graph
          p<-ggplot(c,aes(x=names/1000,y=betas))
          p+geom_smooth(se=FALSE,span=sp,color="blue")+
          geom_smooth(aes(y=betas-se),se=FALSE,span=sp,color="grey")+
          geom_smooth(aes(y=betas+se),se=FALSE,span=sp,color="grey")+
            scale_x_continuous("Dollars (thousands)",breaks = seq(min, max,br),labels = scales::dollar)+
            theme(plot.margin=unit(c(2, 1, 0.5, 0.5),"lines"))
      }

      plotfun<-function(x,name){
      #home eq
        #cut into categories
            #cuts<-c(min(x)-1,-50000,-10,10,seq(from = 10000, to = 100000, by = 10000),seq(from = 200000, to = 1000000, by = 100000),seq(from = 1500000, to = 5000000, by = 500000),max(x)+1)
            # cuts<-c(min(x)-1,-50000,-10,10,seq(from = 10000, to = 100000, by = 10000),seq(from = 200000, to = 1000000, by = 100000),seq(from = 1500000, to = 5000000, by = 500000),max(x)+1)
            # I$eq_home2<-relevel(cut(x, cuts), ref="(-10,10]")
            cuts<-sort(c(1,unique(c(0,quantile(x,c(0,1:50/50,.99,1))))))
            I$eq<-relevel(cut(x, cuts,right=F), ref="[0,1)")
        #run and plot models
            m1<-myfun(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home_d + eq +(1|ind_id)+ (1|fam_id_68),  data=I))
            m2<-myfun(lmer(h_BMI ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home_d + eq +(1|ind_id)+ (1|fam_id_68),  data=I))
            m3<-myfun(lmer(h_activities ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home_d + eq +(1|ind_id)+ (1|fam_id_68),  data=I))
            m4<-myfun(lmer(h_conditions ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home_d + eq +(1|ind_id)+ (1|fam_id_68),  data=I))
            m5<-myfun(lmer(h_lim_work ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home_d + eq +(1|ind_id)+ (1|fam_id_68),  data=I))
            m6<-myfun(lmer(h_stroke ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home_d + eq +(1|ind_id)+ (1|fam_id_68),  data=I))
            m7<-myfun(lmer(h_heart_attack ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home_d + eq +(1|ind_id)+ (1|fam_id_68),  data=I))
            m8<-myfun(lmer(h_hospital ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home_d + eq +(1|ind_id)+ (1|fam_id_68),  data=I))
        #arrange into one plot; save
            title <- ggdraw() + draw_label(paste("Smoothed", name, "Parameter Estimates by Health Measure"), fontface='bold')
            p<-plot_grid(m1,m2,m3,m4,m5,m6,m7,m8,labels=c("General","BMI","Activities","Conditions","Heart Attack","Stroke","Hospital","Work Limitations","Disabled"))
            p<-plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
            save_plot(paste(name,".pdf",sep=""), p,
                      base_height=10,
                      base_aspect_ratio = 1.3 # make room for figure legend
            )
      }

  #run the above functions on each wealth variable
      columns<-head(grep("eq|inc",names(I)),10)
      titles<-c("Home Equity","Business Equity","Savings","Other Real Estate","Stocks","Vehicles","Other Assets","Debt","Income","Wealth")
      for(i in columns){
          plotfun(x=I[,i],name=titles[i-min(columns)+1])
      }
      i=2
      
      x
      name
      columns
      
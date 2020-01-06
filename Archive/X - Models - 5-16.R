{
library(ggplot2)
library(data.table)
library(plm)
library(pglm)
library(lme4)
library(nlme)
library(lmerTest)
library(cowplot)
library(car)
library(xlsx)
options(scipen = 999)

#0 - Set directory
    setwd("C:/Users/bda13/Desktop/Sociology/Statistics/PSID/")

#1 - read data
    I<-readRDS("PreppedData2.rds")

#2 - Create a few other variables
    #Reduce dataset to variables of interest
        I<-I[,c("h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","ego_age","ego_age2","ego_black2","ego_female","ego_dg_advanced","ego_dg_bachelors","ego_dg_highschool","ego_dg_somecollege","eq_home","eq_bus","eq_savings","eq_otr_estate","eq_stock","eq_debt","inc_total","eq_wealth","ind_id", "year","fam_id_68","fam_id","eq_home_d","eq_assets","h_distress","ego_cohort")] #"eq_vehicle","eq_otr_assets"
    #remove any rows with nas
        #I <- I[rowSums(is.na(I))== 0,]
    #Asset to debt ratios - Cap these at 1 and 0, except for debt which can be capped at 2; convert NAS to 0, convert Infs to 1.
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
        I$eq_wealth<-I$eq_wealth/1000
        I$inc_total<-I$inc_total/1000
        
}

#I<-I[I$ego_cohort<10,]
#I<-I[I$ego_cohort>=10,]


#degree variables in 1984...
#Ira, especially prior to 1994.
#medical expenses: need to recode 2013 and 2015
#debt variables: none exist  prior to 2011

#3 - Models prep
    #a - create base model formulas
        form1<-formula(paste(y,"~ ego_black2 + ego_age + ego_age2 + ego_female + ego_dg_highschool+ego_dg_somecollege+ego_dg_bachelors+ego_dg_advanced + (1|ind_id)+ (1|fam_id_68)"))
        form2<-formula(paste(y,"~ ego_black2 + ego_age + ego_age2 + ego_female + ego_dg_highschool+ego_dg_somecollege+ego_dg_bachelors+ego_dg_advanced + eq_home_d + inc_total +(eq_wealth) + (1|ind_id)+ (1|fam_id_68)"))
        form3<-formula(paste(y,"~ ego_black2 + ego_age + ego_age2 + ego_female + ego_dg_highschool+ego_dg_somecollege+ego_dg_bachelors+ego_dg_advanced + eq_home_d + inc_total +(peq_home+peq_debt+peq_stock+peq_savings) + (1|ind_id)+ (1|fam_id_68)"))
        prstars<-function(x){
          x[,2]<-ifelse(x[,2]<.001,"***",ifelse(x[,2]<.01,"**",ifelse(x[,2]<.05,"*","")))
          return(x)
        }
        model<-function(x,df){suppressWarnings(prstars(as.data.frame(summary(lmer(formula=x,data=df))$coefficients)[c("Estimate","Pr(>|t|)")]))}
        # model2<-function(x,df){suppressWarnings(prstars(as.data.frame(summary(glmer(formula=x,data=df, family=binomial()))$coefficients)[,c("Estimate","Pr(>|z|)")]))}
        # model3<-function(x,df){suppressWarnings(prstars(as.data.frame(summary(glmer(formula=x,data=df, family=poisson()))$coefficients)[,c("Estimate","Pr(>|z|)")]))}
    #b - choose variables
        
        vars<-c("h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","h_distress")
        
    #b.2 - cap overly skewed health outcomes
        I$h_BMI[I$h_BMI>quantile(I$h_BMI,.999,na.rm = T)]<-quantile(I$h_BMI,.999,na.rm = T)
        I<-I[I$eq_wealth>=0,]
        
    #c - create wealth partitions
        partition<-data.frame(lower=c(25>=I$eq_wealth & I$eq_wealth>=0),
                              lmiddle=c(50>=I$eq_wealth & I$eq_wealth>25),
                              mmiddle=c(100>=I$eq_wealth & I$eq_wealth>50),
                              umiddle=c(250>=I$eq_wealth & I$eq_wealth>100),
                              uumiddle=c(500>=I$eq_wealth & I$eq_wealth>250),
                              upper=c(1000>=I$eq_wealth &I$eq_wealth>500),
                              uupper=c(I$eq_wealth>1000))

        quantf<-function(x) quantile(x[x!=0],c(1:10)/10,na.rm=T)
        
        summary(I$peq_debt[I$WealthG=="-Lower"])
        summary(I$peq_debt[I$WealthG=="-Middle"])
        summary(I$peq_debt[I$WealthG=="-Upper"])
        quantf(I$peq_debt[I$WealthG=="-Lower"])
        quantf(I$peq_debt[I$WealthG=="-Middle"]) 
        quantf(I$peq_debt[I$WealthG=="-Upper"])
        
        summary(I$peq_savings[I$WealthG=="-Lower"])
        summary(I$peq_savings[I$WealthG=="-Middle"])
        summary(I$peq_savings[I$WealthG=="-Upper"])
        quantf(I$peq_savings[I$WealthG=="-Lower"])
        quantf(I$peq_savings[I$WealthG=="-Middle"])
        quantf(I$peq_savings[I$WealthG=="-Upper"])
        
        summary(I$peq_home[I$WealthG=="-Lower"])
        summary(I$peq_home[I$WealthG=="-Middle"])
        summary(I$peq_home[I$WealthG=="-Upper"])
        quantf(I$peq_home[I$WealthG=="-Lower"])
        quantf(I$peq_home[I$WealthG=="-Middle"])
        quantf(I$peq_home[I$WealthG=="-Upper"])
        
        summary(I$peq_stock[I$WealthG=="-Lower"])
        summary(I$peq_stock[I$WealthG=="-Middle"])
        summary(I$peq_stock[I$WealthG=="-Upper"])
        quantf(I$peq_stock[I$WealthG=="-Lower"])
        quantf(I$peq_stock[I$WealthG=="-Middle"])
        quantf(I$peq_stock[I$WealthG=="-Upper"])
        
        summary((I$peq_stock[I$peq_stock!=0]+1.00001))
        
        I$peq_debt2<-ifelse(I$peq_debt>median(I$peq_debt,na.rm=T),1,0)
        I$peq_stock2<-log10(I$peq_stock+1.000001)
        I$peq_savings2<-log10(I$peq_savings+1.000001)
        I$peq_home2<-ifelse(I$peq_home>median(I$peq_home,na.rm=T),1,0)

        I$peq_stock2<-ifelse(I$peq_stock2>.1,1,0)
        I$peq_savings2<-ifelse(I$peq_savings2>.1,1,0)
        
        # I$peq_stock
        # I$peq_savings
        # I$peq_home
    #variable order
        corder<-c("ego_black2","ego_age","ego_age2","ego_female","ego_dg_highschool","ego_dg_somecollege","ego_dg_bachelors","ego_dg_advanced","eq_home_d","inc_total","eq_wealth","peq_debt","peq_home","peq_stock","peq_savings","(Intercept)")
    #create a partition variable
        I$partition2<-ifelse(partition[,1],1,ifelse(partition[,2],2,ifelse(partition[,3],3,ifelse(partition[,4],4,ifelse(partition[,5],5,ifelse(partition[,6],6,ifelse(partition[,7],7,"")))))))
        I$partition2<-as.factor(I$partition2)
        
        #alternate:
          partition<-data.frame(lower=c(50>=I$eq_wealth),
                              middle=c(500>=I$eq_wealth & I$eq_wealth>50),
                              upper=c(I$eq_wealth>500))
          I$WealthG<-ifelse(partition[,1],"-Lower",ifelse(partition[,2],"-Middle",ifelse(partition[,3],"-Upper","")))
          I$WealthG<-as.factor(I$WealthG)

    #try creating a model
#c("h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","h_distress")
      ml<-list()
      for(i in 1:length(vars)){
      I$y<-I[,vars[i]]
      {
        formt<-y ~ 
          WealthG *  (ego_black2+ego_age+ego_age2) + 
          (ego_female) +
          (ego_cohort) +
          (ego_dg_highschool+ego_dg_somecollege+ego_dg_bachelors+ego_dg_advanced) + 
          (eq_home_d)+
          WealthG *  (inc_total+eq_wealth) + 
          WealthG *  (peq_home+peq_debt+peq_stock+peq_savings) + 
                     (1 | ind_id) + (1 | fam_id_68)
    #run model
        
        (ml[[i]]<-model(formt,df=I))
      }
      print(i)
      }
    #save results
      results<-ml
      roundstr<-function(x){ifelse(substr(x,1,1)=="-",substr(x,1,5),substr(x,1,4))}
      for(i in 1:length(results)){
        tempvar<-paste(vars[i],".xlsx",sep = "")
        tempres<-results[[i]]
        #for(j in 1:length(tempres)){
        tempres2<-tempres#[[j]]
        tempname<-paste("sheet",j,sep="")
        tempres2[,1]<-roundstr(tempres2[,1])
        # tempres2[,4]<-roundstr(tempres2[,4])
        # tempres2[,6]<-roundstr(tempres2[,6])
        if(j==1){
          write.xlsx(tempres2, file=tempvar, sheetName=tempname, row.names=T)
        }else{
          write.xlsx(tempres2, file=tempvar, sheetName=tempname, append=TRUE, row.names=T)
          # }
        }
      }

#4 - run models
    results<-rep(list(vector(mode="list", ncol(partition))),length(vars))
    for(i in 1:length(vars)){
        y<-c(vars[i])
        #choose which type of model to run (gaussian, binomial, or poisson)
            # if(i==1 | i==2 | i==10){
            #   model<-model1
            # }else if(i==5 | i==6 | i==9){
            #   model<-model2
            # }else{
            #   model<-model3
            # }
        for(j in 1:ncol(partition)){
            #create functions
              form1<-formula(paste(y,"~ ego_black2 + ego_age + ego_age2 + ego_female + ego_dg_highschool+ego_dg_somecollege+ego_dg_bachelors+ego_dg_advanced + (1|ind_id)+ (1|fam_id_68)"))
              form2<-formula(paste(y,"~ ego_black2 + ego_age + ego_age2 + ego_female + ego_dg_highschool+ego_dg_somecollege+ego_dg_bachelors+ego_dg_advanced + eq_home_d + inc_total +(eq_wealth) + (1|ind_id)+ (1|fam_id_68)"))
              form3<-formula(paste(y,"~ ego_black2 + ego_age + ego_age2 + ego_female + ego_dg_highschool+ego_dg_somecollege+ego_dg_bachelors+ego_dg_advanced + eq_home_d + inc_total +(peq_home+peq_debt+peq_stock+peq_savings) + (1|ind_id)+ (1|fam_id_68)"))
            #run models
              m1<-model(form1,df=I[partition[,j],])
              m2<-model(form2,df=I[partition[,j],])
              m3<-model(form3,df=I[partition[,j],])
            #prep models for merge
              m1$var<-row.names(m1)
              names(m1)<-c("1","1.1","var")
              m2$var<-row.names(m2)
              names(m2)<-c("2","2.1","var")
              m3$var<-row.names(m3)
              names(m3)<-c("3","3.1","var")
            #prep empty df to put results into
              df<-data.frame(var=unique(c(row.names(m1),row.names(m2),row.names(m3))))
            #bind vars to df
              df<-merge(df,m1,by="var",all=T,sort = F)
              df<-merge(df,m2,by="var",all=T,sort = F)
              df<-merge(df,m3,by="var",all=T,sort = F)
            #reorder
              df<-df[match(corder,df$var),]
            #add empty rows
              emptydf<-as.data.frame(matrix("",nrow=1,ncol=ncol(df)))
              names(emptydf)<-names(df)
              empty1<-which(df$var=="ego_dg_highschool")
              empty2<-which(df$var=="eq_home_d")
              df<-rbind(df[1:(empty1-1),],emptydf,df[(empty1):(empty2-1),],emptydf,df[empty2:nrow(df),])
              df[is.na(df)]<-""
              results[[i]][[j]]<-df
            print(paste(i,j))
        }
    }

    results<-ml
    roundstr<-function(x){ifelse(substr(x,1,1)=="-",substr(x,1,5),substr(x,1,4))}
    for(i in 1:length(results)){
      tempvar<-paste(vars[i],".xlsx",sep = "")
      tempres<-results[[i]]
      for(j in 1:length(tempres)){
        tempres2<-tempres[[j]]
        tempname<-paste("sheet",j,sep="")
        tempres2[,2]<-roundstr(tempres2[,2])
        tempres2[,4]<-roundstr(tempres2[,4])
        tempres2[,6]<-roundstr(tempres2[,6])
        if(j==1){
          write.xlsx(tempres2, file=tempvar, sheetName=tempname, row.names=T)
        }else{
          write.xlsx(tempres2, file=tempvar, sheetName=tempname, append=TRUE, row.names=T)
        }
      }
    }

saveRDS(results,"models.4-16.rds")

results<-readRDS("models.3-31.rds")
#Visualize key findings
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

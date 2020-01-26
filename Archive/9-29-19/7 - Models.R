{
#currently irrelevant problems: 
  #Ira, especially prior to 1994.
  #medical expenses: need to recode 2013 and 2015
  #debt variables: none exist  prior to 2011
  
library(ggplot2)
library(data.table)
library(plm)
library(pglm)
library(lme4)
library(nlme)
library(lmerTest)
library(cowplot)
library(car)
# library(xlsx)
library(ordinal)
library(R2admb) #zero-infalted
library(glmmADMB) #zero-inflated models
options(scipen = 999)


#0 - Set directory
    setwd("C:/Users/briarons/Desktop/Analysis - Data/PSID")
    # setwd("C:/Users/admin/Desktop/Sociology/PSID Data")
    
#1 - read data
    I<-readRDS("PreppedData2.rds")

#2 - Center age
    I$ego_age<-I$ego_age-min(I$ego_age)
    I$ego_age2<-I$ego_age^2

#3 - bring back the dead
    #a - Create dataframe with most recent info
        I2<-I[order(I$year,decreasing = T),]
        I2<-I2[!(duplicated(I2$ind_id)),]
    #b - Find dead folks
        I2<-I2[I2$ego_died==1,]
    #c - create a year died variable in main dataset
        I$just_died<-ifelse(paste(I$year,I$ind_id) %in% paste(I2$year,I2$ind_id),1,0)
    #d - create rows for individuals who are already dead
        #years in data
            uyears<-unique(I$year)
        #create a dataframe consisting of all possible years for dead individuals
            tm<-data.table(ind_id=rep(I2$ind_id,each=length(uyears)), year=rep(uyears,length(I2$ind_id)))
        #remove years where individuals hadn't died yet or weren't born yet (the latter captures children who died)
            #death info
                tm2<-I2[rep(seq_len(nrow(I2)), each=length(uyears)),]
            #birth info
                birthdate<-I2$year-I2$ego_age
                birthdate<-rep(birthdate,each=length(uyears))
            #subset data
                crit<-tm$year>birthdate & tm$year>tm2$year
                tm<-tm[crit,]
                tm2<-tm2[crit,]
            #swap year with missing years
                tm2$year<-tm$year
            #and add label indicating person is already dead
                tm2$just_died<-0
                tm2$already_died<-1
            #append these years into data
                I$already_died<-0
                I<-rbind(I,tm2)
    #e - make sure we don't include folks in 2005-2011 models that don't include at least 2 years while alive
            tm<-as.data.frame(table(I$ind_id[I$year>2004 & I$year<2012 & I$already_died==0]))
            rec.ids<-tm$Var1[tm$Freq>1]
  
#4 - center year
    I$year2<-I$year-min(I$year)

#5 - stabilize education so that time invariant
    #a - for simplicity, create education variable (again)
        I$educ<-ifelse(I$ego_dg_highschool==1,1,ifelse(I$ego_dg_somecollege==1,2,ifelse(I$ego_dg_bachelors==1,3,ifelse(I$ego_dg_advanced==1,4,0))))
    #b - Create dataframe with most recent info
        I2<-I[order(I$year,decreasing = T),]
        I2<-I2[!(duplicated(I2$ind_id)),]
    #c - create df of each person's highest education
        tm<-data.table(ind_id=I2$ind_id,higheduc=I2$educ)
    #d - merge back into df
        I<-data.table(I)
        I<-merge(I,tm,by="ind_id")
        I$ego_dg_lthighschool<-ifelse(I$higheduc==0,1,0)
        I$ego_dg_highschool<-ifelse(I$higheduc==1,1,0)
        I$ego_dg_somecollege<-ifelse(I$higheduc==2,1,0)
        I$ego_dg_bachelors<-ifelse(I$higheduc==3,1,0)
        I$ego_dg_advanced<-ifelse(I$higheduc==4,1,0)
        I<-data.frame(I)
        I$fam_region<-ifelse(I$fam_region=="South",1,0)

#6 - create lagged dependent variables by moving the later year variables up one interval year
    #a - prep new dataset with variables of interest
        J<-I[,c("ind_id","h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","h_distress")]
    #b - make each variable equal row below
        J<-J[-1,]
        J<-rbind(J,J[1,])
    #c - change name of ind_id in J.
        names(J)[1]<-"ind_id.t1"
    #d - remove those variables from I
        I[,c("h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","h_distress")]<-NULL
        I<-cbind(I,J)
    #e - kill rows that are invalid
        I<-I[I$ind_id==I$ind_id.t1,]
        I<-I[!is.na(I$ind_id),]

         
#7 - Create a few other variables
    #a - Reduce dataset to variables of interest
        I<-I[,c("h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","ego_age","ego_age2","ego_black2","ego_female","ego_dg_advanced","ego_dg_bachelors","ego_dg_highschool","ego_dg_somecollege","eq_home","eq_bus","eq_savings","eq_otr_estate","eq_stock","eq_debt","inc_total","eq_wealth","ind_id", "year","fam_id_68","fam_id","eq_home_d","eq_assets","h_distress","ego_cohort","year2","just_died","already_died","fam_region")] #"eq_vehicle","eq_otr_assets"
    #b - create wealth partitions df
        partition<-data.frame(lower=c(25>=I$eq_wealth & I$eq_wealth>=0),
                              lmiddle=c(50>=I$eq_wealth & I$eq_wealth>25),
                              mmiddle=c(100>=I$eq_wealth & I$eq_wealth>50),
                              umiddle=c(250>=I$eq_wealth & I$eq_wealth>100),
                              uumiddle=c(500>=I$eq_wealth & I$eq_wealth>250),
                              upper=c(1000>=I$eq_wealth &I$eq_wealth>500),
                              uupper=c(I$eq_wealth>1000)
                              )
    #c - Asset to debt ratios - Cap these at 1 and 0, except for debt which can be capped at 2; convert NAS to 0, convert Infs to 1.
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
    #d - normalize variables
        I$d_eq_debt<-ifelse(I$peq_debt>0,1,0)
        I$d_eq_stock<-ifelse(I$peq_stock>0,1,0)
        I$d_eq_savings<-ifelse(I$peq_savings>0,1,0)
        I$d_eq_home<-ifelse(I$peq_home>0,1,0)
        I$peq_home<-propfun(I$eq_home)
        I$peq_debt<-propfun(I$eq_debt,debt=T)
        I$peq_stock<-propfun(I$eq_stock)
        I$peq_savings<-propfun(I$eq_savings)
        I$eq_wealth<-I$eq_wealth/1000
        I$inc_total<-I$inc_total/1000
        I$peq_debt2<-I$peq_debt
        I$peq_stock2<-I$peq_stock
        I$peq_savings2<-I$peq_savings
        I$peq_home2<-I$peq_home
        I$peq_debt<-I$peq_debt^(1/3)
        I$peq_stock<-I$peq_stock^(1/3)
        I$peq_savings<-I$peq_savings^(1/3)
        I$peq_home<-I$peq_home^(1/3)
    #e - Correct variable order
        corder<-c("ego_black2","ego_age","ego_age2","ego_female","ego_dg_highschool","ego_dg_somecollege","ego_dg_bachelors","ego_dg_advanced","eq_home_d","inc_total","eq_wealth","peq_debt","peq_home","peq_stock","peq_savings","(Intercept)")
        corder<-c("ego_black2","ego_age","ego_age2","ego_female","ego_black2:ego_age","ego_black2:ego_age2","fam_region","year2","just_died","already_died","log_income","ego_dg_highschool","ego_dg_somecollege","ego_dg_bachelors","ego_dg_advanced","ego_black2:log_income", "ego_black2:ego_dg_highschool","ego_black2:ego_dg_somecollege","ego_black2:ego_dg_bachelors","ego_black2:ego_dg_advanced","log_wealth","ego_black2:log_wealth","eq_home_d", "peq_home","peq_savings","peq_stock","peq_debt","ego_black2:eq_home_d","ego_black2:peq_home", "ego_black2:peq_savings", "ego_black2:peq_stock","ego_black2:peq_debt","(Intercept)")         
    #f - create a partition variable
        I$partition2<-ifelse(partition[,1],1,ifelse(partition[,2],2,ifelse(partition[,3],3,ifelse(partition[,4],4,ifelse(partition[,5],5,ifelse(partition[,6],6,ifelse(partition[,7],7,"")))))))
        I$partition2<-as.factor(I$partition2)
    #g - alternate/better partition variable:
        partition<-data.frame(lower=c(50>=I$eq_wealth),
            middle=c(500>=I$eq_wealth & I$eq_wealth>50),
            upper=c(I$eq_wealth>500))
        I$WealthG<-ifelse(partition[,1],"-Lower",ifelse(partition[,2],"-Middle",ifelse(partition[,3],"-Upper","")))
        I$WealthG<-as.factor(I$WealthG)
    #h - cap overly skewed health outcomes
        I$h_BMI[I$h_BMI>quantile(I$h_BMI,.999,na.rm = T)]<-quantile(I$h_BMI,.999,na.rm = T)
    #i - Reverse code cohorts
        I$ego_cohort<-max(I$ego_cohort,na.rm=T)+1-I$ego_cohort
    #j - logged variables without outliers
        transfun<-function(x){
            x<-x*1000
            x<-ifelse(x>quantile(x,.99,na.rm=T),quantile(x,.99,na.rm=T),x)
            x<-ifelse(x<quantile(x,.01,na.rm=T),quantile(x,.01,na.rm=T),x)
            x<-x-min(x,na.rm = T)+1
            x<-log10(x)
            x<-ifelse(x<quantile(x,.02,na.rm=T),quantile(x,.02,na.rm=T),x)
            return(x)
        }
        I$log_wealth<-transfun(I$eq_wealth)
        I$log_income<-transfun(I$inc_total)
        I$fam_id<-I$fam_id+10000000*I$year
        I$died<-I$just_died+I$already_died
  
#8 - Model prep
    #a - choose variables
        vars<-c("h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","h_distress")
        y<-vars[1]
    #b - create model output function to make results pretty
        prstars<-function(x){
          x[,2]<-ifelse(x[,2]<.001,"***",ifelse(x[,2]<.01,"**",ifelse(x[,2]<.05,"*","")))
          return(x)
        }
        sum.model<-function(model) {
          x<-as.data.frame(round(summary(model)$coefficients,10))
          x[,c(1,ncol(x))]
        }
        I$fam_id<-factor(I$fam_id)
        I$fam_id_68<-factor(I$fam_id_68)
        I$ind_id<-factor(I$ind_id)
    #c - assign models to test
        # form1<-y~ego_black2 + ego_age+ (1|fam_id_68/ind_id) + (1|fam_id)
        # form1<-y~ego_black2 + ego_age+ (1 | fam_id_68) + (1| ind_id)
        form1<-y~ego_black2 + ego_age+ego_age2 + ego_female + ego_black2 *(ego_age+ego_age2)+fam_region +year2+died  +(1|fam_id_68/ind_id) #(1|fam_id_68/ind_id) #(1 | fam_id_68) + (1| ind_id)  + (1|fam_id)
        form2<-update(form1,~. + log_income  + ego_dg_highschool+ego_dg_somecollege+ego_dg_bachelors+ego_dg_advanced)
        form2b<-update(form2,~. + ego_black2*(log_income  + ego_dg_highschool+ego_dg_somecollege+ego_dg_bachelors+ego_dg_advanced))
        form3<-update(form2,~. + log_wealth)
        form3b<-update(form2,~. + log_wealth*ego_black2)
        form4<-update(form3,~. + eq_home_d+ peq_home+peq_debt+peq_stock+peq_savings)
        form4b<-update(form4,~. + ego_black2*(eq_home_d+peq_home+peq_debt+peq_stock+peq_savings))
        form5<-update(form2,~. + eq_home_d+ peq_home+peq_debt+peq_stock+peq_savings)
        form5b<-update(form5,~. + ego_black2*(eq_home_d+peq_home+peq_debt+peq_stock+peq_savings))
    #d - specify years
        I2<-I[I$year>2004 & I$year<2012,]
        I2<-I2[I2$ind_id%in% rec.ids,]

}

#9 - remove the few NAs that are in IVs
    I2<-I2[rowSums(is.na(I2[,!names(I2) %in% vars]))==0,]
    I3 <- I2
    tm<-as.data.frame(table(I3$ind_id[I3$year>2004 & I3$year<2012 & I3$already_died==0]))
    rec.ids<-tm$Var1[tm$Freq>1]
    I3<-I3[I3$ind_id%in% rec.ids,]

#10 - export to SAS
    library(foreign)
    PSID.Sample<-I2
    write.dta(PSID.Sample,"PSID.Sample.dta")
    #5 = heart attack doesn't work; #6 = stroke bad too; #9 = disabled problematic as well
    
#11 - Run models
    results<-list()
    i.model<-list()
    s.model<-list()
    m.model<-list()
    for(i in c(1,2,5,6,9)){ #1:length(vars)
      {
      #a - Select variable
        # I3 <- I2[rowSums(is.na(I2))== 0,]
        I3 <- I2
        tm<-as.data.frame(table(I3$ind_id[I3$year>2004 & I3$year<2012 & I3$already_died==0]))
        rec.ids<-tm$Var1[tm$Freq>1]
        I3<-I3[I3$ind_id%in% rec.ids,]
        I3$y<-I3[,vars[i]]
      #b - Select model
        if(i==1){
                I3$y<-factor(I3[,vars[i]])
                init.model<- function(form,df) clmm(formula=form,data=df, Hess=T)
        }
        if(i==2) init.model<- function(form,df) lmer(formula=form,data=df)
        # if(i==3) init.model<- function(form,df) glmer.nb(formula=form,data=df)
        # if(i==4) init.model<- function(form,df) glmer(formula=form,data=df,family=poisson)
        if(i==5) init.model<- function(form,df) glmer(formula=form,data=df,family = binomial, nAGQ=0)
        if(i==6) init.model<- function(form,df) glmer(formula=form,data=df,family = binomial, nAGQ=0)
        # if(i==7) init.model<- function(form,df) glmer.nb(formula=form,data=df)
        # if(i==8) init.model<- function(form,df) glmmTMB(form, data=df,zi=~1,family='nbinom1')
        if(i==9) init.model<- function(form,df) glmer(formula=form,data=df,family = binomial, nAGQ=0)
        # if(i==10) init.model<- function(form,df) glmer.nb(formula=form,data=df)
      } 
      #c - Run models
        #1
          print(system.time({
            i.model.1<-init.model(form1,df=I3)      
          }))
          s.model.1<-sum.model(i.model.1)
          m1<-prstars(s.model.1)
          m.1.1<-prstars(s.model.1)
          print("m1")
        #2
          i.model.2<-init.model(form2,df=I3)
          s.model.2<-sum.model(i.model.2)
          m2<-prstars(s.model.2)
          m.2.2<-prstars(s.model.2)
          print("m2")
        #3
          i.model.3<-init.model(form2b,df=I3)
          s.model.3<-sum.model(i.model.3)
          m3<-prstars(s.model.3)
          m.3.3<-prstars(s.model.3)
          print("m3")
        #4
          i.model.4<-init.model(form3,df=I3)
          s.model.4<-sum.model(i.model.4)
          m4<-prstars(s.model.4)
          m.4.4<-prstars(s.model.4)
          print("m4")
        #5
          i.model.5<-init.model(form3b,df=I3)
          s.model.5<-sum.model(i.model.5)
          m5<-prstars(s.model.5)
          m.5.5<-prstars(s.model.5)
          print("m5")
        #6
          i.model.6<-init.model(form4,df=I3)
          s.model.6<-sum.model(i.model.6)
          m6<-prstars(s.model.6)
          m.6.6<-prstars(s.model.6)
          print("m6")
        #7
          i.model.7<-init.model(form4b,df=I3)
          s.model.7<-sum.model(i.model.7)
          m7<-prstars(s.model.7)
          m.7.7<-prstars(s.model.7)
          print("m7")
        #8
          i.model.8<-init.model(form5,df=I3)
          s.model.8<-sum.model(i.model.8)
          m8<-prstars(s.model.8)
          m.8.8<-prstars(s.model.8)
          print("m8")
        #9
          i.model.9<-init.model(form5b,df=I3)
          s.model.9<-sum.model(i.model.9)
          m9<-prstars(s.model.9)
          m.9.9<-prstars(s.model.9)
          print("m9")
      #d - prep models for merge
        m1$var<-row.names(m1)
        names(m1)<-c("1","1.1","var")
        m2$var<-row.names(m2)
        names(m2)<-c("2","2.1","var")
        m3$var<-row.names(m3)
        names(m3)<-c("3","3.1","var")
        m4$var<-row.names(m4)
        names(m3)<-c("4","4.1","var")
        m5$var<-row.names(m5)
        names(m5)<-c("5","5.1","var")
        m6$var<-row.names(m6)
        names(m6)<-c("6","6.1","var")
        m7$var<-row.names(m7)
        names(m7)<-c("7","7.1","var")
        m8$var<-row.names(m8)
        names(m8)<-c("8","8.1","var")
        m9$var<-row.names(m9)
        names(m9)<-c("9","9.1","var")
      #e - prep empty df to put results into
        df<-data.frame(var=unique(c(row.names(m1),row.names(m2),row.names(m3),row.names(m4),row.names(m5),row.names(m6),row.names(m7),row.names(m8),row.names(m9))))
      #f - bind vars to df
        df<-merge(df,m1,by="var",all=T,sort = F)
        df<-merge(df,m2,by="var",all=T,sort = F)
        df<-merge(df,m3,by="var",all=T,sort = F)
        df<-merge(df,m4,by="var",all=T,sort = F)
        df<-merge(df,m5,by="var",all=T,sort = F)
        df<-merge(df,m6,by="var",all=T,sort = F)
        df<-merge(df,m7,by="var",all=T,sort = F)
        df<-merge(df,m8,by="var",all=T,sort = F)
        df<-merge(df,m9,by="var",all=T,sort = F)
        results[[i]]<-df
        i.model[[i]]<-list(i.model.1,i.model.2,i.model.3,i.model.4,i.model.5,i.model.6,i.model.7,i.model.8,i.model.9)
        s.model[[i]]<-list(s.model.1,s.model.2,s.model.3,s.model.4,s.model.5,s.model.6,s.model.7,s.model.8,s.model.9)
        m.model[[i]]<-list(m.1.1,m.2.2,m.3.3,m.4.4,m.5.5,m.6.6,m.7.7,m.8.8,m.9.9)
        saveRDS(results,"results.rds")
        saveRDS(i.model,"i.model.rds")
        saveRDS(s.model,"s.model.rds")
        saveRDS(m.model,"m.model.rds")
        print(i)
    }
    

    
    
    

    
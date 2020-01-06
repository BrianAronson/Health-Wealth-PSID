
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
library(xlsx)
options(scipen = 999)

#0 - Set directory
    setwd("C:/Users/bda13/Desktop/Sociology/Statistics/PSID/")
    # setwd("C:/Users/admin/Desktop/Sociology/PSID Data")
    
#1 - read data
    I<-readRDS("PreppedData2.rds")

#1.1 - Center age
    I$ego_age<-I$ego_age-min(I$ego_age)
    I$ego_age2<-I$ego_age^2

#1.5 - bring back the dead
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
  
#1.3 - center year
    I$year2<-I$year-min(I$year)

#1.4 - stabilize education so that time invariant
    #for simplicity, create education variable (again)
        I$educ<-ifelse(I$ego_dg_highschool==1,1,ifelse(I$ego_dg_somecollege==1,2,ifelse(I$ego_dg_bachelors==1,3,ifelse(I$ego_dg_advanced==1,4,0))))
    #a - Create dataframe with most recent info
        I2<-I[order(I$year,decreasing = T),]
        I2<-I2[!(duplicated(I2$ind_id)),]
    #create df of each person's highest education
        tm<-data.table(ind_id=I2$ind_id,higheduc=I2$educ)
    #merge back into df
        I<-data.table(I)
        I<-merge(I,tm,by="ind_id")
        I$ego_dg_lthighschool<-ifelse(I$higheduc==0,1,0)
        I$ego_dg_highschool<-ifelse(I$higheduc==1,1,0)
        I$ego_dg_somecollege<-ifelse(I$higheduc==2,1,0)
        I$ego_dg_bachelors<-ifelse(I$higheduc==3,1,0)
        I$ego_dg_advanced<-ifelse(I$higheduc==4,1,0)
        I<-data.frame(I)
        I$fam_region<-ifelse(I$fam_region=="South",1,0)

#1.8 - Optional - create lagged dependent variables by moving the later year variables up one interval year
    #prep new dataset with variables of interest
        J<-I[,c("ind_id","h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","h_distress")]
    #make each variable equal row below
        J<-J[-1,]
        J<-rbind(J,J[1,])
    #change name of ind_id in J.
        names(J)[1]<-"ind_id.t1"
    #remove those variables from I
        I[,c("h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","h_distress")]<-NULL
        I<-cbind(I,J)
    #kill rows that are invalid
        I<-I[I$ind_id==I$ind_id.t1,]
        I<-I[!is.na(I$ind_id),]

         
#2 - Create a few other variables
    #Reduce dataset to variables of interest
        I<-I[,c("h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","ego_age","ego_age2","ego_black2","ego_female","ego_dg_advanced","ego_dg_bachelors","ego_dg_highschool","ego_dg_somecollege","eq_home","eq_bus","eq_savings","eq_otr_estate","eq_stock","eq_debt","inc_total","eq_wealth","ind_id", "year","fam_id_68","fam_id","eq_home_d","eq_assets","h_distress","ego_cohort","year2","just_died","already_died","fam_region")] #"eq_vehicle","eq_otr_assets"
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
    #create wealth partitions df
        partition<-data.frame(lower=c(25>=I$eq_wealth & I$eq_wealth>=0),
            lmiddle=c(50>=I$eq_wealth & I$eq_wealth>25),
            mmiddle=c(100>=I$eq_wealth & I$eq_wealth>50),
            umiddle=c(250>=I$eq_wealth & I$eq_wealth>100),
            uumiddle=c(500>=I$eq_wealth & I$eq_wealth>250),
            upper=c(1000>=I$eq_wealth &I$eq_wealth>500),
            uupper=c(I$eq_wealth>1000))
        
    #1.5 - normalized variables
        I$d_eq_debt<-ifelse(I$peq_debt>0,1,0)
        I$d_eq_stock<-ifelse(I$peq_stock>0,1,0)
        I$d_eq_savings<-ifelse(I$peq_savings>0,1,0)
        I$d_eq_home<-ifelse(I$peq_home>0,1,0)

        I$peq_debt<-I$peq_debt^(1/3)
        I$peq_stock<-I$peq_stock^(1/3)
        I$peq_savings<-I$peq_savings^(1/3)
        I$peq_home<-I$peq_home^(1/3)
        
    #Correct variable order
        corder<-c("ego_black2","ego_age","ego_age2","ego_female","ego_dg_highschool","ego_dg_somecollege","ego_dg_bachelors","ego_dg_advanced","eq_home_d","inc_total","eq_wealth","peq_debt","peq_home","peq_stock","peq_savings","(Intercept)")
        corder<-c("ego_black2","ego_age","ego_age2","ego_female","ego_black2:ego_age","ego_black2:ego_age2","fam_region","year2","just_died","already_died","log_income","ego_dg_highschool","ego_dg_somecollege","ego_dg_bachelors","ego_dg_advanced","ego_black2:log_income", "ego_black2:ego_dg_highschool","ego_black2:ego_dg_somecollege","ego_black2:ego_dg_bachelors","ego_black2:ego_dg_advanced","log_wealth","ego_black2:log_wealth","eq_home_d", "peq_home","peq_savings","peq_stock","peq_debt","ego_black2:eq_home_d","ego_black2:peq_home", "ego_black2:peq_savings", "ego_black2:peq_stock","ego_black2:peq_debt","(Intercept)")         
    #create a partition variable
        I$partition2<-ifelse(partition[,1],1,ifelse(partition[,2],2,ifelse(partition[,3],3,ifelse(partition[,4],4,ifelse(partition[,5],5,ifelse(partition[,6],6,ifelse(partition[,7],7,"")))))))
        I$partition2<-as.factor(I$partition2)
    #alternate/better partition variable:
        partition<-data.frame(lower=c(50>=I$eq_wealth),
            middle=c(500>=I$eq_wealth & I$eq_wealth>50),
            upper=c(I$eq_wealth>500))
        I$WealthG<-ifelse(partition[,1],"-Lower",ifelse(partition[,2],"-Middle",ifelse(partition[,3],"-Upper","")))
        I$WealthG<-as.factor(I$WealthG)
    #cap overly skewed health outcomes
        I$h_BMI[I$h_BMI>quantile(I$h_BMI,.999,na.rm = T)]<-quantile(I$h_BMI,.999,na.rm = T)
        # I<-I[I$eq_wealth>=0,]
        names(I)
    #Reverse code cohorts
        I$ego_cohort<-max(I$ego_cohort,na.rm=T)+1-I$ego_cohort
    #logged variables without outliers
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
        #summary(I$log_wealth)
        #summary(I$log_income)
  }
    # #determine years of model
    #   #By dependent variables:
    #     for(i in 1:length(vars)){
    #       a<-table(I$year[!is.na(I[,vars[i]])])
    #       year<-names(a)[a==min(a)]
    #       print(paste(vars[i],"-",year))
    #     }
    #   #By independent variables:
    #     table(I$year,is.na(I$ego_black2 + I$ego_age+I$ego_age2 + I$ego_female + I$ego_cohort + I$ego_black2 *(I$ego_age+I$ego_age2) +  I$ind_id + I$fam_id_68+ I$eq_wealth + I$peq_home+I$peq_debt+I$peq_stock+I$peq_savings+I$ego_dg_highschool+I$ego_dg_somecollege+I$ego_dg_bachelors+I$ego_dg_advanced))

#3 - Model prep
    #a - choose variables
        vars<-c("h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","h_distress")
        y<-vars[1]
    #b - create model output function to make results pretty
        prstars<-function(x){
          x[,2]<-ifelse(x[,2]<.001,"***",ifelse(x[,2]<.01,"**",ifelse(x[,2]<.05,"*","")))
          return(x)
        }
        # model<-function(x,df){suppressWarnings(prstars(as.data.frame(summary(lmer(formula=x,data=df))$coefficients)[c("Estimate","Pr(>|t|)")]))}
        #fixed effects model
            # model<-function(x,df){suppressWarnings(prstars(as.data.frame(summary(plm(formula=x,data=df, index=c("ind_id", "year"), model="within"))$coefficients)[c("Estimate","Pr(>|t|)")]))}
        #Generalized models for later
            # model2<-function(x,df){suppressWarnings(prstars(as.data.frame(summary(glmer(formula=x,data=df, family=binomial()))$coefficients)[,c("Estimate","Pr(>|z|)")]))}
            # model<-function(x,df){suppressWarnings(prstars(as.data.frame(summary(glmer(formula=x,data=df, family=poisson()))$coefficients)[,c("Estimate","Pr(>|z|)")]))}
            
            model<-function(x,df) {suppressWarnings(summary(glmer(formula=x,data=df, family=poisson())))}
            prettyit<-function(x) prstars(as.data.frame(x)$coefficients[,c("Estimate","Pr(>|z|)")])
            
            # length(table(I$fam_id))
            # length(table(I$fam_id_68))
            # length(table(I$ind_id))
    #c - models to test
        form1<-y~ego_black2 + ego_age+ego_age2 + ego_female + ego_black2 *(ego_age+ego_age2)+fam_region +year2+just_died+already_died  + (1 | ind_id) + (1 | fam_id_68) + (1|fam_id)
        form2<-update(form1,~. + log_income  + ego_dg_highschool+ego_dg_somecollege+ego_dg_bachelors+ego_dg_advanced)
        form2b<-update(form2,~. + ego_black2*(log_income  + ego_dg_highschool+ego_dg_somecollege+ego_dg_bachelors+ego_dg_advanced))
        form3<-update(form2,~. + log_wealth)
        form3b<-update(form2,~. + log_wealth*ego_black2)
        form4<-update(form3,~. + eq_home_d+ peq_home+peq_debt+peq_stock+peq_savings)
        form4b<-update(form4,~. + ego_black2*(eq_home_d+peq_home+peq_debt+peq_stock+peq_savings))
        
        form5<-update(form2,~. + eq_home_d+ peq_home+peq_debt+peq_stock+peq_savings)
        form5b<-update(form5,~. + ego_black2*(eq_home_d+peq_home+peq_debt+peq_stock+peq_savings))
        
        
        
    #d - specify years and model
        #all years
            # I3<-I
        #recession years.
            I3<-I[I$year>2004 & I$year<2012,]
            I3<-I3[I3$ind_id%in% rec.ids,]


#4 - Run models
    results<-list()
    for(i in 1:1){ #length(vars)
      #a - Select variable
        I3$y<-I3[,vars[i]]
      #b - Run models
        m1.1<-model(form1,df=I3)
        print("m1")
        m2.1<-model(form2,df=I3)
        print("m2")
        m3.1<-model(form2b,df=I3)
        print("m3")
        m4.1<-model(form3,df=I3)
        print("m4")
        m5.1<-model(form3b,df=I3)
        print("m5")
        m6.1<-model(form4,df=I3)
        print("m6")
        m7.1<-model(form4b,df=I3)
        print("m7")
        m8.1<-model(form5,df=I3)
        print("m8")
        m9.1<-model(form5b,df=I3)
        print("m9")
        
        m1<-prettyit(m1.1)
        m2<-prettyit(m2.1)
        m3<-prettyit(m3.1)
        m4<-prettyit(m4.1)
        m5<-prettyit(m5.1)
        m6<-prettyit(m6.1)
        m7<-prettyit(m7.1)
        m8<-prettyit(m8.1)
        m9<-prettyit(m9.1)
        
      #c - prep models for merge
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
      #d - prep empty df to put results into
        df<-data.frame(var=unique(c(row.names(m1),row.names(m2),row.names(m3),row.names(m4),row.names(m5),row.names(m6),row.names(m7),row.names(m8),row.names(m9))))
      #e - bind vars to df
        df<-merge(df,m1,by="var",all=T,sort = F)
        df<-merge(df,m2,by="var",all=T,sort = F)
        df<-merge(df,m3,by="var",all=T,sort = F)
        df<-merge(df,m4,by="var",all=T,sort = F)
        df<-merge(df,m5,by="var",all=T,sort = F)
        df<-merge(df,m6,by="var",all=T,sort = F)
        df<-merge(df,m7,by="var",all=T,sort = F)
        df<-merge(df,m8,by="var",all=T,sort = F)
        df<-merge(df,m9,by="var",all=T,sort = F)
      #f - reorder
        df<-df[match(corder,df$var),]
      #g - add empty rows
        # emptydf<-as.data.frame(matrix("",nrow=1,ncol=ncol(df)))
        # names(emptydf)<-names(df)
        # #identify where you to add rows
        #     # empty1<-which(df$var=="ego_dg_highschool")
        #     empty1<-which(df$var=="ego_dg_highschool")
        #     # empty2<-which(df$var=="peq_debt")
        #     # empty3<-which(df$var=="ego_black2:peq_savings")
        #     empty4<-which(df$var=="eq_home_d")
        # #add them
        #     df<-rbind(df[1:(empty1-1),],
        #         emptydf,df[empty1:(empty4-1),],
        #         # emptydf,df[empty2:(empty3-1),],
        #         # emptydf,df[empty3:(empty4-1),],
        #         emptydf,df[empty4:nrow(df),])
        #change NAs to blanks
            df[is.na(df)]<-""
      #h - append to results
        results[[i]]<-df
        print(i)
    }

#5 - Round results
    roundstr<-function(x){ifelse(substr(x,1,1)=="-",substr(x,1,5),substr(x,1,4))}
    for(i in 1:length(results)){
      a<-as.data.frame(sapply(results[[i]],roundstr))
      a$var<-results[[i]]$var
      results[[i]]<-a
    }

#6 - save results to excel
    for(i in 1:length(results)){
      #determine file name/location
          # fname<-"C:/Users/bda13/Desktop/10-12 Mixed Effects all Data.xlsx"
          fname<-"C:/Users/bda13/Desktop/10-24 Mixed Effects 2005-2011.xlsx"
          # fname<-"C:/Users/bda13/Desktop/10-12 Fixed Effects 2005-2011.xlsx"
      #grab results
          tresult<-results[[i]]
      #determine sheet name
          tsheet<-paste(i,"-",vars[i],sep="")
        if(i==1){
          write.xlsx(tresult, file=fname, sheetName=tsheet, row.names=T)
        }else{
          write.xlsx(tresult, file=fname, sheetName=tsheet, append=TRUE, row.names=T)
        }
      }
      
    # saveRDS(results,"models.4-16.rds")

# 
#             
#             arcsin(I$peq_debt)
#             asin(sign(x) * sqrt(abs(x)))
#             
#             trans.arcsine <- function(x){
#               asin(sign(x) * sqrt(abs(x)))
#             }
#             trans.arcsine(I$peq_debt)
#             
#             summary(I$peq_debt)
#             #     
#             #Possible issues: Skew of variables.
#                 quantf<-function(x) quantile(x[x!=0],c(1:10)/10,na.rm=T)
#                 summary(I$peq_debt)
#                 summary(I$peq_savings)
#                 summary(I$peq_home)
#                 summary(I$peq_stock)
#                 
#                 quantf(trans.arcsine(I$peq_debt[I$peq_debt!=0]))
#                 
#                 plot(density(trans.arcsine(I$peq_debt[I$peq_debt!=0]),na.rm=T))
#                 plot(density((I$peq_debt[I$peq_debt!=0])^(1/3)))
#                 # plot(density((I$peq_debt[I$peq_debt!=0])))
#                 plot(density(trans.arcsine(I$peq_savings[I$peq_savings!=0]),na.rm=T))
#                 plot(density((I$peq_savings[I$peq_savings!=0])^(1/3)))
#                 # plot(density((I$peq_savings[I$peq_savings!=0])))
#                 # plot(density(log10(I$peq_home[I$peq_home!=0]*10+1)))
#                 plot(density((I$peq_home[I$peq_home!=0])))
#                 plot(density(log10(I$peq_stock[I$peq_stock!=0]*10+1)))
#                 # plot(density((I$peq_stock[I$peq_stock!=0])))
#                 
#                 
#                 
#                 plot(density(I$peq_savings))
#                 plot(density(I$peq_home))
#                 plot(density(I$peq_stock))
#                 
#                 quantf(I$peq_debt)
#                 quantf(I$peq_savings)
#                 quantf(I$peq_home)
#                 quantf(I$peq_stock)
#                 
#                 summary(I$peq_debt[I$WealthG=="-Lower"])
#                 summary(I$peq_debt[I$WealthG=="-Middle"])
#                 summary(I$peq_debt[I$WealthG=="-Upper"])
#                 quantf(I$peq_debt[I$WealthG=="-Lower"])
#                 quantf(I$peq_debt[I$WealthG=="-Middle"])
#                 quantf(I$peq_debt[I$WealthG=="-Upper"])
#                 summary(I$peq_savings[I$WealthG=="-Lower"])
#                 summary(I$peq_savings[I$WealthG=="-Middle"])
#                 summary(I$peq_savings[I$WealthG=="-Upper"])
#                 quantf(I$peq_savings[I$WealthG=="-Lower"])
#                 quantf(I$peq_savings[I$WealthG=="-Middle"])
#                 quantf(I$peq_savings[I$WealthG=="-Upper"])
#                 summary(I$peq_home[I$WealthG=="-Lower"])
#                 summary(I$peq_home[I$WealthG=="-Middle"])
#                 summary(I$peq_home[I$WealthG=="-Upper"])
#                 quantf(I$peq_home[I$WealthG=="-Lower"])
#                 quantf(I$peq_home[I$WealthG=="-Middle"])
#                 quantf(I$peq_home[I$WealthG=="-Upper"])
#                 summary(I$peq_stock[I$WealthG=="-Lower"])
#                 summary(I$peq_stock[I$WealthG=="-Middle"])
#                 summary(I$peq_stock[I$WealthG=="-Upper"])
#                 quantf(I$peq_stock[I$WealthG=="-Lower"])
#                 quantf(I$peq_stock[I$WealthG=="-Middle"])
#                 quantf(I$peq_stock[I$WealthG=="-Upper"])
#             #     
#             # #Possible solution: log+1.
#             #     I$peq_debt2<-ifelse(I$peq_debt>median(I$peq_debt,na.rm=T),1,0)
#             #     I$peq_stock2<-log10(I$peq_stock+1.000001)
#             #     I$peq_savings2<-log10(I$peq_savings+1.000001)
            #     I$peq_home2<-ifelse(I$peq_home>median(I$peq_home,na.rm=T),1,0)
            # #alternate solution: binary version
            #     I$peq_stock2<-ifelse(I$peq_stock2>.1,1,0)
            

##Models with quantile interactions 
#       ml<-list()
#       for(i in 1:length(vars)){
#       I$y<-I[,vars[i]]
#       {
#         formt<-y ~ 
#           WealthG *  (ego_black2+ego_age+ego_age2) + 
#           
#           (ego_dg_highschool+ego_dg_somecollege+ego_dg_bachelors+ego_dg_advanced) + 
#           (eq_home_d)+
#           WealthG *  (inc_total+eq_wealth) + 
#           WealthG *  (peq_home+peq_debt+peq_stock+peq_savings) + 
#                      (1 | ind_id) + (1 | fam_id_68)
#        
# for(i in 1:length(vars)){
#   (ml[[i]]<-model(formt,df=I))
# }



#       results<-ml
#       roundstr<-function(x){ifelse(substr(x,1,1)=="-",substr(x,1,5),substr(x,1,4))}
#       for(i in 1:length(results)){
#         tempvar<-paste(vars[i],".xlsx",sep = "")
#         tempres<-results[[i]]
#         #for(j in 1:length(tempres)){
#         tempres2<-tempres#[[j]]
#         tempname<-paste("sheet",j,sep="")
#         tempres2[,1]<-roundstr(tempres2[,1])
#         # tempres2[,4]<-roundstr(tempres2[,4])
#         # tempres2[,6]<-roundstr(tempres2[,6])
#         if(j==1){
#           write.xlsx(tempres2, file=tempvar, sheetName=tempname, row.names=T)
#         }else{
#           write.xlsx(tempres2, file=tempvar, sheetName=tempname, append=TRUE, row.names=T)
#           # }
#         }
#       }
# 
# #4 - run models
#     results<-rep(list(vector(mode="list", ncol(partition))),length(vars))
#     for(i in 1:length(vars)){
#         y<-c(vars[i])
#         for(j in 1:ncol(partition)){
#             #create functions
#               form1<-formula(paste(y,"~ ego_black2 + ego_age + ego_age2 + ego_female + ego_dg_highschool+ego_dg_somecollege+ego_dg_bachelors+ego_dg_advanced + (1|ind_id)+ (1|fam_id_68)"))
#               form2<-formula(paste(y,"~ ego_black2 + ego_age + ego_age2 + ego_female + ego_dg_highschool+ego_dg_somecollege+ego_dg_bachelors+ego_dg_advanced + eq_home_d + inc_total +(eq_wealth) + (1|ind_id)+ (1|fam_id_68)"))
#               form3<-formula(paste(y,"~ ego_black2 + ego_age + ego_age2 + ego_female + ego_dg_highschool+ego_dg_somecollege+ego_dg_bachelors+ego_dg_advanced + eq_home_d + inc_total +(peq_home+peq_debt+peq_stock+peq_savings) + (1|ind_id)+ (1|fam_id_68)"))
#             #run models
#               m1<-model(form1,df=I[partition[,j],])
#               m2<-model(form2,df=I[partition[,j],])
#               m3<-model(form3,df=I[partition[,j],])
#             #prep models for merge
#               m1$var<-row.names(m1)
#               names(m1)<-c("1","1.1","var")
#               m2$var<-row.names(m2)
#               names(m2)<-c("2","2.1","var")
#               m3$var<-row.names(m3)
#               names(m3)<-c("3","3.1","var")
#             #prep empty df to put results into
#               df<-data.frame(var=unique(c(row.names(m1),row.names(m2),row.names(m3))))
#             #bind vars to df
#               df<-merge(df,m1,by="var",all=T,sort = F)
#               df<-merge(df,m2,by="var",all=T,sort = F)
#               df<-merge(df,m3,by="var",all=T,sort = F)
#             #reorder
#               df<-df[match(corder,df$var),]
#             #add empty rows
#               emptydf<-as.data.frame(matrix("",nrow=1,ncol=ncol(df)))
#               names(emptydf)<-names(df)
#               empty1<-which(df$var=="ego_dg_highschool")
#               empty2<-which(df$var=="eq_home_d")
#               df<-rbind(df[1:(empty1-1),],emptydf,df[(empty1):(empty2-1),],emptydf,df[empty2:nrow(df),])
#               df[is.na(df)]<-""
#               results[[i]][[j]]<-df
#             print(paste(i,j))
#         }
#     }
    
    

#descriptives    
    vars<-c("h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","h_distress","ego_black2","ego_age","ego_female","year2","just_died","already_died","inc_total","ego_dg_highschool","ego_dg_somecollege","ego_dg_bachelors","ego_dg_advanced","eq_wealth","eq_home_d", "peq_home","peq_savings","peq_stock","peq_debt")
    I2<-I
#2 - Create dataframe with most recent info
    I2<-I2[order(I2$year,decreasing = T),]
    I2<-I2[!(duplicated(I2$ind_id)),]
    I2$ind_id<-NULL
    I2<-I2[,c(vars)]
#3 - Table of all variables:
        sm<-as.data.frame(matrix(0,ncol(I2),4))
        names(sm)<-c("variable","mean","median","sd")
        for(i in 1:length(I2)){
          try({
            sm$variable[i]<-names(I2)[i]
            sm$mean[i]<-mean(I2[,i],na.rm = T)
            sm$median[i]<-median(I2[,i],na.rm = T)
            sm$sd[i]<-sd(I2[,i],na.rm = T)
          })
        }
#4 - Round and save results
    roundstr<-function(x){ifelse(substr(x,1,1)=="-",substr(x,1,5),substr(x,1,4))}
    sm[,c(2:4)]<-as.data.frame(sapply(sm[,c(2:4)],roundstr))
    write.xlsx(sm, file="C:/Users/bda13/Desktop/Descriptives.xlsx",row.names=T)

#by race
    I3<-I2[I2$ego_black2==0,]
    I4<-I2[I2$ego_black2==1,]
    #3 - Table of all variables:
            sm1<-as.data.frame(matrix(0,ncol(I3),4))
            names(sm1)<-c("variable","mean","median","sd")
            for(i in 1:length(I3)){
              try({
                sm1$variable[i]<-names(I3)[i]
                sm1$mean[i]<-mean(I3[,i],na.rm = T)
                sm1$median[i]<-median(I3[,i],na.rm = T)
                sm1$sd[i]<-sd(I3[,i],na.rm = T)
              })
            }
            sm2<-as.data.frame(matrix(0,ncol(I4),4))
            names(sm2)<-c("variable","mean","median","sd")
            for(i in 1:length(I4)){
              try({
                sm2$variable[i]<-names(I4)[i]
                sm2$mean[i]<-mean(I4[,i],na.rm = T)
                sm2$median[i]<-median(I4[,i],na.rm = T)
                sm2$sd[i]<-sd(I4[,i],na.rm = T)
              })
            }
            sm2$variable<-NULL
            sm<-cbind(sm1,sm2)
            
    #4 - Round and save results
        roundstr<-function(x){ifelse(substr(x,1,1)=="-",substr(x,1,5),substr(x,1,4))}
        sm[,c(2:4)]<-as.data.frame(sapply(sm[,c(2:4)],roundstr))
        write.xlsx(sm, file="C:/Users/bda13/Desktop/Descriptives by race.xlsx",row.names=T)

        
      
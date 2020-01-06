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

#1.5 - Create a few other variables
    #good "other debt" variable
        I$eq_debt[I$year>2012]<-I$debt_creditcard[I$year>2012]+I$debt_student[I$year>2012]+I$debt_medical[I$year>2012]+I$debt_family[I$year>2012]
    #assets
        I$eq_assets<-I$eq_wealth+I$eq_debt
    #impute race with mode race
        J<-data.table(I)
        Mode <- function(x) {
          ux <- unique(x)
          ux[which.max(tabulate(match(x, ux)))]
        }
        J<-J[,.(ego_black=ego_black,mode=Mode(ego_black)),by=ind_id]
        temp<-J[ego_black!=mode,]
        temp<-temp[!duplicated(temp$ind_id),]
        I$ego_black2<-J$mode
    #top code hospitalization
        I$h_hospital<-ifelse(I$h_hospital>10,10,K$h_hospital)
    #Revise h_general
        I$h_general<-6-I$h_genera
    #create age sqared term
        I$ego_age2<-I$ego_age^2
    #Reduce dataset to variables of interest
        I<-I[,c("h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","ego_age","ego_age2","ego_black2","ego_female","ego_dg_advanced","ego_dg_bachelors","ego_dg_highschool","ego_dg_somecollege","eq_home","eq_bus","eq_savings","eq_otr_estate","eq_stock","eq_debt","inc_total","eq_wealth","ind_id", "year","fam_id_68","fam_id","eq_home_d")] #"eq_vehicle","eq_otr_assets"
    #remove any rows with nas
        I <- I[rowSums(is.na(I))== 0,]

#degree variables in 1984...
#Ira, especially prior to 1994.


#2 - prep data model
    #create lagged dependent variables by moving the later year variables up one interval year
        #prep new dataset with variables of interest
            J<-I[,c("ind_id","exp_all_medical","exp_health_insurance","exp_hospital","exp_out_of_pocket_medical","exp_prescriptions","eq_bus","eq_home","eq_debt","eq_otr_assets","eq_otr_estate","eq_savings","eq_stock","eq_vehicle","eq_wealth","eq_bond","eq_ira","eq_mortgage1","eq_mortgage2","eq_home_d","eq_home_cost","inc_lump","inc_lump_d","inc_total","inc_friend","inc_relative","inc_salaried_d","inc_self_empl_d","inc_unemployed","inc_hours","ins_medicaid","ins_empl","ins_any","debt_bus","debt_otrestate","debt_creditcard","debt_student","debt_medical","debt_family","debt_other","eq_assets")]
        #loop through each variable in this dataset, making variable = row above
            J$ind_id<-c(NA,J$ind_id[-nrow(J)])
            for(i in 2:ncol(J)){
              J[,i]<-c(NA,J[,i][-nrow(J)])
            }
        #rename J variables and append to I
            names(J)<-paste(names(J),".t1",sep="")
            K<-cbind(I,J)
        #kill rows that are invalid
            K<-K[K$ind_id==K$ind_id.t1,]
            K<-K[!is.na(K$ind_id),]
  l

        #create squared terms
            K$eq_wealth2<-K$eq_wealth^2
            K$eq_wealth.t12<-K$eq_wealth.t1^2
            K$eq_assets.t12<-K$eq_assets.t1^2
            K$eq_debt.t12<-K$eq_debt.t1^2
            K$inc_total.t12<-K$inc_total.t1^2
            K$inc_total2<-K$inc_total^2

#reduce size of variables
    K$eq_home<-K$eq_home/1000
    K$eq_bus<-K$eq_bus/1000
    K$eq_savings<-K$eq_savings/1000
    K$eq_otr_estate<-K$eq_otr_estate/1000
    K$eq_stock<-K$eq_stock/1000
    K$eq_vehicle<-K$eq_vehicle/1000
    K$eq_otr_assets<-K$eq_otr_assets/1000
    K$eq_ira<-K$eq_ira/1000
    K$eq_debt<-K$eq_debt/1000
#log variables
#Remove negatives, then log
      K$rm<-ifelse(K$eq_otr_assets<0|K$eq_home<0|K$eq_bus<0|K$eq_savings<0|K$eq_otr_estate<0|K$eq_stock<0|K$eq_vehicle<0,1,0)
      K$rm<-ifelse(is.na(K$rm),1,K$rm)
      K<-K[K$rm==0,]
      K$eq_home<-log(K$eq_home-min(K$eq_home,na.rm=T)+1)
      K$eq_bus<-log(K$eq_bus-min(K$eq_bus,na.rm=T)+1)
      K$eq_savings<-log(K$eq_savings-min(K$eq_savings,na.rm=T)+1)
      K$eq_otr_estate<-log(K$eq_otr_estate-min(K$eq_otr_estate,na.rm=T)+1)
      K$eq_stock<-log(K$eq_stock-min(K$eq_stock,na.rm=T)+1)
      K$eq_vehicle<-log(K$eq_vehicle-min(K$eq_vehicle,na.rm=T)+1)
      K$eq_otr_assets<-log(K$eq_otr_assets-min(K$eq_otr_assets,na.rm=T)+1)
      K$eq_ira<-log(K$eq_ira-min(K$eq_ira,na.rm=T)+1)
      K$eq_debt<-log(K$eq_debt-min(K$eq_debt,na.rm=T)+1)
      K$inc_total<-log(K$inc_total-min(K$inc_total,na.rm=T)+1)

#Model 1
      summary(plm(h_general ~ ego_age+ego_age2+ego_black2 + ego_female, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_BMI ~ ego_age+ego_age2+ego_black2 + ego_female, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_activities ~ ego_age+ego_age2+ego_black2 + ego_female, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_conditions ~ ego_age+ego_age2+ego_black2 + ego_female, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_lim_work ~ ego_age+ego_age2+ego_black2 + ego_female, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_stroke ~ ego_age+ego_age2+ego_black2 + ego_female, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_heart_attack ~ ego_age+ego_age2+ego_black2 + ego_female, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_hospital ~ ego_age+ego_age2+ego_black2 + ego_female, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(exp_all_medical ~ ego_age+ego_age2+ego_black2 + ego_female, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
#Model 2
      summary(plm(h_general ~ ego_age+ego_age2+ego_black2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_BMI ~ ego_age+ego_age2+ego_black2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_activities ~ ego_age+ego_age2+ego_black2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_conditions ~ ego_age+ego_age2+ego_black2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_lim_work ~ ego_age+ego_age2+ego_black2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_stroke ~ ego_age+ego_age2+ego_black2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_heart_attack ~ ego_age+ego_age2+ego_black2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_hospital ~ ego_age+ego_age2+ego_black2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(exp_all_medical ~ ego_age+ego_age2+ego_black2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
#model 3
      summary(plm(h_general ~ ego_age+ego_age2+ego_black2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt + inc_total, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_BMI ~ ego_age+ego_age2+ego_black2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt + inc_total, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_activities ~ ego_age+ego_age2+ego_black2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt + inc_total, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_conditions ~ ego_age+ego_age2+ego_black2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt + inc_total, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_lim_work ~ ego_age+ego_age2+ego_black2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt + inc_total, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_stroke ~ ego_age+ego_age2+ego_black2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt + inc_total, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_heart_attack ~ ego_age+ego_age2+ego_black2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt + inc_total, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(h_hospital ~ ego_age+ego_age2+ego_black2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt + inc_total, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      summary(plm(exp_all_medical ~ ego_age+ego_age2+ego_black2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt + inc_total, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))

#By race
    #Model 1
        summary(plm(h_general ~ ego_age+ego_age2+ego_female, data=K[K$eq_wealth<500000 & K$ego_black2==0,], index=c("ind_id", "year"), model="random"))
        summary(plm(h_general ~ ego_age+ego_age2+ego_female, data=K[K$eq_wealth<500000 & K$ego_black2==1,], index=c("ind_id", "year"), model="random"))
    #Model 2
        summary(plm(h_general ~ ego_age+ego_age2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt, data=K[K$eq_wealth<500000 & K$ego_black2==0,], index=c("ind_id", "year"), model="random"))
        summary(plm(h_general ~ ego_age+ego_age2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt, data=K[K$eq_wealth<500000 & K$ego_black2==1,], index=c("ind_id", "year"), model="random"))
    #Model 3
        summary(plm(h_general ~ ego_age+ego_age2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt + inc_total, data=K[K$eq_wealth<500000 & K$ego_black2==0,], index=c("ind_id", "year"), model="random"))
        summary(plm(h_general ~ ego_age+ego_age2+ego_female + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt + inc_total, data=K[K$eq_wealth<500000 & K$ego_black2==1,], index=c("ind_id", "year"), model="random"))


#no transformaions
    summary(plm(h_general ~ (ego_age+ego_age2)*ego_black2+ego_female + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege, data=I[I$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
    summary(plm(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_black2*ego_age2+ego_female + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt + inc_total, data=I[I$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
    a<-(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_black2*ego_age2+ego_female + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home+eq_bus+eq_savings+eq_otr_estate+eq_stock+eq_vehicle+eq_otr_assets+eq_ira+eq_debt + inc_total+(1|ind_id)+ (1|fam_id_68),  data=I))
    summary(a)

#create categorical variable function
    cutfun<-function(x){
      #relevel(cut(x, c(seq(from = -100000, to = -10000, by = 10000),-5000,-10,10,5000,seq(from = 10000, to = 500000, by = 10000))), ref="(-10,10]")
      relevel(cut(x, c(min(x),-5000,-10,10,seq(from = 50000, to = 3000000, by = 50000),max(x))), ref="(-10,10]")
    }


#run cutfunction across all financial variables
  columns<-grep("eq|inc",names(I))
  for(i in columns){
      I[,i]<-cutfun(I[,i])
  }

#manually cut each variable
    summary(I$eq_home)
    quantile(I$eq_home,c(.05,.95))
    cuts<-c(min(I$eq_home),-50000,-1,1,50000,250000,500000,750000,1000000,5000000,max(I$eq_home))
    I$eq_home<-relevel(cut(I$eq_home,cuts), ref="(-1,1]")

#with categorical variables
    summary(plm(h_lim_work ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_black2*ego_age2+ego_female + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home3+eq_home_d, data=I, index=c("ind_id", "year"), model="random"))
    a<-(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_black2*ego_age2+ego_female + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home+ eq_home_d +(1|ind_id)+ (1|fam_id_68),  data=I))
summary(a)
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
#medical expenses: need to recode 2013 and 2015
#debt variables: none exist  prior to 2011

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
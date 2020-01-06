library(ggplot2)
library(data.table)
library(plm)
library(pglm)

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
        #Revise h_general
            K$h_general<-6-K$h_general
              
        #build squared terms
            K$eq_wealth2<-K$eq_wealth^2
            K$eq_wealth.t12<-K$eq_wealth.t1^2
            K$eq_assets.t12<-K$eq_assets.t1^2
            K$eq_debt.t12<-K$eq_debt.t1^2
            K$inc_total.t12<-K$inc_total.t1^2
            K$inc_total2<-K$inc_total^2
            K$ego_age2<-K$ego_age^2
        #reduce size of variables
        #     K$eq_home<-K$eq_home/1000
        #     K$eq_bus<-K$eq_bus/1000
        #     K$eq_savings<-K$eq_savings/1000
        #     K$eq_otr_estate<-K$eq_otr_estate/1000
        #     K$eq_stock<-K$eq_stock/1000
        #     K$eq_vehicle<-K$eq_vehicle/1000
        #     K$eq_otr_assets<-K$eq_otr_assets/1000
        #     K$eq_ira<-K$eq_ira/1000
        #     K$eq_debt<-K$eq_debt/1000
        # #log variables
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
            
        #alternative log    
            # K$eq_home<-log(abs(K$eq_home)*sign(K$eq_home)+1)
            # K$eq_bus<-log(abs(K$eq_bus)*sign(K$eq_bus)+1)
            # K$eq_savings<-log(abs(K$eq_savings)*sign(K$eq_savings)+1)
            # K$eq_otr_estate<-log(abs(K$eq_otr_estate)*sign(K$eq_otr_estate)+1)
            # K$eq_stock<-log(abs(K$eq_stock)*sign(K$eq_stock)+1)
            # K$eq_vehicle<-log(abs(K$eq_vehicle)*sign(K$eq_vehicle)+1)
            # K$eq_otr_assets<-log(abs(K$eq_otr_assets)*sign(K$eq_otr_assets)+1)
            # K$eq_ira<-log(abs(K$eq_ira)*sign(K$eq_ira)+1)
            # K$eq_debt<-log(abs(K$eq_debt)*sign(K$eq_debt)+1)
           
            #3 - prep models
    #Simple model for all with less than 500k
        #all
      #       summary(plm(h_general ~ ego_black2*ego_age+ego_black2*ego_age2+ego_black2*ego_married+ego_black2*eq_wealth.t1+ego_black2*eq_wealth.t12 + ego_black2*inc_total.t1 + ego_black2*inc_total.t12, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="within"))
      #       summary(plm(h_general ~ ego_black2*ego_age+ego_black2*ego_age2+ego_black2*ego_married+ego_black2*eq_wealth+ego_black2*eq_wealth2 + ego_black2*inc_total + ego_black2*inc_total2, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      #   #By race
      #       summary(plm(h_general ~ ego_age+ego_age2+ego_married+ego_age*eq_wealth.t1+ego_age*eq_wealth.t12 + ego_age*inc_total.t1 + ego_age*inc_total.t12, data=K[K$eq_wealth<500000 & K$ego_black==0,], index=c("ind_id", "year"), model="within"))
      #       summary(plm(h_general ~ ego_age+ego_age2+ego_married+eq_wealth.t1+eq_wealth.t12 + inc_total.t1 + inc_total.t12, data=K[K$eq_wealth<500000 & K$ego_black==0 & K$ego_age<50,], index=c("ind_id", "year"), model="within"))
      #       
      #       
      #       summary(plm(h_general ~ ego_age+ego_age2+ego_married+eq_wealth.t1+eq_wealth.t12 + inc_total.t1 + inc_total.t12, data=K[K$eq_wealth<500000 & K$ego_black==1,], index=c("ind_id", "year"), model="within"))
      # #less simple
      #       summary(plm(h_general ~ ego_age+ego_age2+ego_married+ eq_debt.t1 + eq_debt.t12 + eq_wealth.t1 + eq_wealth.t12 + inc_total.t1 + inc_total.t12, data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="within"))
      #       summary(plm(h_general ~ ego_age+ego_age2+ego_married+ eq_debt.t1 + eq_debt.t12 + eq_wealth.t1 + eq_wealth.t12 + inc_total.t1 + inc_total.t12, data=K[K$eq_wealth<500000 & K$ego_black==0,], index=c("ind_id", "year"), model="within"))
      #       summary(plm(h_general ~ ego_age+ego_age2+ego_married+ eq_debt.t1 + eq_debt.t12 + eq_wealth.t1 + eq_wealth.t12 + inc_total.t1 + inc_total.t12, data=K[K$eq_wealth<500000 & K$ego_black==1,], index=c("ind_id", "year"), model="within"))
      # #Final
      #       summary(plm(h_general ~ ego_black2*(ego_age+ego_age2+ego_married+ eq_debt.t1 + eq_debt.t12 + eq_wealth.t1 + eq_wealth.t12 + inc_total.t1 + inc_total.t12), data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="within"))
      #       summary(plm(h_BMI ~ ego_black2*(ego_age+ego_age2+ego_married+ eq_debt.t1 + eq_debt.t12 + eq_wealth.t1 + eq_wealth.t12 + inc_total.t1 + inc_total.t12), data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="within"))
      #       summary(plm(h_activities ~ ego_black2*(ego_age+ego_age2+ego_married+ eq_debt.t1 + eq_debt.t12 + eq_wealth.t1 + eq_wealth.t12 + inc_total.t1 + inc_total.t12), data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="within"))
      #       summary(plm(h_conditions ~ ego_black2*(ego_age+ego_age2+ego_married+ eq_debt.t1 + eq_debt.t12 + eq_wealth.t1 + eq_wealth.t12 + inc_total.t1 + inc_total.t12), data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="within"))
      #       summary(plm(h_lim_work ~ ego_black2*(ego_age+ego_age2+ego_married+ eq_debt.t1 + eq_debt.t12 + eq_wealth.t1 + eq_wealth.t12 + inc_total.t1 + inc_total.t12), data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="within"))
      #       summary(plm(h_stroke ~ ego_black2*(ego_age+ego_age2+ego_married+ eq_debt.t1 + eq_debt.t12 + eq_wealth.t1 + eq_wealth.t12 + inc_total.t1 + inc_total.t12), data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="within"))
      #       summary(plm(h_heart_attack ~ ego_black2*(ego_age+ego_age2+ego_married+ eq_debt.t1 + eq_debt.t12 + eq_wealth.t1 + eq_wealth.t12 + inc_total.t1 + inc_total.t12), data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="within"))
      #       summary(plm(h_hospital ~ ego_black2*(ego_age+ego_age2+ego_married+ eq_debt.t1 + eq_debt.t12 + eq_wealth.t1 + eq_wealth.t12 + inc_total.t1 + inc_total.t12), data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="within"))
      #       summary(plm(exp_all_medical ~ ego_black2*(ego_age+ego_age2+ego_married+ eq_debt.t1 + eq_debt.t12 + eq_wealth.t1 + eq_wealth.t12 + inc_total.t1 + inc_total.t12), data=K[K$eq_wealth<500000,], index=c("ind_id", "year"), model="random"))
      #       
            K$h_hospital<-ifelse(K$h_hospital>10,10,K$h_hospital)
                #medical expenses: need to recode 2013 and 2015
                #debt variables: none exist  prior to 2011
            table(I$eq_debt>0,I$year)
            mean(I$eq_debt[I$year==2015]) 
            
summary(K$eq_home)

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

              
              
              
              
              
              
              
              
              
            #random effects syntax
            # a<-(lmer(h_general ~ ego_age+ego_age2+ego_black2+ego_female + (year|ind_id), data=K[K$eq_wealth<500000,]))
            
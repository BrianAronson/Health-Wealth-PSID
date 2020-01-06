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

#2 - Create a few other variables
    #Reduce dataset to variables of interest
        I<-I[,c("h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","ego_age","ego_age2","ego_black2","ego_female","ego_dg_advanced","ego_dg_bachelors","ego_dg_highschool","ego_dg_somecollege","eq_home","eq_bus","eq_savings","eq_otr_estate","eq_stock","eq_debt","inc_total","eq_wealth","ind_id", "year","fam_id_68","fam_id","eq_home_d")] #"eq_vehicle","eq_otr_assets"
    #remove any rows with nas
        I <- I[rowSums(is.na(I))== 0,]

#degree variables in 1984...
#Ira, especially prior to 1994.
#medical expenses: need to recode 2013 and 2015
#debt variables: none exist  prior to 2011
    
#3 - cut each variable of interest    
    #wealth
        x=I$eq_wealth
        cuts<-c(min(x)-1,-1,1,50000,100000,300000,max(x)+1)
        I$eq_wealth2<-relevel(cut(x, cuts,right=F), ref="[-1,1)")
        levels(I$eq_wealth2)<-c("0","<0","0-50k","50k-100k","100k-300k","300k+")
    #home
        x=I$eq_home
        cuts<-c(min(x)-1,-1,1,50000,100000,300000,max(x)+1)
        I$eq_home2<-relevel(cut(x, cuts,right=F), ref="[-1,1)")
        levels(I$eq_home2)<-c("0","<0","0-50k","50k-100k","100k-300k","300k+")
    #debt
        x=I$eq_debt
        cuts<-c(-1,1,25000,50000,100000,max(x)+1)
        I$eq_debt2<-relevel(cut(x, cuts,right=F), ref="[-1,1)")
        levels(I$eq_debt2)<-c("0","0-25k","25k-50k","50k-100k","100k+")
    #stocks
        x=I$eq_stock
        cuts<-c(min(x)-1,-1,1,50000,100000,300000,max(x)+1)
        I$eq_stock2<-relevel(cut(x, cuts,right=F), ref="[-1,1)")
        levels(I$eq_stock2)<-c("0","<0","0-50k","50k-100k","100k-300k","300k+")
    #savings
        x=I$eq_savings
        cuts<-c(-1,1,25000,50000,100000,max(x)+1)
        I$eq_savings2<-relevel(cut(x, cuts,right=F), ref="[-1,1)")
        levels(I$eq_savings2)<-c("0","0-50k","50k-100k","100k-300k","300k+")
        I$eq_savings2[is.na(I$eq_savings2)]<-"0"
    #income
        I$inc_total2<-I$inc_total^2
        

#4 - Models by wealth variable
    #wealth
        m1.1<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + (1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        m2.1<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_wealth2 +(1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        m3.1<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_wealth2 + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home_d + inc_total + inc_total2 + (1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
    #home
        m1.2<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + (1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        m2.2<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_home2 +(1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        m3.2<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_home2 + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home_d + inc_total + inc_total2 + (1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
    #debt
        m1.3<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + (1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        m2.3<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_debt2 +(1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        m3.3<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_debt2 + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home_d + inc_total + inc_total2 + (1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
    #stocks
        m1.4<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + (1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        m2.4<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_stock2 +(1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        m3.4<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_stock2 + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home_d + inc_total + inc_total2 + (1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
    #savings
        m1.5<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + (1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        m2.5<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_savings2 +(1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        m3.5<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_savings2 + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home_d + inc_total + inc_total2 + (1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        
  #5 - print to excel
        library(xlsx)
        write.xlsx(m1.1, file="Models.xlsx", sheetName="sheet1", row.names=FALSE)
        write.xlsx(m2.1, file="Models.xlsx", sheetName="sheet2", append=TRUE, row.names=FALSE)
        write.xlsx(m3.1, file="Models.xlsx", sheetName="sheet3", append=TRUE, row.names=FALSE)
        write.xlsx(m1.2, file="Models.xlsx", sheetName="sheet4", append=TRUE, row.names=FALSE)
        write.xlsx(m2.2, file="Models.xlsx", sheetName="sheet5", append=TRUE, row.names=FALSE)
        write.xlsx(m3.2, file="Models.xlsx", sheetName="sheet6", append=TRUE, row.names=FALSE)
        write.xlsx(m1.3, file="Models.xlsx", sheetName="sheet7", append=TRUE, row.names=FALSE)
        write.xlsx(m2.3, file="Models.xlsx", sheetName="sheet8", append=TRUE, row.names=FALSE)
        write.xlsx(m3.3, file="Models.xlsx", sheetName="sheet9", append=TRUE, row.names=FALSE)
        write.xlsx(m1.4, file="Models.xlsx", sheetName="sheet10", append=TRUE, row.names=FALSE)
        write.xlsx(m2.4, file="Models.xlsx", sheetName="sheet11", append=TRUE, row.names=FALSE)
        write.xlsx(m3.4, file="Models.xlsx", sheetName="sheet12", append=TRUE, row.names=FALSE)
        write.xlsx(m1.5, file="Models.xlsx", sheetName="sheet13", append=TRUE, row.names=FALSE)
        write.xlsx(m2.5, file="Models.xlsx", sheetName="sheet14", append=TRUE, row.names=FALSE)
        write.xlsx(m3.5, file="Models.xlsx", sheetName="sheet15", append=TRUE, row.names=FALSE)

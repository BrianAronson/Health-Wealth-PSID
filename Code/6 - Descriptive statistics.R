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
    I <- readRDS("PreppedData2.rds")

#2 - Create a few other variables
    #Reduce dataset to variables of interest
        I <- I[, c("h_general", "h_BMI", "h_activities", "h_conditions", "h_heart_attack", "h_stroke", "h_hospital", "h_lim_work", "h_disabled", "ego_age", "ego_age2", "ego_black2", "ego_female", "ego_dg_advanced", "ego_dg_bachelors", "ego_dg_highschool", "ego_dg_somecollege", "eq_home", "eq_bus", "eq_savings", "eq_otr_estate", "eq_stock", "eq_debt", "inc_total", "eq_wealth", "ind_id", "year", "fam_id_68", "fam_id", "eq_home_d", "eq_assets", "h_distress", "ego_cohort")] #"eq_vehicle", "eq_otr_assets"
    #remove any rows with nas
        #I  <-  I[rowSums(is.na(I))== 0, ]
    #Asset to debt ratios - Cap these at 1 and 0, except for debt which can be capped at 2; convert NAS to 0, convert Infs to 1.
        propfun <- function(x, debt=F){
          x <- x/I$eq_assets
          x <- ifelse(x<0, 0, x)
          x <- ifelse(is.na(x), 0, x)
          x <- ifelse(is.infinite(x), 1, x)
          if(debt==T){
            x <- ifelse(x>2, 0, x)
          }else{
            x <- ifelse(x>1, 0, x)
          }
          return(x)
        }
        I$peq_home <- propfun(I$eq_home)
        I$peq_debt <- propfun(I$eq_debt, debt=T)
        I$peq_stock <- propfun(I$eq_stock)
        I$peq_savings <- propfun(I$eq_savings)
        I$eq_wealth <- I$eq_wealth/1000
        I$inc_total <- I$inc_total/1000
        
}
    #order varaibles
        corder <- c("ego_black2", "ego_age", "ego_age2", "ego_female", "ego_dg_highschool", "ego_dg_somecollege", "ego_dg_bachelors", "ego_dg_advanced", "eq_home_d", "inc_total", "eq_wealth", "peq_debt", "peq_home", "peq_stock", "peq_savings", "(Intercept)")
        vars <- c("h_general", "h_BMI", "h_activities", "h_conditions", "h_heart_attack", "h_stroke", "h_hospital", "h_lim_work", "h_disabled", "h_distress")
    #create wealth partitions
        partition <- data.frame(lower=c(25>I$eq_wealth & I$eq_wealth>0), 
                              lmiddle=c(50>I$eq_wealth & I$eq_wealth>25), 
                              mmiddle=c(100>I$eq_wealth & I$eq_wealth>50), 
                              umiddle=c(250>I$eq_wealth & I$eq_wealth>100), 
                              uumiddle=c(500>I$eq_wealth & I$eq_wealth>250), 
                              upper=c(1000>I$eq_wealth &I$eq_wealth>500), 
                              uupper=c(I$eq_wealth>1000))

#Produce racial descriptives among highest wealth group    
    df <- data.frame(matrix(nrow=ncol(I), ncol=7))
    names(df) <- c("Name", "Mean", "Min", "10th", "Median", "90th", "Max")
    for(i in 1:ncol(I)){
        df[i, 1] <- names(I)[i]
        df[i, 2] <- mean(I[partition$uupper==T & I$ego_black2==1, i], na.rm=T)
        df[i, 3] <- min(I[partition$uupper==T & I$ego_black2==1, i], na.rm=T)
        df[i, 4] <- quantile(I[partition$uupper==T & I$ego_black2==1, i], na.rm=T, .1)
        df[i, 5] <- median(I[partition$uupper==T & I$ego_black2==1, i], na.rm=T)
        df[i, 6] <- quantile(I[partition$uupper==T & I$ego_black2==1, i], na.rm=T, .9)
        df[i, 7] <- max(I[partition$uupper==T & I$ego_black2==1, i], na.rm=T)
    }
    
    df2 <- df
    for(i in 1:ncol(I)){
        df2[i, 1] <- names(I)[i]
        df2[i, 2] <- mean(I[partition$uupper==T & I$ego_black2==0, i], na.rm=T)
        df2[i, 3] <- min(I[partition$uupper==T & I$ego_black2==0, i], na.rm=T)
        df2[i, 4] <- quantile(I[partition$uupper==T & I$ego_black2==0, i], na.rm=T, .1)
        df2[i, 5] <- median(I[partition$uupper==T & I$ego_black2==0, i], na.rm=T)
        df2[i, 6] <- quantile(I[partition$uupper==T & I$ego_black2==0, i], na.rm=T, .9)
        df2[i, 7] <- max(I[partition$uupper==T & I$ego_black2==0, i], na.rm=T)
    }
    
#Format
    df$` ` <- ""
    df <- cbind(df, df2)
   
#Save  
    write.xlsx(df, "C:/Users/bda13/Desktop/Sociology/Papers in Progress/Health - Race - Wealth/Figures/Descriptives - Wealthy.xlsx")
    
        
        
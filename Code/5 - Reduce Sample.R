library(data.table)
#Reduce Sample

#0 - Set directory
    setwd("C:/Users/bda13/Desktop/Sociology/Statistics/PSID/")
    
#1 - Load data
    I <- readRDS("PreppedData.rds")

#2 - Assume spousal traits don't matter; collapse heads and wives based on relationship to head
    #create a new set of variables based on head and wife variables so that we can have common variables for each individual
        #Find which variables start with "head" and wife
            indexh <- which(substr(names(I), 1, 4)=="head")
            indexw <- which(substr(names(I), 1, 4)=="wife")    
        #Create empty data frame with new variables based on the unique versions of these variables
            newvarnames <- unique(gsub("head", "ego", names(I)[indexh]))
            tempdf <- as.data.frame(matrix(0, ncol=length(newvarnames), nrow=nrow(I)))
            names(tempdf) <- newvarnames
        #Create dataframes of wife and head variables
            tempdfh <- I[, indexh]
            tempdfw <- I[, indexw]
            #Sometimes there is dispute between who is the head of the family in the individual data. This is common during composition changes. Fix
                #convert sex to match
                    I$sex <- ifelse(I$sex==1, "Male", "Female")
                #convert relationship to head to match
                    I$rel_to_head <- ifelse(I$rel_to_head=="head" & I$sex!=I$head_sex, "wife", I$rel_to_head)
        #for each variable in tempdf,  if head,  grab the corresponding information for being a head
            for(i in 1:length(tempdf)){
                tempdf[, i] <- ifelse(I$rel_to_head=="head", tempdfh[, i], tempdf[, i])
            }
        #repeat for wives
            for(i in 1:length(tempdf)){
              #determine if variable is available for wife
                  if(gsub("ego", "wife", names(tempdf))[i] %in% names(tempdfw)){
                      #find where that variable is in the wife dataframe
                          tempindex <-  match(gsub("ego", "wife", names(tempdf))[i], names(tempdfw)) 
                      #grab wife info from that index
                          tempdf[, i] <- ifelse(I$rel_to_head=="wife", tempdfw[, tempindex], tempdf[, i])
                  }
            } 
        #Replace variables that wife did not have
            tempdf$ego_sex <- ifelse(I$rel_to_head=="wife", I$sex, tempdf$ego_sex)
            tempdf$ego_marital <- ifelse(I$rel_to_head=="wife", "Married", tempdf$ego_marital)
            tempdf$ego_veteran <- ifelse(I$rel_to_head=="wife", NA, tempdf$ego_veteran)
        #append these variables to I
            I <- cbind(I, tempdf)
            
#3 - adjust info based on composition change. Sometimes,  the ind_id of a new family member = the ind_id of an old family member.
    #Fix for each value of comp change
        #wife is not same as in previous wave
    
    
#3 - Reduce data to replicate Courtney:
    #Only heads and wives
        I <- I[I$rel_to_head=="wife" | I$rel_to_head=="head", ]
    #Only age 25+
        I <- I[I$age>25 & I$age!=999, ]
    #only 1984 and later
        #I <- I[I$Year %in% c(1984,  1989,  1994,  1999,  2001,  2003,  2005,  2007), ]
        I <- I[I$Year %in% c(1984,  1989,  1994,  1999,  2001,  2003,  2005,  2007, 2009, 2011, 2013, 2015), ]
    #Only blacks and whites
        I$ego_race <- ifelse(is.na(I$ego_race), "NA", I$ego_race)
        I <- I[(I$ego_race=="White" | I$ego_race=="Black") & I$latino==0, ]
    
    #The remaining people who don't match are people whose info is either missing or who just joined the family unit. Remove those I am sure are not the same
        I <- I[I$age==I$ego_age, ]
    #Have all variables available
    #Only interviewed 3+ times
        #kill places without any id
            I <- I[!is.na(I$ind_id), ]
        #find frequency of each id
            temp <- as.data.frame(table(I$ind_id))
            names(temp) <- c("ind_id", "freq")
            temp$keep <- ifelse(temp$freq>2, 1, 0)
            temp$freq <- NULL
        #create data frame with just ind_ids and an index to merge other data into
            tempdf <- data.frame(ind_id=I$ind_id, index=(1:nrow(I)))
            tempdf <- merge(tempdf, temp, by="ind_id")
        #reorder data,  and remove rows
            tempdf <- tempdf[order(tempdf$index), ]
            I <- I[tempdf$keep==1, ]
#4 - Alter/Add/kill variables
    #kill redundant variables
        I[, c(indexh, indexw)] <- NULL
    #BMI
        I$BMI <- I$ego_weight*0.453592/(I$ego_height*0.3048)^2
    #Activities Index
        I$activities <- rowSums(I[, c("ego_bathing", "ego_dressing", "ego_eating", "ego_bed", "ego_walking", "ego_outside", "ego_toilet")],  na.rm=TRUE)
    #health conditions scale
        I$conditions <- I$ego_stroke+I$ego_heart_attack+I$ego_asthma+I$ego_cancer+I$ego_diabetes+I$ego_hypertension
    #Kill variables that aren't yet needed for analysis
        I <- I[, c("Year", "ego_race", "ego_sex", "ego_age", "ego_degree", "ego_hs_grad", "ego_marital", "rel_to_head", "died_at_all", "ego_cohort", "fam_children_18", "ego_health_gen", "BMI", "activities", "conditions", "ego_heart_attack", "ego_stroke", "ego_health_hosp", "ego_health_lim_work", "ego_disabled", "expenditures_all_medical", "expenditures_health_insurance", "expenditures_hospital", "expenditures_out_of_pocket_medical", "expenditures_prescriptions", "s_value_business", "s_value_house", "s_value_other_debt", "s_value_otr_assets", "s_value_otr_estate", "s_value_savings", "s_value_stock", "s_value_vehicle", "s_value_wealth_withequity", "value_bond", "value_ira", "value_mortgage_principle", "value_mortgage2_principal", "dummy_own_home", "value_house", "value_intervivo", "dummy_lump_sum", "value_total_income", "ego_friend_transfer_value", "ego_relative_transfer_value", "ego_salaried_dummy", "ego_self_empl", "ego_unemployed", "ego_work_hour", "medicaid", "insurance_empl", "insurance_any", "ind_id", "debt_bus", "debt_otrestate", "debt_creditcard", "debt_student", "debt_medical", "debt_legal", "debt_family", "debt_other", "fam_id", "fam_id_68", "ego_distress", "fam_region")]
    #rename variables
        names(I) <- c(c("year", "ego_black", "ego_female", "ego_age", "ego_degree", "ego_hs_grad", "ego_marital", "ego_head", "ego_died", "ego_cohort", "ego_children18", "h_general", "h_BMI", "h_activities", "h_conditions", "h_heart_attack", "h_stroke", "h_hospital", "h_lim_work", "h_disabled", "exp_all_medical", "exp_health_insurance", "exp_hospital", "exp_out_of_pocket_medical", "exp_prescriptions", "eq_bus", "eq_home", "eq_debt", "eq_otr_assets", "eq_otr_estate", "eq_savings", "eq_stock", "eq_vehicle", "eq_wealth", "eq_bond", "eq_ira", "eq_mortgage1", "eq_mortgage2", "eq_home_d", "eq_home_cost", "inc_lump", "inc_lump_d", "inc_total", "inc_friend", "inc_relative", "inc_salaried_d", "inc_self_empl_d", "inc_unemployed", "inc_hours", "ins_medicaid", "ins_empl", "ins_any", "ind_id", "debt_bus", "debt_otrestate", "debt_creditcard", "debt_student", "debt_medical", "debt_legal", "debt_family", "debt_other", "fam_id", "fam_id_68", "h_distress", "fam_region"))    #convert/break out variables
    #alter/add variables
        I$ego_black <- ifelse(I$ego_black=="Black", 1, 0)
        I$ego_female <- ifelse(I$ego_female=="Female", 1, 0)
        I$ego_dg_lthighschool <- ifelse(I$ego_hs_grad==3, 1, 0)
        I$ego_dg_somecollege <- ifelse(I$ego_degree=="Associates", 1, 0)
        I$ego_dg_bachelors <- ifelse(I$ego_degree=="BA", 1, 0)
        I$ego_dg_advanced <- ifelse(I$ego_degree=="Post-Graduate", 1, 0) #these do not exclude NAs
        I$ego_dg_highschool <- ifelse( (I$ego_hs_grad==1|I$ego_hs_grad==2) & I$ego_dg_somecollege==0 & I$ego_dg_bachelors==0 & I$ego_dg_advanced==0, 1, 0)
        I$ego_degree <- NULL
        I$ego_hs_grad <- NULL
        I$ego_married <- ifelse(I$ego_marital=="Married", 1, 0)
        I$ego_marital <- NULL
        I$ego_head <- ifelse(I$ego_head=="head", 1, 0)
        I$h_distress[I$ego_head==0] <- NA
    #reorder variables
        I <- I[, c("year", "ego_black", "ego_female", "ego_age", "ego_head", "ego_died", "ego_married", "ego_children18", "ego_cohort", "ego_dg_lthighschool", "ego_dg_highschool", "ego_dg_somecollege", "ego_dg_bachelors", "ego_dg_advanced", "h_general", "h_BMI", "h_activities", "h_conditions", "h_distress", "h_heart_attack", "h_stroke", "h_hospital", "h_lim_work", "h_disabled", "exp_all_medical", "exp_health_insurance", "exp_hospital", "exp_out_of_pocket_medical", "exp_prescriptions", "eq_bus", "eq_home", "eq_debt", "eq_otr_assets", "eq_otr_estate", "eq_savings", "eq_stock", "eq_vehicle", "eq_wealth", "eq_bond", "eq_ira", "eq_mortgage1", "eq_mortgage2", "eq_home_d", "eq_home_cost", "inc_lump", "inc_lump_d", "inc_total", "inc_friend", "inc_relative", "inc_salaried_d", "inc_self_empl_d", "inc_unemployed", "inc_hours", "ins_medicaid", "ins_empl", "ins_any", "ind_id", "debt_bus", "debt_otrestate", "debt_creditcard", "debt_student", "debt_medical", "debt_legal", "debt_family", "debt_other", "fam_id", "fam_id_68", "fam_region")]

#5 - Inflate numbers to 2015
    #read CPI
        CPI <- read.csv("C:/Users/bda13/Desktop/Sociology/Statistics/CPI info/CPI.csv")
        CPI <- read.csv("C:/Users/admin/Desktop/Sociology/CPI info/CPI.csv")
    #determine year to inflate to; adjust other numbers accordingly
        cpiindex <- CPI$Avg[CPI$Year==max(I$year)]
        CPI$Avg <- cpiindex/CPI$Avg
    #create a vector of these numbers equal to nrow I,  indexed by year
        temp <- data.frame(Year=I$year, index=1:nrow(I))
        temp <- merge(temp, CPI, keep=x)
        inflatenums <- temp[order(temp$index), ]
    #pick variables to inflate
        #scan variables for numbers that go higher than 1000 (this just happens to work)
            varmax <- vector()
            for(i in 1:length(I)){
              try({
                varmax[i] <- max(I[, i], na.rm = T)
              })
            }
          #exclude id variables from vars to inflate
            toinflate <- which(varmax>1000 & varmax!=max(I$year) & varmax!=max(I$ind_id)& varmax!=max(I$fam_id)& varmax!=max(I$fam_id_68))
            
          #inflate those variables and divide by 1000
            for(i in 1:length(toinflate)){
                I[, toinflate[i]] <- inflatenums$Avg*I[, toinflate[i]]
            }
            
#6 - create/transform a few more variables
    #good "other debt" variable
        I$eq_debt[I$year>2012] <- I$debt_creditcard[I$year>2012]+I$debt_student[I$year>2012]+I$debt_medical[I$year>2012]+I$debt_family[I$year>2012]
    #assets
        I$eq_assets <- I$eq_wealth+I$eq_debt
    #impute race with mode race
        J <- data.table(I)
        Mode  <-  function(x) {
          ux  <-  unique(x)
          ux[which.max(tabulate(match(x,  ux)))]
        }
        J <- J[, .(ego_black=ego_black, mode=Mode(ego_black)), by=ind_id]
        temp <- J[ego_black!=mode, ]
        temp <- temp[!duplicated(temp$ind_id), ]
        I$ego_black2 <- J$mode
    #top code hospitalization
        I$h_hospital <- ifelse(I$h_hospital>10, 10, I$h_hospital)
    #Revise h_general
        I$h_general <- 6-I$h_genera
    #create age sqared term
        I$ego_age2 <- I$ego_age^2

#7 - save
    saveRDS(I, "PreppedData2.rds")

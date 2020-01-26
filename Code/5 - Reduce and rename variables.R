library(data.table)

#0 - Set directory
    setwd("C:/Users/admin/Desktop/Sociology/PSID Data")
    
#1 - Load data
    I <- readRDS("4 - Merged_Data.rds")

#2 - create a few important variables
    #BMI
        I$BMI <- I$ego_weight*0.453592/(I$ego_height*0.3048)^2
    #Activities Index
        I$activities <- rowSums(I[, c("ego_bathing", "ego_dressing", "ego_eating", "ego_bed", "ego_walking", "ego_outside", "ego_toilet")],  na.rm=TRUE)
    #health conditions scale
        I$conditions <- I$ego_stroke + I$ego_heart_attack + I$ego_asthma + I$ego_cancer + I$ego_diabetes + I$ego_hypertension
    #alter/add variables
        I$ego_black <- ifelse(I$ego_race == "Black", 1, 0)
        I$ego_sex <- ifelse(I$ego_sex == "Female", 1, 0)
        I$ego_dg_lthighschool <- ifelse(I$ego_hs_grad == 3, 1, 0)
        I$ego_dg_somecollege <- ifelse(I$ego_degree == "Associates", 1, 0)
        I$ego_dg_bachelors <- ifelse(I$ego_degree == "BA", 1, 0)
        I$ego_dg_advanced <- ifelse(I$ego_degree == "Post-Graduate", 1, 0) #these do not exclude NAs
        I$ego_dg_highschool <- ifelse( (I$ego_hs_grad == 1|I$ego_hs_grad == 2) & I$ego_dg_somecollege == 0 & I$ego_dg_bachelors == 0 & I$ego_dg_advanced == 0, 1, 0)
        I$ego_married <- ifelse(I$ego_marital == "Married", 1, 0)
        I$ego_head <- ifelse(I$rel_to_head == "head", 1, 0)
        I$h_distress[I$ego_head == 0] <- NA
        
#3 - rename, remove, and reorder variables
    I <- data.table(I)
    setnames(I,
             old = c("Year", "ego_sex", "died_at_all", "fam_children_18", "ego_health_gen", "BMI", "activities", "conditions", "ego_heart_attack", "ego_stroke", "ego_health_hosp", "ego_health_lim_work", "ego_disabled", "expenditures_all_medical", "expenditures_health_insurance", "expenditures_hospital", "expenditures_out_of_pocket_medical", "expenditures_prescriptions", "s_value_business", "s_value_house", "s_value_other_debt", "s_value_otr_assets", "s_value_otr_estate", "s_value_savings", "s_value_stock", "s_value_vehicle", "s_value_wealth_withequity", "value_bond", "value_ira", "value_mortgage_principle", "value_mortgage2_principal", "dummy_own_home", "value_house", "value_intervivo", "dummy_lump_sum", "value_total_income", "ego_friend_transfer_value", "ego_relative_transfer_value", "ego_salaried_dummy", "ego_self_empl", "ego_unemployed", "ego_work_hour", "medicaid", "insurance_empl", "insurance_any", "ego_distress"),
             new = c("year", "ego_female", "ego_died", "ego_children18", "h_general", "h_BMI", "h_activities", "h_conditions", "h_heart_attack", "h_stroke", "h_hospital", "h_lim_work", "h_disabled", "exp_all_medical", "exp_health_insurance", "exp_hospital", "exp_out_of_pocket_medical", "exp_prescriptions", "eq_bus", "eq_home", "eq_debt", "eq_otr_assets", "eq_otr_estate", "eq_savings", "eq_stock", "eq_vehicle", "eq_wealth", "eq_bond", "eq_ira", "eq_mortgage1", "eq_mortgage2", "eq_home_d", "eq_home_cost", "inc_lump", "inc_lump_d", "inc_total", "inc_friend", "inc_relative", "inc_salaried_d", "inc_self_empl_d", "inc_unemployed", "inc_hours", "ins_medicaid", "ins_empl", "ins_any", "h_distress")
            )
    I <- as.data.frame(I[, c("year", "ego_black", "ego_female", "ego_age", "ego_head", "ego_died", "ego_married", "ego_children18", "ego_cohort", "ego_dg_lthighschool", "ego_dg_highschool", "ego_dg_somecollege", "ego_dg_bachelors", "ego_dg_advanced", "h_general", "h_BMI", "h_activities", "h_conditions", "h_distress", "h_heart_attack", "h_stroke", "h_hospital", "h_lim_work", "h_disabled", "exp_all_medical", "exp_health_insurance", "exp_hospital", "exp_out_of_pocket_medical", "exp_prescriptions", "eq_bus", "eq_home", "eq_debt", "eq_otr_assets", "eq_otr_estate", "eq_savings", "eq_stock", "eq_vehicle", "eq_wealth", "eq_bond", "eq_ira", "eq_mortgage1", "eq_mortgage2", "eq_home_d", "eq_home_cost", "inc_lump", "inc_lump_d", "inc_total", "inc_friend", "inc_relative", "inc_salaried_d", "inc_self_empl_d", "inc_unemployed", "inc_hours", "ins_medicaid", "ins_empl", "ins_any", "ind_id", "debt_bus", "debt_otrestate", "debt_creditcard", "debt_student", "debt_medical", "debt_legal", "debt_family", "debt_other", "fam_id", "fam_id_68", "fam_region","badmatch", "rel_to_head", "ego_race", "latino")])

#4 - save
    saveRDS(I, "5 - Merged_Data.rds")

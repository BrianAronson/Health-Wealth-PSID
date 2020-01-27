#Note: Even with easyPSID,  variable codings often change year to year. This requires many manual edits to the longitudinal dataset.

#1 - Load package
    library(easyPSID)

#2 - Set directory
    setwd("C:/Users/bda13/Desktop/Sociology/Statistics/PSID/")
    setwd("C:/Users/admin/Desktop/Sociology/PSID Data")
    setwd("C:/Users/briarons/Desktop/Analysis - Data/PSID")
    
#3 - Create a custom panel based on Variables of interest
    PSID.Custom.Panel(VariableNames = c("V3", "V5", "V47", "V53", "V81", "V93", "V103", "V117", "V118", "V119", "V181", "V196", "V198", "V239", "V246", "V266", "V313", "V315", "V361", "V398", "V451", "V534", "V542", "V546", "V595", "V603", "V604", "V718", "V1220", "V1296", "V2719", "ER3139", "ER3479", "V3843", "V4099", "V4105", "V4767", "V4841", "V4844", "V4892", "V6662", "V6933", "V7977", "V7985", "V10877", "V10884", "V10899", "V10903", "V10908", "V10913", "V10918", "V10923", "V10933", "V10937", "V11945", "V11956", "V11961", "V12293", "V12300", "V12311", "V12316", "ER15014", "ER15452", "ER15458", "ER15464", "ER15470", "ER15482", "ER15506", "ER15525", "ER15527", "ER15529", "ER15531", "ER15533", "ER15535", "ER15537", "ER15552", "ER15553", "ER15554", "ER15560", "ER15566", "ER15572", "ER15578", "ER15590", "ER15614", "ER15633", "ER15635", "ER15637", "ER15639", "ER15641", "ER15643", "ER15645", "ER15660", "ER15661", "ER15662", "ER15780", "ER15781", "ER15787", "ER15793", "ER15799", "V22142", "V22158", "V22319", "V22335", "ER25053", "S103", "S105", "S107", "S109", "S111", "S113", "S115", "S116", "S117", "S120", "ER2069", "ER21147", "ER2563", "ER21397", "V12292", "ER58155", "ER65354", "ER65364", "ER65382", "ER65386", "ER65390", "ER65394", "ER65398", "ER65402", "ER58165", "ER19833A"))
    
#4 - load panel data
    D <- get(load("PSID Panel.rdata"))
    rm(PSIDPanel)

#5 - Alter variables of interest 
    #General fixes
        D$V718 <- ifelse(D$V718 == 9, NA, ifelse(D$V718 == 8, NA, ifelse(D$V718 == 5, 0, D$V718)))
        D$V5 <- ifelse(D$V5 == 9999999, NA, ifelse(D$V5 == 9999998, NA, D$V5))
        D$V266 <- ifelse(D$V266 == 999999, NA, ifelse(D$V266 == 999998, NA, D$V266))
        D$V10937 <- ifelse(D$V10937 == 5, 0, ifelse(D$V10937 == 8, NA, ifelse(D$V10937 == 9, NA, D$V10937)))
        D$V117 <- ifelse(D$Year < 1997 & (D$V117 == 99 | D$V117 == 0 | D$V117 == 999 | D$V117 == 998), NA, D$V117)
        D$V117 <- ifelse((D$Year == 1968 | D$Year == 1995 | D$Year == 1996) & (D$V117 == 98 | D$V117 == 0), NA, D$V117)
        D$V117 <- ifelse(D$Year > 1996 & (D$V117 == 999 | D$V117 == 998 | D$V117 == 0), NA, D$V117)
        D$V181 <- ifelse((D$V181 == 0 | D$V181 == 8 | D$V181 == 9), NA, D$V181)
        D$V181 <- ifelse(D$Year < 1985,  ifelse(D$V181 == 1, "White", ifelse(D$V181 == 2, "Black", ifelse(D$V181 == 3, "Latino", ifelse(D$V181 == 7, "Other", D$V181)))), D$V181)
        D$V181 <- ifelse(D$Year == 1985,  ifelse(D$V181 == 1, "White", ifelse(D$V181 == 2, "Black", ifelse(D$V181 == 3, "American Indian", ifelse(D$V181 == 4, "Asian/Pac Islander", ifelse(D$V181 == 7, "Other", ifelse(D$V181 == 8, "Multiple", D$V181)))))), D$V181)
        D$V181 <- ifelse(D$Year > 1985 & D$Year < 1990,  ifelse(D$V181 == 1, "White", ifelse(D$V181 == 2, "Black", ifelse(D$V181 == 3, "American Indian", ifelse(D$V181 == 4, "Asian/Pac Islander", ifelse(D$V181 == 7, "Other", D$V181))))), D$V181)
        D$V181 <- ifelse(D$Year > 1989 & D$Year < 2005,  ifelse(D$V181 == 1, "White", ifelse(D$V181 == 2, "Black", ifelse(D$V181 == 3, "American Indian", ifelse(D$V181 == 4, "Asian/Pac Islander", ifelse(D$V181 == 5, "Latino", ifelse(D$V181 == 6, "Other", ifelse(D$V181 == 7, "Other", D$V181))))))), D$V181)
        D$V181 <- ifelse(D$Year > 2004,  ifelse(D$V181 == 1, "White", ifelse(D$V181 == 2, "Black", ifelse(D$V181 == 3, "American Indian", ifelse(D$V181 == 4, "Asian", ifelse(D$V181 == 5, "Pac Islander", ifelse(D$V181 == 7, "Other", D$V181)))))), D$V181)
        D$V239 <- ifelse(D$V239 == 9, NA, ifelse(D$V239 == 6, 5, ifelse(D$V239 == 1, "Married", ifelse(D$V239 == 2, "Single", ifelse(D$V239 == 3, "Widowed", ifelse(D$V239 == 4, "Divorced", ifelse(D$V239 == 5, "Separated", D$V239)))))))
        D$V11956 <- ifelse(D$V11956 == 9, 0, ifelse(D$V11956 == 5, 0, D$V11956))
        D$V4099 <- ifelse(D$V4099 == 9, 0, ifelse(D$V4099 == 5, 0, D$V4099))
        D$V11961 <- ifelse((D$V11961 == 98 | D$V11961 == 99), NA, ifelse((D$V11961 == 4 | D$V11961 == 5 | D$V11961 == 6), 3, ifelse(D$V11961 == 97, 1, D$V11961)))
        D$V11961 <- ifelse(D$V11961 == 0, "No College Degree", ifelse(D$V11961 == 1, "Associates", ifelse(D$V11961 == 2, "BA", ifelse(D$V11961 == 3, "Post-Graduate", D$V11961))))
        D$V313 <- ifelse((D$V313 == 4 | D$V313 == 5), 1, ifelse((D$V313 == 3 | D$V313 == 2 | D$V313 == 1 | D$V313 == 0), 0, ifelse(D$V313 == 6, 2, ifelse(D$V313 == 7, 3, ifelse(D$V313 == 8, 4, NA)))))
        D$V313 <- ifelse(D$V313 == 0, "Less than HS", ifelse(D$V313 == 1, "High School", ifelse(D$V313 == 2, "Some College", ifelse(D$V313 == 3, "BA/BS", ifelse(D$V313 == 4, "Post-Graduate", D$V313)))))
        D$V196 <- ifelse(D$Year > 1996, D$ER2069, D$V196)
        D$V196 <- ifelse(D$V196 == 1, "Employed", ifelse(D$V196 == 2, "Laid off", ifelse(D$V196 == 3, "Unemployed", ifelse(D$V196 == 4, "Retired", ifelse(D$V196 == 5, "Disabled", ifelse(D$V196 == 6, "Homemaker", ifelse(D$V196 == 7, "Student", ifelse(D$V196 == 9, NA, D$V196))))))))#made 9  ==  NA because unexpected value
        D$V198 <- ifelse(D$Year > 2002, D$ER21147, D$V198)
        D$V198 <- ifelse(D$Year > 1968,  ifelse((D$V198 == 1 | D$V198 ==  2 | D$V198 == 8 | D$V198 == 9), 0, ifelse(D$V198 == 3, 1, D$V198)), D$V198)#need to fix 1968
        D$V12293 <- ifelse(D$V12292 > 0 & D$V12292 < 8, "Latino", D$V12293)
        D$V12293 <- ifelse(D$V12293 == 1, "White", ifelse(D$V12293 == 2, "Black", ifelse(D$V12293 == 3, "American Indian", ifelse((D$V12293 == 4 | D$V12293 == 5), "Asian/Pac Islander", ifelse(D$V12293 == 7, "Other", D$V12293)))))
        D$V12311 <- ifelse((D$V12311 == 0 | D$V12311 == 5), 0, ifelse(D$V12311 == 9, NA, D$V12311))
        D$V12311 <- ifelse(D$V12311 == 1, "Attended College", ifelse(D$V12311 == 0, "Did not Attend College", D$V12311))
        D$V4105 <- ifelse((D$V4105 == 0 | D$V4105 == 5), 0, ifelse(D$V4105 == 9, NA, D$V4105))
        D$V4105 <- ifelse(D$V4105 == 1, "Graduated College", ifelse(D$V4105 == 0, "Did not Graduate College", D$V4105))
        D$V12316 <- ifelse((D$V12316 == 3 | D$V12316 == 4 | D$V12316 == 5 | D$V12316 == 6), 3, ifelse((D$V12316 == 98 | D$V12316 == 99), NA, D$V12316))#assumed 99 means NA,  not in notes
        D$V12316 <- ifelse(D$V12316 == "97", 1, D$V12316) #weird.... 97 was "97",  as.numeric didn't work
        D$V12316 <- ifelse(D$V12316 == 0, "No College Degree", ifelse(D$V12316 == 1, "Associates", ifelse(D$V12316 == 2, "BA", ifelse(D$V12316 == 3, "Post-Graduate", D$V12316))))
        D$V246 <- ifelse(D$Year < 1991,  ifelse((D$V246 == 1 | D$V246 == 2 | D$V246 == 3), 0, ifelse((D$V246 == 4 | D$V246 == 5), 1, ifelse(D$V246 == 6, 2, ifelse(D$V246 == 7, 3, ifelse(D$V246 == 8, 4, D$V246))))), D$V246)
        D$V246 <- ifelse(D$V246 == 0, "Less than HS", ifelse(D$V246 == 1, "High School", ifelse(D$V246 == 2, "Some College", ifelse(D$V246 == 3, "BA/BS", ifelse(D$V246 == 4, "Post-Graduate", D$V246)))))
        D$V4841 <- ifelse(D$Year > 1996, D$ER2563, D$V4841)
        D$V4841 <- ifelse(D$V4841 == 1, "Employed", ifelse(D$V4841 == 2, "Laid off", ifelse(D$V4841 == 3, "Unemployed", ifelse(D$V4841 == 4, "Retired", ifelse(D$V4841 == 5, "Disabled", ifelse(D$V4841 == 6, "Homemaker", ifelse(D$V4841 == 7, "Student", ifelse(D$V4841 == 9, NA, D$V4841))))))))#made 9  ==  NA because unexpected value
        D$V4844 <- ifelse(D$Year > 2002, D$ER21397, D$V4844)
        D$V4844 <- ifelse(D$V4844 == 3, 1, ifelse(is.na(D$V4844), NA, 0))
        D$V93 <- ifelse(D$V93 == 99 | D$V93 == 0, NA, D$V93)
        D$V451 <- ifelse(D$V451 == 9999999 | D$V451 == 9999998, NA, D$V451)
        D$V6933 <- ifelse(D$V6933 == 9999, NA, D$V6933)
        D$V6933 <- ifelse(D$Year > 1993 & D$V6933 == 9998, NA, D$V6933)
        D$V3843 <- ifelse(D$Year < 1994 & D$Year > 1977,  ifelse(D$V3843 == 9, NA, D$V3843), D$V3843)
        D$V3843 <- ifelse(D$Year > 1993,  ifelse(D$V3843 == 8 | D$V3843 == 9, NA, D$V3843), D$V3843)
        D$V1220 <- D$V1220
        D$V119 <- ifelse(D$V119 == 0 | D$V119 == 9, NA, D$V119)
        D$V119 <- ifelse(D$V119 == 1, "Male", ifelse(D$V119 == 2, "Female", D$V119))
        D$V118 <- ifelse(D$V118 == 999, NA, D$V118)
        D$V118 <- ifelse(D$Year < 1997 & D$V118 == 99, NA, D$V118)
        D$V103 <- ifelse(D$V103 == 1, "Owns", ifelse(D$V103 == 5, "Rents", ifelse(D$V103 == 8, "Neither", D$V103)))
        D$V595 <- ifelse(D$V595 == 0 | D$V595 == 8 | D$V595 == 9, NA, ifelse(D$V595 == 1, "Yes", ifelse(D$V595 == 5, "No", D$V595)))
        D$V603 <- ifelse(D$V603 == 8 | D$V603 == 9, NA, ifelse(D$V603 == 1, "Yes", ifelse(D$V603 == 5, "No", D$V603)))
        D$V6662 <- ifelse(D$V6662 == 1, "Yes", ifelse(D$V6662 == 2, "No", ifelse(D$V6662 == 3, "Female Head", D$V6662)))
        D$V10877 <- ifelse(D$V10877 == 1, "Excellent", ifelse(D$V10877 == 2, "Very Good", ifelse(D$V10877 == 3, "Good", ifelse(D$V10877 == 4, "Fair", ifelse(D$V10877 == 5, "Poor", ifelse(D$V10877 == 8 | D$V10877 == 9, NA, D$V10877))))))
        D$V10884 <- ifelse(D$V10884 == 1, "Excellent", ifelse(D$V10884 == 2, "Very Good", ifelse(D$V10884 == 3, "Good", ifelse(D$V10884 == 4, "Fair", ifelse(D$V10884 == 5, "Poor", ifelse(D$V10884 == 8 | D$V10884 == 9 | D$V10884 == 0, NA, D$V10884))))))
        D$V10899 <- ifelse(D$Year < 1989 & (D$V10899 == 0 | D$V10899 == 999997 | D$V10899 == 999998 | D$V10899 == 999999), NA, D$V10899)
        D$V10899 <- ifelse(D$Year == 1989 & D$V10899 == 0, NA, D$V10899)
        D$V10899 <- ifelse(D$Year == 1994 & (D$V10899 == 0 | D$V10899 == 9999997 | D$V10899 == 9999998 | D$V10899 == 9999999), NA, D$V10899)
        D$V10899 <- ifelse(D$Year > 1994 & (D$V10899 == 0 | D$V10899 == 999999997 | D$V10899 == 999999998 | D$V10899 == 999999999), NA, D$V10899)
        D$V315 <- ifelse(D$V315 == 1, "Yes", ifelse(D$V315 == 5, "No", ifelse(D$V315 == 9, NA, D$V315)))
    #height
        D$ER15553 <- ifelse(D$ER15553 < 2 | D$ER15553 > 7, NA, D$ER15553)
        D$ER15554 <- ifelse(D$ER15554 > 12, NA, D$ER15554)
        D$ER15553 <- D$ER15553+D$ER15554/12
        D$ER15661 <- ifelse(D$ER15661 < 2 | D$ER15661 > 7, NA, D$ER15661)
        D$ER15662 <- ifelse(D$ER15662 > 12, NA, D$ER15662)
        D$ER15661 <- D$ER15661+D$ER15662/12
    #weight
        D$ER15660 <- ifelse(D$ER15660 < 20 | D$ER15660 > 997, NA, D$ER15660)
        D$ER15552 <- ifelse(D$ER15552 < 20 | D$ER15552 > 997, NA, D$ER15552)
    #physical limitations
        D$ER15633 <- ifelse(D$ER15633 == 5, 0, ifelse(D$ER15633 == 1, 1, NA))
        D$ER15635 <- ifelse(D$ER15635 == 5, 0, ifelse(D$ER15635 == 1, 1, NA))
        D$ER15637 <- ifelse(D$ER15637 == 5, 0, ifelse(D$ER15637 == 1, 1, NA))
        D$ER15639 <- ifelse(D$ER15639 == 5, 0, ifelse(D$ER15639 == 1, 1, NA))
        D$ER15641 <- ifelse(D$ER15641 == 5, 0, ifelse(D$ER15641 == 1, 1, NA))
        D$ER15643 <- ifelse(D$ER15643 == 5, 0, ifelse(D$ER15643 == 1, 1, NA))
        D$ER15645 <- ifelse(D$ER15645 == 5, 0, ifelse(D$ER15645 == 1, 1, NA))
        D$ER15525 <- ifelse(D$ER15525 == 5, 0, ifelse(D$ER15525 == 1, 1, NA))
        D$ER15527 <- ifelse(D$ER15527 == 5, 0, ifelse(D$ER15527 == 1, 1, NA))
        D$ER15529 <- ifelse(D$ER15529 == 5, 0, ifelse(D$ER15529 == 1, 1, NA))
        D$ER15531 <- ifelse(D$ER15531 == 5, 0, ifelse(D$ER15531 == 1, 1, NA))
        D$ER15533 <- ifelse(D$ER15533 == 5, 0, ifelse(D$ER15533 == 1, 1, NA))
        D$ER15535 <- ifelse(D$ER15535 == 5, 0, ifelse(D$ER15535 == 1, 1, NA))
        D$ER15537 <- ifelse(D$ER15537 == 5, 0, ifelse(D$ER15537 == 1, 1, NA))
    #hospital
        D$V7977 <- ifelse(D$V7977 > 366, NA, D$V7977)
        D$V7985 <- ifelse(D$V7985 > 366, NA, D$V7985)
    #Diseases
        D$ER15452 <- ifelse(D$ER15452 == 5, 0, ifelse(D$ER15452 == 1, 1, NA))
        D$ER15458 <- ifelse(D$ER15458 == 5, 0, ifelse(D$ER15458 == 1, 1, NA))
        D$ER15464 <- ifelse(D$ER15464 == 5, 0, ifelse(D$ER15464 == 1, 1, NA))
        D$ER15470 <- ifelse(D$ER15470 == 5, 0, ifelse(D$ER15470 == 1, 1, NA))
        D$ER15482 <- ifelse(D$ER15482 == 5, 0, ifelse(D$ER15482 == 1, 1, NA))
        D$ER15506 <- ifelse(D$ER15506 == 5, 0, ifelse(D$ER15506 == 1, 1, NA))
        D$ER15560 <- ifelse(D$ER15560 == 5, 0, ifelse(D$ER15560 == 1, 1, NA))
        D$ER15566 <- ifelse(D$ER15566 == 5, 0, ifelse(D$ER15566 == 1, 1, NA))
        D$ER15572 <- ifelse(D$ER15572 == 5, 0, ifelse(D$ER15572 == 1, 1, NA))
        D$ER15578 <- ifelse(D$ER15578 == 5, 0, ifelse(D$ER15578 == 1, 1, NA))
        D$ER15590 <- ifelse(D$ER15590 == 5, 0, ifelse(D$ER15590 == 1, 1, NA))
        D$ER15614 <- ifelse(D$ER15614 == 5, 0, ifelse(D$ER15614 == 1, 1, NA))
    #business and otr estate 2013-
        D$S103 <- ifelse(is.na(D$S103), D$ER58155, D$S103)
        D$S109 <- ifelse(is.na(D$S109), D$ER58165, D$S109)
    #debt
        D$ER65402[D$Year == 2011] <- 0
        D$ER65364[D$Year == 2011] <- 0
        D$ER65354[D$Year == 2011] <- 0
        D$S107 <- ifelse(D$Year > 2010, D$ER65354+D$ER65364+D$ER65382+D$ER65386+D$ER65390+D$ER65394+D$ER65398+D$ER65402, D$S107)
    #deletions
        D$ER58155 <- NULL
        D$ER21147 <- NULL
        D$ER2069 <- NULL
        D$V12292 <- NULL
        D$ER2563 <- NULL
        D$ER21397 <- NULL
        D$ER15554 <- NULL
        D$ER15662 <- NULLD$ER58165 <- NULL
        
#6 - Rename Variables
    #Some s_variables share numbers with other variables; rename to fix
        names(D) <- gsub("[S]",  "99",  names(D))
    #Sort columns by variable names
        D <- D[, order(as.numeric(gsub("[a-zA-Z]",  "",  names(D))))]
    #Replace Names
        a <- c("fam_id", "value_house", "head_work_hour", "wife_work_hour", "value_total_income", "fam_state", "dummy_own_home", "head_age", "wife_age", "head_sex", "head_race", "head_employ_stat", "head_self_empl", "head_marital", "wife_education", "value_intervivo", "head_education", "head_veteran", "fam_region", "fam_children_18", "value_mortgage_principle", "fam_id_68", "fam_comp_change", "fam_mover_out", "dummy_second_mortgage", "fam_moved", "fam_moved_reason", "dummy_lump_sum", "value_subs_all", "head_salaried_dummy", "head_health_lim_work", "head_total_wage_income", "wife_total_wage_income", "value_subs_food", "head_college_deg", "wife_college_deg", "wife_health_lim_work", "wife_employ_stat", "wife_self_empl", "wife_salaried_dummy", "wife_in_fam", "value_subs_heat", "head_health_hosp", "wife_health_hosp", "head_health_gen", "wife_health_gen", "value_otr_estate", "value_vehicle", "value_business", "value_stock", "value_savings", "value_bond", "value_debt", "dummy_inheritance", "head_hs_grad", "head_college_att", "head_degree", "wife_race", "wife_hs_grad", "wife_college_att", "wife_degree", "value_ira", "head_stroke", "head_hypertension", "head_diabetes", "head_cancer", "head_heart_attack", "head_asthma", "head_bathing", "head_dressing", "head_eating", "head_bed", "head_walking", "head_outside", "head_toilet", "head_weight", "head_height", "wife_stroke", "wife_hypertension", "wife_diabetes", "wife_cancer", "wife_heart_attack", "wife_asthma", "wife_bathing", "wife_dressing", "wife_eating", "wife_bed", "wife_walking", "wife_outside", "wife_toilet", "wife_weight", "wife_height", "expenditures_health_insurance", "expenditures_hospital", "expenditures_out_of_pocket_medical", "expenditures_prescriptions", "expenditures_all_medical", "head_distress", "head_relative_transfer_value", "head_friend_transfer_value", "wife_relative_transfer_value", "wife_friend_transfer_value", "value_mortgage2_principal", "debt_bus", "debt_otrestate", "debt_creditcard", "debt_student", "debt_medical", "debt_legal", "debt_family", "debt_other", "s_value_business", "s_value_savings", "s_value_other_debt", "s_value_otr_estate", "s_value_stock", "s_value_vehicle", "s_value_otr_assets", "s_value_wealth_noequity", "s_value_wealth_withequity", "s_value_house", "Year")
        names(D) <- c("fam_id", "value_house", "head_work_hour", "wife_work_hour", "value_total_income", "fam_state", "dummy_own_home", "head_age", "wife_age", "head_sex", "head_race", "head_employ_stat", "head_self_empl", "head_marital", "wife_education", "value_intervivo", "head_education", "head_veteran", "fam_region", "fam_children_18", "value_mortgage_principle", "fam_id_68", "fam_comp_change", "fam_mover_out", "dummy_second_mortgage", "fam_moved", "fam_moved_reason", "dummy_lump_sum", "value_subs_all", "head_salaried_dummy", "head_health_lim_work", "head_total_wage_income", "wife_total_wage_income", "value_subs_food", "head_college_deg", "wife_college_deg", "wife_health_lim_work", "wife_employ_stat", "wife_self_empl", "wife_salaried_dummy", "wife_in_fam", "value_subs_heat", "head_health_hosp", "wife_health_hosp", "head_health_gen", "wife_health_gen", "value_otr_estate", "value_vehicle", "value_business", "value_stock", "value_savings", "value_bond", "value_debt", "dummy_inheritance", "head_hs_grad", "head_college_att", "head_degree", "wife_race", "wife_hs_grad", "wife_college_att", "wife_degree", "value_ira", "head_stroke", "head_hypertension", "head_diabetes", "head_cancer", "head_heart_attack", "head_asthma", "head_bathing", "head_dressing", "head_eating", "head_bed", "head_walking", "head_outside", "head_toilet", "head_weight", "head_height", "wife_stroke", "wife_hypertension", "wife_diabetes", "wife_cancer", "wife_heart_attack", "wife_asthma", "wife_bathing", "wife_dressing", "wife_eating", "wife_bed", "wife_walking", "wife_outside", "wife_toilet", "wife_weight", "wife_height", "expenditures_health_insurance", "expenditures_hospital", "expenditures_out_of_pocket_medical", "expenditures_prescriptions", "expenditures_all_medical", "head_distress", "head_relative_transfer_value", "head_friend_transfer_value", "wife_relative_transfer_value", "wife_friend_transfer_value", "value_mortgage2_principal", "debt_bus", "debt_otrestate", "debt_creditcard", "debt_student", "debt_medical", "debt_legal", "debt_family", "debt_other", "s_value_business", "s_value_savings", "s_value_other_debt", "s_value_otr_estate", "s_value_stock", "s_value_vehicle", "s_value_otr_assets", "s_value_wealth_noequity", "s_value_wealth_withequity", "s_value_house", "Year")
    #Set fam_id_68 to id in 1968
        D$fam_id_68 <- ifelse(is.na(D$fam_id_68), D$fam_id, D$fam_id_68)
        
#7 - Fix more variables
    #Change hours to daily
        D$head_work_hour <- ifelse(D$head_work_hour == 9999, NA, D$head_work_hour)
        D$head_work_hour <- D$head_work_hour/50
        D$wife_work_hour <- ifelse(D$wife_work_hour == 9999, NA, D$wife_work_hour)
        D$wife_work_hour <- D$wife_work_hour/50
    #Kill hours above 80
        D$head_work_hour <- ifelse(D$head_work_hour > 80, 80, D$head_work_hour)
        D$wife_work_hour <- ifelse(D$wife_work_hour > 80, 80, D$wife_work_hour)
    #Simplify races
        D$head_race <- ifelse(D$head_race == "Asian/Pac Islander" | D$head_race == "Pac Islander" | D$head_race == "Asian" | D$head_race == "American Indian", "Other", D$head_race)
    #Other
        D$fam_state <- as.factor(D$fam_state)
        D$fam_children_18 <- ifelse(D$fam_children_18 > 5, 5, D$fam_children_18)
        D$value_mortgage_principle <- ifelse(is.na(D$value_mortgage_principle), 0, ifelse(D$value_mortgage_principle > 500000, 500000, D$value_mortgage_principle))
        D$value_intervivo <- ifelse(D$value_intervivo > 500000, 500000, D$value_intervivo)
        D$value_subs_all <- ifelse(D$value_subs_all > 50000, 50000, D$value_subs_all)
        D$value_total_income <- ifelse(D$value_total_income < 0,  0, ifelse(D$value_total_income > 500000, 500000, D$value_total_income))
        D$value_house <- ifelse(as.numeric(D$value_house) > 9999997, NA, D$value_house)
        D$value_logged_total_income <- sign(D$value_total_income)*log10(abs(D$value_total_income)+1)
        D$s_value_logged_wealth_withequity <- sign(D$s_value_wealth_withequity)*log10(abs(D$s_value_wealth_withequity)+1)
        D$dummy_own_home <- ifelse(D$dummy_own_home == "0" | D$dummy_own_home == "9", NA, D$dummy_own_home)
        D$expenditures_all_medical <- ifelse(D$expenditures_all_medical > 9999997, NA, D$expenditures_all_medical)
        D$expenditures_health_insurance <- ifelse(D$expenditures_health_insurance > 999997, NA, D$expenditures_health_insurance)
        D$expenditures_hospital <- ifelse(D$expenditures_hospital > 999997, NA, D$expenditures_hospital)
        D$expenditures_out_of_pocket_medical <- ifelse(D$expenditures_out_of_pocket_medical > 999997, NA, D$expenditures_out_of_pocket_medical)
        D$expenditures_prescriptions <- ifelse(D$expenditures_prescriptions > 999997, NA, D$expenditures_prescriptions)
        D$s_value_business <- ifelse(D$s_value_business == 9999999, NA, D$s_value_business)
        D$s_value_house <- ifelse(D$s_value_house == 999999, NA, D$s_value_house)
        D$s_value_otr_estate <- ifelse(D$s_value_otr_estate == 9999999, NA, D$s_value_otr_estate)
        D$value_bond <- ifelse(D$value_bond == "9999997" | D$value_bond == "9999998" | D$value_bond == "9999999" | D$value_bond == "99999998" | D$value_bond == "99999999" | D$value_bond == "999999998" | D$value_bond == "999999999",  NA,  D$value_bond)
        D$value_business <- ifelse(D$value_business == "9999997" | D$value_business == "9999998" | D$value_business == "9999999" | D$value_business == "99999998" | D$value_business == "99999999" | D$value_business == "999999998" | D$value_business == "999999999",  NA,  D$value_business)
        D$value_debt <- ifelse(D$value_debt == "9999997" | D$value_debt == "9999998" | D$value_debt == "9999999" | D$value_debt == "99999998" | D$value_debt == "99999999" | D$value_debt == "999999998" | D$value_debt == "999999999",  NA,  D$value_debt)
        D$value_house <- ifelse(D$value_house == "9999997" | D$value_house == "9999998" | D$value_house == "9999999" | D$value_house == "99999998" | D$value_house == "99999999" | D$value_house == "999999998" | D$value_house == "999999999",  NA,  D$value_house)
        D$value_ira <- ifelse(D$value_ira == "9999997" | D$value_ira == "9999998" | D$value_ira == "9999999" | D$value_ira == "99999998" | D$value_ira == "99999999" | D$value_ira == "999999998" | D$value_ira == "999999999",  NA,  D$value_ira)
        D$value_mortgage2_principal <- ifelse(D$value_mortgage2_principal == "9999997" | D$value_mortgage2_principal == "9999998" | D$value_mortgage2_principal == "9999999" | D$value_mortgage2_principal == "99999998" | D$value_mortgage2_principal == "99999999" | D$value_mortgage2_principal == "999999998" | D$value_mortgage2_principal == "999999999",  NA,  D$value_mortgage2_principal)
        D$value_otr_estate <- ifelse(D$value_otr_estate == "9999997" | D$value_otr_estate == "9999998" | D$value_otr_estate == "9999999" | D$value_otr_estate == "99999998" | D$value_otr_estate == "99999999" | D$value_otr_estate == "999999998" | D$value_otr_estate == "999999999",  NA,  D$value_otr_estate)
        D$value_vehicle <- ifelse(D$value_vehicle == "999997" | D$value_vehicle == "999998" | D$value_vehicle == "999999" | D$value_vehicle == "9999997" | D$value_vehicle == "9999998" | D$value_vehicle == "9999999" | D$value_vehicle == "99999998" | D$value_vehicle == "99999999" | D$value_vehicle == "999999998" | D$value_vehicle == "999999999",  NA,  D$value_vehicle)
        D$value_savings <- NULL #Data redudant with s_value_savings,  but covers less recent years
        D$value_stock <- NULL #Data totally redudant with s_value_stock
        D$value_business <- NULL
        D$head_age <- ifelse(D$head_age == "999", NA, D$head_age)
        D$head_employ_stat <- ifelse(D$head_employ_stat == "0"|D$head_employ_stat == "22"|D$head_employ_stat == "98"|D$head_employ_stat == "99", NA, D$head_employ_stat)
        D$head_self_empl <- ifelse(D$Year == 1968 & D$head_self_empl == 3, 1, ifelse(D$head_self_empl == 9, NA, 0))
        D$head_marital <- ifelse(D$head_marital == "8", NA, D$head_marital)
        D$head_veteran <- ifelse(D$head_veteran == "0", NA, D$head_veteran)
        D$head_salaried_dummy <- ifelse(D$head_salaried_dummy == 0, 0, ifelse(D$head_salaried_dummy == 1 | D$head_salaried_dummy == 2, 1, ifelse(D$head_salaried_dummy == 3 | D$head_salaried_dummy == 4 | D$head_salaried_dummy == 5 | D$head_salaried_dummy == 6, 0, NA)))
        D$head_health_lim_work <- ifelse(D$head_health_lim_work == 1, 3, ifelse(D$head_health_lim_work == 3, 2, ifelse(D$head_health_lim_work == 5, 1, ifelse(D$head_health_lim_work == 7 | D$head_health_lim_work == 0, 0, NA))))
        D$head_health_gen <- ifelse(D$head_health_gen == 0, NA, D$head_health_gen)
        D$head_health_gen <- factor(D$head_health_gen, levels=c("Poor", "Fair", "Good", "Very Good", "Excellent"))
        D$head_friend_transfer_value <- ifelse(D$head_friend_transfer_value == "99999" | D$head_friend_transfer_value == "99998" |D$head_friend_transfer_value > 999996, NA, D$head_friend_transfer_value)
        D$head_relative_transfer_value <- ifelse(D$head_relative_transfer_value == "99999" | D$head_relative_transfer_value == "99998" | D$head_relative_transfer_value > 999996, NA, D$head_relative_transfer_value)
        D$head_total_wage_income <- ifelse(D$head_total_wage_income > 9999996, NA, D$head_total_wage_income)
        D$head_degree <- ifelse(D$head_degree == "97", NA, D$head_degree)
        D$wife_age <- ifelse(D$wife_age == "999", NA, D$wife_age)
        D$wife_health_lim_work <- ifelse(D$wife_health_lim_work == 1, 3, ifelse(D$wife_health_lim_work == 3, 2, ifelse(D$wife_health_lim_work == 5, 1, ifelse(D$wife_health_lim_work == 7 | D$wife_health_lim_work == 0, 0, NA))))
        D$wife_race <- ifelse(D$wife_race == "Asian/Pac Islander" | D$wife_race == "Pac Islander" | D$wife_race == "Asian" | D$wife_race == 6 | D$wife_race == "American Indian", "Other", D$wife_race)
        D$wife_health_gen <- factor(D$wife_health_gen, levels=c("Poor", "Fair", "Good", "Very Good", "Excellent"))        
        D$wife_education <- ifelse(D$wife_education == 9, NA, D$wife_education)
        D$wife_friend_transfer_value <- ifelse(D$wife_friend_transfer_value > 99996, NA, D$wife_friend_transfer_value)
        D$wife_relative_transfer_value <- ifelse(D$wife_relative_transfer_value > 99996 | D$wife_relative_transfer_value == "999998", NA, D$wife_relative_transfer_value)
        D$wife_total_wage_income <- ifelse(D$wife_total_wage_income > 9999996 | D$wife_total_wage_income == "999999", NA, D$wife_total_wage_income)
        D$wife_degree <- ifelse(D$wife_degree == "97", NA, D$wife_degree)
        #Fix years prior to 1985.
            D$head_hs_grad <- ifelse(D$Year < 1985 & D$head_education == "Less than HS", 3, ifelse(D$Year < 1985, 1, D$head_hs_grad))
            D$wife_hs_grad <- ifelse(D$Year < 1985 & D$wife_education == "Less than HS", 3, ifelse(D$Year < 1985, 1, D$wife_hs_grad))
            D$head_education <- ifelse(D$head_education == "High School" | D$head_education == "Less than HS",  "No College Degree", ifelse(D$head_education == "BA/BS", "BA", ifelse(D$head_education == "Some College", "No College Degree", D$head_education))) #unfortunately,  this variable does not indicate whether someone has an associates prior to 1985. No data exists for that.
            D$head_degree <- ifelse(D$Year < 1985, D$head_education, D$head_degree)
            D$wife_education <- ifelse(D$wife_education == "High School" | D$wife_education == "Less than HS",  "No College Degree", ifelse(D$wife_education == "BA/BS", "BA", ifelse(D$wife_education == "Some College", "No College Degree", D$wife_education))) #unfortunately,  this variable does not indicate whether someone has an associates prior to 1985. No data exists for that.
            D$wife_degree <- ifelse(D$Year < 1985, D$wife_education, D$wife_degree)
        D$wife_education <- NULL #Data redudant with degree,  but covers less recent years
        D$head_education <- NULL #Data redudant with degree,  but covers less recent years
        D$dummy_own_home <- ifelse(D$dummy_own_home == "Owns", 1, 0)
        D$head_disabled <- ifelse(D$head_employ_stat == "Disabled", 1, 0)
        D$wife_disabled <- ifelse(D$wife_employ_stat == "Disabled", 1, 0)
        D$head_unemployed <- ifelse(D$head_employ_stat == "Unemployed", 1, 0)
        D$wife_unemployed <- ifelse(D$wife_employ_stat == "Unemployed", 1, 0)
        D$wife_salaried_dummy <- ifelse(D$wife_salaried_dummy == 0, 0, ifelse(D$wife_salaried_dummy == 1 | D$wife_salaried_dummy == 2, 1, ifelse(D$wife_salaried_dummy == 3 | D$wife_salaried_dummy == 4 | D$wife_salaried_dummy == 5 | D$wife_salaried_dummy == 6, 0, NA)))
        D$head_distress[D$head_distress == 99] <- NA
    #Cohort
        tempborn_head <- D$Year-D$head_age
        tempborn_wife <- D$Year-D$wife_age
        tempborn_wife <- ifelse(D$wife_age == 0, 0, tempborn_wife)
        wifecuts <- cut(tempborn_wife,  breaks = seq(1865,  2005,  by = 10))
        headcuts <- cut(tempborn_head,  breaks = seq(1865,  2005,  by = 10))
        D$head_cohort <- headcuts
        D$wife_cohort <- wifecuts
    #Region
        D$fam_region <- factor(D$fam_region,  levels = c(1,  2,  3,  4,  5,  6),  labels = c("Northeast", "North Central", "South", "West", "Alaska", "Hawaii"))

#7 - Order variables by name
    D <- D[, order(names(D))]
    
#8 - Save as RDS
    saveRDS(D, "PanelData.rds")


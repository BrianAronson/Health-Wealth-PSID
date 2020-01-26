library(data.table)

#0) Set directory
    setwd("C:/Users/admin/Desktop/Sociology/PSID Data")

        
#1) read data
    df <- data.table(readRDS("7 - Merged_Data.rds"))

        
#2) Reduce sample based on criteria
    #a) make criteria to subset by
        df[, years.after.1984 := year %in% c(1984, 1989, 1994, 1999, 2001, 2003, 2005, 2007,2009,2011,2013,2015)]
        df[, years.2005.2011 := year %in% c(2005:2011)]
        df[, over.25 := !is.na(ego_age) & ego_age >= 0 & ego_age != 999]
        df[, only.black.or.white := latino == 0 & ifelse(is.na(ego_race), F, ifelse(ego_race %in% c("White", "Black"), T, F))]
        # df[, all.dv.missing := is.na(h_general) & is.na(h_conditions) & is.na(h_lim_work) & is.na(h_disabled) & is.na(h_distress) & is.na(h_BMI)]

    #b) subset 2005 
        df.2005 <- df[years.2005.2011 == T, ]
        df.2005 <- df.2005[over.25 == T, ]
        df.2005 <- df.2005[only.black.or.white == T, ]
        df.2005[, two.obs.since.2005 := ind_id %in% df.2005[year >= 2005 & year <= 2011 & !already_died, .N , by = "ind_id"][N >= 2, ind_id]]
        df.2005 <- df.2005[two.obs.since.2005 == T, ]
        
    #c) subset 1984
        df.1984 <- df[years.after.1984 == T, ]
        df.1984 <- df.1984[over.25 == T, ]
        df.1984 <- df.1984[only.black.or.white == T, ]
        df.1984[, three.obs.since.1984 := ind_id %in% df.1984[years.after.1984 == T & !already_died, .N , by = "ind_id"][N >= 3, ind_id]]
        df.1984 <- df.1984[three.obs.since.1984 == T, ]
        
    #d) na.omit variables that aren't DVs 
        na.rows <- rowSums(is.na(df.1984[, c("ego_age", "ego_age2", "ego_black2", "ego_female", "ego_dg_advanced", "ego_dg_bachelors", "ego_dg_highschool", "ego_dg_somecollege", "eq_home", "eq_bus", "eq_savings", "eq_otr_estate", "eq_stock", "eq_debt", "inc_total", "eq_wealth", "ind_id", "year", "fam_id_68", "fam_id", "eq_home_d", "eq_assets", "ego_cohort", "year2", "just_died", "already_died", "fam_region")])) > 0
        df.1984 <- df.1984[!na.rows, ]
        na.rows <- rowSums(is.na(df.2005[, c("ego_age", "ego_age2", "ego_black2", "ego_female", "ego_dg_advanced", "ego_dg_bachelors", "ego_dg_highschool", "ego_dg_somecollege", "eq_home", "eq_bus", "eq_savings", "eq_otr_estate", "eq_stock", "eq_debt", "inc_total", "eq_wealth", "ind_id", "year", "fam_id_68", "fam_id", "eq_home_d", "eq_assets", "ego_cohort", "year2", "just_died", "already_died", "fam_region")])) > 0
        df.2005 <- df.2005[!na.rows, ]

        
#3) Save datasets         
    saveRDS(df.1984, "8 - df.1984.rds")
    saveRDS(df.2005, "8 - df.2005.rds")


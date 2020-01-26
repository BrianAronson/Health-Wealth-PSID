library(data.table)
library(xlsx)

#0) Set directory
    setwd("C:/Users/admin/Desktop/Sociology/PSID Data")


#1) read data
    df <- data.table(readRDS("7 - Merged_Data.rds"))
    fam.df <- readRDS("PanelData.rds")


#2) Estimate number of rows lost by sample criteria
    #a) make criteria to subset by
        df[, years.after.1984 := year %in% c(1984, 1989, 1994, 1999, 2001, 2003, 2005, 2007,2009,2011,2013,2015)]
        df[, years.2005.2011 := year %in% c(2005:2011)]
        df[, over.25 := !is.na(ego_age) & ego_age >= 0 & ego_age != 999]
        df[, only.black.or.white := latino == 0 & ifelse(is.na(ego_race), F, ifelse(ego_race %in% c("White", "Black"), T, F))]

    #b) intiate table to append stats to
        tab <- data.frame("Subset Condition" = 0, "Initial Sample" = 0, "Remaining Sample" = 0)
    
    #c) count dropped cases by subset criteria
        tab[1, ] <- c("Family to Heads and Spouses", nrow(fam.df), nrow(df))
        
        tab <- rbind(tab, c("2005 - 2011", nrow(df), df[, sum(years.2005.2011)]))
        df <- df[years.2005.2011 == T, ]

        tab <- rbind(tab, c("Black or White", nrow(df), df[, sum(only.black.or.white)]))
        df <- df[only.black.or.white == T, ]
        
        tab <- rbind(tab, c("Age >= 25", nrow(df), df[, sum(over.25)])) #
        df <- df[over.25 == T, ]
        
        df[, two.obs.since.2005 := ind_id %in% df[year >= 2005 & year <= 2011 & !already_died, .N , by = "ind_id"][N >= 2, ind_id]]
        tab <- rbind(tab, c("Observations > 2", nrow(df), df[, sum(two.obs.since.2005)])) #
        df <- df[two.obs.since.2005 == T, ]
        
        na.rows <- rowSums(is.na(df[, c("ego_age", "ego_age2", "ego_black2", "ego_female", "ego_dg_advanced", "ego_dg_bachelors", "ego_dg_highschool", "ego_dg_somecollege", "eq_home", "eq_bus", "eq_savings", "eq_otr_estate", "eq_stock", "eq_debt", "inc_total", "eq_wealth", "ind_id", "year", "fam_id_68", "fam_id", "eq_home_d", "eq_assets", "ego_cohort", "year2", "just_died", "already_died", "fam_region")])) > 0
        tab <- rbind(tab, c("Rows with NA IVs", nrow(df), df[, sum(!na.rows)])) #
        df <- df[!na.rows, ]

#3) Save
    write.xlsx(tab, "C:/Users/admin/Desktop/Appendix 1 - Dropped cases by subset condition.xlsx")



        
        
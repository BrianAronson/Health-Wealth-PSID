library(data.table)
library(xlsx)

#0) Set directory
    setwd("C:/Users/admin/Desktop/Sociology/PSID Data")

#1) read data
    df <- data.table(readRDS("8 - df.2005.rds"))
        
#2) Reduce dataset to last observation of each individual
    tdf <- copy(df)
    tdf <- tdf[order(year, decreasing = T)]
    # tdf <- tdf[!duplicated(ind_id), ]
    
#3) Reduce the sample to key variables
    tdf$x <- ""
    tdf <- tdf[, c(
        "x",
        "h_general",
        "h_conditions",
        "h_lim_work",
        "h_disabled",
        "h_distress",
        "h_BMI",
        "x",
        "eq_wealth",
        "eq_home",
        "eq_savings",
        "eq_stock",
        "eq_debt",
        "x",
        "peq_home",
        "peq_savings",
        "peq_stock",
        "peq_debt",
        "eq_home_d",
        "inc_total",
        "x",
        "ego_age",
        "ego_female",
        "fam_region",
        "x",
        "ego_dg_lthighschool",
        "ego_dg_highschool",
        "ego_dg_somecollege",
        "ego_dg_bachelors",
        "ego_dg_advanced",
        "died",
        "ego_black2"
    )]

#4) Create df by mssing health variable
    df.all <- as.data.frame(tdf)
    df.general <- as.data.frame(tdf[is.na(tdf$h_general), ])
    df.conditions <- as.data.frame(tdf[is.na(tdf$h_conditions), ])
    df.lim_work <- as.data.frame(tdf[is.na(tdf$h_lim_work), ])
    df.disabled <- as.data.frame(tdf[is.na(tdf$h_disabled), ])
    df.distress <- as.data.frame(tdf[is.na(tdf$h_distress), ])
    df.BMI <- as.data.frame(tdf[is.na(tdf$h_BMI), ])
    df.missing.any <- as.data.frame(tdf[is.na(tdf$h_general) | is.na(tdf$h_conditions) | is.na(tdf$h_lim_work) | is.na(tdf$h_disabled) | is.na(tdf$h_BMI) | is.na(tdf$h_distress), ])
    
    
#5) Prep table of results
    sm <- as.data.frame(matrix(0, ncol(tdf), 9))
    names(sm) <- c("variable", "df.all", "df.missing.any", "df.general", "df.conditions", "df.lim_work", "df.disabled", "df.distress", "df.BMI")

#6) Get stats
    for(i in 1:length(df.all)){
        try({
            sm$variable[i] <- names(df.general)[i]
            sm$df.all[i] <- mean(df.all[, i], na.rm = T)
            sm$df.missing.any[i] <- mean(df.missing.any[, i], na.rm = T)
            sm$df.general[i] <- mean(df.general[, i], na.rm = T)
            sm$df.conditions[i] <- mean(df.conditions[, i], na.rm = T)
            sm$df.lim_work[i] <- mean(df.lim_work[, i], na.rm = T)
            sm$df.disabled[i] <- mean(df.disabled[, i], na.rm = T)
            sm$df.distress[i] <- mean(df.distress[, i], na.rm = T)
            sm$df.BMI[i] <- mean(df.BMI[, i], na.rm = T)
        })
    }

#7) Format table
    sm[grepl("ego_age", sm$variable), ][2:ncol(sm)] <- sm[grepl("ego_age", sm$variable), ][2:ncol(sm)] + 25
    sm$df.all <- format(round(sm$df.all, 2), nsmall = 2, big.mark=",")
    sm$df.missing.any <- format(round(sm$df.missing.any, 2), nsmall = 2, big.mark=",")
    sm$df.general <- format(round(sm$df.general, 2), nsmall = 2, big.mark=",")
    sm$df.conditions <- format(round(sm$df.conditions, 2), nsmall = 2, big.mark=",")
    sm$df.lim_work <- format(round(sm$df.lim_work, 2), nsmall = 2, big.mark=",")
    sm$df.disabled <- format(round(sm$df.disabled, 2), nsmall = 2, big.mark=",")
    sm$df.distress <- format(round(sm$df.distress, 2), nsmall = 2, big.mark=",")
    sm$df.BMI <- format(round(sm$df.BMI, 2), nsmall = 2, big.mark=",")
    sm[grepl("x", sm$variable), ] <- ""
    
#Save
    write.xlsx(sm, "C:/Users/admin/Desktop/Appendix 2 - Descriptives by missing health.xlsx")

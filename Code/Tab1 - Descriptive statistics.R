library(data.table)
library(xlsx)

#0) Set directory
    setwd("C:/Users/admin/Desktop/Sociology/PSID Data")

#1) read data
    df <- data.table(readRDS("8 - df.2005.rds"))

#2) Descriptive Statistics of Full sample vs NAs
    #a) Reduce dataset to last observation of each individual
        tdf <- copy(df)
        tdf <- tdf[order(year, decreasing = T)]
        tdf <- tdf[!duplicated(ind_id), ]
    #b) Reduce the sample to key variables
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
            "ego_died",
            "ego_black"
        )]

    #c) Tables of all variables by race:
        #Create white df
            W <- as.data.frame(tdf[tdf$ego_black == 0, ])
        #Create black df
            B <- as.data.frame(tdf[tdf$ego_black == 1, ])
        #Prep table
            sm <- as.data.frame(matrix(0, ncol(tdf), 4))
            names(sm) <- c("variable", "meanW", "meanB", "pval")
        #Get stats
            for(i in 1:length(W)){
                try({
                    sm$variable[i] <- names(W)[i]
                    sm$meanW[i] <- mean(as.numeric(as.character(W[, i])), na.rm = T)
                    sm$meanB[i] <- mean(as.numeric(as.character(B[, i])), na.rm = T)
                    sm$pval[i] <- t.test(as.numeric(as.character(W[, i])), as.numeric(as.character(B[, i])))$p.val
                })
            }
        
        #Format results
            sm[grepl("ego_age", sm$variable), ][2:ncol(sm)] <- sm[grepl("ego_age", sm$variable), ][2:ncol(sm)] + 25
            sm$pval <- format(round(as.numeric(sm$pval), 3), nsmall = 3)
            # sm$pval <- substr(sm$pval, 2,5)
            sm$pval <- ifelse(sm$pval == ".000", "<.001", paste(" ", sm$pval, sep = ""))
            sm$meanW <- format(round(sm$meanW, 2), nsmall = 2, big.mark=",")
            sm$meanB <- format(round(sm$meanB, 2), nsmall = 2, big.mark=",")
            sm[grepl("x", sm$variable), ] <- ""
        #Save
            write.xlsx(sm, "C:/Users/admin/Desktop/Table 1.xlsx")

summary(df$died)
summary(df$already_died)
summary(df$ego_died)
summary(df$just_died)
summary(tdf$ego_died)


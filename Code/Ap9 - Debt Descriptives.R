library(data.table)
library(xlsx)

#0) Set directory
    setwd("C:/Users/admin/Desktop/Sociology/PSID Data")

#1) Read data
    df <- data.table(readRDS("8 - df.2005.rds"))

#2) Identify debt variables
    debt.vars <- grep("debt", names(df), value = T)
    
#3) Get Descriptive Statistics of Debt Vars
    #a) Reduce dataset to last observation of each individual
        tdf <- copy(df)
        tdf <- tdf[order(year, decreasing = T)]
        # tdf <- tdf[!duplicated(ind_id), ]
        
    #b) Reduce the sample to key variables
        tdf$x <- ""
        tdf <- tdf[year == 2011, c(debt.vars), with = F]
        tdf <- as.data.frame(tdf)
        
    #c) Tables of all variables by race:
        sm <- as.data.frame(matrix(0, ncol(tdf), 5))
        names(sm) <- c("variable", "mean", "sd", "min", "max")
        #Get stats
            for(i in 1:length(tdf)){
                try({
                    sm$variable[i] <- names(tdf)[i]
                    sm$mean[i] <- mean(as.numeric(as.character(tdf[, i])), na.rm = T)
                    sm$sd[i] <- sd(as.numeric(as.character(tdf[, i])), na.rm = T)
                    sm$min[i] <- min(as.numeric(as.character(tdf[, i])), na.rm = T)
                    sm$max[i] <- max(as.numeric(as.character(tdf[, i])), na.rm = T)
                })
            }
        #Save
            write.xlsx(sm, "C:/Users/admin/Desktop/Ap.9 -.xlsx")



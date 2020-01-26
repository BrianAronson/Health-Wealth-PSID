library(data.table)

#0 - Set directory
    setwd("C:/Users/admin/Desktop/Sociology/PSID Data")

#1 - Load data
    I <- readRDS("5 - Merged_Data.rds")

#2 - Inflate numbers to 2015
    #read CPI
        CPI <- read.csv("C:/Users/bda13/Desktop/Analysis - Data/CPI info/CPI.csv")
        CPI <- read.csv("C:/Users/admin/Desktop/Sociology/CPI info/CPI.csv")
    #determine year to inflate to; adjust other numbers accordingly
        cpiindex <- CPI$Avg[CPI$Year == max(I$year, na.rm = T)]
        CPI$Avg <- cpiindex/CPI$Avg
    #create a vector of these numbers equal to nrow I,  indexed by year
        temp <- data.frame(Year = I$year, index = 1:nrow(I))
        temp <- merge(temp, CPI, all.x = T)
        inflatenums <- temp[order(temp$index), ]
    #pick variables to inflate
        #scan variables for numbers that go higher than 1000 (this just happens to work)
            varmax <- vector()
            for(i in 1:length(I)){
              try({
                if(class(I[, i]) != "factor")
                varmax[i] <- max(I[, i], na.rm = T)
              })
            }
          #exclude id variables from vars to inflate
            toinflate <- grep("^eq|^inc|^debt", names(I))
          #inflate those variables and divide by 1000
            for(i in 1:length(toinflate)){
                I[, toinflate[i]] <- inflatenums$Avg*I[, toinflate[i]]
            }

#3 - save
    saveRDS(I, "6 - Merged_Data.rds")

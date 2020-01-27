# library(ggplot2)
# library(data.table)
# library(plm)
# library(pglm)
# library(lme4)
# library(nlme)
# library(lmerTest)
# library(cowplot)
# library(car)
# library(xlsx)
# options(scipen = 999)
# 
# #0 - Set directory
#   setwd("C:/Users/bda13/Desktop/Sociology/Statistics/PSID/")
# 
# #1 - read data
#   # i.model <- readRDS("i.model.rds")
#   # s.model <- readRDS("s.model.rds")
#   # m.model <- readRDS("m.model.rds")
#   results <- readRDS("results.rds")
#   
# #2 - 
#   vars <- c("h_general", "h_BMI", "h_activities", "h_conditions", "h_heart_attack", "h_stroke", "h_hospital", "h_lim_work", "h_disabled", "h_distress")
#   vars <- vars[sapply(results, length)!=0]
#   results <- results[sapply(results, length)!=0]
#   orders <- c("ego_black2", "ego_age", "ego_age2", "ego_female", "ego_black2:ego_age", "ego_black2:ego_age2", "fam_region", "year2", "died", "log_income", "ego_black2:log_income", "ego_dg_highschool", "ego_dg_somecollege", "ego_dg_bachelors", "ego_dg_advanced", "ego_black2:ego_dg_highschool", "ego_black2:ego_dg_somecollege", "ego_black2:ego_dg_bachelors", "ego_black2:ego_dg_advanced", "log_wealth", "ego_black2:log_wealth", "eq_home_d", "peq_home", "peq_savings", "peq_stock", "peq_debt", "ego_black2:eq_home_d", "ego_black2:peq_home", "ego_black2:peq_savings", "ego_black2:peq_stock", "ego_black2:peq_debt", "(Intercept)", "1|2", "2|3", "3|4", "4|5")
#   
#   
# #3 - save results to excel
#     for(i in 1:length(results)){
#       #determine file name/location
#           # fname <- "C:/Users/bda13/Desktop/10-12 Mixed Effects all Data.xlsx"
#           # fname <- "C:/Users/bda13/Desktop/10-24 Mixed Effects 2005-2011.xlsx"
#           # fname <- "C:/Users/bda13/Desktop/10-12 Fixed Effects 2005-2011.xlsx"
#       fname <- "C:/Users/bda13/Desktop/1-3 Models.xlsx"
#       #grab results
#           tresult <- results[[i]]
#       #format results
#           tresult$var <- as.character(tresult$var)
#           tresult <- tresult[match(orders, tresult$var), ]
#           ind0 <- nrow(tresult)+1
#           tresult[ind0, ] <- ""
#           ind1 <- which(tresult$var=="ego_black2:log_income")
#           ind2 <- which(tresult$var=="ego_black2:log_wealth")
#           tresult <- tresult[c(1:ind1, ind0, (ind1+1):ind2, ind0, (ind2+1):ind0), ]
#           tresult[is.na(tresult)] <- ""
# 
#         #determine sheet name
#           tsheet <- paste(i, "-", vars[i], sep="")
#         if(i==1){
#           write.xlsx(tresult, file=fname, sheetName=tsheet, row.names=T)
#         }else{
#           write.xlsx(tresult, file=fname, sheetName=tsheet, append=TRUE, row.names=T)
#         }
#     }
# 
# #add standard erros to self-reported health
#     #prep data for keeping row order  
#       temp <- data.frame(var=tresult$var, V1=tresult$`9.1`)
#     #prep empty dataframe with extra rows for SEs
#       tresult2 <- matrix(nrow = nrow(tresult), ncol=floor(ncol(tresult)*1.5))
#       for(i in 1:9){
#         #keep old rows
#             tresult2[, (i-1)*3+1] <- tresult[, (i-1)*2+1]
#             tresult2[, (i-1)*3+2] <- tresult[, (i-1)*2+2]
#         #add a row with SEs
#             a <- as.data.frame(summary(grl[[i]])$coefficients[, 1:2])
#             a$var <- row.names(a)
#             t2 <- merge(temp, a, by.x="var", all.x = T)
#             t2 <- t2[match(temp$var, t2$var), ]
#             tresult2[, (i-1)*3+3] <- t2$`Std. Error`
#       }
#     #change to numeric
#       tresult3 <- sapply(as.data.frame(tresult2), function(x) as.numeric(as.character(x)))
#       for(i in 1:9){
#         #exponentiate all estimates
#             tresult3[, (i-1)*3+4] <- exp(tresult3[, (i-1)*3+2]-tresult3[, (i-1)*3+3]*1.96)
#             tresult3[, (i-1)*3+3] <- exp(tresult3[, (i-1)*3+2]+ tresult3[, (i-1)*3+3]*1.96)
#             tresult3[, (i-1)*3+2] <- exp(tresult3[, (i-1)*3+2])
#       }
#     #convert to character with two decimals
#       tresult4 <- sapply(as.data.frame(tresult3), function(x)format(round(x, 2), nsmall = 2))
#     #kill spaces
#       tresult4 <- sapply(as.data.frame(tresult4), function(x)gsub(" ", "", x))
#     #prep final dataframe with old row amounts  
#       tresult5 <- matrix(nrow = nrow(tresult), ncol=floor(ncol(tresult)))
#       
#       for(i in 1:9){
#         #format standard errors prettily
#             tresult5[, (i-1)*2+2] <- tresult4[, (i-1)*3+2]
#             tresult5[, (i-1)*2+3] <- paste("(", tresult4[, (i-1)*3+3], "-", tresult4[, (i-1)*3+4], ")", sep="")
#       }
#     #kill anything with NA in it
#       tresult5[1:5, 1:5]
#       NAs <- sapply(as.data.frame(tresult5), function(x)grepl("NA", x))
#       tresult5[NAs] <- ""
#       tresult5[, 1] <- ""
#     #save    
#       write.xlsx(tresult5, "C:/Users/bda13/Desktop/Self-report Model.xlsx")
#       

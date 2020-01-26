#Merge individual and family

#0 - Set directory
    setwd("C:/Users/admin/Desktop/Sociology/PSID Data")
    
#1 - load data
    I <- readRDS("IndividualData.rds")
    D <- readRDS("PanelData.rds")

#2 - Match by year and id in that year
    DI <- merge(D, I, by = c("fam_id", "Year"))
    
#3 - Order by fam id and year
    DI <- DI[order(DI$ind_id, DI$ind_fam_id_68, DI$Year), ]
 
#4 - Assume spousal traits don't matter; collapse heads and wives based on relationship to head.
#    create a new set of variables based on head and wife variables so that we can have common 
#    variables for each individual
    #a - Find which variables start with "head" and wife
        indexh <- which(substr(names(DI), 1, 4) == "head")
        indexw <- which(substr(names(DI), 1, 4) == "wife")
    #b - Create empty data frame with new variables based on the unique versions of these variables
        newvarnames <- unique(gsub("head", "ego", names(DI)[indexh]))
        tempdf <- as.data.frame(matrix(0, ncol=length(newvarnames), nrow=nrow(DI)))
        names(tempdf) <- newvarnames
    #c - Create dataframes of wife and head variables
        tempdfh <- DI[, indexh]
        tempdfw <- DI[, indexw]
    #d - Address dispute between who is the head of the family in the individual data.
        DI$sex <- ifelse(DI$sex == 1, "Male", "Female")
        DI$rel_to_head <- ifelse(DI$rel_to_head == "head" & DI$sex!=DI$head_sex, "wife", DI$rel_to_head)
    #e - For each variable in tempdf, if head, grab the corresponding information for being a head
        for(i in 1:length(tempdf)){
            tempdf[, i] <- ifelse(DI$rel_to_head == "head", tempdfh[, i], tempdf[, i])
        }
    #f) repeat for wives
        for(i in 1:length(tempdf)){
            if(gsub("ego", "wife", names(tempdf))[i] %in% names(tempdfw)){
              #find where that variable is in the wife dataframe
                  tempindex <-  match(gsub("ego", "wife", names(tempdf))[i], names(tempdfw))
              #grab wife info from that index
                  tempdf[, i] <- ifelse(DI$rel_to_head == "wife", tempdfw[, tempindex], tempdf[, i])
            }
        }
    #g) Replace variables that wife did not have
        tempdf$ego_sex <- ifelse(DI$rel_to_head == "wife", DI$sex, tempdf$ego_sex)
        tempdf$ego_marital <- ifelse(DI$rel_to_head == "wife", "Married", tempdf$ego_marital)
        tempdf$ego_veteran <- ifelse(DI$rel_to_head == "wife", NA, tempdf$ego_veteran)
    #h) append these variables to DI
        DI <- cbind(DI, tempdf)
    #i) remove bad matches between individual and family-level variables (i.e. when not the same)
        DI$badmatch <- DI$age != DI$ego_age | DI$ego_age < 15 
        DI <- DI[!DI$badmatch, ]
    #j) kill redundant variables
        DI[, c(indexh, indexw)] <- NULL
        
#5 - Save
    saveRDS(DI, "4 - Merged_Data.rds")
    
 
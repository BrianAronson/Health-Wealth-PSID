library(data.table)

#0 - Set directory
    setwd("C:/Users/bda13/Desktop/Sociology/Statistics/PSID/")

#1 - load data
    load("Individual 2015  - Long Format.rdata")

#2 - Prep inIvidual dataframe
    I <- as.data.frame(ind.longdf$data)
    names(I) <- ind.longdf$names

#3 - Append nearly identical variables
    I$ER30526 <- ifelse(I$Year>1997, I$ER33518, I$ER30526)
    I$ER30526 <- ifelse(I$Year>2011, I$ER34129, I$ER30526)
    I$ER30013 <- ifelse(I$Year>1993, I$ER33536Q, I$ER30013) 
    
#4 - Remove redudant variables
    I$ER33518 <- NULL
    I$ER34129 <- NULL
    I$ER33536Q <- NULL
    
#5 - rename variables
    names(I) <- c("id", "rel_to_head",  "age",  "moved_in",  "educ_years",  "inc_type",  "inc_personal",  "hours_worked",  "shares_expenses",  "weight",  "seq_num",  "hours_housework",  "health_disabled",  "head",  "orig_id",  "inc_other_any",  "inc_any_asset",  "inc_personal",  "inc_other",  "empl_status",  "is_student",  "medicaid",  "health",  "ind_id",  "sex",  "latino",  "died",  "mother_id",  "mother_per_id",  "father_id",  "father_per_id",  "death_year",  "id_68",  "Year")
    
#6 - Fix variables
    #Transform relationship to head
        I$rel_to_head2 <- ifelse(I$rel_to_head==10 | I$rel_to_head==1, "head", I$rel_to_head)
        I$rel_to_head2 <- ifelse((I$rel_to_head>19 & I$rel_to_head<=29) | I$rel_to_head==2, "wife", I$rel_to_head2)
        I$rel_to_head2 <- ifelse((I$rel_to_head>29 & I$rel_to_head<=39) | I$rel_to_head==3, "child", I$rel_to_head2)
        I$rel_to_head2 <- ifelse((I$rel_to_head>39 & I$rel_to_head<=49) | I$rel_to_head==4, "sibling", I$rel_to_head2)
        I$rel_to_head2 <- ifelse((I$rel_to_head>49 & I$rel_to_head<=59) | I$rel_to_head==5, "parent", I$rel_to_head2)
        I$rel_to_head2 <- ifelse((I$rel_to_head>59 & I$rel_to_head<99) | (I$rel_to_head>=6 & I$rel_to_head<=9), "other", I$rel_to_head2)
        I$rel_to_head <- I$rel_to_head2
        I$rel_to_head2 <- NULL
    #Health Insurance; might not work with latinos
        I$insurance_empl <- ifelse(I$Year>1997 & I$medicaid==1, 1, 0)
        I$insurance_any <- ifelse(I$Year>1997 & I$medicaid>97, NA, 0)    
        I$insurance_any <- ifelse(I$Year>1997 & I$medicaid<98 & I$medicaid>0, 1, I$insurance_any)    
        I$insurance_any <- ifelse(I$Year<1998, NA, I$insurance_any)
        I$medicaid <- ifelse(I$Year<1998 & (I$medicaid==9 | I$medicaid==0), NA, I$medicaid)
        I$medicaid <- ifelse(I$Year<1998 & I$medicaid==5, 0, I$medicaid)
        I$medicaid <- ifelse(I$Year>1997 & I$medicaid!=5, 0, I$medicaid)
        I$medicaid <- ifelse(I$Year>1997 & I$medicaid==5, 1, I$medicaid)
    #death variables (assume death occurs at last date in range)
        I$death_year <- as.numeric(I$death_year)
        I$death_year <- ifelse(I$death_year==9999 | 0, NA, I$death_year)
        temp <- substr(I$death_year, 3, 4)
        temp <- ifelse(as.numeric(temp)<30 & temp!="", paste("20", temp, sep=""), temp)
        temp <- as.numeric(ifelse(as.numeric(temp)<100& temp!="", paste("19", temp, sep=""), temp))
        temp <- ifelse(temp==209, 2009, temp)
        I$death_year <- ifelse(I$death_year>2050 | I$death_year<1900, temp, I$death_year)
        I$death_year <- round(I$death_year, 0)
        I$should_be_dead <- ifelse(I$Year>I$death_year, 1, 0) #some folks reported dead at obviously wrong date
        I$died_at_all <- ifelse(I$died>1, 1, 0)
        I$just_died <- ifelse(I$seq_num>80, 1, 0)
        I$just_died <- ifelse(I$Year==1968, 0, I$just_died)
    #other
        I$age <- ifelse(I$age==999, NA, I$age)
        I$educ_years <- ifelse(I$educ_years>90, NA, I$educ_years)
        I$health_disabled <- ifelse(I$health_disabled==1, 1, 0)
        I$health_disabled <- ifelse(I$Year>1978 & I$empl_status==5, 1, ifelse(I$Year>1978, 0, I$health_disabled))
        I$health <- ifelse(I$Year>1987, ifelse(I$health==1, 0, ifelse(I$health==5, 1, NA)), NA) #1986 seems unreliable
        I$sex <- ifelse(I$sex==9, NA, I$sex)
        I$latino <- ifelse(I$latino>1, 1, 0)
        I$ind_id <- I$id_68*1000+I$ind_id
        I$seq_num <- ifelse(is.na(I$seq_num), 1, I$seq_num)
        names(I)[names(I)=="id_68"] <- "ind_fam_id_68"
        names(I)[names(I)=="id"] <- "fam_id"
        
#7 - Kill temp variables
    I <- I[, !names(I) %in% c("inc_other", "inc_other_any", "inc_any_asset", "inc_type",  "inc_personal", "shares_expenses", "weight", "hours_housework", "is_student", "mother_id", "mother_per_id", "father_id", "father_per_id", "head")]

#8 - Save
    saveRDS(I, "IndividualData.rds")
    

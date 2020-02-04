library(data.table)

#0) Set directory
    setwd("C:/Users/admin/Desktop/Sociology/PSID Data")

        
#1) read data
    df <- data.table(readRDS("6 - Merged_Data.rds"))


#2) Minor variable alterations/creations
    #a) cap overly skewed health outcomes
        df$h_BMI[df$h_BMI > quantile(df$h_BMI, .999, na.rm = T)] <- quantile(df$h_BMI, .999, na.rm = T)
    #b) Reverse code cohorts
        df$ego_cohort <- max(df$ego_cohort, na.rm=T)+1-df$ego_cohort
    #c) Change family ids to track year
        df$fam_id <- df$fam_id+10000000*df$year
    #d) Center year
        df$year2  <-  df$year - 2005
        df$year.1984  <-  df$year - 1984
    #e) Center age
        df$ego_age <- df$ego_age - 25
        df$ego_age2 <- df$ego_age ^ 2
    #f) change ids to factor
        df$fam_id <- factor(df$fam_id)
        df$fam_id_68 <- factor(df$fam_id_68)
        df$ind_id <- factor(df$ind_id)
    #g) Make assets variable 
        df$eq_assets <- df$eq_wealth + df$eq_debt
    #h) impute race with mode race
        J <- data.table(df)
        Mode  <-  function(x) {
          ux  <-  unique(x)
          ux[which.max(tabulate(match(x,  ux)))]
        }
        J <- J[, .(ego_black=ego_black, mode=Mode(ego_black)), by=ind_id]
        temp <- J[ego_black!=mode, ]
        temp <- temp[!duplicated(temp$ind_id), ]
        df$ego_black2 <- J$mode
    #i) deal with unkown black
        df[is.na(ego_black2), ego_black2 := ego_black]
    #j) top code hospitalization
        df$h_hospital <- ifelse(df$h_hospital>10, 10, df$h_hospital)

        
#3) Create observations for those who had died
    #a - Create dataframe with most recent info
        last.df <- df[order(df$year, decreasing = T), ]
        last.df <- last.df[!(duplicated(last.df$ind_id)), ]
    #b - Subset last.df to just those who died
        last.df <- last.df[last.df$ego_died==1, ]
    #c - create a year died variable in main dataset
        df$just_died <- ifelse(paste(df$year, df$ind_id) %in% paste(last.df$year, last.df$ind_id), 1, 0)
    #d - create rows for individuals who are already dead
        #find unique years in data
            uyears <- unique(df$year)
        #create a dataframe consisting of all possible years for dead individuals
            ind.years <- data.table(ind_id=rep(last.df$ind_id, each=length(uyears)), year=rep(uyears, length(last.df$ind_id)))
        #create dataframe that repeats last row of data for each individual for all possible years
            dead.df <- last.df[rep(seq_len(nrow(last.df)), each=length(uyears)), ]
        #identify people's birthdate
            birthdate <- last.df$year-last.df$ego_age
            birthdate <- rep(birthdate, each=length(uyears))
        #subset data to only years where people were already dead
            crit <- ind.years$year > birthdate & ind.years$year > dead.df$year
            ind.years <- ind.years[crit, ]
            dead.df <- dead.df[crit, ]
        #swap year with missing years
            dead.df$year <- ind.years$year
        #and add label indicating person is already dead
            dead.df$just_died <- 0
            dead.df$already_died <- 1
        #append these years into data
            df$already_died <- 0
            df <- rbind(df, dead.df)
            df$died <- df$just_died+df$already_died


#4) stabilize education so that time invariant
    #a - for simplicity, create education variable (again)
        df$educ <- ifelse(df$ego_dg_highschool==1, 1, ifelse(df$ego_dg_somecollege==1, 2, ifelse(df$ego_dg_bachelors==1, 3, ifelse(df$ego_dg_advanced==1, 4, 0))))
    #b - Create dataframe with most recent info
        tmp <- df[order(df$year, decreasing = T), ]
        tmp <- tmp[!(duplicated(tmp$ind_id)), ]
    #c - create df of each person's highest education
        tmp <- data.table(ind_id=tmp$ind_id, higheduc=tmp$educ)
    #d - merge back into df
        df <- data.table(df)
        df <- merge(df, tmp, by="ind_id")
        df$ego_dg_lthighschool <- ifelse(df$higheduc==0, 1, 0)
        df$ego_dg_highschool <- ifelse(df$higheduc==1, 1, 0)
        df$ego_dg_somecollege <- ifelse(df$higheduc==2, 1, 0)
        df$ego_dg_bachelors <- ifelse(df$higheduc==3, 1, 0)
        df$ego_dg_advanced <- ifelse(df$higheduc==4, 1, 0)
        df$fam_region <- ifelse(df$fam_region=="South", 1, 0)

        
#5) create lagged dv for each DV
    #a) sort variables by patid and time.id
        setorderv(df, c("ind_id", "year"))  
    #b) identify variables to lag
        dv.names <- grep("^h\\_", names(df), value = T)
    #c) lag DVs
        df <- copy(df)
        df[, (dv.names) := lapply(.SD, function(x) c(x[-1], NA)), by = ind_id, .SDcols = dv.names]
    #d) Revise health vars
        df$h_general <- 6-df$h_general
        df$h_general <- factor(df$h_general, levels = sort(unique(df$h_general)), ordered = T)
        df$h_lim_work <- factor(df$h_lim_work, levels = sort(unique(df$h_lim_work)), ordered = T)
        
        
#6) Create different versions of wealth variables
    #a - Create function for estimating wealth to asset ratio
        propfun <- function(x, debt=F){
          x <- x/df$eq_assets
          x <- ifelse(x<0, 0, x)
          x <- ifelse(is.na(x), 0, x)
          x <- ifelse(is.infinite(x), 1, x)
          if(debt==T){
            x <- ifelse(x>2, 0, x)
          }else{
            x <- ifelse(x>1, 0, x)
          }
          return(x)
        }
    #b - create function for capping outliers and logging
        transfun <- function(x){
          x <- x*1000
          x <- ifelse(x > quantile(x, .99, na.rm = T), quantile(x, .99, na.rm = T), x)
          x <- ifelse(x < quantile(x, .02, na.rm = T), quantile(x, .02, na.rm = T), x)
          x <- x - min(x, na.rm = T)+1
          x <- log10(x)
          return(x)
        }
    #c - create IHS tranformation function
        ihs <- function(x) {
          y <- log(x + sqrt(x ^ 2 + 1))
          return(y)
        }
    #d - transform wealth
        df[, ':='(
            #total wealth dummies
              wealth.group =
                ifelse(25000 > eq_wealth, "<25k", 
                ifelse(50000 > eq_wealth, "25k-50k", 
                ifelse(100000 > eq_wealth, "50k-100k", 
                ifelse(250000 > eq_wealth, "100k-250k", 
                ifelse(500000 > eq_wealth, "250k-500k", ">500k" ))))),
            #wealth component dummies
                d_eq_debt = ifelse(eq_debt > 0, 1, 0),
                d_eq_stock = ifelse(eq_stock > 0, 1, 0),
                d_eq_savings = ifelse(eq_savings > 0, 1, 0),
                d_eq_home = ifelse(eq_home > 0, 1, 0),
            #wealth to asset ratios
                peq_home = propfun(eq_home)^(1/3),
                peq_debt = propfun(eq_debt, debt=T)^(1/3),
                peq_stock = propfun(eq_stock)^(1/3),
                peq_savings = propfun(eq_savings)^(1/3),
            #log transformations
                log_wealth = transfun(eq_wealth/1000),
                log_income = transfun(inc_total/1000),
            #ihs transformation
                ihs_wealth = ihs(eq_wealth),
                ihs_income = ihs(inc_total),
                ihs_debt = ihs(eq_debt),
                ihs_stock = ihs(eq_stock),
                ihs_savings = ihs(eq_savings),
                ihs_home = ihs(eq_home),
            #neg networth
                neg_wealth = eq_wealth < 0
        )]
        df[, ':='(
            #set negative components to zero
                ihs_stock = ifelse(ihs_stock < 0, 0 ,ihs_stock),
                ihs_savings = ifelse(ihs_savings < 0, 0 , ihs_savings)
        )]

        
#7) save data
    saveRDS(df, "7 - Merged_Data.rds")
        
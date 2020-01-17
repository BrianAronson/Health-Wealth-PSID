library(data.table)

#0) Set directory
    # setwd("C:/Users/briarons/Desktop/Analysis - Data/PSID")
    setwd("C:/Users/bda13/Desktop/Analysis - Data/PSID")
    # setwd("C:/Users/admin/Desktop/Sociology/PSID Data")

        
#1) read data
    df <- data.table(readRDS("5 - Merged_Data.rds"))


#2) Create observations for those who had died
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


#3) stabilize education so that time invariant
    #a - for simplicity, create education variable (again)
        df$educ <- ifelse(df$ego_dg_highschool==1, 1, ifelse(df$ego_dg_somecollege==1, 2, ifelse(df$ego_dg_bachelors==1, 3, ifelse(df$ego_dg_advanced==1, 4, 0))))
    #b - Create dataframe with most recent info
        I2 <- df[order(df$year, decreasing = T), ]
        I2 <- I2[!(duplicated(I2$ind_id)), ]
    #c - create df of each person's highest education
        tm <- data.table(ind_id=I2$ind_id, higheduc=I2$educ)
    #d - merge back into df
        df <- data.table(df)
        df <- merge(df, tm, by="ind_id")
        df$ego_dg_lthighschool <- ifelse(df$higheduc==0, 1, 0)
        df$ego_dg_highschool <- ifelse(df$higheduc==1, 1, 0)
        df$ego_dg_somecollege <- ifelse(df$higheduc==2, 1, 0)
        df$ego_dg_bachelors <- ifelse(df$higheduc==3, 1, 0)
        df$ego_dg_advanced <- ifelse(df$higheduc==4, 1, 0)
        df <- data.frame(df)
        df$fam_region <- ifelse(df$fam_region=="South", 1, 0)

        
#4) create lagged dv for each DV
    df <- data.table(df)
    #a) sort variables by patid and time.id
        setorderv(df, c("ind_id", "year"))  
    #b) identify variables to lag
        dv.names <- grep("^h\\_", names(df), value = T)
    #c) lag DVs
        df[, (dv.names) := lapply(.SD, function(x) c(x[-1], NA)), by = ind_id, .SDcols = dv.names]

        
#5) Create different versions of wealth variables
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
    #c - transform wealth
        df[, ':='(
            #total wealth dummies
              wealth.group =
                ifelse(25 < eq_wealth, "<25k", 
                ifelse(50 < eq_wealth, "25k-50k", 
                ifelse(100 < eq_wealth, "50k-100k", 
                ifelse(250 < eq_wealth, "100k-250k", 
                ifelse(500 < eq_wealth, "250k-500k", ">500k" ))))),
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
                eq_wealth = eq_wealth/1000,
                inc_total = inc_total/1000,
                log_wealth = transfun(eq_wealth),
                log_income = transfun(inc_total)
        )]


#6) Other minor changes
    #a) cap overly skewed health outcomes
        df$h_BMI[df$h_BMI > quantile(df$h_BMI, .999, na.rm = T)] <- quantile(df$h_BMI, .999, na.rm = T)
    #b) Reverse code cohorts
        df$ego_cohort <- max(df$ego_cohort, na.rm=T)+1-df$ego_cohort
    #c) change family ids to track year
        df$fam_id <- df$fam_id+10000000*df$year
    #d) center year
        df$year2  <-  df$year - min(df$year)
    #e) Center age
        df$ego_age <- df$ego_age-min(df$ego_age, na.rm = T)
        df$ego_age2 <- df$ego_age^2
    #f) change ids to fact
        df$fam_id <- factor(df$fam_id)
        df$fam_id_68 <- factor(df$fam_id_68)
        df$ind_id <- factor(df$ind_id)
    #g) deal with unkown black
        df[is.na(ego_black2), ego_black2 := ego_black]
        
        
#7) save data
    saveRDS("6 - Merged_Data.rds")
        
        
#7) Reduce sample based on criteria
    #a) make criteria to subset by
        df[, years.after.1984 := year %in% c(1984, 1989, 1994, 1999, 2001, 2003, 2005, 2007,2009,2011,2013,2015)]
        df[, years.2005.2011 := year %in% c(2005:2011)]
        df[, head.or.wife := rel_to_head == "wife" | rel_to_head == "head"]
        df[, over.25 := !is.na(ego_age) & ego_age > 25 & ego_age != 999]
        df[, only.black.or.white := latino == 0 & ifelse(is.na(ego_race), F, ifelse(ego_race %in% c("White", "Black"), T, F))]
    #b) subset 2005 
        df.2005 <- df[years.2005.2011 == T, ]
        df.2005 <- df.2005[head.or.wife == T, ]
        df.2005 <- df.2005[over.25 == T, ]
        df.2005 <- df.2005[only.black.or.white == T, ]
        df.2005 <- df.2005[badmatch == F, ]
        df.2005[, two.obs.since.2005 := ind_id %in% df.2005[year >= 2005 & year <= 2011 & !already_died, .N , by = "ind_id"][N >= 2, ind_id]]
        df.2005 <- df.2005[two.obs.since.2005 == T, ]
        
    #c) subset 1984
        df.1984 <- df[years.after.1984 == T, ]
        df.1984 <- df.1984[head.or.wife == T, ]
        df.1984 <- df.1984[over.25 == T, ]
        df.1984 <- df.1984[only.black.or.white == T, ]
        df.1984 <- df.1984[badmatch == F, ]
        df.1984[, three.obs.since.1984 := ind_id %in% df.1984[years.after.1984 == T & !already_died, .N , by = "ind_id"][N >= 3, ind_id]]
        df.1984 <- df.1984[three.obs.since.1984 == T, ]
        
    #d) na.omit variables that aren't DVs 
        na.rows <- rowSums(is.na(df.1984[, c("ego_age", "ego_age2", "ego_black2", "ego_female", "ego_dg_advanced", "ego_dg_bachelors", "ego_dg_highschool", "ego_dg_somecollege", "eq_home", "eq_bus", "eq_savings", "eq_otr_estate", "eq_stock", "eq_debt", "inc_total", "eq_wealth", "ind_id", "year", "fam_id_68", "fam_id", "eq_home_d", "eq_assets", "ego_cohort", "year2", "just_died", "already_died", "fam_region")])) > 0
        df.1984 <- df.1984[!na.rows, ]
        na.rows <- rowSums(is.na(df.2005[, c("ego_age", "ego_age2", "ego_black2", "ego_female", "ego_dg_advanced", "ego_dg_bachelors", "ego_dg_highschool", "ego_dg_somecollege", "eq_home", "eq_bus", "eq_savings", "eq_otr_estate", "eq_stock", "eq_debt", "inc_total", "eq_wealth", "ind_id", "year", "fam_id_68", "fam_id", "eq_home_d", "eq_assets", "ego_cohort", "year2", "just_died", "already_died", "fam_region")])) > 0
        df.2005 <- df.2005[!na.rows, ]
        
        
        
#8) Reduce dataset to variables of interest
    df <- df[, c("h_general", "h_BMI", "h_activities", "h_conditions", "h_heart_attack", "h_stroke", "h_hospital", "h_lim_work", "h_disabled", "ego_age", "ego_age2", "ego_black2", "ego_female", "ego_dg_advanced", "ego_dg_bachelors", "ego_dg_highschool", "ego_dg_somecollege", "eq_home", "eq_bus", "eq_savings", "eq_otr_estate", "eq_stock", "eq_debt", "inc_total", "eq_wealth", "ind_id", "year", "fam_id_68", "fam_id", "eq_home_d", "eq_assets", "h_distress", "ego_cohort", "year2", "just_died", "already_died", "fam_region")] #"eq_vehicle", "eq_otr_assets"

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     
#8 - Model prep
    #a - choose variables
        vars <- c("h_general", "h_BMI", "h_activities", "h_conditions", "h_heart_attack", "h_stroke", "h_hospital", "h_lim_work", "h_disabled", "h_distress")
        y <- vars[1]
    #b - create model output function to make results pretty
        prstars <- function(x){
          x[, 2] <- ifelse(x[, 2]<.001, "***", ifelse(x[, 2]<.01, "**", ifelse(x[, 2]<.05, "*", "")))
          return(x)
        }
        sum.model <- function(model) {
          x <- as.data.frame(round(summary(model)$coefficients, 10))
          x[, c(1, ncol(x))]
        }
    #c - assign models to test
        # form1 <- y~ego_black2 + ego_age+ (1|fam_id_68/ind_id) + (1|fam_id)
        # form1 <- y~ego_black2 + ego_age+ (1 | fam_id_68) + (1| ind_id)
        form1 <- y~ego_black2 + ego_age+ego_age2 + ego_female + ego_black2 *(ego_age+ego_age2)+fam_region +year2+died  +(1|fam_id_68/ind_id) #(1|fam_id_68/ind_id) #(1 | fam_id_68) + (1| ind_id)  + (1|fam_id)
        form2 <- update(form1, ~. + log_income  + ego_dg_highschool+ego_dg_somecollege+ego_dg_bachelors+ego_dg_advanced)
        form2b <- update(form2, ~. + ego_black2*(log_income  + ego_dg_highschool+ego_dg_somecollege+ego_dg_bachelors+ego_dg_advanced))
        form3 <- update(form2, ~. + log_wealth)
        form3b <- update(form2, ~. + log_wealth*ego_black2)
        form4 <- update(form3, ~. + eq_home_d+ peq_home+peq_debt+peq_stock+peq_savings)
        form4b <- update(form4, ~. + ego_black2*(eq_home_d+peq_home+peq_debt+peq_stock+peq_savings))
        form5 <- update(form2, ~. + eq_home_d+ peq_home+peq_debt+peq_stock+peq_savings)
        form5b <- update(form5, ~. + ego_black2*(eq_home_d+peq_home+peq_debt+peq_stock+peq_savings))

}


    
#11 - Run models
    results <- list()
    df.model <- list()
    s.model <- list()
    m.model <- list()
    for(df in c(1, 2, 5, 6, 9)){ #1:length(vars)
      {
      #a - Select variable
        # I3  <-  I2[rowSums(is.na(I2))== 0, ]
        I3  <-  I2
        tm <- as.data.frame(table(I3$ind_id[I3$year>2004 & I3$year<2012 & I3$already_died==0]))
        rec.ids <- tm$Var1[tm$Freq>1]
        I3 <- I3[I3$ind_id%in% rec.ids, ]
        I3$y <- I3[, vars[df]]
      #b - Select model
        if(df==1){
                I3$y <- factor(I3[, vars[df]])
                init.model <-  function(form, df) clmm(formula=form, data=df, Hess=T)
        }
        if(df==2) init.model <-  function(form, df) lmer(formula=form, data=df)
        # if(df==3) init.model <-  function(form, df) glmer.nb(formula=form, data=df)
        # if(df==4) init.model <-  function(form, df) glmer(formula=form, data=df, family=poisson)
        if(df==5) init.model <-  function(form, df) glmer(formula=form, data=df, family = binomial, nAGQ=0)
        if(df==6) init.model <-  function(form, df) glmer(formula=form, data=df, family = binomial, nAGQ=0)
        # if(df==7) init.model <-  function(form, df) glmer.nb(formula=form, data=df)
        # if(df==8) init.model <-  function(form, df) glmmTMB(form, data=df, zi=~1, family='nbinom1')
        if(df==9) init.model <-  function(form, df) glmer(formula=form, data=df, family = binomial, nAGQ=0)
        # if(df==10) init.model <-  function(form, df) glmer.nb(formula=form, data=df)
      } 
      #c - Run models
        #1
          print(system.time({
            df.model.1 <- init.model(form1, df=I3)      
          }))
          s.model.1 <- sum.model(df.model.1)
          m1 <- prstars(s.model.1)
          m.1.1 <- prstars(s.model.1)
          print("m1")
        #2
          df.model.2 <- init.model(form2, df=I3)
          s.model.2 <- sum.model(df.model.2)
          m2 <- prstars(s.model.2)
          m.2.2 <- prstars(s.model.2)
          print("m2")
        #3
          df.model.3 <- init.model(form2b, df=I3)
          s.model.3 <- sum.model(df.model.3)
          m3 <- prstars(s.model.3)
          m.3.3 <- prstars(s.model.3)
          print("m3")
        #4
          df.model.4 <- init.model(form3, df=I3)
          s.model.4 <- sum.model(df.model.4)
          m4 <- prstars(s.model.4)
          m.4.4 <- prstars(s.model.4)
          print("m4")
        #5
          df.model.5 <- init.model(form3b, df=I3)
          s.model.5 <- sum.model(df.model.5)
          m5 <- prstars(s.model.5)
          m.5.5 <- prstars(s.model.5)
          print("m5")
        #6
          df.model.6 <- init.model(form4, df=I3)
          s.model.6 <- sum.model(df.model.6)
          m6 <- prstars(s.model.6)
          m.6.6 <- prstars(s.model.6)
          print("m6")
        #7
          df.model.7 <- init.model(form4b, df=I3)
          s.model.7 <- sum.model(df.model.7)
          m7 <- prstars(s.model.7)
          m.7.7 <- prstars(s.model.7)
          print("m7")
        #8
          df.model.8 <- init.model(form5, df=I3)
          s.model.8 <- sum.model(df.model.8)
          m8 <- prstars(s.model.8)
          m.8.8 <- prstars(s.model.8)
          print("m8")
        #9
          df.model.9 <- init.model(form5b, df=I3)
          s.model.9 <- sum.model(df.model.9)
          m9 <- prstars(s.model.9)
          m.9.9 <- prstars(s.model.9)
          print("m9")
      #d - prep models for merge
        m1$var <- row.names(m1)
        names(m1) <- c("1", "1.1", "var")
        m2$var <- row.names(m2)
        names(m2) <- c("2", "2.1", "var")
        m3$var <- row.names(m3)
        names(m3) <- c("3", "3.1", "var")
        m4$var <- row.names(m4)
        names(m3) <- c("4", "4.1", "var")
        m5$var <- row.names(m5)
        names(m5) <- c("5", "5.1", "var")
        m6$var <- row.names(m6)
        names(m6) <- c("6", "6.1", "var")
        m7$var <- row.names(m7)
        names(m7) <- c("7", "7.1", "var")
        m8$var <- row.names(m8)
        names(m8) <- c("8", "8.1", "var")
        m9$var <- row.names(m9)
        names(m9) <- c("9", "9.1", "var")
      #e - prep empty df to put results into
        df <- data.frame(var=unique(c(row.names(m1), row.names(m2), row.names(m3), row.names(m4), row.names(m5), row.names(m6), row.names(m7), row.names(m8), row.names(m9))))
      #f - bind vars to df
        df <- merge(df, m1, by="var", all=T, sort = F)
        df <- merge(df, m2, by="var", all=T, sort = F)
        df <- merge(df, m3, by="var", all=T, sort = F)
        df <- merge(df, m4, by="var", all=T, sort = F)
        df <- merge(df, m5, by="var", all=T, sort = F)
        df <- merge(df, m6, by="var", all=T, sort = F)
        df <- merge(df, m7, by="var", all=T, sort = F)
        df <- merge(df, m8, by="var", all=T, sort = F)
        df <- merge(df, m9, by="var", all=T, sort = F)
        results[[df]] <- df
        df.model[[df]] <- list(df.model.1, df.model.2, df.model.3, df.model.4, df.model.5, df.model.6, df.model.7, df.model.8, df.model.9)
        s.model[[df]] <- list(s.model.1, s.model.2, s.model.3, s.model.4, s.model.5, s.model.6, s.model.7, s.model.8, s.model.9)
        m.model[[df]] <- list(m.1.1, m.2.2, m.3.3, m.4.4, m.5.5, m.6.6, m.7.7, m.8.8, m.9.9)
        saveRDS(results, "results.rds")
        saveRDS(df.model, "df.model.rds")
        saveRDS(s.model, "s.model.rds")
        saveRDS(m.model, "m.model.rds")
        print(df)

    
#For each model, predict health by race and proportion assets in savings. Unfortunately, the clmm predict function is bugged, so have to do this manually.

library(ggplot2)
library(data.table)
library(plm)
library(pglm)
library(lme4)
library(nlme)
library(lmerTest)
library(cowplot)
library(car)
library(xlsx)
library(ordinal)
library(R2admb) #zero-infalted
library(glmmADMB) #zero-inflated models
library(ordinal)
library(merTools)
library(ggthemes)

#0) Set directory
    setwd("C:/Users/admin/Desktop/Sociology/PSID Data")

    
#1) read model and raw data
    df.modll <- readRDS("l.results.2005.rds")
    df <- data.table(readRDS("8 - df.2005.rds"))
    

#2) Do the following for models 3 and 7 for SRH
    df.modll <- list(df.modll[[1]][[2]], df.modll[[1]][[7]])
    # df.modll <- unlist(df.modll, recursive = F)

    
#3) put all model results into a data.frame
    #i) identify variables
        vars <- c("Self-Rated Health",
                  "# of Conditions",
                  "Work Limitations",
                  "Disability",
                  "Distress",
                  "BMI"
                )
    #ii) for each dv and for each model, pull out info
        df.mod <- list()
          for(i in 1:length(df.modll)) {
            tdf <- data.frame(summary(df.modll[[i]])$coefficient)
            tdf <- tdf[,c(1:2, ncol(tdf))]
            tdf$var <- row.names(tdf)
            tdf <- data.table(tdf)
            names(tdf) <- c("est", "se", "pr", "var")
            tdf[, ':='(dv = vars[i])]
            df.mod[[i]] <- tdf
          }

        
#4) identify bounds and significance
    for(i in 1:length(df.mod)){
      df.mod[[i]][, ':='(
        CIlower = est - se * 1.96,
        CIupper = est + se * 1.96,
        significant = ifelse(abs(pr) < .05, "black", "grey")
      )]
    }
        
        
#5) make a dataset to base predictions on 
    #a) reduce df to just outcomes of interest
        df <- df[, names(df) %in% c(df.mod[[2]]$var), with = F]
    #b) find the median of everything.
        df.manip <- df[, lapply(.SD, function(x) ifelse(class(x)=="factor", x[1], median(x)))]
    #c) add a row where ego_black2 == 1
        df.manip <- df.manip[rep(1, 2), ]
        df.manip[2, ego_black2 := 1]
    #d) add rows for 20 unique values of age
        df.manip <- df.manip[rep(1:2, each = 21), ]
        age.range <- max(df$ego_age) - min(df$ego_age)
        ego_age.vals <- min(df$ego_age) + 0:20 * age.range/20
        df.manip[, ego_age := rep(ego_age.vals, 2)]
        df.manip[, ego_age2 := ego_age^2]
    #e) add variables for interactions
        df.manip[, ':='(
          `ego_black2:ego_age` = ego_black2 * ego_age,
          `ego_black2:ego_age2` = ego_black2 * ego_age2,
          `ego_black2:ego_dg_highschool` = ego_black2 * ego_dg_highschool,
          `ego_black2:ego_dg_somecollege` = ego_black2 * ego_dg_somecollege,
          `ego_black2:ego_dg_bachelors` = ego_black2 * ego_dg_bachelors,
          `ego_black2:ego_dg_advanced` = ego_black2 * ego_dg_advanced,
          `ego_black2:ihs_income` = ego_black2 * ihs_income,
          `ego_black2:ihs_wealth` = ego_black2 * ihs_wealth,
          `ego_black2:eq_home_d` = ego_black2 * eq_home_d,
          `ego_black2:ihs_home` = ego_black2 * ihs_home,
          `ego_black2:ihs_debt` = ego_black2 * ihs_debt,
          `ego_black2:ihs_stock` = ego_black2 * ihs_stock,
          `ego_black2:ihs_savings` = ego_black2 * ihs_savings
        )]
        

#6) make predicted estimates for each manipulation and each model + CIs
    l.df.pred <- list()
    for(j in 1: length(df.mod)){
      #a) grab model
          x <- df.mod[[j]]
      #b) cop structure of manipulations
          df.pred <- copy(df.manip)
          df.pred <- df.pred[, names(df.pred) %in% x$var, with = F]
      #c) pull variable names
          pred.vars <- x$var[!grepl("^[1-9]", x$var)]
      #d) for each variable, multiply estimates by manipulations to get variable-specific predicted values
          for(i in 1:length(pred.vars)){
              df.pred[, (pred.vars[i]) := x$est[x$var == pred.vars[i]] * .SD, .SDcols = pred.vars[i]]
          }
      #e) add intercept
          df.pred$intercept <- x$est[x$var == "(Intercept)" | x$var == "2|3"]
      #f) sum variable based predictions
          df.pred$preds <- rowSums(df.pred[, names(df.pred), with = F])
      #g) append to list
          l.df.pred[[j]] <- df.pred
    }

  

#7) format predictions
    tempdfl <- list()
    for(i in 1:length(l.df.pred)){
      tempdfl[[i]] <- data.table(
                        Estimate.x = df.manip$ego_age, 
                        Estimate.y = l.df.pred[[i]]$preds, 
                        Black = df.manip$ego_black2,
                        DV = df.mod[[i]]$dv[1],
                        model = i
                      )
    }
    tempdf <- rbindlist(tempdfl)
    tempdf$group = paste(ifelse(tempdf$Black == 1, "Black", "White"), ifelse(tempdf$model == 1, "Model 2b", "Model 4b"))
    tempdf$Black <- factor(tempdf$Black)
    tempdf$Estimate.x <- tempdf$Estimate.x +25
    tempdf <- tempdf[Estimate.x <= 75, ]
    tempdf[model == 2, Estimate.y := Estimate.y + .25]
    tempdf[, model := ifelse(model == 1, "Model 2b", "Model 4b") ]
    
    
#8) Graph
        p <- ggplot(tempdf, aes(x=Estimate.x, y=Estimate.y, color = Black)) +
          geom_line(size = 2)+
          facet_grid(~model)+
          scale_color_manual(values=c("grey", "black"))+
          theme_pander()+
                theme(
                      axis.line.x = element_line(colour = "black"),
                      axis.line.y = element_line(colour = "black"),
                      plot.title = element_text(size=15, hjust=0.5),
                      # axis.title.x=element_blank(),
                      # axis.title.y=element_blank(),
                      strip.text.x = element_text(size=13, hjust=0.5, margin = margin(b = 10), family = "serif", face="bold"),
                      axis.title = element_text(family = "serif", size = 13, face = "bold"), 
                      title = element_text(family = "serif", size = 13, face = "bold"),
                      text = element_text(family = "serif"),
                      #legend.position="none",
                      panel.spacing = unit(2.5, "lines"),
                      axis.ticks= element_blank(),
                      plot.margin = unit(c(l=2, t=2, b=.2, 2), "lines")
                      ) +
          labs(x = "\n Age", y = "Self-Rated Health (log-odds) \n")
        
    ggsave(fname <- "C:/Users/admin/Desktop/Ap3 - Black-White of Health Gaps by Age and Wealth-Conditioning.png", p, width = 7, height = 4)
    browseURL(fname)
    

    
    
    
    
    sapply(df.modll[[1]], AIC)


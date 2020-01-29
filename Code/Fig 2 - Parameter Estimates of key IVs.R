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
library(ggthemes)

#0) Set directory
    setwd("C:/Users/admin/Desktop/Sociology/PSID Data")

    
#1) read models
    dfll <- readRDS("l.std.results.2005.rds")
    
    
#2) put all model results into a data.frame
    #i) identify variables
        vars <- c("Self-Rated Health",
                  "# of Conditions",
                  "Work Limitations",
                  "Disability",
                  "Distress",
                  "BMI"
                )

    #ii) for each dv and for each model, pull out info and append to one df
        df <- list()
        for(j in 1:length(dfll)) {
          dfl <- dfll[[j]]
          for(i in 1:length(dfl)) {
            tdf <- data.frame(summary(dfl[[i]])$coefficient)
            tdf <- tdf[,c(1:2, ncol(tdf))]
            tdf$var <- row.names(tdf)
            tdf <- data.table(tdf)
            names(tdf) <- c("est", "se", "pr", "var")
            tdf[, ':='(dv = vars[j], model = i)]
            dfl[[i]] <- tdf
          }
          dfl <- rbindlist(dfl)
          df[[j]] <- dfl
        }
        df <- rbindlist(df)

        
#3) format dataframe
    #a) remove control variables
        ivs <- c("eq_home_d", "ihs_home", "ihs_debt", "ihs_stock", "ihs_savings", "ego_black2:eq_home_d", "ego_black2:ihs_home", "ego_black2:ihs_debt", "ego_black2:ihs_stock", "ego_black2:ihs_savings")
        df <- df[var %in% ivs, ]
    #b) identify bounds and significance
        df[, ':='(
          CIlower = est - se * 1.96,
          CIupper = est + se * 1.96,
          significant = ifelse(abs(pr) < .05, "black", "grey")
        )]
    #c) relabel var names
        df[, var := 
            ifelse(var == "eq_home_d", "Own Home",
            ifelse(var == "ihs_home", "Home Equity",
            ifelse(var == "ihs_debt", "Debt",
            ifelse(var == "ihs_stock", "Stocks",
            ifelse(var == "ihs_savings", "Savings",
            ifelse(var == "ego_black2:eq_home_d", "Black * Own Home",
            ifelse(var == "ego_black2:ihs_home", "Black * Home Equity",
            ifelse(var == "ego_black2:ihs_debt", "Black * Debt",
            ifelse(var == "ego_black2:ihs_stock", "Black * Stocks",
            ifelse(var == "ego_black2:ihs_savings", "Black * Savings", ""             
        ))))))))))]
    #d) order IVs
        df[, var := factor(var, levels = rev(c("Own Home", "Home Equity", "Savings", "Stocks", "Debt", "Black * Own Home", "Black * Home Equity", "Black * Savings", "Black * Stocks", "Black * Debt")))]
    #e) order DVs
        df[, dv := factor(dv, levels = vars)]
    

#6) dynamically alter tick marks (breaks)
    #a) function for rounding based on base to round by
        mround  <-  function(x, base){
          base*round(x/base)
        }
    #b) function to set ggplot axis breaks based on input data
        five_breaks  <-  function(...){
            function(x){
              #get real limits
                  lim <- round(c(min(x)/1.1, max(x)/1.1)*100)/100
              #set limit to be simpler number if limits not equal
                  if(lim[1]!=lim[2]){
                    if(nchar(abs(lim[1]))<nchar(lim[2])){
                      lim[2] <- abs(lim[1])
                    }else{
                      lim[1] <- (-lim[2])
                    }
                  }
              #determine 5 breaks with one break at 0
                  br <- sort(c(lim, lim/2, 0))
                  br
              #if can't be broken into nice units, break into 7
                  # if(!br[4] %in% c(.01, .025, .05, .1, .25, .5, 1, 1.25, 2.5))
                  # br <- c(lim[1], lim[1]-((lim[1]/3)*1:6))
                  # round(br, 3)
            }
        }
    #c) function to label axis breaks with correct number of digits
        labsfun <- function(x){
            # if(nchar(x[2])>5 | nchar(x[1])>5){
              paste0(sprintf("%.3f", x))
            # }else if(nchar(x[2])>4 | nchar(x[1])>4){
            #   paste0(sprintf("%.2f", x))
            # }else{
            #   paste0(sprintf("%.1f", x))
            # }
          }

        
#7) set limits for each y axis
    dft <- copy(df)
    dft[, y_min := -max(c(abs(CIupper), abs(CIlower))), by = dv]
    dft$y_min <- ceiling(dft$y_min*22)/20
    dft$y_max <- (-dft$y_min)
    #manually adjust limits
        dft[dv == "Disability", y_max := .75]
        dft[dv == "BMI", y_max := .3] 
        dft$y_min <- (-dft$y_max)
    
    
#8) reduce to just key DVs and final model
    dft <- dft[var %in% c("Own Home", "Home Equity", "Savings", "Stocks", "Debt"), ]
    dft <- dft[model == 7, ]

    
#9) plot    
    p <- ggplot(dft, aes(y=est, x=var))+
      coord_flip()+
      geom_linerange(aes(ymin=CIlower, ymax=CIupper), size=.7, color=dft$significant) +
      theme_bw() +
      geom_hline(yintercept = 0)+
      geom_point(size=2.5, color=dft$significant) +
      labs(x="", y = "\n Parameter Estimate")+
      ggtitle("Figure 2: Parameter Estimates of Key Independent Variables by Dependent Variable")+
      theme(panel.background = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"), 
            text=element_text(family="serif"), 
            title = element_text(size=10, face="bold"), 
            strip.text.x = element_text(size = 10, margin = margin(0.05, 0.05, 0.05, 0.05, "cm")), 
            axis.text.x = element_text(size=9), 
            axis.text.y = element_text(size=9), 
            panel.spacing = unit(1, "lines"))+
      facet_wrap(~dv, scales = "free_x")+
      geom_blank(aes(y = y_min))+
      geom_blank(aes(y = y_max))+
      scale_y_continuous(breaks = five_breaks(), labels = labsfun)

    f.name <- "C:/Users/admin/Desktop/Figure 2 - Parameter Estimates of key IVs.png"
    ggsave(f.name, p, width = 7.5*1.1, height = 4*1.1, units = "in")
    browseURL(f.name)
    
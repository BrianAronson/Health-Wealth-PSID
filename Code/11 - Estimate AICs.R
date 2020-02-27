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
    sapply(df.modll[[1]], AIC)
    sapply(df.modll[[2]], AIC)
    sapply(df.modll[[3]], AIC)
    sapply(df.modll[[4]], AIC)
    sapply(df.modll[[5]], AIC)
    sapply(df.modll[[6]], AIC)


library(data.table)
library(xlsx)
library(ggplot2)

#0) Set directory
    setwd("C:/Users/admin/Desktop/Sociology/PSID Data")

#1) Read data
    df <- data.table(readRDS("8 - df.1984.rds"))
        
#2) Pull general health, age, wealth, and race
    tdf <- df[already_died == 0, c("h_general", "eq_wealth", "ego_age", "ego_black", "wealth.group")]
    
#3) plot health by age and race
    gdf <- copy(tdf)
    gdf[, ego_black := ifelse(ego_black == 1, "Black", "White")]
    gdf <- gdf[ego_age <= 72, ]
    gdf <- na.omit(gdf)
    
    gdf2 <- gdf[, lapply(.SD, mean), by = c("ego_black","ego_age", "wealth.group")]
    gdf2[, wealth.group := factor(wealth.group, levels = unique(gdf2[order(eq_wealth), ]$wealth.group))]
    gdf3 <- gdf[, .(h_general = mean(h_general)), by = c("ego_black","ego_age")]
    
    png(file = "C:/Users/admin/Desktop/Health by Age and Race.png", height=4, width=7,units = "in",res = 300)
    ggplot(gdf3, aes(y = h_general, x = ego_age, color = ego_black)) +
        # facet_wrap(~ wealth.group)+
        scale_color_grey(start = 0, end = .8) +
        theme_bw() +
        geom_smooth(size = 2, se = F) +
        labs(x = "Age", y = "General Health", color = "Race") +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"), 
              legend.text = element_text(size = 12),
              text = element_text(family = "serif"),
              legend.title = element_text(size = 12, face = "bold"),
              title = element_text(size = 12, face = "bold"),
              legend.position = "bottom", 
              axis.text.x = element_text(size = 11.5),
              # axis.text.y = element_blank(),
              axis.text.y = element_text(size = 11.5), 
              axis.title.x= element_text(size = 12, face = "bold", margin = margin(t = 10, r = 0, b = 0, l = 0)),
              axis.title.y= element_text(size = 13.5, face = "bold", margin = margin(t = 0, r = 15, b = 0, l = 0)),
              strip.text.x = element_text(size = 14))
    dev.off()
    browseURL("C:/Users/admin/Desktop/Ap3 - BW health gaps by age.png")
    

#4) Estimate wealth-adjusted version
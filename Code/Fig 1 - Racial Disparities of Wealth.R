library(ggplot2)
library(data.table)

#0) Set directory
    setwd("C:/Users/admin/Desktop/Sociology/PSID Data")

    
#1) Read data
    df <- data.table(readRDS("8 - df.2005.rds"))

    
#2) Prepare data
    #a) remove duplicate observations
        df <- df[order(df$year, decreasing = T), ]
        df <- df[!(duplicated(df$ind_id)), ]
    #b) divide wealth by 1000
        df$eq_wealth <- df$eq_wealth / 1000
    #c) remove wealth outliers
        df <- df[, c("eq_wealth", "ego_black2")]
        df <- df[df$eq_wealth < 500 & df$eq_wealth > -100, ]
    #d) reformat race variable
        df$ego_black2 <- ifelse(df$ego_black2 == 0, "White   ", "Black   ")
        df$ego_black2 <- factor(df$ego_black2, levels = sort(unique(df$ego_black2), decreasing = F))
    #e) smooth tail for blacks better
        tdf <- df[eq_wealth > 100 & ego_black2 == "Black   ", ][order(eq_wealth)]
        tdf[, index := 1:.N]
        preds <- predict(loess(eq_wealth ~ index, tdf, span = .25))
        df[eq_wealth > 100 & ego_black2 == "Black   ", eq_wealth := preds]


#3) plot data
    p <- ggplot()+
        geom_density(data = df[df$ego_black2 == "Black   ", ],
            aes(x = eq_wealth),
            alpha = 1,
            fill = "black") +
        geom_density(data = df[df$ego_black2 == "White   ", ],
            aes(x = eq_wealth),
            alpha = .6,
            fill = "grey90") +
        scale_fill_grey(start = 0, end = .8) +
        scale_x_continuous(labels = scales::dollar_format(), breaks = c(0, 100, 200, 300, 400, 500)) +
        labs(x = "Wealth (Thousands USD)", y = "Density", fill = "") +
        ggtitle("Figure 1 - Racial Disparities in Net Worth") +
        theme_bw() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          text = element_text(family = "serif"),
          title = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(size = 11.5),
          axis.text.y = element_text(size = 11.5),
          axis.title.x = element_text( size = 12, face = "bold", margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text( size = 13.5, face = "bold", margin = margin(t = 0, r = 15, b = 0, l = 0)),
          strip.text.x = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12, face = "bold"),
          legend.position = "bottom"
        )

#4) save graph
    f.name <- "C:/Users/admin/Desktop/Figure 1 - Racial Disparities in Net Worth.png"
    ggsave(f.name, p, width = 7, height = 3.5, units = "in")
    browseURL(f.name)
    
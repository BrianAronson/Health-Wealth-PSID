library(ggplot2)
library(data.table)

#0) Set directory
    setwd("C:/Users/admin/Desktop/Sociology/PSID Data")


#1) Read data
    df <- data.table(readRDS("8 - df.2005.rds"))


#2) Prepare data
    #a) Keep only 2005
        df <- df[year == 2005, ]
        df$ego_age <- df$ego_age + 25

#3) plot data
    p <- ggplot()+
        geom_density(data = df,
            aes(x = ego_age),
            alpha = 1,
            fill = "grey75") +
        scale_x_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(0,100)) +
        labs(x = "Age (years)", y = "Density", fill = "") +
        ggtitle("Ap.11 - Age Distribution of Sample in 2005") +
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
    f.name <- "C:/Users/admin/Desktop/Ap.11 - Age Distribution of Sample.png"
    ggsave(f.name, p, width = 7, height = 3.5, units = "in")
    browseURL(f.name)

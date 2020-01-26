library(epitools)

df <- data.frame (
  time = rep(c("2010", "2011", "2012", "2013", "2014"),4),
  age = rep(c("40-44", "45-49", "50-54", "55-59", "60-64"),4),
  weight = rep(c(0.38, 0.23, 0.19, 0.12, 0.08),4),
  ethnic = rep(c(rep("M",5),rep("NM",5)),2),
  gender = c(rep("M",10), rep("F",10)),
  pop = round((runif(10, min = 10000, max = 99999)), digits = 0),
  count = round((runif(10, min = 100, max = 999)), digits = 0)
)
df$rate = df$count / df$pop


age_adjust_test <- ageadjust.direct(count = df$count, pop = df$pop, 
                                    rate = df$rate, stdpop = df$weight)


library(tidyverse)
df %>%
  group_by(time, age, ethnic, gender) %>%
  summarise(age_adjust = list(
    ageadjust.direct(
      count = count,
      pop = pop,
      rate = rate,
      stdpop = weight
    )
  )) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list))  %>%
  unnest(cols = c(age_adjust))

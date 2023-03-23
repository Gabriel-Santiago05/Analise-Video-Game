setwd("D:/Jupyter/Estudos/kaggle/Analise-Video-Game")

#### Setup do projeto ####
library(readr)
library(dplyr)

### Analise ###
df <- read_csv("Video_Games.csv")
View(df)
glimpse(df)

# ano esta como texto, vou alterar para inteiro
df$Year_of_Release <- as.integer(df$Year_of_Release)

table(df$Year_of_Release)
filter(df, Year_of_Release >= 2010)

?filter

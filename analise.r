setwd("D:/Jupyter/Estudos/kaggle/Analise-Video-Game")

#### Setup do projeto ####
library(readr)
library(dplyr)


#### Analise Inicial ####
df <- read_csv("Video_Games.csv")
View(df)
glimpse(df)

# ano esta como texto. Alterando para inteiro
df$Year_of_Release <- as.integer(df$Year_of_Release)

table(df$Year_of_Release)
# temos poucos lancamentos depois de 2011

df %>% 
  filter( (Year_of_Release >= 2010) & (Publisher == "Nintendo") ) %>%
  select(Publisher) %>%
  dim()
# temos apenas 174 lancamentos da nintendo entre 2010 e 2020
# e apenas um lancamento no mundo inteiro em 2020. Isso indica claramente uma inconsistencia
# na base. Para deixar o problema mais divertido e proveitoso, vou considerar que estamos no ano
# de 2012 e a nintendo deseja lancar seu titulo nos proximos anos (2012 em diante).

df <- filter(df, Year_of_Release <= 2011)
# com o problema melhor definido, hora das analises!

#### Analise Exploratoria ####
summary(df)
df$NA_Sales

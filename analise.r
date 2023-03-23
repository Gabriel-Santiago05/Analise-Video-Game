#### Setup do projeto ####
setwd("D:/Jupyter/Estudos/kaggle/Analise-Video-Game")

library(readr)
library(dplyr)
library(ggplot2)
library(cowplot)

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

# como a nintendo vai ser a base do estudo, criarei um df para ela
df_nintendo <- filter(df, Publisher=="Nintendo")


# verificando as vendas da nintendo
df_nintendo %>% 
  summarise(
    vendas_na = sum(NA_Sales),
    vendas_eu= sum(EU_Sales),
    vendas_JP = sum(JP_Sales)
  )
# identifiquei que a nintendo vende menos na Europa inteira do que no Japao
glimpse(df_nintendo)

# observando as vendas na europa ao longo do tempo
a <- ggplot(data=df_nintendo, aes(x=Year_of_Release , y=EU_Sales)) +
  geom_bar(stat="identity", fill='#009E73') +
  xlab('Anos') + ylab('Vendas da Nintendo (em milhões)') + ggtitle("Vendas da Nintendo na Europa")

b <-
  df_nintendo %>%
  filter(Year_of_Release >= 2002) %>%
  ggplot(aes(x=Year_of_Release, y=EU_Sales)) +
    geom_bar(stat='identity', fill='#56B4E9') +
    xlab('Anos') + ylab('Vendas da Nintendo (em milhões)') + ggtitle("Vendas da Nintendo na Europa no Últimos 10 Anos")

plot_grid(a, b, nrow=1, ncol=2)
# e notavel o auge da nintendo nos anos de 2005 e 2006 na Europa, porem, depois disso a popularidade da empresa
# comeca a cair drasticamente.
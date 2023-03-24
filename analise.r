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

# alterando os tipos char para factor
df <- df %>% mutate_if(is.character, as.factor)

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

# vou criar um df para os ultimos 10 anos da nintendo
# quero entender melhor generos de jogos e os consoles que mais fizeram sucesso
df_nintendo10 <- filter(df_nintendo, Year_of_Release >= 2002)

# qtd de jogos lancados por plataforma

table(df_nintendo10$Platform)[table(df_nintendo10$Platform)>0]
prop.table(table(df_nintendo10$Platform))[prop.table(table(df_nintendo10$Platform))>0]*100
# a plataforma campea foi o Nintendo DS

# hora de descobrir a plataforma campea de vendas
resumo_nintendo10 <- df_nintendo10 %>% group_by(Platform) %>% summarise(
  Vendas_Milhoes = sum(EU_Sales),
  Vendas_QTD = n())
resumo_nintendo10

c <-
  resumo_nintendo10 %>%
  ggplot(aes(x=Platform, y=Vendas_Milhoes)) +
  geom_bar(stat='identity', mapping=aes(fill=Platform)) +
  geom_label(mapping = aes(label=Vendas_Milhoes), fill = "#006400", size = 4, color = "white", fontface = "bold") +
  xlab('Plataformas') + ylab('Vendas (em milhões) da Nintendo por Plataforma') + ggtitle("Vendas da Nintendo na Europa no Últimos 10 Anos por Plataforma")

d <-
  resumo_nintendo10 %>%
  ggplot(aes(x=Platform, y=Vendas_QTD)) +
  geom_bar(stat='identity', mapping=aes(fill=Platform)) +
  geom_label(mapping = aes(label=Vendas_QTD), fill = "#006400", size = 4, color = "white", fontface = "bold") +
  xlab('Plataformas') + ylab('Qtd de Jogos Vendidos por Plataforma') + ggtitle("Qtd Vendidos nas Plataformas Nintendo na Europa nos Últimos 10 Anos")
plot_grid(c, d, nrow=1, n_col=2)


# Como mencionado anteriormente, a plataforma DS foi a plataforma que mais vendeu jogos nos últimos 10 anos
# porém, ainda sim, nao foi a plataforma lider em questão monetaria. A principio, seguiria apenas com a plataforma DS por sua facilidade de venda.
# Mas, vou optar por estudar a plataforma Wii e Ds para um melhor entendimento sobre as duas e para decidir qual sera a plataforma mais indicada para se investir
# quando o assunto e aumentar a relevancia da empresa na Europa.

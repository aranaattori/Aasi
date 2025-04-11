#GAM: changes in activity of bats in different habitats

library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(ggridges)
library(ggpubr)
library(RColorBrewer)
library(padr)
library(scales)
library(mgcv)
library(suncalc)
library(jtools)
library(ggeffects)
library(broom)
library(tidymv)
library(tidygam)
library(lme4)
library(gratia)
library(ggthemes)
library(marginaleffects)
library(flextable)
library(modelsummary)
library(systemfonts)
library(flexOR)

df_full <- read.csv("data.csv", sep=";")
df_full$date <- as.Date(df_full$date, "%d.%m.%Y")
df_full<-(df_full[df_full$date >= "2023-08-28" & df_full$date <= "2024-05-03", ])

####1. ANALYYSI####
df1<-df_full
df1<- df1 %>% 
  dplyr::select(date,Eptnil,Myosp,Pipnat,Site,Type,temperature)
df1$Eptnil <-as.numeric(df1$Eptnil)
df1$Myosp <-as.numeric(df1$Myosp)
df1$Pipnat <-as.numeric(df1$Pipnat)
df1$date <-as.numeric(df1$date)
df1$Type <-as.factor(df1$Type)
df1$Site <-as.factor(df1$Site)
df1<-na.omit(df1)

####1.1. Eptnil####

gamenil1<- gam(Eptnil ~ s(date, by=Type, k=18) +
                 Type + temperature +
                 s(Site, bs = "re"), 
               data = df1, 
               method = "REML", family = nb())

summary(gamenil1)


enil <- plot_predictions(gamenil1, 
                         condition = c('date',"Type"),
                         type = 'response') + 
  theme_nice() +
  ggplot2::xlab("Date") +
  ggplot2::ylab("Minutes with observations")+
  ggtitle("Eptesicus nilssonii") +
  scale_x_continuous(breaks = c(19601, 19631, 19662, 19692, 19723,19754, 19783,19814,19844))

####1.2. Myosp####

gammyo1 <- gam(Myosp ~ s(date, by=Type, k=18) + 
                 temperature + 
                 Type + 
                 s(Site,bs="re"), 
               data = df1, 
               method = "REML",
               family=nb())

summary(gamenil1)

myosp <- plot_predictions(gammyo1, condition = c('date',"Type"),
                          type = 'response') + 
  ggplot2::xlab("Date") +
  ggplot2::ylab("Minutes with observations")+
  ggtitle("Myotis sp.") +
  theme_nice()  +
  scale_x_continuous(breaks = c(19601, 19631, 19662, 19692, 19723,19754, 19783,19814,19844))


####1.3. Pipnat####

gampnat1 <- gam(Pipnat ~ s(date, by=Type, k=18) + 
                  temperature + 
                  Type +
                  s(Site,bs="re"),
                data = df1, 
                method = "REML",
                family=nb())

pnat <- plot_predictions(gampnat1, condition = c('date',"Type"),
                         type = 'response') + ggplot2::theme_minimal() +
  ggplot2::xlab("Date") +
  ggplot2::ylab("Minutes with observations")+
  ggtitle("Pipistrellus nathusii") +
  theme_nice()  +
  scale_x_continuous(breaks = c(19601, 19631, 19662, 19692, 19723,19754, 19783,19814,19844))+
  coord_cartesian(ylim=c(0,1.25))

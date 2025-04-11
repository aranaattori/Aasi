#lämpötilan vaikutus aktiivisuuteen
#logistinen regressio, poistetaan paikoilta lajit joita ei havaittu lainkaan koska ne ei tn talvehdi siellä

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
library(patchwork)
library(segmented)
library(lubridate)
library(broom.mixed)
library(tidyverse)
library(sjPlot)
library(MuMIn)
library(suncalc)
library(interactions)
library(pROC)

####fulldata.csv####
df_full <- read.csv("data.csv", sep=";")
df_full$date <- as.Date(df_full$date, "%d.%m.%Y")
df_full<- df_full %>% 
  dplyr::select(date,Eptnil,Myosp,Pipnat,Site,Type,temperature)
df_full$Eptnil <-as.numeric(df_full$Eptnil)
df_full$Myosp <-as.numeric(df_full$Myosp)
df_full$Pipnat <-as.numeric(df_full$Pipnat)
df_full$Type <-as.factor(df_full$Type)
df_full$Site <-as.factor(df_full$Site)
df_full<-na.omit(df_full)

df_full$Eptnil <- ifelse(df_full$Eptnil == 0, 0, 1)
df_full$Myosp <- ifelse(df_full$Myosp == 0, 0, 1)
df_full$Pipnat <- ifelse(df_full$Pipnat == 0, 0, 1)
df_full<-(df_full[df_full$date >= "2023-11-01" & df_full$date <= "2024-03-29", ])

#remove species from sites they were not active mid-winter
df1 <- df_full %>%
  pivot_longer(cols=Eptnil:Pipnat,
               names_to="Sp")

df1 <- filter(df1, Site != "Bjornhuvud") #none
df1 <- df1[!(df1$Site == "Batmansberget" & df1$Sp == "Myosp"),]
df1 <- df1[!(df1$Site == "Bistorpbergen" & df1$Sp == "Pipnat"),]
df1 <- df1[!(df1$Site == "Borgberg" & df1$Sp == "Myosp"),]
df1 <- df1[!(df1$Site == "Getabergen" & df1$Sp == "Myosp"),]
df1 <- df1[!(df1$Site == "Getabergen" & df1$Sp == "Pipnat"),]
df1 <- df1[!(df1$Site == "harmala" & df1$Sp == "Myosp"),]
df1 <- df1[!(df1$Site == "harmala" & df1$Sp == "Pipnat"),]
df1 <- df1[!(df1$Site == "Herroskatan" & df1$Sp == "Pipnat"),]
df1 <- df1[!(df1$Site == "Husklint" & df1$Sp == "Myosp"),]
df1 <- df1[!(df1$Site == "Husklint" & df1$Sp == "Pipnat"),]
df1 <- df1[!(df1$Site == "Kallavuori" & df1$Sp == "Myosp"),]
df1 <- df1[!(df1$Site == "Kallavuori" & df1$Sp == "Pipnat"),]
df1 <- df1[!(df1$Site == "Karhuvuori" & df1$Sp == "Myosp"),]
df1 <- df1[!(df1$Site == "Karhuvuori" & df1$Sp == "Pipnat"),]
df1 <- df1[!(df1$Site == "Kasberg" & df1$Sp == "Myosp"),]
df1 <- df1[!(df1$Site == "Kasberg" & df1$Sp == "Pipnat"),]
df1 <- df1[!(df1$Site == "Kilovuori" & df1$Sp == "Myosp"),]
df1 <- df1[!(df1$Site == "Kloddberget" & df1$Sp == "Pipnat"),]
df1 <- df1[!(df1$Site == "Knutsbodaberget" & df1$Sp == "Myosp"),]
df1 <- df1[!(df1$Site == "Langbergen" & df1$Sp == "Myosp"),]
df1 <- df1[!(df1$Site == "Langbergen" & df1$Sp == "Pipnat"),]
df1 <- df1[!(df1$Site == "lepainen" & df1$Sp == "Myosp"),]
df1 <- df1[!(df1$Site == "lepainen" & df1$Sp == "Pipnat"),]
df1 <- df1[!(df1$Site == "Solbacka" & df1$Sp == "Myosp"),]
df1 <- df1[!(df1$Site == "Solbacka" & df1$Sp == "Pipnat"),]
df1 <- df1[!(df1$Site == "Tummamaki" & df1$Sp == "Myosp"),]
df1 <- df1[!(df1$Site == "Tummamaki" & df1$Sp == "Pipnat"),]
df1 <- df1[!(df1$Site == "Vargberget" & df1$Sp == "Myosp"),]
df1$date <-as.numeric(df1$date)
df1$date<-rescale(df1$date, to = c(1, 77.5))
df1<-na.omit(df1)

#full model
m1 <-lme4::glmer(value ~ temperature + Sp + temperature*Sp + date + date*Sp + (1 | Site),
           data = df1,
           family = binomial(link="logit"),na.action='na.fail')
summary(m1)

#model selection
m1.selection <- dredge(m1)
nrow(m1.selection)
m1.selection[1:5,]

#best model
m.best <- get.models(m1.selection, delta==0)[[1]]
summary(m.best) #value ~ date + Sp + temperature + (1 | Site) + Sp:temperature

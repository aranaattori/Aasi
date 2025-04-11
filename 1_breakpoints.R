#Breakpoints

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

df1 <- read.csv("data.csv", sep=";")
df1<- df1 %>% 
  dplyr::select(date,Eptnil,Pipnat,Myosp,Type,Site,temperature)
df1<-na.omit(df1)
df1$date <-as.Date(df1$date, "%d.%m.%Y")
df1<-(df1[df1$date >= "2023-08-28" & df1$date <= "2024-05-03", ])

####BP-ANALYSES FOR AUTUMN AND SPRING####
df.autumn <- print(df1[df1$date >= "2023-08-28" & df1$date <= "2023-12-31", ])
df.autumn$date <- as.numeric(df.autumn$date)
df.spring <- print(df1[df1$date >= "2024-01-01" & df1$date <= "2024-05-03", ])
df.spring$date <- as.numeric(df.spring$date)

#####EPTNIL#####
#AUTUMN
autumn.eptnil.date <- glm(Eptnil ~ date, data=df.autumn) #glm
summary(autumn.eptnil.date)

davies.test(autumn.eptnil.date, 
            seg.Z = ~ date)

my.seg <- segmented(autumn.eptnil.date, 
                    seg.Z = ~ date, 
                    psi = list(date = c(19639)))
summary(my.seg)
my.seg$psi # 19632.64

autumn.eptnil.date.formatted <- as_date(c(19633), origin = lubridate::origin)
autumn.eptnil.date.formatted <- format(autumn.eptnil.date.formatted, "%d-%m-%y") 

#SPRING
spring.eptnil.date <- glm(Eptnil ~ date, data=df.spring) #glm
summary(spring.eptnil.date)

davies.test(spring.eptnil.date, 
            seg.Z = ~ date)

my.seg <- segmented(spring.eptnil.date, 
                    seg.Z = ~ date, 
                    psi = list(date = c(19831)))
summary(my.seg)
my.seg$psi # breakpoint keväällä 19837 

spring.eptnil.date.formatted <- as_date(c(19837), origin = lubridate::origin)
spring.eptnil.date.formatted <- format(spring.eptnil.date.formatted, "%d-%m-%y") #24-04-24

#####MYOSP#####
#AUTUMN
autumn.myosp.date <- glm(Myosp ~ date, data=df.autumn) #glm
summary(autumn.myosp.date)

davies.test(autumn.myosp.date, 
            seg.Z = ~ date)

my.seg <- segmented(autumn.myosp.date, 
                    seg.Z = ~ date, 
                    psi = list(date = c(19625)))
summary(my.seg)
my.seg$psi # breakpoint syksyllä 19621

autumn.myosp.date.formatted <- as_date(c(19621), origin = lubridate::origin)
autumn.myosp.date.formatted <- format(autumn.myosp.date.formatted, "%d-%m-%y")

#SPRING: april 24
spring.myosp.date <- glm(Myosp ~ date, data=df.spring) #glm
summary(spring.myosp.date)

davies.test(spring.myosp.date, 
            seg.Z = ~ date)

my.seg.myo.spring <- segmented(spring.myosp.date, 
                               seg.Z = ~ date, 
                               psi = list(date = c(19831)))
summary(my.seg.myo.spring)
my.seg.myo.spring$psi

spring.myosp.date.formatted <- as_date(c(19836), origin = lubridate::origin)
spring.myosp.date.formatted <- format(spring.myosp.date.formatted, "%d-%m-%y") #24-04-24
#####PIPNAT####
#Autumn
autumn.pipnat.date <- glm(Pipnat ~ date, data=df.autumn) #glm
summary(autumn.pipnat.date)

davies.test(autumn.pipnat.date, 
            seg.Z = ~ date)

my.seg <- segmented(autumn.pipnat.date, 
                    seg.Z = ~ date, 
                    psi = list(date = c(19639)))
summary(my.seg) 
my.seg$psi # 19643.41

autumn.pipnat.date.formatted <- as_date(c(19643), origin = lubridate::origin)
autumn.pipnat.date.formatted <- format(autumn.pipnat.date.formatted, "%d-%m-%y") #13-10-23

#Spring: no breakpoint
spring.pipnat.date <- glm(Pipnat ~ date, data=df.spring) #glm
summary(spring.pipnat.date)

davies.test(spring.pipnat.date, 
            seg.Z = ~ date) # p = 0.3493, no break point

####WHOLE SEASON####
####EPTNIL:19634-19832/"04-10-23"-"19-04-24"####
eptnil.date <- glm(Eptnil ~ date, data=df1)
summary(eptnil.date)

davies.test(eptnil.date, 
            seg.Z = ~ date)

my.seg <- segmented(eptnil.date, 
                    seg.Z = ~ date, 
                    psi = list(date = c(19651,19836)))
summary(my.seg)
my.seg$psi # 19634.55, 19832.00

eptnil.date.format <- as_date(c(19635,19832), origin = lubridate::origin)
eptnil.date.date <- format(eptnil.date.format, "%d-%m-%y")
eptnil.date.date

#FIGURE
my.fitted <- fitted(my.seg)
my.model <- data.frame(date = df1$date, Eptnil_e = my.fitted)

ggplot(my.model, aes(x = date, y = Eptnil_e)) + 
  geom_point(data=df1, aes(y=Eptnil, x=date),
             colour="orange", alpha=0.5)+
  geom_vline(xintercept = as.numeric(as.Date("2023-10-04")),
             linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2024-04-19")),
             linetype="dashed") +
  geom_line() + 
  theme(axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"))+
  theme_nice() +
  labs(x="Date", y="Minutes with Enil/night")+
  ggtitle("Eptnil activity, breakpoints 04.10.2023 & 19.04.2024")

####MYOSP: 19622-19832/"22-09-23"-"19-04-24"####
####PIPNAT: 19642/"12-10-23"->####
pipnat.date <- glm(Pipnat ~ date, data=df1) #glm
summary(pipnat.date)

davies.test(pipnat.date, 
            seg.Z = ~ date)
my.seg <- segmented(pipnat.date, 
                    seg.Z = ~ date, 
                    psi = list(date = c(19651)))
summary(my.seg)
my.seg$psi #19642.82 

autumn.pipnat.date.formatted <- as_date(c(19643), origin = lubridate::origin)
autumn.pipnat.date.formatted <- format(autumn.pipnat.date.formatted, "%d-%m-%y")
autumn.pipnat.date.formatted

#FIGURE
my.fitted <- fitted(my.seg)
my.model <- data.frame(date = df1$date, Pipnat_e = my.fitted)

ggplot(my.model, aes(x = date, y = Pipnat_e)) + 
  geom_point(data=df1, aes(y=Pipnat, x=date),
             colour="gold", alpha=0.5)+
  geom_vline(xintercept = as.numeric(as.Date("2023-10-12")),
             linetype="dashed") +
  geom_line() + 
  theme(axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"))+
  theme_nice() +
  labs(x="Date", y="Minutes with Pipnat/night")+
  ggtitle("Pipnat activity, breakpoint 12.10.2023")

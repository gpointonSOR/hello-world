rm(list = ls())

setwd("C:\\Users\\grant\\Desktop\\School\\Skateboarding MRT\\")

library(ggplot2)
library(tidyr)
df <- read.csv("Sk_MRT_Data.csv")

#Demographic graphs----
ggplot(df, mapping = aes(x = Sk_totexp, fill = Gender)) +
  geom_histogram(binwidth = 1, position = "dodge") + 
  theme_bw()


ggplot(df, mapping = aes(x = Stance, fill = Gender)) +
  geom_bar(position = "dodge") + 
  theme_bw()

ggplot(df, mapping = aes(x = Comfort, fill = Gender)) +
  geom_bar(position = "dodge") + 
  theme_bw()

ggplot(df, mapping = aes(x = Occupation, fill = Gender)) +
  geom_bar(position = "dodge") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df, mapping = aes(x = Edu, fill = Gender)) +
  geom_bar(position = "dodge") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df, mapping = aes(x = Method, fill = Gender)) +
  geom_bar(position = "dodge") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df, mapping = aes(x = Sk_totexp, y = Trick_rscore, color = Gender)) +
  geom_point() + 
  theme_bw() 

ggplot(df, mapping = aes(x = Sk_start, y = Trick_rscore, color = Gender)) +
  geom_point() + 
  theme_bw() 



#MRT Data ----

#Summarizes 3min scores by gender
describeBy(df$Tscore_tot, group = df$Gender)

#Summarizes 4min scores by gender
describeBy(df$FScore_tot, group = df$Gender)

#Summarizes grade by gender for 3 & 4min
describeBy(df$Tgrade, group = df$Gender)
describeBy(df$Fgrade, group = df$Gender)

#Plot of MRT scores by skate experience
ggplot(data = df, mapping = aes(x = Sk_totexp, y = Tscore_tot, color = Gender)) +
  geom_point() +
  theme_classic()

ggplot(data = df, mapping = aes(x = Sk_totexp, y = FScore_tot, color = Gender)) +
  geom_point() +
  theme_classic()

#Plot of MRT scores by trick image score
ggplot(data = df, mapping = aes(x = Trick_rscore, y = Tscore_tot, color = Gender)) +
  geom_point() +
  theme_classic()

ggplot(data = df, mapping = aes(x = Trick_rscore, y = FScore_tot, color = Gender)) +
  geom_point() +
  theme_classic()

#Plot of MRT scores by vg exp
ggplot(data = df, mapping = aes(x = Vg_totexp, y = FScore_tot, color = Gender)) +
  geom_point() +
  theme_classic()

#Analyses ----
df$cntrl <- df$Ski_totexp + df$Vg_totexp  + df$Mus_totexp + df$other_totexp

m1 <- lm(FScore_tot ~ cntrl, data = df)
plot(m1)
summary(m1)

m2 <- lm(FScore_tot ~ Sk_totexp + cntrl, data = df)
plot(m2)
summary(m2)

m3 <- lm(FScore_tot ~ Trick_rscore + cntrl, data = df)
plot(m3)
summary(m3)

cor.test(df$Sk_totexp, df$FScore_tot)


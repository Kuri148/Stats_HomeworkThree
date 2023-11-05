library(tidyverse)
library(ggplot2)
library(sjPlot)
library(lme4)
library(PerformanceAnalytics)
library(corrplot)
library(car)

Sys.setenv(LANG = "en")

sourceData <- read.csv(file = "D:\\R\\HomeworkThree\\HW3_data.csv", header = T)

hist(sourceData$comprehensibility)

sourceData1 <- sourceData %>%
  mutate(institution = case_when(
    speaker < 700 ~ 0,
    speaker > 699 ~ 1
  )) %>%
  group_by(institution) %>%
  arrange(institution, .locale = "en")

sourceData2 <- sourceData1 %>%
  na.omit() %>%
  select(-c(listener, ns_status))

sourceData3 <- sourceData %>%
  select(-c(listener, ns_status, speaker))

colMeans(sourceData2)

stdev <- apply(sourceData2, 2, sd)
stdev

median <- apply(sourceData2, 2, median)
median
#--------------------------------------------------------------------Assumptions


# (4) normality
hist(sourceData$comprehensibility)
qqPlot(sourceData$comprehensibility)
shapiro.test(sourceData$comprehensibility)

plot(comprehensibility ~ eit, data=sourceData) #Not sure what to do with these
plot(comprehensibility ~ listener, data=sourceData)
plot(comprehensibility ~ accentedness, data=sourceData)

#--------------------------------------------------------------------Correlations

cor(sourceData3, use = "pairwise.complete.obs")
testing = cor(sourceData2)
corrplot(testing, method = 'number')

compInt <- cor.test(sourceData1$comprehensibility, sourceData1$intelligibility)
compInt

compAcc <- cor.test(sourceData1$comprehensibility, sourceData1$accentedness)
compAcc

#-----------------------------------------------------------checking institution

boxplot(comprehensibility ~ institution, data = sourceData1)

(colour_plot <- ggplot(sourceData1, aes(x = accentedness, y = comprehensibility, colour = institution)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(legend.position = "top"))

(split_plot <- ggplot(aes(comprehensibility, accentedness), data = sourceData1) +
    geom_point() +
    facet_wrap(~ institution) + # create a facet for each mountain range
    xlab("comprehensibility") +
    ylab("accentedness"))

#---------------------------------------------------------------regression model

listener.lmer <- lmer(comprehensibility ~ accentedness + eit + (1|listener), data = sourceData1)
summary(listener.lmer)
tab_model(listener.lmer)

#get list of residuals
res <- resid(listener.lmer)

plot(fitted(listener.lmer), res)

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res)

durbinWatsonTest(listener.lmer)

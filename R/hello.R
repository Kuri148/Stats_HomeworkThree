library(tidyverse)
library(ggplot)
library(sjPlot)
library(lme4)
library(PerformanceAnalytics)
library(corrplot)

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

colMeans(sourceData2)

stdev <- apply(sourceData2, 2, sd)
stdev

medi <- apply(RemovePerson, 2, median)
medi
#--------------------------------------------------------------------Assumptions

# , , (3) independence of errors, ,
# and (5) independence of independent variables

# (4) normality
hist(MainData$Comprehensibility)
ggqqplot(MainData$Comprehensibility)
shapiro.test(MainData$Comprehensibility)

cor(sourceData2, use = "pairwise.complete.obs")
testing = cor(sourceData2)
corrplot(testing, method = 'number')

compInt <- cor.test(sourceData1$comprehensibility, sourceData1$intelligibility)
tab_model(compInt)

compSpeechRate <- cor.test(RemovePerson$Comprehensibility, RemovePerson$SpeechRate)
compSpeechRate

compEIT <- cor.test(RemovePerson$Comprehensibility, RemovePerson$EIT)
compEIT

chart.Correlation(sourceData2, histogram=TRUE, pch=19) #This is fucking awesome!
# Intelligibility, Speech Rate, EIT


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

##this graph for listener is also interesting in facet_rap

institution.lmer <- lmer(comprehensibility ~ accentedness + (1|listener), data = sourceData1)
summary(institution.lmer)
tab_model(institution.lmer)

institution.lmer <- lmer(comprehensibility ~ accentedness + eit +  (1|institution), data = sourceData1)
summary(institution.lmer)
tab_model(institution.lmer)

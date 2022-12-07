
library(tidyverse)
library(dplyr)
library(broom)
library(ggpubr)
library(ggplot2)

## Initial analysis
ggplot(forestfires_1_, aes(x=month, y=area)) +geom_point()
ggplot(forestfires_1_, aes(x=month, y=temp)) +geom_point()
ggplot(forestfires_1_, aes(x=month, y=RH)) +geom_point()
hist(forestfires_1_$area)

## run a regression
FF.lm<-lm(area ~ month + FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = forestfires_1_)
summary(FF.lm)

## second regression attempt
FF2.lm<-lm(area ~ month + FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = FF2)
summary(FF2.lm)

## Third regression attempt
FF4.lm<-lm(logarea ~ month + FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = FF4)
summary(FF4.lm)

## Fourth Regression Attempt
FF2.lm<-lm(area ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = FF2)
summary(FF2.lm)

## Fifth Regression Attempt
FF2.lm<-lm(area ~ month + FFMC + DMC + DC + temp + RH + wind + rain, data = FF2)
summary(FF2.lm)

## Sixth Regression Attempt
FF2.lm<-lm(area ~ DC + DMC + FFMC, data = FF2)
summary(FF2.lm)

## attempt at data reorganizing 
firesbymonth = forestfires_1_ %>% group_by(month)
firesbymonth2 <- forestfires_1_ %>% count(month)
summarize(firesbymonth)

## Create vector with lowercase month abreviations
UMabb <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

##Create dataframe with avg high temp data.
high <- c(47, 51, 57, 60, 67, 76, 84, 83, 75, 64, 54, 48)
Hightemp <- data.frame(UMabb, high) %>% rename(month = UMabb) %>%
  mutate(month = factor(month, levels = UMabb[UMabb %in% unique(month)]))



## Recode data with months as numbers
FF3 <- forestfires_1_ %>% mutate(month = match(month, UMabb))

## Bar Graph showing number of fires per month CHRONOLOGICALLY with numerical months
ggplot(FF3, aes(x=month)) +geom_bar()


## Recode data with months as abbreviations that plot chronologically
FF2 <- forestfires_1_ %>%
  mutate(month = factor(month, levels = UMabb[UMabb %in% unique(month)]))


## Add avg temp data and log of area
FF4 <- left_join(FF2, Hightemp) %>%
  mutate(month = factor(month, levels = UMabb[UMabb %in% unique(month)])) %>% mutate(logarea =log(area +1))

ggplot(FF4, aes(x=month)) + geom_bar()

ggplot(FF4, aes(x=month)) + geom_bar() + geom_point(aes(y=high), color = "red", size = 6) 


## Plot graph with month vs wind
ggplot(FF4, aes(x=month)) + geom_boxplot(aes(y=wind))


## Plot graphs of area vs other variables.
ggplot(FF4, aes(y=area)) + geom_point(aes(x=wind), bins=100) + ylim(0,300)

ggplot(FF4, aes(y=area)) + geom_point(aes(x=RH))

ggplot(FF4, aes(y=area)) + geom_point(aes(x=DC))


## Plot histograms of some variables 

FF4 %>% ggplot(aes(x=wind)) + geom_histogram(binwidth = .1)

FF4 %>% ggplot(aes(x=FFMC)) + geom_histogram(binwidth = 1)

FF4 %>% ggplot(aes(x=DC)) + geom_histogram(binwidth = 10)

FF4 %>% ggplot(aes(x=DMC)) + geom_histogram(binwidth = 1)

FF4 %>% ggplot(aes(x=area)) + geom_histogram(binwidth = 100)

FF4 %>% ggplot(aes(x=ISI)) + geom_histogram() + xlim(0, 30)

FF4 %>% ggplot(aes(x=temp)) + geom_histogram()

FF4 %>% ggplot(aes(x=RH)) + geom_histogram()


## Area histogram vs Log(area + 1) histogram

FF4 %>% ggplot(aes(x=area)) + geom_histogram()

FF4 %>% ggplot(aes(x=logarea)) + geom_histogram()


## Create new data frame grouped by month with average size of fires

FM1 <- data.frame(FF4 %>% group_by(month) %>% summarize(mean(area)))

ggplot(data=FM1, aes(x=month, y=mean.area.)) + geom_bar(stat="identity")


## Bar plot showing avg high temp by month

ggplot(Hightemp, aes(x=month)) + geom_bar(aes(y=high),stat = 'identity')


## Bar Graph showing number of fires per month CHRONOLOGICALLY
ggplot(FF2, aes(x=month)) +geom_bar()

ggplot(firesbymonth2, aes(x=month)) + geom_bar()

## Bar Graph showing number of fires per month (not chronological)
ggplot(forestfires_1_, aes(x=month)) +geom_bar()




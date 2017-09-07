---
title: "Multilevel Modeling in R"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
In this example, I am documenting how to formulate equations for random intercepts and slopes in multilevel modeling along with the r code using the nlme package with interpretation of the output.  In this current data set there are four variables id, school, sex (i.e. gender), and like (i.e. a measure on a scale 1 to 8 how much the student likes school).  Here I am just tidying up the data by making the f and m 1 and 0 respectively as well as getting rid of nonsensical values such as 19.1 for the sex variable.  Overall there are 1,380 participants.  In this demonstration we will want to predict how much a student likes schools with the covariate sex while accounting for the nesting of students within school and make the model more complicated further into the example.  In later models, we will use the level two covariate school funding.
```{r}
setwd("~/Desktop")
dat = read.csv("SkillsDataSet.csv", header = TRUE)
dat = dat[c("id", "school", "sex", "like")]
dat = as.data.frame(apply(dat, 2, function(x){ifelse(x == "f", 1, ifelse(x == "m", 0, ifelse(x == 19.1, NA, x)))}))
dat = as.data.frame(na.omit(dat))
dat = write.csv(dat, "dat.csv",row.names = FALSE)
dat = read.csv("dat.csv", header = TRUE)
dat = as.data.frame(dat[-c(1:3),])
dat$id = 1:length(dat$like)
dat$school = rep(1:30, each =  46)
set.seed(12345)
dat$sf = round(rep(rnorm(46,0, 20), each  = 30),0)
head(dat)
datGraph = subset(dat, school <=5, select = c(school, like, sex))

```
Graphing
```{r}
library(lattice)
# Looks at the distribution of data for level for a variable.
dotplot(school ~ like, data = datGraph, jitter.y = TRUE, ylab = "school", main= "Dotplot of ID's over Like")

# Look at the distribution of data at each level for all 
xyplot(like ~ sex | school,strip = strip.custom(strip.names = FALSE, strip.levels = c(FALSE, TRUE)),
 data = datGraph)

library(nlme)
model1 = lme(fixed = like ~ sex, random = ~1 | school, data = dat)
sumModel1 = summary(model1); sumModel1

hist(scale(resid(model1)))
qqnorm(scale(resid(model1)))
qqline(scale(resid(model1)))
```

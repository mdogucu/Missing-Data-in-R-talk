## Mine Dogucu & Sunwoo Ha
## Handling Missing Data in R

## Reading the datasets
comp<-read.csv("complete.csv")
miss<-read.csv("missing.csv")

## Missing Data Diagnostics
library(ggplot2)
library(naniar)

ggplot(data = miss,
       aes(x = age,
           y = distance)) +
  geom_miss_point()

miss_case_summary(miss)


library(BaylorEdPsych)
library(mvnmle)
LittleMCAR(miss)

## Sample Statistics

coef(lm(distance~age,data=comp))

## Complete Case Analysis

coef(lm(distance~age,data=miss))

## Mean Imputation

### Creating mean imputed dataset
miss_meanimp<-miss
miss_meanimp[is.na(miss)]<-mean(miss$distance,na.rm = TRUE)

### Fitting the model to mean imputed dataset


coef(lm(distance~age,data=miss_meanimp))



## Multiple Imputation
library(mice)
temp<-mice(data=miss, m=3, seed=12345)
temp$imp

m1<-complete(temp,1)
m2<-complete(temp,2)
m3<-complete(temp,3)

mimodel<-with(temp,lm(distance~age)) 
summary(pool(mimodel)) 


## Maximum Likelihood in R
library(stats4)
mle()
library(lavaan)
sem(model,data,missing='fiml')
library(stats)
glm(model, family=poisson)

## Additional MI packages

library(mice)
library (mi)
library(Amelia)
library(missForest)
library(Hmisc)
library(countimp)



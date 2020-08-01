# MedicalCosts
# Insurance Data Analysis
---
title: "Insurance Data Analysis"
author: "Rebecca Jun"
date: "Spring 2020"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Introduction
  The purpose of this data project is to explore variables that may influence medical costs. This data analysis will be conducted by using an insurance data set from [Kaggle](www.kaggle.com/mirichoi0218/insurance). Some questions that will be explored include: 1.) How are variables like smoking status, age, gender, body mass index, and one’s location in the United States associated with health care costs? 2.) Which explanatory variable has the most impact on medical costs? 3.) Can health care costs be predicted given these variables? In order to answer these questions, a multiple linear regression and logistic regression will be conducted along with additional data analysis methods.
```{r}
insurance <- read.csv("insurance.csv")
summary(insurance)
```
## Data Description
  The data used in this analysis is the Kaggle Medical Cost Data Set. This data set contains 1,338 observations of beneficiaries enrolled in an insurance plan. There are 6 explanatory variables: age, sex (male or female), BMI, number of children or dependents, smoking status (yes or no), and US region (northeast, southeast, southwest, or northwest). Age, BMI, and number of children are continuous explanatory variables. Sex, smoking status, and US region are categorical explanatory variables. The explained or response variable is charges, i.e. individual medical costs billed by health insurance in one year, and it is measured in US dollars. 
  
### Data Preparation.
  By using the original variables of the data set, four additional variables were created in order to gain a better understanding of the data set; bmi_status, age_class, charges_class, and smoker_binary were created. The variable bmi_status was created by converting bmi into a categorical variable that consists of underweight, healthy, overweight, and obese according to the criteria in Table 1. The variable age_class was created by converting age into a categorical variable consisting of young_adult (18-34), adult (35-50), and senior (>=51). 

#### BMI Status  
| **Body Mass Index (BMI)** | **Weight Status** |
| -------------| ------------- |
|   < 18.5     |  Underweight  |
|  18.5 - 24.9 |    Healthy    |
| 25.0 - 29.9  |  Overweight   |
|   > 30.0     |     Obese     |

```{r, include=FALSE}
insurance$bmi_status[insurance$bmi < 18.5] <- "underweight" 
insurance$bmi_status[insurance$bmi >= 18.5 & insurance$bmi <= 24.9999] <- "healthy" 
insurance$bmi_status[insurance$bmi >= 25.0 & insurance$bmi <= 29.9999] <- "overweight"
insurance$bmi_status[insurance$bmi >= 30] <- "obese" 

insurance$bmi_status <- factor(insurance$bmi_status, order = TRUE, 
                                  levels <- c("underweight", "healthy", "overweight", "obese"))
```

  Furthermore, charges_class was created by converting charges into a categorical variable consisting of low (charges less than mean value $13,270) and high (charges greater than mean). The variable smoker_binary was created by converting smoker into a binary variable, where being a smoker equals 1 and 0 otherwise; smoker_binary will be utilized in the logistic regression section. 
  
 #### Age Class
```{r, include=FALSE}
insurance$age_class[insurance$age >= 18 & insurance$age <= 34] <- "young_adult"
insurance$age_class[insurance$age >= 35 & insurance$age <= 50] <- "adult"
insurance$age_class[insurance$age >= 51] <- "senior"

insurance$age_class <- factor(insurance$age_class, order = TRUE, 
                              levels <- c("young_adult", "adult", "senior"))
```

## Including Plots

You can also embed plots, for example:

```{r, echo=FALSE}
x <- insurance$charges
h<-hist(x, breaks=10, col="pink", xlab="Charges in $USD",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
```

```{r, echo=FALSE}
par(mar=c(4.1,4.1,1,1))
plot(insurance$age, insurance$charges, xlab = "Age", ylab = "Charges in $USD")
bmi_obese <- which(insurance$bmi >= 30)
points(insurance$age[bmi_obese], insurance$charges[bmi_obese], pch=16, col = "pink")

bmi_overweight <- which(insurance$bmi >= 25 & insurance$bmi <= 29.9999)
points(insurance$age[bmi_overweight], insurance$charges[bmi_overweight], pch=16, 
       col = rgb(0.1,0.1,0.5,0.2))
bmi_healthy <- which(insurance$bmi >= 18.5 & insurance$bmi <= 24.9999)
points(insurance$age[bmi_healthy], insurance$charges[bmi_healthy], pch=16,
       col = "blue")
bmi_underweight <- which(insurance$bmi < 18.5)
points(insurance$age[bmi_underweight], insurance$charges[bmi_underweight], pch=16,
       col = "green")
legend("topright", legend = c("Obese", "Overweight", "Healthy", "Underweight"), 
       col = c("pink", rgb(0.1,0.1,0.5,0.2), "blue", "green"), pch=16, cex=0.9)
```

```{r}
cor(insurance$bmi, insurance$charges)
```

```{r}
par(mar=c(4.1,4.1,1,1))
plot(insurance$bmi, insurance$charges, xlab = "BMI", ylab = "Charges in $USD")
smoker_yes <- which(insurance$smoker == "yes")
points(insurance$bmi[smoker_yes], insurance$charges[smoker_yes], pch = 16, col = "green")
smoker_no <- which(insurance$smoker == "no")
points(insurance$bmi[smoker_no], insurance$charges[smoker_no], pch = 16, col = rgb(0.1,0.1,0.5,0.2))
legend("topright", legend = c("smoker - yes", "smoker - no"), col = c("green", rgb(0.1,0.1,0.5,0.2)), 
       pch=16, cex = 0.9)
```

```{r, echo=FALSE}
par(mfrow=c(2,3))
## age vs. charges
boxplot(charges~age_class, data=insurance, col=c("deepskyblue", "darkolivegreen", "cadetblue"),
        xlab="Age Group", ylab="Charges in $USD", 
        main="Boxplot: Age and Charges")
## sex vs charges
boxplot(charges~sex, data=insurance, col=c("pink","blue"),
        xlab = "Sex", ylab = "Charges in $USD", 
        main="Boxplot: Sex and Charges")
## bmi vs charges
boxplot(charges~bmi_status, data=insurance, col=c("coral","cornflowerblue","darkorchid","darksalmon"),
        xlab="BMI Status", ylab="Charges in $USD",
        main="Boxplot: BMI Status and Charges")
## children vs. charges 
boxplot(charges~children, data=insurance, 
        col=c("green","lightblue","honeydew","pink","lightslategray","mistyrose"), 
        xlab = "Num. of Child/Dep.", ylab = "Charges in $USD",
        main="Boxplot: Num. of Children and Charges")
## smoker status vs charges
boxplot(charges~smoker, data=insurance, col = c("blue", "red"), 
        xlab = "Smoking Status", ylab = "Charges in $USD",
        main="Boxplot: Smoking Status and Charges")
## region vs charges
boxplot(charges~region, data=insurance, col = c("blue", "red", "green", "pink"),
        xlab = "Region", ylab = "Charges in $USD",
        main="Boxplot: Region and Charges")
```

```{r}
cor(insurance[c("age", "bmi", "children", "charges")])
pairs(insurance[c("age", "bmi", "children", "charges")])
```

## Principal Component Analysis
```{r, echo=FALSE}
insurance$charges_class <- ifelse(insurance$charges > mean(insurance$charges), "high", "low")
insurance$smoker_binary[insurance$smoker == "yes"] <- 1
insurance$smoker_binary[insurance$smoker == "no"] <- 0
#install.packages("factoextra")
library("factoextra")
data.psa <- insurance[,c(1,3,4,11)]
pc.insurance <- prcomp(data.psa, scale.=TRUE)

fviz_pca_biplot(pc.insurance, col.ind = insurance$charges_class, col = "black",
                geom = "point", addEllipses = TRUE, legend.title = "Charges")
summary(pc.insurance)
plot(pc.insurance, type="l")
```

## Regression Analysis
```{r}
reg <- lm(charges~age+bmi+children+sex+smoker+region, data = insurance)
summary(reg)

reg1 <- lm(charges~age+bmi+children+smoker+region, data = insurance)
summary(reg1)
```

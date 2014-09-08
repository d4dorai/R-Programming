---
title: "Motor Trend"
author: "DS"
date: "Saturday, August 23, 2014"
output: pdf_document
---

##Executive Summary
(1) Is an automatic or manual transmission better for MPG? A Manual transmission is not necessarily better for MPG!

(2) Quantifying the MPG difference between automatic and manual transmissions: A Manual transmission is associated with 1.8092 more Miles/gallon than an automatic transmission. However this difference is not statistically significant, therefore the null hypothesis of 0 difference between transmission types cannot be rejected.


###Exploratory Analysis

```{r}
data(mtcars)
attach(mtcars)
am <- factor(am,labels=c('Automatic','Manual'))
```

```{r}
summary(mpg)
summary(am)
summary(mpg[am == "Automatic"]) 
summary(mpg[am == "Manual"])
```


###Linear Regression Models

For estimating a linear regression of mpg on am, I need to determine the model with the best covariates. For this purpose I perform a stepwise model selection using backwards elimination. 

```{r}
fit_all <- lm(mpg ~ as.factor(cyl) + as.factor(vs) + as.factor(am) + as.factor(gear) + as.factor(carb) + disp + hp + drat + wt + qsec, data = mtcars)

fit_all
```
```{r, results='hide'}
fit_best <- step(fit_all, direction = "backward")
```

```{r}
summary(fit_best)
```

###Quantify the difference

The best fit model regresses MPG on number of cylinders (cyl6 and cyl8), transmission (am), gross horsepower (hp) and weight(wt). In this model am has a coefficient of 1.8092, which represents the average difference in MPG between manual and automatic transmission. This difference is however not statistically significant (p-value >> 0.1) and therefore the null hypothesis of 0 difference cannot be rejected. For further diagnostics see the residual plots in the appendix. 

Below, I also run a t-test on the reduced model of mpg regressed just on am. This model  finds that manual tranmission had a statsitically significant (5% level) higher MPG than automatic transmission. As this is a reduced model it is likely to not be correct. 

```{r}
t.test(mpg~am)
```



###Appendix

```{r, echo = FALSE, fig.height= 4}
boxplot(mpg ~ am, ylab = "Miles per Gallon")
```
```{r, echo = FALSE}
par(mfrow=c(2, 2))
plot(fit_best)
```

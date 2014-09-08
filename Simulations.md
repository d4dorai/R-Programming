---
title: "Simulations"
author: "DS"
date: "Saturday, August 23, 2014"
output: pdf_document
---

The simulations:

```{r}
lambda <- 0.2 
n <- 40 
nrsim <- 1000 
v <- rep(0,nrsim) 

for (i in 1:nrsim){
  y <- rexp(n, lambda) 
  mn <- mean(y)
  v[i] <- mn
}
```

Q1. Show where the distribution is centered at and compare it to the theoretical center of the distribution.

```{r}
mean_sample <- mean(v)
mean_sample #sample mean
1/lambda  #real distribution mean
```



2. Show how variable it is and compare it to the theoretical variance of the distribution.

```{r}
variance_sample <- var(v)
variance_sample #sample variance
(1/lambda)^2/nrsim  #real distribution variance
```

3. Show that the distribution is approximately normal.

```{r}
v_normal <- rnorm(1000, mean = 1/lambda, sd = sqrt((1/lambda)^2/nrsim) )
vec1 <- data.frame(x = v, grp="Sample")
vec2 <- data.frame(x = v_normal, grp="Normal")
df <- rbind(vec1, vec2)
library(ggplot2)
g <- ggplot(df, aes(x, colour=grp)) + geom_density()
g


```
The central limit theorem states that given a population with a finite mean mu and a finite non-zero variance sigma2, the sampling distribution of the mean approaches a normal distribution with a mean of mu and a variance of sigma2/N as N, the sample size, increases.

4. Evaluate the coverage of the confidence interval for 1/lambda.

```{r}
ci_1lambda <- mean_sample +c(-1, 1)* qt(0.975,nrsim-1)* sd(v)/sqrt(nrsim)
ci_1lambda
```

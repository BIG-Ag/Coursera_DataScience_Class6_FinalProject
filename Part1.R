## This is the analysis for Coursera DataScience Class6 Final Project
## Charles 08/02/2017

## Set working directory
setwd("C:/Study/Coursera/1 Data-Science/2 RStudio/6 Class 6/Coursera_DataScience_Class6_FinalProject")
set.seed(135246987)
library(ggplot2)
library(gridExtra)
library(grid)

## Example
hist(runif(1000))
mns = NULL
for (i in 1 : 1000) mns = c(mns, mean(runif(40)))
hist(mns)

## Set parameter
n = 40
lambda = 0.2
numSample = 1000

## Make transfer
dataExp <- matrix( rexp(n*numSample,lambda), nrow = numSample)
dataMean <- data.frame(Mean = apply(dataExp,1,mean)) 
names(dataMean) <- c("Mean")

##Compare results
mu <- mean(dataExp)
sd <- sd(dataExp)/sqrt(n)
1/lambda/sqrt(n)

## Plot
g <- ggplot(data = dataMean, aes(x=Mean)) +
        geom_histogram(binwidth = 0.2, aes(y=..density..), alpha=0.3) +
        geom_vline(xintercept = mean(dataExp), size = 1, color = "blue") +
        geom_density(color="blue",size=1) +
        stat_function(fun = dnorm, args = list(mean=mu, sd=sd), color="red",size=1) +
        geom_vline(xintercept = 1/lambda, color="red",size=1) +
        ggtitle("Density Function of Sample distribution and normal distribution") +
        xlab("Value of sample mean") +
        ylab("Density")
plot(g)

## Part2
library(datasets)
data("ToothGrowth")
dataTG <- ToothGrowth
str(dataTG)
head(dataTG)
summary(dataTG)
dataTG$dose <- as.factor(dataTG$dose)

mean(subset(dataTG,supp=="OJ")$len)
mean(subset(dataTG,supp=="VC")$len)

# Plot len v.s. dose
g1 <- ggplot(data = dataTG, aes(x=dose, y=len)) +
        geom_boxplot(aes(fill = dose))
g2 <- ggplot(data = dataTG, aes(x=supp ,y=len)) +
        geom_boxplot(aes(fill = supp))
grid.arrange(g1,g2,nrow=1)

# Test len ~ supp
t.test(len ~ supp, data = dataTG, alternative ="two.sided")

t.test(subset(dataTG, supp == 'OJ')$len, subset(dataTG, supp == 'VC')$len,
       alternative ="two.sided")

# Not working like this
difference = subset(dataTG, supp == 'OJ')$len - subset(dataTG, supp == 'VC')$len
mu = mean(difference)
sd = sd(difference)
mu + c(-1,1)*qt(0.95,df=2)*sd/sqrt(30)

# Test len ~ dose
dataTG_0.5_1.0 <- subset(dataTG, dose %in% c("0.5","1"))
dataTG_0.5_2.0 <- subset(dataTG, dose %in% c("0.5","2"))
dataTG_1.0_2.0 <- subset(dataTG, dose %in% c("1","2"))

t.test(len ~ dose, data = dataTG_0.5_1.0)
t.test(len ~ dose, data = dataTG_0.5_2.0)
t.test(len ~ dose, data = dataTG_1.0_2.0)




































############################
### Exercise 1:         ###
############################

# one-sample t-test and confidence interval

# read in stone age
age=c(249, 254, 243, 268, 253, 269, 287, 241, 273, 306, 303, 280, 260, 256, 278, 344, 304, 283, 310)

#Compute mean, median and standard deviation:
mean(age)
median(age)
sd(age)

# Make a histogram (cf. slide 4)
hist(age)

# Plot the empirical distribution function (cf. slide 4)
plot(ecdf(age))                                         # Basic plot
plot(ecdf(age),verticals=T, do.points=F)          # Nicer looking plot

# Compute min, first quartile, median, third quartile, and max (cf. slide 5)
quantile(age)

# Make a boxplot (cf. slide 5)
boxplot(age)

#We will then consider confidence intervals and hypothesis testing.
# We will illustrate direct calculations of the quantities involved as well as the use of a special R-command.

# Compute the 97.5% percentile of the t-distribution with 18 degrees of freedom:
qt(0.975,18)

# Compute lower and upper limit of the 95% confidence interval:
mean(age) - qt(0.975,18)*(sd(age)/sqrt(19))      # lower limit
mean(age) + qt(0.975,18)*(sd(age)/sqrt(19))     # upper limit

# Compute t-statistic:
tstat=(mean(age)-265)/(sd(age)/sqrt(19))       #t-statistic
tstat

# Compute P-value:
1-pt(tstat,18)

# Use the command "t.test" to compute the confidence interval (this gives a two-sided test):
t.test(age,mu=265)

# Use the command "t.test" to compute a one-sided test (this gives a one-sided confidence interval):
t.test(age,alternative="greater",mu=265)

#### Exercise 2:  two-sample t-test and confidence interval
#At the lectures we looked an example on bone mineral density (cf. slide 25 from the lectures)
cont=c(0.228, 0.207, 0.234, 0.220, 0.217, 0.228, 0.209, 0.221, 0.204, 0.220, 0.203, 0.219, 0.218, 0.245, 0.210)
treat=c(0.250, 0.237, 0.217, 0.206, 0.247, 0.228, 0.245, 0.232, 0.267, 0.261, 0.221, 0.219, 0.232, 0.209, 0.255)

# Find the means and standard deviations, and check that you get the same results as in the lectures (slide 25)
mean(cont)
median(cont)
sd(cont)

mean(treat)
median(treat)
sd(treat)

# Use the command "t.test" to compute the confidence interval, t-statistic and P-value:
t.test(treat, cont, var.equal=T)

# Optional: Use the formulas given on slide 26 to compute the pooled standard deviation, the standard error of the effect of treatment, and the 95% confidence interval.

############################
### R exercises 3         ##
############################

speed <- scan("http://www.uio.no/studier/emner/matnat/math/STK4900/v17/exer2.dat")

## hist
hist(speed)

## sd, interquartile range
sd(speed)
IQR(speed)

# compute 95% confidence interval using all data and without outliers
mean(speed) + qt(0.95, 18)*(sd(speed)/sqrt(length(speed)))
mean(speed) - qt(0.95, 18)*(sd(speed)/sqrt(length(speed)))

t.test(speed)

mean(speed[speed > 0]) + qt(0.95, 18)*(sd(speed[speed > 0])/sqrt(length(speed[speed > 0])))
mean(speed[speed > 0]) - qt(0.95, 18)*(sd(speed[speed > 0])/sqrt(length(speed[speed > 0])))

t.test(speed[speed > 0])

# comment on CI since true value 33.02
#CI too small, but CI for data wo/ outliers more reasonable

############################
## LECTURE TWO            ##
## exercise 4             ##
############################

# read data
solvents=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/data/solvents.txt",header=T)

# boxplots
boxplot(rate~type,data=solvents)

# anova
solvents$type <- factor(solvents$type)
aov.solvents <- aov(rate~type, data = solvents)
summary(aov.solvents)

#############################
## lecture two, exercise 5 ##
#############################

# read data
pef <- data.frame("Person" = c(1, 2, 3, 4, 5, 6, 7, 8), "pef" = c(494, 395, 516, 434, 476, 413, 442, 433), "minipef" = c(512, 430, 520, 428, 500, 364, 380, 445))

plot(pef$pef, pef$minipef)

# compute pearson correlation coefficient
cor(pef$pef, pef$minipef, method = "pearson")

# Fit a linear regression model with "minipef" as the outcome and "pef" as the predictor. 
# Give an interpretation of the estimated slope, i.e. the least square estimate for "pef". 
# (See slide 36 from the lectures for R help.) 
pef.model <- lm(pef$minipef ~ pef$pef)
sum_model <- summary(pef.model)
print("least square estimate for pef is change in pef per one unit increase in minipef.")

# 95% CI (theoretical) of correlation coefficient
cor.test(pef$pef, pef$minipef)

# Multiply the estimated slope from question d with the ratio of the empirical standard deviations of "pef" and "minipef"
1.1642 * sd(pef$pef) / sd(pef$minipef)
print("result same as pearson correlation")

############################
## lecture 2, exercise 6 ##
############################

# data
hers.sample=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v17/hers.sample.txt",header=T)
plot(hers.sample$age,hers.sample$sbp)

# Fit a linear regression model to the data, using systolic blood pressure as the outcome and age as the predictor:
hers.fit.b=lm(sbp~age,data=hers.sample)
summary(hers.fit.b)
abline(hers.fit.b)

# Give an interpretation of the slope of the regression line (i.e. the effect of age on sbp). 
# Does age have a significant effect on systolic blood pressure?
print("slope is expected yearly increase in systolic blood preasure in women over 45 years of age. Intercept indicates mean systolic blood pressure measured in 45 year-old women.")

#To illustrate this point, we will here fit a linear regression model where age is measured in units of then years:
hers.fit.d=lm(sbp~I(age/10),data=hers.sample)
summary(hers.fit.d)

# How can you now interpret the slope?
print("Increase over time")
  
############################
## lecture 2, exercise 7 ###
############################

library(MASS)

n=25
rho=0.30
m=matrix(c(0,0),nrow=2)
S=matrix(c(1,rho,rho,1),nrow=2)
obs=mvrnorm(n,m,S)

x=obs[,1]
y=obs[,2]

cor(x,y)
plot(x,y)

# Repeat a) for correlation 0.60 and correlation 0.90. Note how the plots look like when the correlation is 0.60 and 0.90.
n=25
rho=0.90

# Repeat a) and b) for n=100 and n=400. Note how the variation in the Pearson correlation coefficient depends on the sample size. 
n=400
rho=0.90

############################
### LECTURE THREE        ##
## Lecture 3, exercise 8 ##
############################

# read in data
cafe=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/data/exer3_1.dat")
names(cafe)=c("no","sale")

# Attach the dataframe (making the variables available):
attach(cafe)


# plot the data
plot(cafe$no,cafe$sale)

# diff transformations for linear relationship
plot(cafe$no, log(cafe$sale))

linfit <- lm(cafe$sale~cafe$no)
summary(linfit)
abline(linfit)

# Fit a second order polynomial (note that inside lm-command
# we have to write the second order term inside I( )
#(otherwise the sign ^  will be misinterpreted by R):
poly <- lm(sale~no+I(no^2))

# Compute and draw the fitted second order polynomial:
x=seq(0,7,0.1)
koef=lm(sale~no+I(no^2))$coef
koef
lines(x,koef[1]+koef[2]*x+koef[3]*x^2,lty=2)

############################
## Lecture 3, exercise 9 ###
############################

# read in table
insurance=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/data/exer3_2.dat")

colnames(insurance) <- c("income", "risk", "insurance")
  
par(mfrow=c(1,2)) 
plot(insurance$income, insurance$insurance)
plot(insurance$risk, insurance$insurance)
par(mfrow=c(1,1)) 

# correlation
cor(insurance)

# models
insurance.reg <- lm(insurance~income, data=insurance)
summary(insurance.reg)


insurance.reg2 <- lm(insurance~risk, data=insurance)
summary(insurance.reg2)


insurance.reg3 <- lm(insurance~risk+income, data=insurance)
summary(insurance.reg3)

############################
## Lecture 3, exercise 10 ##
############################

hers=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/data/hers.txt",sep="\t",header=T,na.strings=".")
hers.no=hers[hers$diabetes==0, ]

# Make a summary and boxplot of the glucose levels according to the level of exercise:

summary(hers.no$glucose[hers.no$exercise==0])
summary(hers.no$glucose[hers.no$exercise==1])
boxplot(hers.no$glucose~hers.no$exercise)

# Test if there is a difference in glucose level and make a confidence interval:
t.test(glucose~exercise, var.equal=T,data=hers.no)

# Perform a simple linear regression with glucose level as outcome and exercise as predictor:
fit.c=lm(glucose~exercise,data=hers.no)
summary(fit.c)

# d)

# Perform a simple linear regression with glucose level as outcome and exercise, age, and BMI as predictors:
fit.d=lm(glucose~exercise+age+BMI,data=hers.no)
summary(fit.d)

fit.c=lm(glucose~BMI,data=hers.no)
summary(fit.c)

############################
## Lecture 4, exercise 11 ##
############################

# You may read the HERS data into R by the command:
hers=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v17/hers.txt",sep="\t",header=T,na.strings=".")

# Before we start doing our analysis we define the change in LDL, denoted LDLch:
hers$LDLch=hers$LDL1 - hers$LDL

#We also defined the centered LDL at baseline (by subtracting the mean value 145 mg/dL), denoted cLDL
hers$cLDL=hers$LDL-145  

# Fit a linear model with the change in LDL as the response and hormone therapy (HT) and baseline LDL (not centered) as covariates:
fit.a=lm(LDLch~HT+LDL, data=hers)
summary(fit.a)

# We then fit a model with HT and centered LDL at baseline as covariates:
fit.b=lm(LDLch~HT+cLDL, data=hers)
summary(fit.b)

# We then fit a model with interaction:
fit.c=lm(LDLch~HT+cLDL+HT:cLDL, data=hers)
summary(fit.c)

############################
## Lecture 4, exercise 12 ##
############################

# read data
gun=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v17/gun.dat")
colnames(gun) <- c("method", "physique", "team", "rounds")
  
# correlation
cor(gun)

# Define the covariates as factors (categorical covariates):
gun$method=factor(gun$method)
gun$physique=factor(gun$physique)
gun$team=factor(gun$team)

# three-way analysis of variance
analysis <- aov(rounds~method+physique+team, data = gun)
summary(analysis)

analysis <- aov(rounds~method*physique*team, data = gun)
summary(analysis)

anova(analysis)

# different hypotheses, use anova table to test them
# method, physique, physiqu:team significant

# a) Set up a data frame for values where you would like to make predictions, e.g.
testdata=data.frame(method=factor(c(1,2,1,2)), physique=factor(c(1,1,2,3)), team=factor(c(1,2,3,1)))

# Then find fitted/predicted values for your favourite model gfitfav from R-exercise 12 by
predict(analysis, newdata=testdata)

# Then obtain confidence intervals for the expected values at this levels of the factors by
predict(analysis, newdata=testdata, interval="confidence")

# Next find the corresponding prediction intervals by
predict(analysis, newdata=testdata, interval="prediction")





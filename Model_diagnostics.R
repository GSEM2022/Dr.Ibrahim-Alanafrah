
#http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/

# https://www.statmethods.net/stats/rdiagnostics.html

#https://rpubs.com/aryn999/LinearRegressionAssumptionsAndDiagnosticsInR


df <- read.csv('C:/Users/ibrahim/Desktop/Data/df.csv')

#Make index for data
rownames(df) <- df$ï..year
df$ï..year <- NULL
df
#Rename variables
colnames(df) <- c("GDP_growth", "GDP_Per_Capita", "Gross_Capital_Formation",
                    "Inflation","Research_and_Development")
colnames(df)

# Change colname of one column
colnames(data)[colnames(data) == "Old_Name"] <- "New_Name"

#Evaluate data
summary(df)

#Attach data
attach(df)
#check for missing data
colSums(is.na(df))
# Count missing in each column
missings <- colSums(is.na(df))
# Evaluate the breakdown of missings
summary(missings)

#Evaluate the correlation matrix
library("PerformanceAnalytics")
chart.Correlation(df, histogram=TRUE, pch=19)
#visulaize corr matrix
library(corrplot)
M <-cor(df)
head(round(M,2))
corrplot(M, method="circle")
corrplot(M, method="pie")
corrplot(M, method="number")
corrplot(M, type="upper")
corrplot(M, type="lower")


#Building a regression model
model <- lm(GDP_Per_Capita ~ Inflation + Research_and_Developent + 
              Gross_Capital_Formation, data = df)
model
summary(model)
ols_plot_diagnostics(model)

ols_plot_resid_regressor(model, 'drat')

#Regression diagnostics
library(ggfortify)
autoplot(model)

#Linearity of the Data
plot(model, 1)

library(lmtest)
resettest(model)


#Normality of Residuals
plot(model,2)
#Perform a Shapiro-Wilk Normality Test
library(MASS)
# distribution of studentized residuals
sresid <- studres(model) 
shapiro.test(sresid)


#Outliers and high levarage points
# High leverage points
plot(model, 5)
#Influential values
#Cook's distance
plot(model, 4)



library(olsrr)
ols_plot_cooksd_bar(model)
ols_plot_cooksd_chart(model)
ols_plot_dfbetas(model)
ols_plot_resid_stud(model)
ols_plot_resid_lev(model)
ols_plot_resid_stud_fit(model)
ols_plot_resid_pot(model)


hat_model <- as.data.frame(hatvalues(model))
hat_model
#or
round(as.vector(lm.influence(model)$hat),4)
#sort observations by leverage, descending
hat_model[order(-hat_model['hatvalues(model)']), ] # It should not be > 2
plot(hatvalues(model), type = 'h')


#Outliers test
library(outliers)
test <- grubbs.test(df$GDP_Per_Capita)
test

library(car)
outlierTest(model)
qqPlot(model, main="QQ Plot")
leveragePlots(model)



#Residuals vs Leverage
plot(model, 5)
#Testing the Homoscedasticity Assumption
plot(model, 3)
#ncvTest() For Homoscedasticity
library(car)
# non-constant error variance test
ncvTest(model)
#Breusch-Pagan Test For Homoscedasticity
library(lmtest)
bptest(model)

plot(fitted(model), resid(model), xlab='Fitted Values', ylab='Residuals')
abline(0,0)
library(lmtest)
bptest(model)

#Perform Weighted Least Squares Regression as a solution of Heteroscedasticity
#define weights to use
wt <- 1 / lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2
model_5 <- lm(GDP_Per_Capita ~ Inflation + Research_and_Developent + 
                Gross_Capital_Formation, data = df, weights = wt)
summary(model_5)
plot(fitted(model_5), resid(model_5), xlab='Fitted Values', ylab='Residuals')
abline(0,0)

library(lmtest)
bptest(model_5)



#Testing the Independence (Autocorrelation) Assumption
# durbin watson test
durbinWatsonTest(model)
#Testing the Multicollinearity Assumption
#Correlation Matrix
M <-cor(df)
corrplot(M, method="number")

X<-df[,1:5]
library(GGally)
ggpairs(X)
library(corpcor)
cor2pcor(cov(X))
#Farrar - Glauber Test
library(mctest)
omcdiag(model, GDP_Per_Capita)
vif(model)

#Variance Inflation Factor for Multicollinearity (After 5 for VIF =, there is severe correlation)
library(car)
vif(model)
ols_vif_tol(model) # Tolerance is Percent of variance in the predictor that cannot be accounted for by other predictors.
# tolerance  = 1 - R^2 

vif_values <- vif(model)    
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)



#Diagnostic plots
par(mfrow = c(2, 2))
plot(model)
#Linearity of the data
plot(model, 1)
#Ideally, the residual plot will show no fitted pattern. That is, the red line should be approximately horizontal at zero. The presence of a pattern may indicate a problem with some aspect of the linear model.
#Homogeneity of variance
plot(model, 3)
#This plot shows if residuals are spread equally along the ranges of predictors. It is good if you see a horizontal line with equally spread points. In our example, this is not the case.
#It can be seen that the variability (variances) of the residual points increases with the value of the fitted outcome variable, suggesting non-constant variances in the residuals errors (or heteroscedasticity).




#Normality of residuals
plot(model, 2)

# Normality of Residuals
# qq plot for studentized resid
library(car)
qqPlot(model, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(model)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(model)
# plot studentized residuals vs. fitted values
spreadLevelPlot(model)

#Multi-collinearity
vif(model) # variance inflation factors
sqrt(vif(model)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot
crPlots(model)

# Test for Autocorrelated Errors
durbinWatsonTest(model)

acf(resid(model), main="acf(resid(model))")
acf_resid(model, main="acf_resid(model)")

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(model)
summary(gvmodel)




#Outliers and high levarage points
plot(model, 5)

#Outliers
boxplot(df$GDP_growth)
boxplot(df$GDP_Per_Capita)
boxplot(df$Gross_Capital_Formation)
boxplot(df$Gross_Capital_Formation)
boxplot(df$Research_and_Developent)

library(car)
outlierTest(model)
qqPlot(model, main="QQ Plot")
leveragePlots(model)
#The plot above highlights the top 3 most extreme points (#USA, #Tajikstan and #Uzbekistan), with a standardized residuals below -2. However, there is no outliers that exceed 3 standard deviations, what is good.
#Additionally, there is no high leverage point in the data. That is, all data points, have a leverage statistic below 2(p + 1)/n = 4/200 = 0.02.

#Influential values
avPlots(model)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(df)-length(model$coefficients)-2))
plot(model, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(model, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )
# Cook's distance
plot(model, 4)


# Residuals vs Leverage
plot(model, 5)

plot(model, 4, id.n = 5)


#Plot residuals vs row number (index)
plot(residuals(model))
#If there is a pattern, this means the residuals lack of independence 

#Plot residuals vs explanatory variable
plot(df$Gross_Capital_Formation, residuals(model))
#Show if the linear model is appropriate

#ggResidpanel
install.packages("ggResidpanel")
library(ggResidpanel)
resid_panel(model, plots = "R")

resid_panel(model, plots = c("qq", "hist", "resid", "index", "yvp", "cookd"),
            bins = 30, smoother = TRUE, qqbands = TRUE,
            type = "standardized")
resid_xpanel(model)


#Ramsey Regression Equation Specification Error Test (RESET)
library(lmtest)
resettest(model)
anova(model)
anova(model2)



#Senesitivity analysis
library(sensemakr)
model.sensitivity <- sensemakr(model = model, 
                                treatment = GDP_Per_Capita,
                                kd = 1:3)

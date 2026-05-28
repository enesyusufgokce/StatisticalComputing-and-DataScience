# SIMPLE AND MULTIPLE LINEAR REGRESSION

# SIMPLE LINEAR REGRESSION
# the goal of this analysis is to model and understand the relationship between one predictor variables 
# and a continuous outcoma variable.

# AIM
# in the simple linear regression, we assess how horsepower affects fuel efficiency (mpg)

# HYPHOTESES
# (Bu, basit linear regressionda hipotez testi kurma şekli)
# mpg=β0+β1⋅hp+ε
# bizim tahmin = ε'suz versiyondur. ε, tahminden ne kadar saptığımızı sonradan ölçmek için var(residual)

# H0: β1 = 0 (no relationship between hp and mpg) etki var diyorsan bunu sen ispatlamalısın. o yüzden
# onu H1 e koyacaz
# H1 = β1 != 0 (significant relationship exist)

# ASSUMPTIONS
# linearity: the relationship between variables are linear
# normality: residuals are normally distributed
# homoscedasticity: constant variance of residuals
# independence: observations are independent

# ASSUMPTION CHECKS

# (linearity check)
# scatter plot 
plot(mtcars$hp, mtcars$mpg)  # bunu yazsan yeter dediydi hoca.

# correlation test (linearity check)  plota bakıp emin olamıyosak bunu yapabiliriz
# H0: the correlation between hp and mpg is zero
# H1: the correlation between hp and mph is not zero
cor.test(mtcars$hp, mtcars$mpg)  # p is less than alpha so reject the H0. so there is a statictically
# significant linear dependence between these two variables

# (normality check)
# residual diagnostics
# first, create your linear model
lm_model = lm(mpg ~ hp, data = mtcars)  # mpg is y (dependent var.) hp is x (independent var.)
par(mfrow = c(2,2))
plot(lm_model)
# look at residuals vs fitted. I dont want constantly increasing or decreasing residuals
# I want residuals to be homogenously distributed around zero

# normality için en garanti shapiro test
# H0: Residual'lar normal dağılımdan geliyor  - default bunlar değiştiremezsin.
# H1: Residual'lar normal dağılımdan gelmiyor - bu da elbette
# Linear regression normality varsayımı veriyle değil, residual'larla ilgilidir.
# Veri normal olmak zorunda değil, hatta çoğu zaman değildir.
shapiro.test(residuals(lm_model))
# p is less than 0.05 so we reject H0  -  so, residuals are not normally distributed. if we decided alpha
# to be 0.05 by the way.

# (homoscedasticity check)
# homoscedasticity: The variance of the error terms in a regression is constant across all observations
# heteroscedasticity: the variance of the error terms in a regression is not constant and changes from
# observations to observations
plot(fitted(lm_model), residuals(lm_model),
     abline(h = 0))
# there is mostly homogeneous distribution around zero, so the variance of residuals are homogeneous
# so, it passes the check.

# (independence check)
# we assumed the data is independent

# model output
summary(lm_model)
# H0 dediği beta1 in 0 olması, yani hp ve mpg arasında statistically significant bir ilişki yok demek
# neden o H0 da dersen, ispat yükü iddiayı ortaya atanda. varsayılan net değer (etki = 0) H0 a gider.
# etki var demek net değildir, her bir nokta için (sonsuz aralıktaki) farklı farklı değerde etkiler 
# olabilir.
# p-value for hp is 1.79e-07, reject H0. so, there is statistiaclly significant relationship between hp 
# and mpg our regression model expains the variance of residuals with 58% (Adjusted R-squared: 0.5892)
# F-statistic: p-value: 1.788e-07 (p value is about our general model and less than alpha.
# so, the model is significant)


# MULTIPLE LINEAR REGRESSION

# AIM
# to understand the relationship between two or more predictor variables and a continuous outcome 
# in mult,ple linear regression model, we evaluate the combined effects of horsepower, vehicle weight,
# and engine displacement on fuel efficiency, while also checking for multicollinearity and interpreting
# the contrubution of each predictor.
# H0: All β (β1, β2, β3) not β0 coefficients = 0
# H1: At leas one β (β1 or β2 or β3) != 0
multi_model = lm(mpg ~ hp + wt + disp, data = mtcars)

# ASSUMPTIONS
# linearity, normality, homoscedasticity, independence
# no multicollinearity among predictors - multicollinearity means correlation exist
# between the x variables so we dont want it.

# Diagnostics
par(mfrow = c(2,2))
plot(multi_model)

# (normality check)
shapiro.test(residuals(multi_model)) # p-value = 0.03305 so reject H0. but in the course, we said the 
# violation is not much. so we continued.

# Correlation matrix (linearity and multicollinearity in one)
# note: correlation tells the linear dependence between the variables
cor(mtcars[, c("mpg", "hp", "wt", "disp")])  # the diagonals are 1 , because correlation between the
# same variables are exactly same.
# linearity assumption: 
# check the y and x1, y and x2, y and x3. 
# 1.0000000 and -0.7761684, 1.0000000 and -0.8676594, 1.0000000 and -0.8475514
# there are high correlations. so linearity assumptions are satisfied
# multicollinearity: correlations between x variables should not be very high
# hp and wt, hp and disp, wt and disp
# 0.6587479, 0.7909486, 0.8879799  there is a slight multicollinearity between variables. but we continued in lecture
# Interpretation: predictors should not be highly correlated with each other.(to avoid multicollinearity)
# predictors should show moderate correlation with the outcome (mpg) to be useful

# (homoscedasticity check)
plot(fitted(multi_model), residuals(multi_model),
     abline(h = 0))

# assumed the data is independently collected

# Model Output
summary(multi_model)
# p-value for hp is less than alpha so reject H0. so, there is a statistically significant relationship
#  between hp and mpg. And also this is true for wt and mpg bcz. p-value is 0.00133. but not true for
# disp and mpg bcz. p-value between them is 0.92851. this is greater than alpha so do not reject H0.
# and we say that there is no statistically significant relationship between disp and mpg.
# reject H0: β1 = 0 and H0: β2 = 0 but do not reject H0: β3 = 0
# reminder: rejecting H0 means accepting H1
# also the linear model is significant because F-statistic: p-value: 8.65e-11 (p value is about
# our general model and less than alpha. so, the model is significant)
# The model expains the variance of residuals with 80% (Adjusted R-squared: 0.8083)  -- proportion
# of variance of residuals in mpg explained by all three predictors.
#
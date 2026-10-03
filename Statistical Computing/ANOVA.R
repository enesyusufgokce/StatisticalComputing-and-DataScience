# data preparation
data(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)

# boxplot
boxplot(mpg~cyl, data = mtcars, col="skyblue",
        main="MPG by Number of Cylinders", ylab="Miles per Gallon")

# H0: all group means are equal
# H1: at least one group mean is different
# x axis: 4 silindirli, 6 silindirli, 8 silindirli arabalar. factor olarak ayrılmış
# y axis: miles per gallon

# Assumptions
# - observations are independent
# - residuals are normally distributed
# - groups have equal variances

# Assumption tests
anova_model <- aov(mpg~cyl, data=mtcars)  # mpg is dependent, cyl is independent variable

# normality check
shapiro.test(residuals(anova_model))

# note: her aracınn mpg'sinden, ait olduğu silindir grubunun ortalama mpg'sini çıkardık. 
# Bu işlem grup farklarını ortadan kaldırır ve geriye sadece "rastgele dalgalanma" kalır. 
# normallik varsayımı bu dalgalanmayla ilgili. yani anovadaki normality assumption, her grubun
# kendi ortalamasından sapması (yani residual'ı) ile alakalı.


# homogenity of variances
bartlett.test(mpg~cyl, data=mtcars) # p-value is 0.01505 < 0.05. so, we reject H0 which assumes
# the variance between groups is homogeneous. so, the variance between groups are heterogeneous

# ANOVA Results
summary(anova_model) # p value is 4.98e-09 for anova model. so we reject H0 and we can say that there
# is a statistically difference between the means of these three groups in terms of MPG (miles per gallon).

# post hoc test (tukey test) - bir fark olduğuunu gördük, şimdi hangileri arasında var ona bakıyoz
TukeyHSD(anova_model)
# p-values. all of them are less than alpha so all group means differ from each other.
# 6-4 0.0003424
# 8-4 0.0000000
# 8-6 0.0112287
# there is a statistically significant difference between 6-4, 8-4, and 8-6 cylinders in terms of MPG


library(MASS)
str(birthwt)

colnames(birthwt)

# the default names are not very descriptive
colnames(birthwt) <- c("birthwt.below.2500", "mother.age", "mother.weight", "race", "mother.smokes", "previous.prem.labor",
              "hypertension", "uterine.irr", "physician.visits", "birthwt.grams")

library(plyr)
birthwt <- transform(birthwt,
                     race = as.factor(mapvalues(race, c(1, 2, 3), c("white", "black", "other"))),
                     mother.smokes = as.factor(mapvalues(mother.smokes, c(0, 1), c("no", "yes"))),
                     hypertension = as.factor(mapvalues(hypertension, c(0, 1), c("no", "yes"))),
                     uterine.irr = as.factor(mapvalues(uterine.irr, c(0, 1), c("no", "yes"))),
                     birthwt.below.2500 = as.factor(mapvalues(birthwt.below.2500, c(0, 1), c("no", "yes")))
                    )
                     
# descriptive statistics part
summary(birthwt) # numerical and factor variables are there

# use tapply() function to see what average birthweight looks like when broken down by race and smoking status
with(birthwt, tapply(birthwt.grams, INDEX = list(race, mother.smokes), FUN = mean))

# aggregate()
with(birthwt, aggregate(birthwt.grams, by = list(race, mother.smokes), FUN = mean))

weight.smoke.tbl <- with(birthwt, table(birthwt.below.2500, mother.smokes))
weight.smoke.tbl


# Normalde bir veri setindeki s??tuna eri??mek i??in veri$s??tun_ad?? ??eklinde bir yaz??m kullan??l??r.
# with() kulland??????n??zda ise R'a "??u veri setinin i??ine bak ve i??indeki de??i??kenleri do??rudan kullan" demi?? olursunuz. 

# Her de??i??kenin ba????na 'mtcars$' eklemek gerekir
# sonuc <- mtcars$mpg * mtcars$hp

# Veri setini bir kez belirtmek yeterlidir
# sonuc <- with(mtcars, mpg * hp)


# is the mother's age correlated with birth weight?
with(birthwt, cor(birthwt.grams, mother.age))  # calculate the correlation
# does this change when we account for smoking status?
with(birthwt, cor(birthwt.grams[mother.smokes == "yes"], mother.age[mother.smokes == "yes"]))
with(birthwt, cor(birthwt.grams[mother.smokes == "no"], mother.age[mother.smokes == "no"]))


# Single Variable Plots
par(mfrow = c(2,2)) # display plots in a single 2x2 figure
plot(birthwt$mother.age)
with(birthwt, hist(mother.age))
plot(birthwt$mother.smokes)
plot(birthwt$birthwt.grams)

par(mfrow = c(1,1))
plot(birthwt$mother.smokes,
     main = "Mothers Who Smoked in Pregnancy",
     xlab = "Smooking During Pregnancy",
     ylab = "Count of Mothers",
     col = "Lightgrey")

# NOTE: use histogram for continuous variables, use barchart for factor variables


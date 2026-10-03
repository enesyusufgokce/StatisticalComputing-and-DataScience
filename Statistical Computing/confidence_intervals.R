library(MASS)
library(plyr)
library(ggplot2)

# EXPLORING THE BIRTHWD DATA - the first thing we'll do is to get the data into a nicer form

# rename the columns to have more descriptive names
colnames(birthwt) <- c("birthwt.below.2500", "mother.age", "mother.weight", "race", "mother.smokes",
                       "previous.prem.labor", "hypertension", "uterine.irr", "physician.visits", "birthwt.grams")
# btw, our dependent variable is birthwt.grams
colnames(birthwt)

# transform variables to factors with descriptive levels
# (factor, categorical variable'lar?? depolamak i??in kullan??lan bir veri yap??s??d??r.)
birthwt$race
birthwt$mother.smokes
birthwt$hypertension
birthwt$uterine.irr
birthwt <- transform(birthwt,
                     race = as.factor(mapvalues(race, c(1,2,3), c("white", "black", "other"))),
                     mother.smokes = as.factor(mapvalues(mother.smokes, c(0,1), c("no", "yes"))),
                     hypertension = as.factor(mapvalues(hypertension, c(0,1), c("no", "yes"))),
                     uterine.irr = as.factor(mapvalues(uterine.irr, c(0,1), c("no", "yes")))
                     )
class(typeof(birthwt$hypertension)) # factor ler, arka planda bellek tasarrufu i??in integer olarak saklan??r
# ve her say??ya bir label atan??r. o y??zden direkt typeof diyince integer dedi. ama class(typeof()blabla)) dersek
# o zaman nesnenin kullan??m amac??n??, yani "factor" oldu??unu s??yler


# TESTING DIFFERENCES IN MEANS
# create boxplot showing how birthwt.grams varies between
# smoking status
qplot(x = mother.smokes, y = birthwt.grams,
       geom = "boxplot", data = birthwt,
       xlab = "Mother Smokes",
       ylab = "Birthweight {grams}",
       fill = I("lightblue"))

# note: if there is a statistically difference between these two groups, we need hypothesis test
# how can we assess whether this difference is statistically significant?
# (the std is good to have, but to assess statistical significance we really want to have the 
# standard error( which the sd adjusted by the group size)

)) 
aggregate(birthwt.grams ~ mother.smokes,
          data = birthwt,
          FUN = function(x){
            c(mean = mean(x), sd = sd(x))
          })


mean(birthwt$birthwt.grams)  # for the whole, not seperately
sd(birthwt$birthwt.grams)    #  "   "    "     "      "


# Confidence Intervals for Mean
# -------standard deviation is known--------
norm.interval = function(data, variance, conf.level = 0.95){
  z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
  xbar = mean(data)
  sdx = sqrt(variance/length(data))
  c(xbar - z * sdx, xbar + z * sdx)
}
birthwt.var <- 800^2
norm.interval(birthwt$birthwt.grams, birthwt.var)
# not: if population sd is given, you use z. instead of t   burdan anlad??k it is known:  birthwt.var <- 800^2

# -----sd is not known-----
birthwt.CI <- t.test(birthwt$birthwt.grams)$conf.int # t testinden bir??ok sonu?? ????k??yor ve onlardan conf.int i al??yoz
birthwt.CI  # if we  know the sd, (population sd) then we use z test instead of t

birthwt.CI <- t.test(birthwt$birthwt.grams, conf.level = 0.9)$conf.int
birthwt.CI  # became tighter

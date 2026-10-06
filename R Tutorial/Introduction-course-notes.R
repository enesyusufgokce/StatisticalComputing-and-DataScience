# A few commands into the R console:
# ls() list of objects in the current workspace
# rm(x) deletes only object x. if we say ls() after that, x will not be in the object list anymore
# data() find out what standard data sets there are
# plot(iris) plot Fisher's iris data

# -----------------------------------------------
  
View(iris)
sum(3,4)

summary(iris, maxsum=2, # integer, indicating how many levels should be shown for factors
        digits=2)

pbirthday(40, classes=365, coincident=3) # pbirthday is one of the exceptions that has been 
# specifically added because it is frequently encountered



# ---- Data Structures ----
# vectors and factors, matrices and arrays, lists, data frames

# Vectors ---
x <- c(2,3,6)
x
y <- 5 # one element vector
y
# note: the elements of a vector must be of the same type. If they differ, R converts them to the
# most general type (logical < integer < numeric < character)
# so, c(2,3,6,"yusuf") becomes c("2","3","6","yusuf")
x <- 1:3
x
x <- rep(1,3) # repeat
x
y <- c("red", "yellow", "green")
y
z <- c(TRUE, FALSE)
z

# Data Frames ---
# data sets are stored in R as data frames. These are structured as a list of objects, typically
# vectors, of the same length
str(iris) # structure

x <- 1:3
y <- c("red", "yellow", "green")
mydata <- data.frame(x, y)
mydata
ls()


# ---- Install Packages ----
install.packages("survival", dependencies=TRUE)
library(survival)


# ---- Data Preprocessing ----
# The dplyr package can be used to help with many of preprocessing steps

install.packages("UsingR")
infant <- UsingR::babies
head(infant)

install.packages("dplyr")
library(dplyr)

class(infant) # "data.frame"

# filter() selects rows of data by criteria
infant
infant <- filter(infant, smoke != 9 & age > 18) # Keeps only the records of mothers whose 
# smoking status is known and who are older than 18, removing the rest from the infant table.
infant

# select() selects variables from the data frame
select(infant, gestation, sex) # select gestation and sex columns from the infant data frame
select(infant, pluralty:gestation, parity) # selects all columns from pluralty to 
# gestation (both inclusive), plus parity from the infant data frame
select(infant, -(id:outcome), -sex) # Select all columns except specified columns

#mutate() 
mutate(infant,
       wt = ifelse(wt == 999, NA, wt), # replace 999 with NA
       wt = wt * 0.4536) # convert ounces to kg 

# Chaining %>% : soldaki sonucu al, sağdaki fonksiyona ver. don't write infant <- blabla in each step
infant <- infant %>%
  filter(smoke != 9 & age > 18) %>%
  select(-(id:outcome), -sex) %>%
  mutate(wt = ifelse(wt == 999, NA, wt),
         wt = wt * 0.4536)
# Bir önceki satırın ürettiği sonuç, bir sonraki fonksiyonun içine "hangi data frame üzerinde
# çalışayım?" sorusunun cevabı olarak giriyor. Bu yüzden filter, select, mutate içinde infant
# yazmamıza gerek kalmıyor

# summarise()
stats <- summarise(infant, mean_wt = mean(wt, na.rm = TRUE),
                   n = n()) # number of rows
# na.rm = TRUE, hesaplamadan önce eksik NA değerleri atlar. Yazmazsak içinde bilinmeyen değer var,
# sonucu da bilemem der ve ortalama "NA" çıkar
stats


# ---- Plots ----

# Boxplots ---
boxplot(infant$wt) # ~ means "grouped by": wt ~ race splits birth weights into groups by race
with(infant, boxplot(wt ~ race)) # # wt ~ race: groups birth weights by mother's race (White, 
# Latino, Black, Asian, Mixed) and draws one box per race group

# Histogram/Density ---
hist(infant$wt) # infant$wt, takes the wt column from the infant data frame
plot(density(infant$wt))

# Scatter Plots ---
plot(infant$gestation, infant$wt); plot(x=infant$gestation, y=infant$wt)
infant_cleaned <- infant %>% 
  filter(gestation > 200 & gestation != 999) # Some observations with very low and high gestation
# look suspect, so we will exclude them from the rest of the analysis
plot(infant_cleaned$gestation, infant_cleaned$wt); plot(x=infant_cleaned$gestation, y=infant_cleaned$wt)

# With ggplot2 ---

# Box Plots ---
library(ggplot2)
ggplot(infant, aes(y=wt * 28.35, x = factor(race))) +
  geom_boxplot() +
  ylab("Birth Weight (g)")

# Scatter Plots ---
infant <- mutate(infant, smoke=recode_factor(smoke,
                                             "1"="currently", "2"="until pregnancy",
                                             "3"="used to", "0"="never"))
p <- ggplot(infant, aes(y=wt*28.35, x=gestation, color=smoke)) + 
  geom_point() + labs(x="Gestation", y="Birth Weight(g)", color="Smoking status") 
p
infant_cleaned <- filter(infant, gestation != 999) # removes records where gestation is unknown (coded as 999)
p <- ggplot(infant_cleaned, aes(y=wt*28.35, x=gestation, color=smoke)) + 
  geom_point() + labs(x="Gestation", y="Birth Weight(g)", color="Smoking status") 
p
# infant: the data frame ggplot takes the columns (gestation, bwt, smoke) from
# color=smoke: each smoking status gets a different color
# in the smoke column, replaces the old values on the left with the new labels on the
# right (e.g. 1 becomes "currently")

# Facetting ---
p + facet_wrap(~ smoke) + guides(color = FALSE) # takes the existing plot (p) and adds two things
p + facet_grid(smoke ~ race) + guides(color = FALSE)
# NOTE: # facet_wrap: panels in a wrapped row (one variable); facet_grid: panels in a 
# rows x columns table (two variables)


# ---- Simple Linear Modelling ----
model <- lm(wt ~ gestation, data=infant) # fits a linear model that predicts birth weight (wt) from gestation
model # wt = a * gestation + b: intercept(the point where the line crosses the y-axis) is b, the 
#gestation coefficient is the slope a
plot(model)
summary(model)

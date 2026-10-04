# FUNCTIONS
# note: a vector is a data structure for storing similar kinds of data,
# a list is a data structure that can be used to store different kinds of data

my.vector.1 <- c("Ela", 22, FALSE)
my.vector.1  # prints all of them as character vectors
typeof(my.vector.1)

my.vector.2 <- c(FALSE, TRUE, 1)
my.vector.2
typeof(my.vector.2)  # prints them all as double


my.list <- list("Ela", 22, TRUE)
my.list
sapply(my.list, typeof)

kid.1 <- list(name="Ela", age=22, is.male=FALSE)
kid.1

kid.1$name
kid.1$weight

c(typeof(kid.1$name), typeof(kid.1["name"]))  # the first one is character, second one is list

# -----------------

addOne <- function(x){
  x + 1
}

addOne(12)


calculatePercentage <- function(x, y, d){  # numerator, denominator, desired num. of decimal values
   decimal <- x / y  # calculate decimal value
   round(100 * decimal, d)  # convert to decimal and round to d digits
}
calculatePercentage(3, 51, 2)


createPatientRecord <- function(full.name, weight, height) {
  name.list <- strsplit(full.name, split=" ")[[1]]
  first.name <- name.list[1]
  last.name <- name.list[2]
  weight.in.kg <- weight / 2.2
  height.in.m <- height * 0.0254
  bmi <- weight.in.kg / (height.in.m ^ 2)  # body mass index
  list(first.name=first.name, last.name=last.name, weight=weight.in.kg, height=height.in.m, bmi=bmi)
}

createPatientRecord("yusuf gokce", 56, 175)


fiveAverages <- function(x){
  c(average=mean(x), trimmed=mean(x, trim = 0.10), median=median(x), geometricmean = prod(x)^(1/length(x)),
    harmonicmean=1/mean(1/x))
}
x <- rnorm(200, mean=20, sd=5)  # vector of 200 kids with weigh mean 20 and standard deviation 5
# this function generates randomly normally distributed variables
par(mfrow=c(1,3))
hist(x)
qqnorm(x)
qqline(x)
boxplot(x)

fiveAverages(x)


# if else
calculateLetterGrade <- function(x){
  if(x >= 90) {
    grade <- "A"
  }
  else if(x >= 80) {
    grade <- "B"
  }
  else if(x >= 70) {
    grade <- "C"
  }
  else{
    grade <- "F"
  }
}

# note: c means "combine" Parantez içindeki değerleri (sayılar, metinler vb.) alıp tek bir 
# vektör (veya liste) haline getirir.
# sayılar <- c(1, 5, 8, 10)
# output: [1]  1  5  8 10
# isimler <- c("Ali", "Ayşe", "Fatma")
# output: [1] "Ali"   "Ayşe"  "Fatma"

course.grades <- c(92, 88, 84, 75, 91)
course.grades
sapply(course.grades, FUN=calculateLetterGrade)  # sapply bir liste veya vektör üzerindeki her öğeye bir
# fonksiyon uygulayan ve sonucu mümkünse basitleştirerek (vektör veya matris olarak) döndüren 
# lapply'ın simplified bir versiyonudur.

addOne <- function(x){
  return(x+1)
}
addOne(15)

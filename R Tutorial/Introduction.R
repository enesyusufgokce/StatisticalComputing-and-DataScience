# LECTURE 1:  INTRODUCTION

"hello world"

getwd()

7 + 5
7 - 5
7 * 5
7 / 5
7 %/% 5  # integer divison
7 ^ 5   # exponential

exp(1)
log(1)

7 > 5
7 < 5
7 <= 7
7 == 5
7 != 5

(5 > 7) & (6 * 7 == 42)
(5 < 7) & (6 * 7 == 42)

typeof(7)  # double
is.numeric(7)
is.na(7)   # is a missing value (not available)
is.character(7)
is.character("seven")

pi
letters

month.abb  # abbrevated letters
month.name

x <- 2
y <- 4

x

x + y
x * y
y ^ x

hours <- 12
days <- 2.5
total.hours = hours * days

# deleting an object
ls()
rm(x)
rm(y)
ls()  # list the variables

vec1 <- c(1,2,3,4,5)  # c is "combine", "concatenate"
days*vec1

x <- c(1:10)  # x is a vector   --  days, hours are just numbers
x
dim(x)  # used for metrices.  for x, it will be NULL 
length(x)  
typeof(x) # integer

class(x) # integer dedi

dim(x) <- c(2,5) # two lines with five columns
# başlangıçta x, sadece 1 den 10 a kadar bir sayı dizisiydi. dim(x) <- c(2,5) dediğimizde R'a:
# "Bu 10 tane sayıyı al, onları 2 satır ve 5 sütun olacak şekilde bir kutuya (matrise) yerleştir," diyoruz
class(x) # dim den sonra artık matris oldu x
# R dilinde her matris aslında bir array'dir, ama her array bir matris değildir

y <- c("hello", "world", "!")  # character vector
z <- c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)
dim(z) <- c(1, 7)

t <- list("R", 12345, FALSE) # CAN CONTAIN DIFFERENT TYPES OF VALUES
typeof(t) # list

students <- c("yusuf", "gokce", "aysel", "enise")
midterms <- c(100, 90, 85, 100)

st_grades <- cbind.data.frame(students, midterms) # column bind. our first data frame
ls()
st_grades$students  # use the $ sign to use belonging to
st_grades$midterms
str(st_grades) # structure of st_grades data frame
sort(st_grades$midterms)  # sorted version of midterms in ascending order
students[(c(-2, -3))]

st_grades[4, 2]  # fourth row and second column of the data frame
st_grades[1, 1]

st_grades[4,2] <- 99 
colnames(st_grades)

matrix1 <- matrix(NA, 5, 2)  # the matrix contains NA values. and 5 rows 2 columns
matrix1
matrix2 <- matrix(0, 5, 2)
matrix2
matrix3 <- matrix(1:12, nrow=3,byrow = T)
matrix3
matrix4 <- cbind(A=1:4, B=5:8, c=9:12)
matrix4
matrix5 <- rbind(A=1:4, B=5:8, c=9:12)
matrix5


library(BSDA)

# -- One sample z-test --

# import dataset
grade1 <- c(25, 41, 45, 54, 39, 44, 32, 31, 21, 12, 43, 42)
xmean <- mean(grade1)
# normality testing
qqnorm(grade1)
qqline(grade1)

plot(density(grade1))

# Shapiro-Wilk testi, bir veri setinin normal dağılıma (çan eğrisi) uygun olup olmadığını denetleyen 
# istatistiksel bir varsayım testidir

# mantığı:
# H0: Veriler normal dağılıma sahiptir.  hep
# H1: Veriler normal dağılıma sahip değildir. hep
# p-value yorumlama: Eğer p > 0.05 ise: H0 reddedilemez. Yani verilerin normal dağıldığını kabul ederiz (it doesn't mean that we accept H0).
# Eğer p < 0.05 ise: H0 reddedilir.Verilerinizin normal dağılımdan anlamlı şekilde saptığı sonucuna varırız.
shapiro.test(grade1)

# one sample z test : testing H0:mu=40 vs H1:mu not=40 
z.test(grade1, mu=40, sigma.x=10)
# Özetle amacımız: Elimizdeki verinin, genel kabul görmüş bir değerden (40(popülasyon mean)) 
# standard sapmasının "statistically significant" mı yoksa "sıradan bir tesadüf" mü olduğunu anlamaktır.
mu <- 40
sigma.grade1 <- 10 # sigma known
n <- 16
z <- (xmean-mu) / (sigma.grade1/sqrt(n))
z

alpha <- 0.05 # Type1 error rate
z.half.alpha <- qnorm(1-alpha/2)
c(-z.half.alpha, z.half.alpha) # critical region

pval <- 2*pnorm(z) # p value
pval

# One sample t test - sigma not known
# sigma not known. so we use t function
alpha <- 0.05
# testing H0: mu=40 vs H1: mu not= 40
t.test(grade1, mu=40)


# one sample t test: testing H0: mu >= 40 vs H1: mu < 40 
t.test(grade1, mu=40, alternative="less")

# one sample t test: testing H0: mu >= 40 vs H1: mu < 40 
t.test(grade1, mu=40, conf.level=0.99, alternative = "two.sided") 
# conf.level yazmazsak varsayılan 0.95 tir. yani alpha is 0.05

# one sided t test: testing H0: mu >= 40 vs H1: mu < 40 
t.test(grade1, mu=40, alternative = "greater")

# two independent sample z test
# note: grade1 is already defined
grade2 <- c(16, 32, 43, 56, 33, 41, 39, 31, 22, 16, 43, 42)
xmean <- mean(grade2)
# normality testing
shapiro.test(grade1)
shapiro.test(grade2)

# sigma.grade1 and sigma.grade2 are known
sigma.grade1 <- 10
sigma.grade2 <- 10
# two sample z test: testing H0:mu1=mu2 vs H1: mu1 not= mu2
z.test(grade1, sigma.x = sigma.grade1,
       grade2, sigma.y = sigma.grade2)
# p value 0.7595 çıktı. so, I cannot reject H0

# two sample z test: testing H0:mu1=mu2 vs H1: mu1 not= mu2
# where confidence level is 0.90
z.test(grade1, sigma.x = sigma.grade1,
       grade2, sigma.y = sigma.grade2,
       conf.level = 0.90)  # confidence level azaldığı için conf interval daraldı. 
                           # yanılma payını artırdık çünkü o yüzden güven azaldı
# in both cases, p-value is greater than alpha. so, we do not reject H0 in both cases

# twi independent sample t test (which means population sd is unknown)
# normality testing
shapiro.test(grade1)  # p-value = 0.5978  H0: grade1 is normally distributed. and we do not reject it
shapiro.test(grade2)  # p-value = 0.4613   because of p-value is greater than alpha

# variance homogenity check.  H0: variance of grade1 = variance of grade2
var.test(grade1, grade2)
# p value > alpha (biz 0.05 diyoz şimdi. bu bağımsız değişken bu arada alpha)
# so we do not reject H0. tanım gereği H0 budur. so we can say that these two groups have
# equal variences

# var.test() fonksiyonu da (ve dünyadaki tüm standart F-testleri) H0 hipotezini her zaman
# "Eşitlik" üzerine kurar (=, >=, <=). Bu bir tercih değil, testin matematiksel tanımıdır.
# Süreci şöyle düşünebiliriz:
# Varsayılan Ayar (H0)
# var.test(grade1, grade2) yazdığımızda, R arka planda otomatik olarak şu cümleyi kurar:
#"Şu an iki grubun varyansının birbirine eşit olduğunu (oranlarının 1 olduğunu) varsayıyorum."
# Bu, testin başlangıç noktasıdır. R bunu "biliyor" çünkü F-testinin algoritması bu varsayım
# üzerinden bir olasılık hesaplamak üzere programlanmıştır.

# standart bir Two Sample T-Test (Bağımsız İki Örneklem T-Testi) yapabilmek için 
# varyansların homojen (birbirine eşit veya çok yakın) olması gerekir.
# variance homogenity check ile bunu doğruladık, shapiro ile de zaten ikisinin de normally distributed
# olduğunu biliyoz. so we can make two sample t-test

# two sample t-test: testing H0: mu1=mu2 vs H1: mu1 not= mu2
t.test(grade1, grade2, var.equal = TRUE) # p is 0.8 which is greater than alpha so do not reject
# that mu1 = mu2

# two dependent sample t test (paired t test) - dependent because those two groups are the same persons
# teacher wants to test pre and post math grades
# she wants to show that the new technique creates a difference in grades
precourse.grade <- c(11,43,32,23,34,55,56,65,45,54,59,48,38,73)
postcourse.grade <- c(62,32,31,29,39,59,67,62,33,81,93,88,90,87)
diff <- precourse.grade - postcourse.grade
# normality testing
shapiro.test(diff)  # difference of these two groups is normally distributed (bcz p > alpha)

# testing H0: mean of difference = 0, H1: mean of difference not = 0
t.test(diff) # p is 0.01896 which is < alpha (0.05)  so we reject H0 (means accept H1)
# and we say that there is a statistical significant change between these two groups. and conclude
# that the mean of difference is not 0
# NOTE: null hypothesis (H0) is never accepted

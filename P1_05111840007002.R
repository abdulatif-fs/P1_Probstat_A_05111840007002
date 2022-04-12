p=0.20
n=3

#1.a
dgeom(x=n, prob=p)

#1.b
mean(rgeom(n=10000, prob=p) == 3)

#1.d
library(dplyr)
library(ggplot2)

data.frame(x = 0:10, prob = dgeom(x = 0:10, prob = p)) %>%
  mutate(Failures = ifelse(x == n, n, "other")) %>%
ggplot(aes(x = factor(x), y = prob, fill = Failures)) +
  geom_col() +
  geom_text(
    aes(label = round(prob,2), y = prob + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Peluang X = 3 gagal sebelum sukses pertama",
       subtitle = "Geometric(.2)",
       x = "gagal sebelum sukses pertama (x)",
       y = "Probabilitas")

#1.e rataan dan varians
mean = mean(rgeom(n = 10000, prob = p)) + 1
mean
var = var(rgeom(n = 100000, prob = p))
var
===============================================================================
n=20 
p=0.2 
#a. 4 pasien sembuh
x=4 
dbinom(x = x, size = n, prob = p)

#b. histogram
library(dplyr)
library(ggplot2)
#library(scales)

data.frame(heads = 0:10, prob = dbinom(x = 0:10, size = n, prob = p)) %>%
  mutate(Heads = ifelse(heads == x, "4", "other")) %>%
ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
  geom_col() +
  geom_text(
    aes(label = round(prob,4), y = prob + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Peluang terdapat 4 pasien sembuh",
       subtitle = "b(20, 0.2)",
       x = "Successes (x)",
       y = "probability") 

#Mean dan varians
mean=mean(rbinom(10000,20,0.2))
mean
var=var(rbinom(10000,20,0.2))
var

=================================================================================
#no 3 (poisson) 
#3a
lamda=4.5
p6=dpois(6,lamda)
p6

#3b
n=365

library(ggplot2)
library(dplyr)
library(tidyr)
head = 1:n
head
data.frame(head, prob =dpois(6, lamda)) %>%
  mutate(Heads = ifelse(prob == p6, "p6", "other")) %>%
ggplot(aes(x = head , y = prob, fill = Heads)) +
  geom_col() +
  geom_text(
    aes(label = round(prob,6), y = prob + 0.01),
    position = position_dodge(0.9),
    size = 2,
    vjust = 0
  ) +
  labs(title = "Peluang lahir terdapat 6 Bayi dalam setahun",
       subtitle = "p(6, lamda)",
       x = "Hari",
       y = "Jumlah Bayi") 

#3c
 data.frame(head, prob = rpois(n, lamda)) %>%
   mutate(Heads = ifelse(prob == 6, "6", "other")) %>%
 ggplot(aes(x = head , y = prob, fill = Heads)) +
   geom_col() +
   geom_text(
     aes(label = round(prob,6), y = prob + 0.01),
     position = position_dodge(0.9),
     size = 2,
     vjust = 0
   ) +
   labs(title = "Jumlah Bayi lahir tiap hari",
        subtitle = "p(6, lamda)",
        x = "Hari",
        y = "Jumlah Bayi")

#3d Mean dan varians
mean1=mean(rpois(10000,lamda))
mean1
var1=var(rpois(10000,lamda))
var1
=========================================================================


#4.
#a. peluang chi-square
x=2
v=10
dchisq(x, df=v)

#b. histogram
n=100
chi_square= rchisq(n, df=v)
hist(chi_square)

#c. rataan dan varians
mean = v
mean
var = 2*v
var

========================================================================
#5. distribusi eksponensial
#a.
n=1
#n dapat berupa nilai random
dexp(n,rate=3)

#b
set.seet(1)
hist(rexp(10, rate=3))

set.seet(1)
hist(rexp(100, rate=3))

set.seet(1)
hist(rexp(1000, rate=3))

set.seet(1)
hist(rexp(10000, rate=3))

#c. rataan dan varians
rate = 3
mean = rate
mean

var = 2*mean
var

=====================================================================
##6.distribusi normal

#a
n=100
mean=50
std=8

#merandom data
rand = rnorm(n, mean, std)

#menentukan nilai x1 dan x2
rataan=mean(rand)
rataan
x1=floor(rataan)
x1
x2=round(rataan)
x2
	

#fungsi
func = pnorm(x2, mean, std, lower.tail = TRUE) -
  pnorm(x1, mean, std, lower.tail = TRUE)
func

z_score1 <- (x1-mean)/std
z_score1
z_score2 <- (x2-mean)/std 
z_score2


#plot
plot(rand, col="blue")

#b.histogram

x <- rnorm(n, mean, std)
hist(x, breaks = 50, main = "05111840007002_Probstat_A_DNHistogram")

#varians
var(rnorm(n, mean, std))



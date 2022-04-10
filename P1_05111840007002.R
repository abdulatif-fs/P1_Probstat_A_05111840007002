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



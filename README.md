# P1_Probstat_A_05111840007002

## **1.Distribusi Geometrik**

**1.a**

![Getting Started](P1/1.a.PNG)

**1.b**

![Getting Started](P1/1.b.PNG)

**1.c**

Bila dilihat, hasil a dan b tidak begitu jauh. padahal pada bagian b, dibuat random data dan diacak. dengan demikian dapat disimpulkan nilai yang didapat menggunakan R adalah nilai yang mendekati niali sebenarnya.

**1.d**

![Getting Started](P1/1.d.png)

**1.e**

**Rataan**

![Getting Started](P1/1.e_mean.png)

 bila dihitung secara langsung, rataan dengan p=0.2 adalah 1/p
 maka 1/0.2 = 5
 dengan n= 100000 maka dicari secara acak mean dengan R, dan didapatkan nilai yang mendekati

**Varians**

![Getting Started](P1/1.e_var.png)

 bila dihitung manual, varian dari distribusi geometric adalah (1 - p) / p^2
 maka akan didaptkan var = 20
 dengan R, digunakan n=10000 dan didapatkan nilai yang mendekati


## **2.Distribusi Binomial**

**2.a**

![Getting Started](P1/2.a.png)

**2.b**

![Getting Started](P1/2.b.png)

**2.c**
**mean**

![Getting Started](P1/2.c_mean.png)

**varians**

![Getting Started](P1/2.c_var.png)

## **3.Distribusi Poisson**

**3.a**

![Getting Started](P1/3.a.png)

**3.b**

![Getting Started](P1/3.b.png)

**3.c**

bila dibandingkan, dalam satu tahun, peluang muncul lahir 6 bayi adalah sama. yaitu p6 = 0.1281201. dengan kata lain peluang lahir dengan jumlah tertentu akan sama meski diulang berkali-kali. namun bila ditinjau ulang, maka kemungkinan lahir bayi setiap harinya akan berbeda. berikut merupakan histogram dari banyaknya bayi yang akan lahir setiap harinya dalam satu tahun

![Getting Started](P1/3.c.png)

**3.d**

**mean**

![Getting Started](P1/3.c_mean.png)

**varians**

![Getting Started](P1/3.c_var.png)

## **4.Distribusi Chi-Square**

**4.a**

![Getting Started](P1/4.a.png)

**4.b**

![Getting Started](P1/4.b.png)

**4.c**
**rataan**

rataan dari chi-square sama dengan nilai kebebasan nya sehingga rataan dari chi-square = v.

![Getting Started](P1/4.c_mean.png)

**varians**

varian dari chi-square adalah 2 kali rataan/derajat kebebasan. sehingga varian dari chi-square = 2*v

![Getting Started](P1/4.c_var.png)

## 5.Distribusi Eksponensial

**5.a**

n=1
#n dapat berupa nilai random
dexp(n,rate=3)

**5.b**

n = 10

![Getting Started](P1/5.b_10.png)

n = 100

![Getting Started](P1/5.b_100.png)

n = 1000

![Getting Started](P1/5.b_1000.png)

n = 10000

![Getting Started](P1/5.b_10000.png)

**5.c**

**Rataan**

![Getting Started](P1/5.c_mean.png)

**Varians**

![Getting Started](P1/5.c_var.png)

## **6.Distribusi Normal**

**6.a**

Menentukan x1 dan x2

![Getting Started](P1/6.a_x1x2.png)

Fungsi Probabilitas dari Distribusi Normal P(X1 ??? x ??? X2)

![Getting Started](P1/6.a_func.png)

z_score dari x1 dan z_score x2

![Getting Started](P1/6.a_zscore.png)

Plot data random

![Getting Started](P1/6.a_plot.png)

**6.b Histogram**

![Getting Started](P1/6.b.png)

**6.c Varians**

![Getting Started](P1/6.c.png)
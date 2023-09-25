#Analisis Deskriptif
library(psych)
library(readxl)
Data_VER_1 <- read.csv("D:/KULIAH/TUGAS/SEMESTER 7/SKRIPSI/DATA/Data VER 1.xlsx.csv", sep = ";")
Data_JURNAL <- read.csv("D:/KULIAH/TUGAS/SEMESTER 7/Submit Jurnal/WJARR/Data.csv", sep = ";")
Data_Kel_Jurnal <- read.csv("D:/KULIAH/TUGAS/SEMESTER 7/Submit Jurnal/WJARR/Data + kelompok.csv", sep = ";")
Data_VER_2 <- Data_VER_1[,c(2:7)]
Data_VER_1
str(Data_VER_1)
Data_VER_2
describe(Data_VER_2, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1,
         type=3,check=TRUE,fast=NULL,quant=NULL,IQR=FALSE,omit=FALSE,data=NULL)
describe(Data_JURNAL, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1,
         type=3,check=TRUE,fast=NULL,quant=NULL,IQR=FALSE,omit=FALSE,data=NULL)
summary(Data_VER_2)
summary(Data_JURNAL)

#Cek UHH
boxplot(Data_VER_1$UHH) # Specify the variable to graph
shapiro.test(Data_VER_1$UHH)
mean1 <- mean(Data_VER_1$UHH)
std1 <- sd(Data_VER_1$UHH)
Kurva1 <- hist(Data_VER_1$UHH, probability = TRUE)
y1 <- dnorm(x = x, mean = mean1, sd = std1)
lines(x = x, y = y, col = "blue")
qqnorm(Data_VER_1$UHH)
qqline(Data_VER_1$UHH)

#Cek Air Minum
mean2 <- mean(Data_VER_1$AIRMINUM)
std2 <- sd(Data_VER_1$AIRMINUM)
Kurva2 <- hist(Data_VER_1$AIRMINUM, probability = TRUE)
x2 <- 0:200
y2 <- dnorm(x = x2, mean = mean2, sd = std2)
lines(x = x2, y = y2, col = "blue")
qqnorm(Data_VER_1$AIRMINUM)
qqline(Data_VER_1$AIRMINUM)
boxplot(Data_VER_1$AIRMINUM) # Specify the variable to graph
shapiro.test(Data_VER_1$AIRMINUM)

#Cek Sanitasi
mean3 <- mean(Data_VER_1$SANITASI)
std3 <- sd(Data_VER_1$SANITASI)
Kurva3 <- hist(Data_VER_1$SANITASI, probability = TRUE)
x3 <- 0:110
y3 <- dnorm(x = x3, mean = mean3, sd = std3)
lines(x = x3, y = y3, col = "blue")
qqnorm(Data_VER_1$SANITASI)
qqline(Data_VER_1$SANITASI)
boxplot(Data_VER_1$SANITASI)
shapiro.test(Data_VER_1$SANITASI)

#Cek ASI Eksklusif
mean4 <- mean(Data_VER_1$ASIEKS)
std4 <- sd(Data_VER_1$ASIEKS)
Kurva4 <- hist(Data_VER_1$ASIEKS, probability = TRUE)
x4 <- 0:110
y4 <- dnorm(x = x4, mean = mean4, sd = std4)
lines(x = x4, y = y4, col = "blue")
qqnorm(Data_VER_1$ASIEKS)
qqline(Data_VER_1$ASIEKS)
boxplot(Data_VER_1$ASIEKS)
shapiro.test(Data_VER_1$ASIEKS)

#Cek Persalinan Nakes
mean5 <- mean(Data_VER_1$PERSALINAN.NAKES)
std5 <- sd(Data_VER_1$PERSALINAN.NAKES)
Kurva5 <- hist(Data_VER_1$PERSALINAN.NAKES, probability = TRUE)
x5 <- 0:110
y5 <- dnorm(x = x5, mean = mean5, sd = std5)
lines(x = x5, y = y5, col = "blue")
qqnorm(Data_VER_1$PERSALINAN.NAKES)
qqline(Data_VER_1$PERSALINAN.NAKES)
boxplot(Data_VER_1$PERSALINAN.NAKES)
shapiro.test(Data_VER_1$PERSALINAN.NAKES)

#Cek Rasio Dokter
mean6 <- mean(Data_VER_1$RASIODOKTER)
std6 <- sd(Data_VER_1$RASIODOKTER)
Kurva6 <- hist(Data_VER_1$RASIODOKTER, probability = TRUE)
x6 <- 0:300
y6 <- dnorm(x = x6, mean = mean6, sd = std6)
lines(x = x6, y = y6, col = "blue")
qqnorm(Data_VER_1$RASIODOKTER)
qqline(Data_VER_1$RASIODOKTER)
boxplot(Data_VER_1$RASIODOKTER)
shapiro.test(Data_VER_1$RASIODOKTER)

#Analisis Regresi OLS
library(lmtest)
library(car)
library(stats)
OLS_UHH <- lm(UHH~AIRMINUM+SANITASI+ASIEKS+PERSALINAN.NAKES+RASIODOKTER, data = Data_VER_1)
OLS_Jurnal <- lm(UHH~SANITASI+ASIEKS+RASIO.POSYANDU.AKTIF.PER.100.BALITA+X.IMUNISASI.DASAR.LENGKAP+X.PERKAPITA.MAKANAN, data = Data_JURNAL)
summary(OLS_UHH)
summary(OLS_Jurnal)
sse <- sum((fitted(OLS_UHH) - Data_VER_1$UHH)^2) #mencari SSE
sse
ssr <- sum((fitted(OLS_UHH) - mean(Data_VER_1$UHH))^2) #mencari SSR
ssr
sst <- sse + ssr #mencari SST
sst
mse <- sse/32
mse
msr <- ssr/5
msr
Fcritic <- qf(p=.05, df1=5, df2=32, lower.tail=FALSE)
Fcritic
Tcritic <- qt(p=.05/2, df=32, lower.tail=FALSE)
Tcritic
AIC(OLS_UHH)
AIC(OLS_Jurnal)
logLik(OLS_UHH)
2.8967/1.4563

#Membentuk Residual
Res_UHH <- resid(OLS_UHH)
Res_UHH
plot(fitted(OLS_UHH), Res_UHH)
abline(0,0)
qqnorm(Res_UHH)
qqline(Res_UHH)

Res_Jurnal <- resid(OLS_Jurnal)
Res_Jurnal
plot(fitted(OLS_Jurnal), Res_Jurnal)
abline(0,0)
qqnorm(Res_Jurnal)
qqline(Res_Jurnal)

#Uji Normalitas Error/Residual
shapiro.test(Res_UHH)
shapiro.test(Res_Jurnal)

#Uji Multikolinearitas
vif(OLS_UHH)
vif(OLS_Jurnal)

#Uji Heterogenitas Spasial
library(spgwr)
library(sp)
library(spData)
bptest(OLS_UHH, studentize = FALSE)
bptest.sarlm(OLS_UHH)
bptest(OLS_Jurnal, studentize = FALSE)

#Uji Autokorelasi
durbinWatsonTest(OLS_UHH)
durbinWatsonTest(OLS_Jurnal)

#Uji Autokorelasi Spasial
library(sf)
library(spdep)
library(tmap)
nb1 <- poly2nb(Map_Jatim, queen=TRUE)
nb2 <- poly2nb(Map_Jatim_Jurnal, queen=TRUE)
lw1 <- nb2listw(nb, style="W", zero.policy=TRUE)
lw2 <- nb2listw(nb2, style="W", zero.policy=TRUE)
moran.test(Map_Jatim$UHH,lw1, alternative="two.sided")
moran.test(Map_Jatim_Jurnal$UHH,lw, alternative="two.sided")
nb [18]
view(Map_Jatim)

#Coba File GEODA
Geoda <- read_sf("D:/KULIAH/TUGAS/SEMESTER 7/SKRIPSI/DATA/Coba Moran dari Geoda/Coba import Moran.shp")
nb2 <- poly2nb(Geoda, queen=TRUE)
lw2 <- nb2listw(nb, style="W", zero.policy=TRUE)
moran.test(Geoda$UHH ,lw, alternative="two.sided")
MC<- moran.mc(Geoda$UHH, lw, nsim=999, alternative="two.sided")
MC


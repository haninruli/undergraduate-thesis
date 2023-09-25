library(spgwr)
#Jarak Euclidean
euclid_mat <- dist(Data_VER_1[,c("LATITUDE","LONGITUDE")],method = "euclidean")
euclid_mat_Jurnal <- dist(Data_JURNAL[,c("LATITUDE","LONGITUDE")],method = "euclidean")
euclid_mat
options(max.print=100000)

#PEMBOBOT FIXED GAUSSIAN
#Bandwidth Optimal
Band_FixedG <- gwr.sel(UHH~AIRMINUM+SANITASI+ASIEKS+PERSALINAN.NAKES+RASIODOKTER, coords = cbind(Data_VER_1$LATITUDE, Data_VER_1$LONGITUDE), data = Data_VER_1, adapt = FALSE, gweight = gwr.Gauss)

#Matriks Pembobot Fixed Gaussian
Bobot_FixedG <- exp(-0.5*((euclid_mat/GWR_FixedG$bandwidth)^2))
Bobot_FixedG

#Estimasi Parameter Pembobot Fixed Gaussian
GWR_FixedG <- gwr(UHH~AIRMINUM+SANITASI+ASIEKS+PERSALINAN.NAKES+RASIODOKTER, coords = cbind(Data_VER_1$LATITUDE, Data_VER_1$LONGITUDE), bandwidth = Band_FixedG , data = Data_VER_1, hatmatrix=TRUE, gweight = gwr.Gauss)
GWR_FixedG
names(GWR_FixedG)
names(GWR_FixedG$SDF)

#Koefisien 
GWR_FixedG$SDF$"(Intercept)"
GWR_FixedG$SDF$"AIRMINUM"
GWR_FixedG$SDF$"SANITASI"
GWR_FixedG$SDF$"ASIEKS"
GWR_FixedG$SDF$"PERSALINAN.NAKES"
GWR_FixedG$SDF$"RASIODOKTER"
GWR_FixedG$SDF$"localR2"
GWR_FixedG$SDF$"AIRMINUM_se"

#t Hitung
thit_inter1 <- GWR_FixedG$SDF$"(Intercept)"/GWR_FixedG$SDF$"(Intercept)_se"
thit_inter1
thit_air1 <- GWR_FixedG$SDF$"AIRMINUM"/GWR_FixedG$SDF$"AIRMINUM_se"
thit_air1
thit_sani1 <- GWR_FixedG$SDF$"SANITASI"/GWR_FixedG$SDF$"SANITASI_se"
thit_sani1
thit_asi1 <- GWR_FixedG$SDF$"ASIEKS"/GWR_FixedG$SDF$"ASIEKS_se"
thit_asi1
thit_persalinan1 <- GWR_FixedG$SDF$"PERSALINAN.NAKES"/GWR_FixedG$SDF$"PERSALINAN.NAKES_se"
thit_persalinan1
thit_dokter1 <- GWR_FixedG$SDF$"RASIODOKTER"/GWR_FixedG$SDF$"RASIODOKTER_se"
thit_dokter1
Tcritic <- qt(p=.05/2, df=32, lower.tail=FALSE)
Tcritic

#Uji Kecocokan Model
BFC02.gwr.test(GWR_FixedG)
var.test(GWR_FixedG)#coba
anova.gwr(GWR_FixedG)#coba
LMZ.F1GWR.test(GWR_FixedG)#coba
LMZ.F2GWR.test(GWR_FixedG)#coba
BFC99.gwr.test(GWR_FixedG)

#Uji Pengaruh Geografis terhadap Setiap Prediktor
LMZ.F3GWR.test(GWR_FixedG)

#Melihat Nilai Bandwidth
GWR_FixedG$bandwidth

#Uji Goodness Of Fit Pembobot Fixed Gaussian
anova.gwr(GWR_FixedG)
ftabelgwr1 <- qf(.05, df1=5.764, df2=26.236, lower.tail = FALSE)
ftabelgwr1 <- qf(.05, df1=24.145, df2=29.457, lower.tail = FALSE)
ftabelgwr1

#PEMBOBOT FIXED BI-SQUARE
#Bandwidth Optimal
Band_FixedB <- gwr.sel(UHH~AIRMINUM+SANITASI+ASIEKS+PERSALINAN.NAKES+RASIODOKTER, coords = cbind(Data_VER_1$LATITUDE, Data_VER_1$LONGITUDE), data = Data_VER_1, adapt = FALSE, gweight = gwr.bisquare)

#Matriks Pembobot Fixed Bi-Square
Bobot_FixedB <- (1-((euclid_mat/GWR_FixedB$bandwidth)^2))^2
Bobot_FixedB
print(euclid_mat <= GWR_FixedB$bandwidth)

#Estimasi Parameter Pembobot Fixed Bi-Square
GWR_FixedB <- gwr(UHH~AIRMINUM+SANITASI+ASIEKS+PERSALINAN.NAKES+RASIODOKTER, coords = cbind(Data_VER_1$LATITUDE, Data_VER_1$LONGITUDE), bandwidth = Band_FixedB , data = Data_VER_1, hatmatrix=TRUE, gweight = gwr.bisquare)
GWR_FixedB
names(GWR_FixedB)
names(GWR_FixedB$SDF)

#Koefisien 
GWR_FixedB$SDF$"(Intercept)"
GWR_FixedB$SDF$"AIRMINUM"
GWR_FixedB$SDF$"SANITASI"
GWR_FixedB$SDF$"ASIEKS"
GWR_FixedB$SDF$"PERSALINAN.NAKES"
GWR_FixedB$SDF$"RASIODOKTER"
GWR_FixedB$SDF$"localR2"
GWR_FixedB$SDF$"AIRMINUM_se"

#t Hitung
thit_inter2 <- GWR_FixedB$SDF$"(Intercept)"/GWR_FixedB$SDF$"(Intercept)_se"
thit_inter2
thit_air2 <- GWR_FixedB$SDF$"AIRMINUM"/GWR_FixedB$SDF$"AIRMINUM_se"
thit_air2
thit_sani2 <- GWR_FixedB$SDF$"SANITASI"/GWR_FixedB$SDF$"SANITASI_se"
thit_sani2
thit_asi2 <- GWR_FixedB$SDF$"ASIEKS"/GWR_FixedB$SDF$"ASIEKS_se"
thit_asi2
thit_persalinan2 <- GWR_FixedB$SDF$"PERSALINAN.NAKES"/GWR_FixedB$SDF$"PERSALINAN.NAKES_se"
thit_persalinan2
thit_dokter2 <- GWR_FixedB$SDF$"RASIODOKTER"/GWR_FixedB$SDF$"RASIODOKTER_se"
thit_dokter2
Tcritic <- qt(p=.05/2, df=32, lower.tail=FALSE)
Tcritic

#Uji Kecocokan Model
BFC02.gwr.test(GWR_FixedB)
LMZ.F1GWR.test(GWR_FixedB)#coba
LMZ.F2GWR.test(GWR_FixedB)#coba
BFC99.gwr.test(GWR_FixedB)


#Uji Goodness Of Fit Pembobot Fixed Bi-Square
anova.gwr(GWR_FixedB)
ftabelgwr2 <- qf(.05, df1=20.729, df2=29.552, lower.tail = FALSE)
ftabelgwr2

#Uji Pengaruh Geografis terhadap Setiap Prediktor
LMZ.F3GWR.test(GWR_FixedB)

#Melihat Nilai Bandwidth
GWR_FixedB$bandwidth

#PEMBOBOT ADAPTIVE GAUSSIAN
#Bandwidth Optimal
Band_AdaptG <- gwr.sel(UHH~AIRMINUM+SANITASI+ASIEKS+PERSALINAN.NAKES+RASIODOKTER, coords = cbind(Data_VER_1$LATITUDE, Data_VER_1$LONGITUDE), data = Data_VER_1, adapt = TRUE, gweight = gwr.Gauss)

#Matriks Pembobot Adaptive Gaussian
BAG1 <- 1.5449895
BAG2 <- 1.1448205
BAG3 <- 0.9312966
BAG4 <- 0.9160695
BAG5 <- 1.0030834
BAG6 <- 0.7677337
BAG7 <- 0.9262731
BAG8 <- 1.1452119
BAG9 <- 1.4808960
BAG10 <- 2.0680959
BAG11 <- 1.4511918
BAG12 <- 1.5795179
BAG13 <- 1.0269357
BAG14 <- 0.8666750
BAG15 <- 0.8306823
BAG16 <- 0.7019146
BAG17 <- 0.6801434
BAG18 <- 0.8204378
BAG19 <- 0.9185804
BAG20 <- 1.2100711
BAG21 <- 1.1640089
BAG22 <- 0.9160695
BAG23 <- 1.1295951
BAG24 <- 1.0089598
BAG25 <- 0.8723264
BAG26 <- 1.0325877
BAG27 <- 1.0684842
BAG28 <- 1.1932553
BAG29 <- 1.5612977
BAG30 <- 0.7751075
BAG31 <- 0.9454717
BAG32 <- 0.8218051
BAG33 <- 0.9842364
BAG34 <- 0.8703615
BAG35 <- 0.6975604
BAG36 <- 1.0260139
BAG37 <- 0.8998186
BAG38 <- 0.7605788
Bobot_AdaptG1 <- exp(-0.5*((euclid_mat/BAG1)^2))
Bobot_AdaptG1
Bobot_AdaptG2 <- exp(-0.5*((euclid_mat/BAG2)^2))
Bobot_AdaptG2
Bobot_AdaptG3 <- exp(-0.5*((euclid_mat/BAG3)^2))
Bobot_AdaptG3
Bobot_AdaptG4 <- exp(-0.5*((euclid_mat/BAG4)^2))
Bobot_AdaptG4
Bobot_AdaptG5 <- exp(-0.5*((euclid_mat/BAG5)^2))
Bobot_AdaptG5
Bobot_AdaptG6 <- exp(-0.5*((euclid_mat/BAG6)^2))
Bobot_AdaptG6
Bobot_AdaptG7 <- exp(-0.5*((euclid_mat/BAG7)^2))
Bobot_AdaptG7
Bobot_AdaptG8 <- exp(-0.5*((euclid_mat/BAG8)^2))
Bobot_AdaptG8
Bobot_AdaptG9 <- exp(-0.5*((euclid_mat/BAG9)^2))
Bobot_AdaptG9
Bobot_AdaptG10 <- exp(-0.5*((euclid_mat/BAG10)^2))
Bobot_AdaptG10
Bobot_AdaptG11 <- exp(-0.5*((euclid_mat/BAG11)^2))
Bobot_AdaptG11
Bobot_AdaptG12 <- exp(-0.5*((euclid_mat/BAG12)^2))
Bobot_AdaptG12
Bobot_AdaptG13 <- exp(-0.5*((euclid_mat/BAG13)^2))
Bobot_AdaptG13
Bobot_AdaptG14 <- exp(-0.5*((euclid_mat/BAG14)^2))
Bobot_AdaptG14
Bobot_AdaptG15 <- exp(-0.5*((euclid_mat/BAG15)^2))
Bobot_AdaptG15
Bobot_AdaptG16 <- exp(-0.5*((euclid_mat/BAG16)^2))
Bobot_AdaptG16
Bobot_AdaptG17 <- exp(-0.5*((euclid_mat/BAG17)^2))
Bobot_AdaptG17
Bobot_AdaptG18 <- exp(-0.5*((euclid_mat/BAG18)^2))
Bobot_AdaptG18
Bobot_AdaptG19 <- exp(-0.5*((euclid_mat/BAG19)^2))
Bobot_AdaptG19
Bobot_AdaptG20 <- exp(-0.5*((euclid_mat/BAG20)^2))
Bobot_AdaptG20
Bobot_AdaptG21 <- exp(-0.5*((euclid_mat/BAG21)^2))
Bobot_AdaptG21
Bobot_AdaptG22 <- exp(-0.5*((euclid_mat/BAG22)^2))
Bobot_AdaptG22
Bobot_AdaptG23 <- exp(-0.5*((euclid_mat/BAG23)^2))
Bobot_AdaptG23
Bobot_AdaptG24 <- exp(-0.5*((euclid_mat/BAG24)^2))
Bobot_AdaptG24
Bobot_AdaptG25 <- exp(-0.5*((euclid_mat/BAG25)^2))
Bobot_AdaptG25
Bobot_AdaptG26 <- exp(-0.5*((euclid_mat/BAG26)^2))
Bobot_AdaptG26
Bobot_AdaptG27 <- exp(-0.5*((euclid_mat/BAG27)^2))
Bobot_AdaptG27
Bobot_AdaptG28 <- exp(-0.5*((euclid_mat/BAG28)^2))
Bobot_AdaptG28
Bobot_AdaptG29 <- exp(-0.5*((euclid_mat/BAG29)^2))
Bobot_AdaptG29
Bobot_AdaptG30 <- exp(-0.5*((euclid_mat/BAG30)^2))
Bobot_AdaptG30
Bobot_AdaptG31 <- exp(-0.5*((euclid_mat/BAG31)^2))
Bobot_AdaptG31
Bobot_AdaptG32 <- exp(-0.5*((euclid_mat/BAG32)^2))
Bobot_AdaptG32
Bobot_AdaptG33 <- exp(-0.5*((euclid_mat/BAG33)^2))
Bobot_AdaptG33
Bobot_AdaptG34 <- exp(-0.5*((euclid_mat/BAG34)^2))
Bobot_AdaptG34
Bobot_AdaptG35 <- exp(-0.5*((euclid_mat/BAG35)^2))
Bobot_AdaptG35
Bobot_AdaptG36 <- exp(-0.5*((euclid_mat/BAG36)^2))
Bobot_AdaptG36
Bobot_AdaptG37 <- exp(-0.5*((euclid_mat/BAG37)^2))
Bobot_AdaptG37
Bobot_AdaptG38 <- exp(-0.5*((euclid_mat/BAG38)^2))
Bobot_AdaptG38

#Estimasi Parameter Pembobot Adaptive Gaussian
GWR_AdaptG <- gwr(UHH~AIRMINUM+SANITASI+ASIEKS+PERSALINAN.NAKES+RASIODOKTER, 
               coords=cbind(Data_VER_1$LATITUDE, Data_VER_1$LONGITUDE),
               data=Data_VER_1, adapt=Band_AdaptG,hatmatrix=TRUE,gweight=gwr.Gauss)
GWR_AdaptG
names(GWR_AdaptG)
names(GWR_AdaptG$SDF)

#Koefisien 
GWR_AdaptG$SDF$"(Intercept)"
GWR_AdaptG$SDF$"AIRMINUM"
GWR_AdaptG$SDF$"SANITASI"
GWR_AdaptG$SDF$"ASIEKS"
GWR_AdaptG$SDF$"PERSALINAN.NAKES"
GWR_AdaptG$SDF$"RASIODOKTER"
GWR_AdaptG$SDF$"localR2"
GWR_AdaptB$SDF$"AIRMINUM_se"

#t Hitung
thit_inter3 <- GWR_AdaptG$SDF$"(Intercept)"/GWR_AdaptG$SDF$"(Intercept)_se"
thit_inter3
thit_air3 <- GWR_AdaptG$SDF$"AIRMINUM"/GWR_AdaptG$SDF$"AIRMINUM_se"
thit_air3
thit_sani3 <- GWR_AdaptG$SDF$"SANITASI"/GWR_AdaptG$SDF$"SANITASI_se"
thit_sani3
thit_asi3 <- GWR_AdaptG$SDF$"ASIEKS"/GWR_AdaptG$SDF$"ASIEKS_se"
thit_asi3
thit_persalinan3 <- GWR_AdaptG$SDF$"PERSALINAN.NAKES"/GWR_AdaptG$SDF$"PERSALINAN.NAKES_se"
thit_persalinan3
thit_dokter3 <- GWR_AdaptG$SDF$"RASIODOKTER"/GWR_AdaptG$SDF$"RASIODOKTER_se"
thit_dokter3
Tcritic <- qt(p=.05/2, df=32, lower.tail=FALSE)
Tcritic

#Uji Kecocokan Model
BFC02.gwr.test(GWR_AdaptG)
BFC99.gwr.test(GWR_AdaptG)

#Uji Goodness Of Fit Pembobot Adaptive Gaussian
anova.gwr(GWR_AdaptG)
ftabelgwr3 <- qf(.05, df1=31.302, df2=30.363, lower.tail = FALSE)
ftabelgwr3

#Uji Pengaruh Geografis terhadap Setiap Prediktor
LMZ.F3GWR.test(GWR_AdaptG)

#Melihat Nilai Bandwidth
GWR_AdaptG$bandwidth

#PEMBOBOT ADAPTIVE BI-SQUARE
#Bandwidth Optimal
Band_AdaptB <- gwr.sel(UHH~AIRMINUM+SANITASI+ASIEKS+PERSALINAN.NAKES+RASIODOKTER, coords = cbind(Data_VER_1$LATITUDE, Data_VER_1$LONGITUDE), data = Data_VER_1, adapt = TRUE, gweight = gwr.bisquare)

#Matriks Pembobot Adaptive B
BAB1 <- 2.999151
BAB2 <- 2.548824
BAB3 <- 2.388723
BAB4 <- 2.224841
BAB5 <- 1.991808
BAB6 <- 1.988547
BAB7 <- 1.715035
BAB8 <- 1.954656
BAB9 <- 2.427833
BAB10 <- 3.089657
BAB11 <- 2.505921
BAB12 <- 2.673179
BAB13 <- 2.089477
BAB14 <- 1.576072
BAB15 <- 1.776069
BAB16 <- 1.587465
BAB17 <- 1.773869
BAB18 <- 2.102708
BAB19 <- 2.354235
BAB20 <- 2.673230
BAB21 <- 2.574827
BAB22 <- 2.192544
BAB23 <- 2.099975
BAB24 <- 1.692023
BAB25 <- 1.808909
BAB26 <- 1.992143
BAB27 <- 1.964921
BAB28 <- 2.200220
BAB29 <- 2.614473
BAB30 <- 2.005110
BAB31 <- 2.015902
BAB32 <- 1.561361
BAB33 <- 1.887623
BAB34 <- 1.579459
BAB35 <- 1.579681
BAB36 <- 2.482775
BAB37 <- 1.884593
BAB38 <- 1.597431
Bobot_AdaptB1 <- (1-((euclid_mat/BAB1)^2))^2
Bobot_AdaptB1
Bobot_AdaptB2 <- (1-((euclid_mat/BAB2)^2))^2
Bobot_AdaptB2
Bobot_AdaptB3 <- (1-((euclid_mat/BAB3)^2))^2
Bobot_AdaptB3
Bobot_AdaptB4 <- (1-((euclid_mat/BAB4)^2))^2
Bobot_AdaptB4
Bobot_AdaptB5 <- (1-((euclid_mat/BAB5)^2))^2
Bobot_AdaptB5
Bobot_AdaptB6 <- (1-((euclid_mat/BAB6)^2))^2
Bobot_AdaptB6
Bobot_AdaptB7 <- (1-((euclid_mat/BAB7)^2))^2
Bobot_AdaptB7
Bobot_AdaptB8 <- (1-((euclid_mat/BAB8)^2))^2
Bobot_AdaptB8
Bobot_AdaptB9 <- (1-((euclid_mat/BAB9)^2))^2
Bobot_AdaptB9
Bobot_AdaptB10 <- (1-((euclid_mat/BAB10)^2))^2
Bobot_AdaptB10
Bobot_AdaptB11 <- (1-((euclid_mat/BAB11)^2))^2
Bobot_AdaptB11
Bobot_AdaptB12 <- (1-((euclid_mat/BAB12)^2))^2
Bobot_AdaptB12
Bobot_AdaptB13 <- (1-((euclid_mat/BAB13)^2))^2
Bobot_AdaptB13
Bobot_AdaptB14 <- (1-((euclid_mat/BAB14)^2))^2
Bobot_AdaptB14
Bobot_AdaptB15 <- (1-((euclid_mat/BAB15)^2))^2
Bobot_AdaptB15
Bobot_AdaptB16 <- (1-((euclid_mat/BAB16)^2))^2
Bobot_AdaptB16
Bobot_AdaptB17 <- (1-((euclid_mat/BAB17)^2))^2
Bobot_AdaptB17
Bobot_AdaptB18 <- (1-((euclid_mat/BAB18)^2))^2
Bobot_AdaptB18
Bobot_AdaptB19 <- (1-((euclid_mat/BAB19)^2))^2
Bobot_AdaptB19
Bobot_AdaptB20 <- (1-((euclid_mat/BAB20)^2))^2
Bobot_AdaptB20
Bobot_AdaptB21 <- (1-((euclid_mat/BAB21)^2))^2
Bobot_AdaptB21
Bobot_AdaptB22 <- (1-((euclid_mat/BAB22)^2))^2
Bobot_AdaptB22
Bobot_AdaptB23 <- (1-((euclid_mat/BAB23)^2))^2
Bobot_AdaptB23
Bobot_AdaptB24 <- (1-((euclid_mat/BAB24)^2))^2
Bobot_AdaptB24
Bobot_AdaptB25 <- (1-((euclid_mat/BAB25)^2))^2
Bobot_AdaptB25
Bobot_AdaptB26 <- (1-((euclid_mat/BAB26)^2))^2
Bobot_AdaptB26
Bobot_AdaptB27 <- (1-((euclid_mat/BAB27)^2))^2
Bobot_AdaptB27
Bobot_AdaptB28 <- (1-((euclid_mat/BAB28)^2))^2
Bobot_AdaptB28
Bobot_AdaptB29 <- (1-((euclid_mat/BAB29)^2))^2
Bobot_AdaptB29
Bobot_AdaptB30 <- (1-((euclid_mat/BAB30)^2))^2
Bobot_AdaptB30
Bobot_AdaptB31 <- (1-((euclid_mat/BAB31)^2))^2
Bobot_AdaptB31
Bobot_AdaptB32 <- (1-((euclid_mat/BAB32)^2))^2
Bobot_AdaptB32
Bobot_AdaptB33 <- (1-((euclid_mat/BAB33)^2))^2
Bobot_AdaptB33
Bobot_AdaptB34 <- (1-((euclid_mat/BAB34)^2))^2
Bobot_AdaptB34
Bobot_AdaptB35 <- (1-((euclid_mat/BAB35)^2))^2
Bobot_AdaptB35
Bobot_AdaptB36 <- (1-((euclid_mat/BAB36)^2))^2
Bobot_AdaptB36
Bobot_AdaptB37 <- (1-((euclid_mat/BAB37)^2))^2
Bobot_AdaptB37
Bobot_AdaptB38 <- (1-((euclid_mat/BAB38)^2))^2
Bobot_AdaptB38

#Estimasi Parameter Pembobot Adaptive Bi-Square
GWR_AdaptB <- gwr(UHH~AIRMINUM+SANITASI+ASIEKS+PERSALINAN.NAKES+RASIODOKTER, 
                  coords=cbind(Data_VER_1$LATITUDE, Data_VER_1$LONGITUDE),
                  data=Data_VER_1, adapt=Band_AdaptB,hatmatrix=TRUE,gweight=gwr.bisquare)
GWR_AdaptB
names(GWR_AdaptB)
names(GWR_AdaptB$SDF)
gwr

#Koefisien 
GWR_AdaptB$SDF$"(Intercept)"
GWR_AdaptB$SDF$"AIRMINUM"
GWR_AdaptB$SDF$"SANITASI"
GWR_AdaptB$SDF$"ASIEKS"
GWR_AdaptB$SDF$"PERSALINAN.NAKES"
GWR_AdaptB$SDF$"RASIODOKTER"
GWR_AdaptB$SDF$"localR2"

#t Hitung
thit_inter4 <- GWR_AdaptB$SDF$"(Intercept)"/GWR_AdaptB$SDF$"(Intercept)_se"
thit_inter4
thit_air4 <- GWR_AdaptB$SDF$"AIRMINUM"/GWR_AdaptB$SDF$"AIRMINUM_se"
thit_air4
thit_sani4 <- GWR_AdaptB$SDF$"SANITASI"/GWR_AdaptB$SDF$"SANITASI_se"
thit_sani4
thit_asi4 <- GWR_AdaptB$SDF$"ASIEKS"/GWR_AdaptB$SDF$"ASIEKS_se"
thit_asi4
thit_persalinan4 <- GWR_AdaptB$SDF$"PERSALINAN.NAKES"/GWR_AdaptB$SDF$"PERSALINAN.NAKES_se"
thit_persalinan4
thit_dokter4 <- GWR_AdaptB$SDF$"RASIODOKTER"/GWR_AdaptB$SDF$"RASIODOKTER_se"
thit_dokter4
Tcritic <- qt(p=.05/2, df=32, lower.tail=FALSE)
Tcritic

#Uji Kecocokan Model
BFC02.gwr.test(GWR_AdaptB)
BFC99.gwr.test(GWR_AdaptB)

#Uji Goodness Of Fit Pembobot Adaptive Bi-Square
anova.gwr(GWR_AdaptB)
ftabelgwr4 <- qf(.05, df1=28.556, df2=29.210, lower.tail = FALSE)
ftabelgwr4

#Uji Pengaruh Geografis terhadap Setiap Prediktor
LMZ.F3GWR.test(GWR_AdaptB)

#Melihat Nilai Bandwidth
GWR_AdaptB$bandwidth

#UNTUK JURNAL
#PEMBOBOT FIXED TRICUBE
#Bandwidth Optimal
Band_FixedT <- gwr.sel(UHH~SANITASI+ASIEKS+RASIO.POSYANDU.AKTIF.PER.100.BALITA+X.IMUNISASI.DASAR.LENGKAP+X.PERKAPITA.MAKANAN, coords = cbind(Data_JURNAL$LATITUDE, Data_JURNAL$LONGITUDE), data = Data_JURNAL, adapt = FALSE, gweight = gwr.tricube)

Bobot_FixedT <- ((1-((euclid_mat_Jurnal/GWR_FixedT$bandwidth)^3))^3)
Bobot_FixedT

#Estimasi Parameter Pembobot Fixed Tricube
GWR_FixedT <- gwr(UHH~SANITASI+ASIEKS+RASIO.POSYANDU.AKTIF.PER.100.BALITA+X.IMUNISASI.DASAR.LENGKAP+X.PERKAPITA.MAKANAN, coords = cbind(Data_JURNAL$LATITUDE, Data_JURNAL$LONGITUDE), bandwidth = Band_FixedT , data = Data_JURNAL, hatmatrix=TRUE, gweight = gwr.tricube)
GWR_FixedT
names(GWR_FixedT)
names(GWR_FixedT$SDF)

#Koefisien 
GWR_FixedT$SDF$"(Intercept)"
GWR_FixedT$SDF$"SANITASI"
GWR_FixedT$SDF$"ASIEKS"
GWR_FixedT$SDF$"X.PERKAPITA.MAKANAN"
GWR_FixedT$SDF$"localR2"

#t Hitung
thit_inter5 <- GWR_FixedT$SDF$"(Intercept)"/GWR_FixedT$SDF$"(Intercept)_se"
thit_inter5
thit_sani5 <- GWR_FixedT$SDF$"SANITASI"/GWR_FixedT$SDF$"SANITASI_se"
thit_sani5
thit_asi5 <- GWR_FixedT$SDF$"ASIEKS"/GWR_FixedT$SDF$"ASIEKS_se"
thit_asi5
thit_posyandu <- GWR_FixedT$SDF$"RASIO.POSYANDU.AKTIF.PER.100.BALITA"/GWR_FixedT$SDF$"RASIO.POSYANDU.AKTIF.PER.100.BALITA_se"
thit_posyandu
thit_imunisasi <- GWR_FixedT$SDF$"X.IMUNISASI.DASAR.LENGKAP"/GWR_FixedT$SDF$"X.IMUNISASI.DASAR.LENGKAP_se"
thit_imunisasi
thit_perkapita <- GWR_FixedT$SDF$"X.PERKAPITA.MAKANAN"/GWR_FixedT$SDF$"X.PERKAPITA.MAKANAN_se"
thit_perkapita
Tcritic <- qt(p=.05/2, df=32, lower.tail=FALSE)
Tcritic

#Uji Goodness Of Fit Pembobot Fixed Tricube
anova.gwr(GWR_FixedT)
BFC99.gwr.test(GWR_FixedT)
ftabelgwr5 <- qf(.05, df1=14.293, df2=30.716, lower.tail = FALSE)
ftabelgwr5

#PEMBOBOT FIXED GAUSSIAN
#Bandwidth Optimal
Band_FixedG2 <- gwr.sel(UHH~SANITASI+ASIEKS+RASIO.POSYANDU.AKTIF.PER.100.BALITA+X.IMUNISASI.DASAR.LENGKAP+X.PERKAPITA.MAKANAN, 
                       coords = cbind(Data_JURNAL$LATITUDE, Data_JURNAL$LONGITUDE), data = Data_JURNAL, adapt = FALSE, gweight = gwr.Gauss)

#Estimasi Parameter Pembobot Fixed Gaussian
GWR_FixedG2 <- gwr(UHH~SANITASI+ASIEKS+RASIO.POSYANDU.AKTIF.PER.100.BALITA+
                    X.IMUNISASI.DASAR.LENGKAP+X.PERKAPITA.MAKANAN, 
                  coords=cbind(Data_JURNAL$LATITUDE, Data_JURNAL$LONGITUDE),
                bandwidth = Band_FixedG2 , data = Data_JURNAL, hatmatrix=TRUE, gweight = gwr.Gauss)
GWR_FixedG2
names(GWR_FixedG)
names(GWR_FixedG$SDF)

#t Hitung
thit_inter6 <- GWR_FixedG2$SDF$"(Intercept)"/GWR_FixedG2$SDF$"(Intercept)_se"
thit_inter6
thit_sani6 <- GWR_FixedG2$SDF$"SANITASI"/GWR_FixedG2$SDF$"SANITASI_se"
thit_sani6
thit_asi6 <- GWR_FixedG2$SDF$"ASIEKS"/GWR_FixedG2$SDF$"ASIEKS_se"
thit_asi6
thit_posyandu2 <- GWR_FixedG2$SDF$"RASIO.POSYANDU.AKTIF.PER.100.BALITA"/GWR_FixedG2$SDF$"RASIO.POSYANDU.AKTIF.PER.100.BALITA_se"
thit_posyandu2
thit_imunisasi2 <- GWR_FixedG2$SDF$"X.IMUNISASI.DASAR.LENGKAP"/GWR_FixedG2$SDF$"X.IMUNISASI.DASAR.LENGKAP_se"
thit_imunisasi2
thit_perkapita2 <- GWR_FixedG2$SDF$"X.PERKAPITA.MAKANAN"/GWR_FixedG2$SDF$"X.PERKAPITA.MAKANAN_se"
thit_perkapita2
Tcritic <- qt(p=.05/2, df=32, lower.tail=FALSE)
Tcritic

#Uji Goodness Of Fit Pembobot Fixed Tricube
anova.gwr(GWR_FixedG2)
BFC99.gwr.test(GWR_FixedG2)
ftabelgwr6 <- qf(.05, df1=23.067, df2=30.260, lower.tail = FALSE)
ftabelgwr6

#PEMBOBOT FIXED BI-SQUARE
#Bandwidth Optimal
Band_FixedB2 <- gwr.sel(UHH~SANITASI+ASIEKS+RASIO.POSYANDU.AKTIF.PER.100.BALITA+X.IMUNISASI.DASAR.LENGKAP+X.PERKAPITA.MAKANAN, 
                        coords = cbind(Data_JURNAL$LATITUDE, Data_JURNAL$LONGITUDE), data = Data_JURNAL, adapt = FALSE, gweight = gwr.bisquare)

#Estimasi Parameter Pembobot Fixed Bi-Square
GWR_FixedB2 <- gwr(UHH~SANITASI+ASIEKS+RASIO.POSYANDU.AKTIF.PER.100.BALITA+
                     X.IMUNISASI.DASAR.LENGKAP+X.PERKAPITA.MAKANAN, 
                   coords=cbind(Data_JURNAL$LATITUDE, Data_JURNAL$LONGITUDE),
                   bandwidth = Band_FixedB2 , data = Data_JURNAL, hatmatrix=TRUE, gweight = gwr.bisquare)
GWR_FixedG2
names(GWR_FixedG)
names(GWR_FixedG$SDF)

#t Hitung
thit_inter7 <- GWR_FixedB2$SDF$"(Intercept)"/GWR_FixedB2$SDF$"(Intercept)_se"
thit_inter7
thit_sani7 <- GWR_FixedB2$SDF$"SANITASI"/GWR_FixedB2$SDF$"SANITASI_se"
thit_sani7
thit_asi7 <- GWR_FixedB2$SDF$"ASIEKS"/GWR_FixedB2$SDF$"ASIEKS_se"
thit_asi7
thit_posyandu3 <- GWR_FixedB2$SDF$"RASIO.POSYANDU.AKTIF.PER.100.BALITA"/GWR_FixedB2$SDF$"RASIO.POSYANDU.AKTIF.PER.100.BALITA_se"
thit_posyandu3
thit_imunisasi3 <- GWR_FixedB2$SDF$"X.IMUNISASI.DASAR.LENGKAP"/GWR_FixedB2$SDF$"X.IMUNISASI.DASAR.LENGKAP_se"
thit_imunisasi3
thit_perkapita3 <- GWR_FixedB2$SDF$"X.PERKAPITA.MAKANAN"/GWR_FixedB2$SDF$"X.PERKAPITA.MAKANAN_se"
thit_perkapita3
Tcritic <- qt(p=.05/2, df=32, lower.tail=FALSE)
Tcritic

#Uji Goodness Of Fit Pembobot Fixed Bi-Square
anova.gwr(GWR_FixedB2)
BFC99.gwr.test(GWR_FixedB2)
ftabelgwr6 <- qf(.05, df1=19.688, df2=30.387, lower.tail = FALSE)
ftabelgwr6

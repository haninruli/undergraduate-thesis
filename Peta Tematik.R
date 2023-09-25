library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(raster)
library(sf)
library(sp)
library(scales)
library(ggsn)
library(plotly)
library(dbscan)
library(rgdal)
library(spatialreg)
library(spatial)
library(mapview)
library(dplyr)
library(tidyselect)
library(tidyr)
library(rgeos)
UHH_sf <- st_as_sf((Data_VER_1), coords = c("LONGITUDE","LATITUDE"), crs = 4326)
class(UHH_sf)
Nama_SHP_Indonesia <- "D:/KULIAH/TUGAS/SEMESTER 7/SKRIPSI/DATA/gadm41_IDN_shp/gadm41_IDN_2.shp"
Map_Indonesia <- st_read(Nama_SHP_Indonesia)
Jatim <- Map_Indonesia %>%
    subset(Map_Indonesia$NAME_1 == "Jawa Timur")
Jatim$NAME_2
Jatim
map_coba <- ggplot() + geom_sf(data = Jatim)
map_coba
Map_Jatim <- left_join(Jatim, Data_VER_1, by = c("NAME_2" = "KABKOT"))
Map_Jatim_Jurnal <- left_join(Jatim, Data_JURNAL, by = c("NAME_2" = "KABKOT"))
Map_Jatim_KategoriJurnal <- left_join(Jatim, Data_Kel_Jurnal, by = c("NAME_2" = "KABKOT"))
view(Map_Jatim)

library(readr)
library(janitor)
library(tmap)

#Peta Tematik UHH
tm_shape(Map_Jatim) + 
  tm_fill("UHH", style="quantile", title = "UHH") +
  tm_borders(alpha=0.1) +
  tm_layout(legend.position = c("right", "bottom"), 
            legend.title.size = 0.8,
            legend.text.size = 0.5, main.title = "Umur Harapan Hidup (UHH) Kabupaten/Kota Provinsi Jawa Timur", main.title.position = "center", main.title.size = 0.8) +
  tm_text("NAME_2", size = 0.4 )

#Peta Tematik AIRMINUM
tm_shape(Map_Jatim) + 
  tm_fill("AIRMINUM", style="quantile", title = "Akses Air Minum Layak") +
  tm_borders(alpha=0.1) +
  tm_layout(legend.position = c("right", "bottom"), 
            legend.title.size = 0.8,
            legend.text.size = 0.5, main.title = "Persentase Rumah Tangga Memiliki Akses Air Minum Layak Kabupaten/Kota Provinsi Jawa Timur", main.title.position = "center", main.title.size = 0.7) +
  tm_text("NAME_2", size = 0.4 )

#Peta Tematik SANITASI
tm_shape(Map_Jatim) + 
  tm_fill("SANITASI", style="quantile", title = "Akses Sanitasi Layak") +
  tm_borders(alpha=0.1) +
  tm_layout(legend.position = c("right", "bottom"), 
            legend.title.size = 0.8,
            legend.text.size = 0.5, main.title = "Persentase Rumah Tangga Memiliki Akses Sanitasi Layak Kabupaten/Kota Provinsi Jawa Timur", main.title.position = "center", main.title.size = 0.7) +
  tm_text("NAME_2", size = 0.4 )

#Peta Tematik ASIEKS
tm_shape(Map_Jatim) + 
  tm_fill("ASIEKS", style="quantile", title = "ASI Eksklusif") +
  tm_borders(alpha=0.1) +
  tm_layout(legend.position = c("right", "bottom"), 
            legend.title.size = 0.8,
            legend.text.size = 0.5, main.title = "Cakupan Pemberian ASI Eksklusif Kabupaten/Kota Provinsi Jawa Timur", main.title.position = "center", main.title.size = 0.7) +
  tm_text("NAME_2", size = 0.4 )

#Peta Tematik PERSALINAN.NAKES
tm_shape(Map_Jatim) + 
  tm_fill("PERSALINAN.NAKES", style="quantile", title = "Persentasi Persalinan") +
  tm_borders(alpha=0.1) +
  tm_layout(legend.position = c("right", "bottom"), 
            legend.title.size = 0.8,
            legend.text.size = 0.5, main.title = "Persentase Persalinan Ditolong Tenaga Kesehatan Kabupaten/Kota Provinsi Jawa Timur", main.title.position = "center", main.title.size = 0.7) +
  tm_text("NAME_2", size = 0.4 )

#Peta Tematik RASIODOKTER
tm_shape(Map_Jatim) + 
  tm_fill("RASIODOKTER", style="quantile", title = "Rasio Dokter") +
  tm_borders(alpha=0.1) +
  tm_layout(legend.position = c("right", "bottom"), 
            legend.title.size = 0.8,
            legend.text.size = 0.5, main.title = "Rasio Dokter per 100.000 Penduduk Kabupaten/Kota Provinsi Jawa Timur", main.title.position = "center", main.title.size = 0.7) +
  tm_text("NAME_2", size = 0.4 )

#Kategori GWR 
#FG
FG_cat <- cut(Data_VER_1$CAT_FG, breaks=2, labels=FALSE,include.lowest=TRUE)
FG_cat
Data_VER_1$CAT_FG <- as.factor(FG_cat)
tm_shape(Map_Jatim) + 
  tm_fill("CAT_FG", style="cat", title = "Kelompok", palette = "Paired") +
  tm_borders(alpha=0.1) +
  tm_layout(legend.position = c("right", "bottom"), 
            legend.title.size = 0.8,
            legend.text.size = 0.5, main.title = "Kelompok Wilayah Berdasarkan Model GWR Pembobot Fixed Gaussian", main.title.position = "center", main.title.size = 0.7) +
  tm_text("NAME_2", size = 0.4 )

#FB
FG_cat <- cut(Data_VER_1$CAT_FG, breaks=2, labels=FALSE,include.lowest=TRUE)
FG_cat
Data_VER_1$CAT_FB <- as.factor(Data_VER_1$CAT_FB)
tm_shape(Map_Jatim) + 
  tm_fill("CAT_FB", style="cat", title = "Kelompok", palette = "Paired") +
  tm_borders(alpha=0.1) +
  tm_layout(legend.position = c("right", "bottom"), 
            legend.title.size = 0.8,
            legend.text.size = 0.5, main.title = "Kelompok Wilayah Berdasarkan Model GWR Pembobot Fixed Bi-Square", main.title.position = "center", main.title.size = 0.7) +
  tm_text("NAME_2", size = 0.4 )

#AG
FG_cat <- cut(Data_VER_1$CAT_FG, breaks=2, labels=FALSE,include.lowest=TRUE)
FG_cat
Data_VER_1$CAT_AG <- as.factor(Data_VER_1$CAT_AG)
tm_shape(Map_Jatim) + 
  tm_fill("CAT_AG", style="cat", title = "Kelompok", palette = "Paired") +
  tm_borders(alpha=0.1) +
  tm_layout(legend.position = c("right", "bottom"), 
            legend.title.size = 0.8,
            legend.text.size = 0.5, main.title = "Kelompok Wilayah Berdasarkan Model GWR Pembobot Adaptive Gaussian", main.title.position = "center", main.title.size = 0.7) +
  tm_text("NAME_2", size = 0.4 )

#AB
FG_cat <- cut(Data_VER_1$CAT_FG, breaks=2, labels=FALSE,include.lowest=TRUE)
FG_cat
Data_VER_1$CAT_AB <- as.factor(Data_VER_1$CAT_AB)
tm_shape(Map_Jatim) + 
  tm_fill("CAT_AB", style="cat", title = "Kelompok", palette = "Paired") +
  tm_borders(alpha=0.1) +
  tm_layout(legend.position = c("right", "bottom"), 
            legend.title.size = 0.8,
            legend.text.size = 0.5, main.title = "Kelompok Wilayah Berdasarkan Model GWR Pembobot Adaptive Bi-Square", main.title.position = "center", main.title.size = 0.7) +
  tm_text("NAME_2", size = 0.4 )

#JURNAL FIXEDTRICUBE
TRICUBE_cat <- cut(Data_VER_1$CAT_FG, breaks=2, labels=FALSE,include.lowest=TRUE)
FG_cat
Data_VER_1$CAT_AB <- as.factor(Data_VER_1$CAT_AB)
tm_shape(Map_Jatim_KategoriJurnal) + 
  tm_fill("Kelompok", style="cat", title = "Group", palette = "Paired") +
  tm_borders(alpha=0.1) +
  tm_layout(legend.position = c("right", "bottom"), 
            legend.title.size = 0.8,
            legend.text.size = 0.5, ) +
  tm_text("NAME_2", size = 0.4 )

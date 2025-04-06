#--KUTUPHANE KURULUMLARI---
library(readxl)
library(dplyr)
library(ggplot2)
library(forecast)
library(writexl)
library(lubridate)
library(stringr)
library(tidyr)
#--------------- Veri seti okuma ve ön inceleme ---------------
df <- read_xlsx("2024-yili-yangin-mudahale..xlsx")
View(df)

#--- Eksik deger kontrolu---
sum(is.na(df))
df <- na.omit(df) 
head(df,10)

#--ILK 10 satiri gosterme-- 
head(df, 10)

# Veri seti hakkinda bilgi---
str(df)

# Satir ve sutun sayisi
dim(df)

# Veri setini inceleme 
head(df)

# Veri seti ozet istatistikleri
summary(df)


#--------------2024 Yilinda En Cok Yangin Cikan ilk 10 ilce--------------------
df %>%
  group_by(ILCE) %>%
  summarise(yangin_sayisi = n()) %>%
  arrange(desc(yangin_sayisi)) %>%
  print(n=10)

#----EN ÇOK YANGIN ÇIKAN MEVSİM----
df %>%
  group_by(MEVSIM) %>%
  summarise(yangin_sayisi = n()) %>%
  arrange(desc(yangin_sayisi)) %>%
  print(n=10)


#---Toplam Yangın Sayısı-----
df %>% summarise(toplam_yangin = n())

#----Sebebine Göre Toplam Yangın Sayısı---
df %>%
count(YANGIN_SEBEBI, sort = TRUE) %>%
  print(n = Inf)

#----En Çok Çıkan Yangın Türü-----
df %>%
  group_by(YANGIN_TURU) %>%
  summarise(yangin_sayisi = n()) %>%
  arrange(desc(yangin_sayisi)) %>%
  print(n=10)

#--------------Turlerine Gore Yangin Sayilari---------------------------
df %>%
  count(YANGIN_TURU, sort = TRUE) %>%
  ggplot(aes(x = reorder(YANGIN_TURU, n), y = n)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(title = "Yangın Turlerinin Dagilimi", x = "Yangın Turu", y = "Olay Sayisi")

#----Mevsime Göre Yangın Sayısı-----
df %>%
  group_by(MEVSIM) %>%
  summarise(yangin_sayisi = n()) %>%
  arrange(desc(yangin_sayisi)) %>%
  print(n=Inf)

#----İlcelere Göre Yangın Sayısı-----
df %>%
  group_by() %>%
  summarise(yangin_sayisi = n()) %>%
  arrange(desc(yangin_sayisi)) %>%
  print(n=Inf)

#------------------Aylara GORE TOPLAM YANGIN SAYISI-------------------------

df %>%
  count(AY) %>%
  ggplot(aes(x = factor(AY, levels = c("OCAK", "SUBAT", "MART", "NISAN", "MAYIS", "HAZIRAN",
                                       "TEMMUZ", "AGUSTOS", "EYLUL", "EKIM", "KASIM", "ARALIK")),
             y = n)) +
  geom_col(fill = "darkorange") +
  labs(title = "Aylara Gore Yangin Sayisi",
       x = "Ay",
       y = "Yangin Sayisi") +
  theme_minimal()


#--------------Mevsimlere Göre Yangin Sayisi --------------------------
yangin_mevsimsel <- df %>%
  group_by(MEVSIM) %>%
  summarise(yangin_sayisi = n())

ggplot(yangin_mevsimsel, aes(x = MEVSIM, y =yangin_sayisi, fill = MEVSIM)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Mevsimlere Gore Tum Yangınlar",
       x = "Mevsim",
       y = "Yangin Sayisi") +
  theme_minimal()



#------------Mevsimlere Gore Yangin Sebep Sayisi Karsilastirma------------------------------

mevsim_sebep <- df %>%
  group_by(MEVSIM, YANGIN_SEBEBI) %>%
  summarise(yangin_sayisi = n()) %>%
  ungroup()

ggplot(mevsim_sebep, aes(x = reorder(YANGIN_SEBEBI, yangin_sayisi), 
                         y = yangin_sayisi, 
                         fill = MEVSIM)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ MEVSIM, scales = "free_y") +
  labs(title = "Mevsimlere Gore Yangin Sebepleri",
       x = "YangD1n Sebebi",
       y = "YangD1n SayD1sD1") +
  theme_minimal(base_size = 10)

# Mevsimlerde En Cok Gorulen Ilk 3 Yangin Sebebi-----------------

en_cok_yangin_3sebeb <- mevsim_sebep %>%
  group_by(MEVSIM) %>%
  slice_max(order_by = yangin_sayisi, n = 3) %>%
  ungroup()


ggplot(en_cok_yangin_3sebeb, aes(x = reorder(YANGIN_SEBEBI, yangin_sayisi), 
                                 y = yangin_sayisi, 
                                 fill = MEVSIM)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ MEVSIM, scales = "free_y") +
  labs(title = "Mevsimlere GC6re En Sik Gorulen 3 Yangin Sebebi",
       x = "Yangin Sebebi",
       y = "Yangin Sayisi") +
  theme_minimal(base_size = 11)


#------- Ilcelere Gore"Sigara_Kibrit" Kaynakli Yangin Sayilari ---------------


sigara_kibrit_yanginlar <- df %>%
  filter(YANGIN_SEBEBI == "SIGARA/KIBRIT") %>%
  group_by(ILCE) %>%
  summarise(yangin_sayisi = n()) %>%
  arrange(desc(yangin_sayisi))

ggplot(sigara_kibrit_yanginlar, aes(x = reorder(ILCE, yangin_sayisi), y = yangin_sayisi)) +
  geom_bar(stat = "identity", fill = "firebrick") +
  coord_flip() +
  labs(title = "Ilcelere Gore Sigara/Kibrit Kaynakli Yanginlar",
       x = "Ilce",
       y = "Yangin Sayisi") +
  theme_minimal()

#---------- En uzun surede ulasilan ilce------------------------

df <- df %>%
  separate(`VARIS_SURESI (DAK.)`, into = c("saat", "dakika"), sep = ":", convert = TRUE) %>%
  mutate(
    VARIS_SURESI_DK = saat * 60 + dakika
  )

#------Adres BOLGESİNE GORE VARIS SURELERI

ilce_varis_suresi <- df %>%
  group_by(ILCE,ADRES_BOLGESI) %>%
  summarise(ortalama_varis_suresi = mean(VARIS_SURESI_DK, na.rm = TRUE)) %>%
  arrange(desc(ortalama_varis_suresi))

#IL DISI VERISI ANALIZDEN SILINMELİDİR (Neden:Bir ilceye ne de KENT/KIRSAL bolgeye bagli degildir)

ilce_varis_suresi_clean <- ilce_varis_suresi %>%
  filter(ILCE != "IL DISI")

#-------------- EN KISA SUREDE VARIS YAPILAN BOLGE-------------

ilce_varis_suresi_clean %>%
  ungroup()%>%
  arrange(ortalama_varis_suresi) %>%
  slice(1)

#---------------- EN UZUN SuREDE VARIs YAPILAN Bolge -------------

ilce_varis_suresi_clean %>%
  ungroup()%>%
  arrange(desc(ortalama_varis_suresi)) %>%
  slice(1)


#----------- En geç ulasilan 5 bolge ------------

en_gec5 <- ilce_varis_suresi_clean %>%
  ungroup() %>%
  arrange(desc(ortalama_varis_suresi)) %>%
  slice_head(n = 5)

# En gec  ulasılan 5 bolge grafiği

ggplot(en_gec5, aes(x = reorder(paste(ILCE, ADRES_BOLGESI, sep = " - "), ortalama_varis_suresi),
                    y = ortalama_varis_suresi,
                    fill = ILCE)) +
  geom_col() +
  coord_flip() +
  labs(title = "En Gec Mudahale Edilen 5 Bolge",
       x = "D0lC'e - Adres Bolgesi",
       y = "Ortalama Varis Suresi (dk)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

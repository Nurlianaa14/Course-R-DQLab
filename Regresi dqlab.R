#Regresi merupakan suatu model dalam statistik yang berfungsi untuk mencari hubungan antara 
#variabel bebas atau yang sering disebut dengan variabel independen atau prediktor
#dengan variabel dependen atau sering disebut dengan respon

#y = a + bx
#Dimana y merupakan nilai prediksi, a merupakan sebuah variabel intercept. 
#Intercept merupakan titik dimana garis regresi akan memotong sumbu y ketika x bernilai sama dengan 0.
#Dengan kata lain, intercept adalah nilai perubahan variabel y ketika x bernilai 0. 
#Variabel b merupakan konstanta untuk menunjukkan besar perubahan variabel y untuk setiap kenaikan 1
#dan e merupakan nilai error.

penjualan_permen = read.csv("https://storage.googleapis.com/dqlab-dataset/tingkat_penjualan_kota_x_dqlab.tsv", header = TRUE, sep = "\t")
kunjungan_dokter = read.csv("https://storage.googleapis.com/dqlab-dataset/kunjungan_dokter_gigi_kota_x_dqlab.tsv", header = TRUE, sep = "\t")

print(penjualan_permen)
print(kunjungan_dokter)

#Merge data
data_gabungan = merge(kunjungan_dokter, penjualan_permen, by.x=c("Bulan","Tahun"), by.y=c("Bulan","Tahun"), sort = FALSE)
data_gabungan

summary(data_gabungan$tingkat.kunjungan.ke.dokter.gigi)
summary(data_gabungan$penjualan.permen)
summary(data_gabungan$penjualan.sereal)
summary(data_gabungan$penjualan.buah.pisang)


#Eksplorasi data dengan visual
#Melakukan explorasi data kunjungan dokter dengan penjualan permen
plot(data_gabungan$tingkat.kunjungan.ke.dokter.gigi, data_gabungan$penjualan.permen,
     pch = 19,
     xlab = "Kunjungan dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan dokter dengan penjualan permen",     
     col = "blue")

#Melakukan explorasi data kunjungan dokter dengan penjualan sereal
plot(data_gabungan$tingkat.kunjungan.ke.dokter.gigi, data_gabungan$penjualan.sereal,
     pch = 19,
     xlab = "Kunjungan dokter",
     ylab = "Penjualan Sereal",
     main = "Kunjungan dokter dengan penjualan sereal",     
     col = "blue")

#Melakukan explorasi data kunjungan dokter dengan penjualan buah pisang
plot(data_gabungan$tingkat.kunjungan.ke.dokter.gigi, data_gabungan$penjualan.buah.pisang,
     pch = 19,
     xlab = "Kunjungan dokter",
     ylab = "Penjualan Buah Pisang",
     main = "Kunjungan dokter dengan penjualan buah pisang",
     col = "blue")

#Makan permen tidak langsung membuat sakit gigi 
#Maka perlu melihat efek keterlambatan memakan permen ini terhadap kunjungan ke rumah sakit
#Delay effect
library (dplyr)
data_delayed_effect = data.frame(
  month = data_gabungan$Bulan,
  year = data_gabungan$Tahun,
  kunjungan_dokter = data_gabungan$tingkat.kunjungan.ke.dokter.gigi,
  penjualan_permen = data_gabungan$penjualan.permen,
  penjualan_permen_1 = lag(data_gabungan$penjualan.permen),
  penjualan_permen_2 = lag(data_gabungan$penjualan.permen,2),
  penjualan_permen_3 = lag(data_gabungan$penjualan.permen,3),
  penjualan_permen_4 = lag(data_gabungan$penjualan.permen,4),
  penjualan_permen_5 = lag(data_gabungan$penjualan.permen,5),
  penjualan_permen_6 = lag(data_gabungan$penjualan.permen,6)
)
data_delayed_effect


#plot delay effect
plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen,
     pch = 19,
     xlab = "Kunjungan dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan dokter dengan penjualan permen", 
     col = "blue")

plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen_1,
     pch = 19,
     xlab = "Kunjungan dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan dokter dengan penjualan permen (delay 1 bulan)", 
     col = "blue")

plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen_2,
     pch = 19,
     xlab = "Kunjungan dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan dokter dengan penjualan permen (delay 2 bulan)", 
     col = "blue")

plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen_3,
     pch = 19,
     xlab = "Kunjungan dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan dokter dengan penjualan permen (delay 3 bulan)", 
     col = "blue")

plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen_4,
     pch = 19,
     xlab = "Kunjungan dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan dokter dengan penjualan permen (delay 4 bulan)", 
     col = "blue")

plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen_5,
     pch = 19,
     xlab = "Kunjungan dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan dokter dengan penjualan permen (delay 5 bulan)", 
     col = "blue")

plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen_6,
     pch = 19,
     xlab = "Kunjungan dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan dokter dengan penjualan permen (delay 6 bulan)", 
     col = "blue")


#PAda delayed effect selama 4 bulan, hubungan antara tingkat kunjungan dokter
#dengan penjualan permen terlihat cukup kuat dan berkorelasi positif. 

#Analisis Regresi menggunakan R
data_regresi = data.frame(
  month = data_delayed_effect$month,
  tahun = data_delayed_effect$year,
  kunjungan_dokter = data_delayed_effect$kunjungan_dokter, 
  penjualan_permen = data_delayed_effect$penjualan_permen_4)

#Mengeliminasi data NA
data_regresi = na.omit(data_regresi)
data_regresi

#Model regresi
model = lm(kunjungan_dokter ~ penjualan_permen, data = data_regresi)
summary(model)
#Hasil menunjukkan Pr(>|t|) = 1.4e-05 < 0.05
#Artinya Penjualan permen 4 bulan yang lalu memiliki pengaruh yang signifikan 
#terhadap kunjungan dokter bulan ini

#y = 12,93 + 0,0002303
#Jika terjadi penjualan permen sebanyak 100000 unit
#Maka nilai kunjungan dokternya adalah 35,96
#Ini berati setiap ada kenaikan penjualan permen sebanyak 100000
#Maka akan meningkatkan kunjungan dokter sebanyak 36 orang dalam 4 bulan kedepan


# Prak2_Probstat2023_C_5025211171
Praktikum Modul 2

# Soal 1 
<blockquote> 
  Penelitian mengenai pengaruh aktivitas 𝐴 terhadap kadar saturasi oksigen pada manusia. 
  Sampel sebanyak 9 responden. 
  Sebelum melakukan aktivitas 𝐴, mencatat kadar saturasi oksigen dari 9 responden. 
  Diminta melakukan aktivitas 𝐴. 
  Setelah 15 menit, mencatat kembali kadar saturasi oksigen. 
</blockquote>

| Responden |   X   |   Y   |
| :-------: |  :--: | :--:  |
| 1         | 78    | 100   |
| 2         | 75    | 95    |
| 3         | 67    | 70    |
| 4         | 77    | 90    |
| 5         | 70    | 90    |
| 6         | 72    | 90    |
| 7         | 78    | 89    |
| 8         | 70    | 100   |
| 9         | 77    | 100   |

**A. Standar Deviasi**

'''
X <- C(78, 75, 67, 77, 70, 72, 78, 70, 77)
Y <- C(100, 95, 70, 90, 90, 90, 89, 100, 100)
different = ( X - Y )
sd(different)
'''

Jadi, didapatkan standar deviasi adalah 7.838

**B. Carilah nilai t (p-value)**
'''
t = 2 * pt(-abs(((mean(different) - 0) / )(sd / sqrt(9)))), df = 8
'''
Jadi, didapatkan nilai t = 6.803 dan nilai p-value = 0.0013

**C. Apakah terdapat pengaruh signifikan**
'''
var.test(X, Y)
t.test (X, Y, mu = 0, alternatif = "two.sided", var.equal = True, conf.level = 0.95)
'''
Didapatkan p-value lebih kecil dari significant level (0.05) sehingga hipotesis nol ditolak dan hipotesis alternatif diterima. Hal ini menunjukkan bahwa aktivitas A memberikan pengaruh signifikan dalam kadar saturasi oksigen.

# Soal 2
<blockquote>
Mobil dikemudikan rata-rata lebih dari 25.000 km/tahun. 100 pemilik mobil yang dipilih secara acak diminta untuk mencatat jarak yang mereka tempuh. Jika sampel acak menunjukkan rata-rata 23.500 km dan standar deviasi 3.000 km
</blockquote>

'''
H0 : µ ≤ 25.000 km
H1 : µ > 25.000 km
'''

**A.Apakah setuju dengan klaim tersebut?**
Setuju, karena dari perhitungan dengan tanpa menggunakan uji statistik atau uji z-sum 
memungkinkan untuk mendapatkan rata-rata lebih dari 25.000 km per tahun.

**B. kesimpulan berdasarkan p-value yang dihasilkan**
'''
install.packages("BSDA")
library(BSDA)

mean <- 25000
n <- 100
sample <- 23500
sdpopulasi <- 3000

zsum.test (mean.x=23500, sigma.x = 3000, n.x = 100,  
          alternative = "greater", mu = 25000,
          conf.level = 0.95)
'''
Nilai p-value = 1
Yang berarti nilai p-value lebih besar daripada nilai α, nilai α = 0.05
bukti dari yang di dapatkan menunjukkan bahwa tidak cukup untuk menolak hipotesis nol H0 atau hipotesis alternatif.

# Soal 3
<blockquote>
Data analyst yang ingin memecahkan permasalahan pengambilan keputusan dalam perusahaan. Selanjutnya
didapatkanlah data berikut dari perusahaan saham tersebut.Asumsikan
nilai variancenya sama, apakah ada perbedaan pada rata-ratanya (α= 0.05)?
</blockquote>

**A. H0 dan H1**
'''
H0 : Mean_Bandung = Mean_Bali
H1 : Mean_Bandung != Mean_Bali
'''

**B. Sampel Statistik**
'''
alfa <- 0.05
confLevel <- 0.95

nBdg = 20
mean_bdg = 3.64
sigma_bdg = 1.67

nBali = 27
mean_Bali = 2.79
sigma_Bali = 1.5

tsum.test(mean.x = mean_bdg, s.x = sigma_bdg, n.x =  nBdg,
          mean.y = mean_Bali, s.y = sigma_Bali, n.y = nBali, 
          alternative = "greater", mu = 0, var.equal = TRUE,
          conf.level = confLevel)
'''
Dengan asumsi varian sama, hasil uji t-sum akan menghasilkan nilai t-statistik, derajat kebebasan, dan nilai p-value yang dapat digunakan untuk membuat keputusan statistik.

**C. Uji statistik (df = 2)**
'''
install.packages("mosaic")
library(mosaic)

df <- 2
plotDist(dist = 't', df, col = "purple")
'''
argumen dist = 't' digunakan untuk menunjukkan bahwa kita ingin menggambar distribusi t (distribusi t-Student). Argumen df = 2 menentukan derajat kebebasan dari distribusi t (dalam hal ini, df = 2).

**D. Nilai Kritikal**
'''
nilaiKritis2 <- qt(p = alfa, df = 2 , lower.tail = FALSE)
nilaiKritis2
'''
dihasilkan dua nilai kritis dari distribusi t-student dengan derajat kebebasan 2 pada level signifikansi 0.05. Nilai-nilai kritis ini dapat digunakan untuk mengambil keputusan statistik dalam pengujian hipotesis menggunakan uji t.

**E. Keputusan**
p-value = 0.03691
Nilai tersebut lebih kecil dari 𝛼 = 0.05.
Maka, Hipotesis Nol H0 ditolak

**F. kesimpulan**
Tidak ada bukti yang cukup
Teterdapat perbedaan antara rata-rata saham di Bandung dan rata-rata saham di Bali


# Soal 4
<blockquote>
Data merupakan hasil eksperimen yang dilakukan untuk
mengetahui pengaruh suhu operasi (100˚C, 125˚C dan 150˚C) dan tiga jenis kaca
pelat muka (A, B dan C) pada keluaran cahaya tabung osiloskop.
</blockquote>

'''
install.packages("multcompView")
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)
'''

**A. Plot sederhana untuk visualisasi data.**
'''
ata <- read_csv("GTL.csv")
head(data)
str(data)

qplot(x = Temp, y = Light, geom = "point", data = data) + facet_grid(.~Glass, labeller = label_both)
'''


**B. Uji ANOVA dua arah.**
'''
data$Glass = as.factor(data$Glass)
data$Temp_Factor = as.factor(data$Temp)
str(data)

anova = aov(Light ~ Glass*Temp_Factor, data = data)
summary(anova)
'''

**C. Tabel dengan mean dan standar deviasi**
'''
data_summary = group_by(data, Glass, Temp) %>%
  summarise(mean = mean(Light), sd = sd(Light)) %>%
  arrange(desc(mean))
print(data_summary)
'''
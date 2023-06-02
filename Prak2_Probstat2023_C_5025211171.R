# Nama : Rani Listian Anggraeni
# NRP  : 5025211171

# -------- Praktikum Modul 2 --------


# -- NO 1 --

X <- C(78, 75, 67, 77, 70, 72, 78, 70, 77)
Y <- C(100, 95, 70, 90, 90, 90, 89, 100, 100)

# A
different = ( X - Y )
sd(different)

# B
t = 2 * pt(-abs(((mean(different) - 0) / )(sd / sqrt(9)))), df = 8

# c
var.test(X, Y)
t.test (X, Y, mu = 0, alternatif = "two.sided", var.equal = True, conf.level = 0.95)


# -- N0 2 --

# A
# Setuju, karena dari perhitungan dengan tanpa menggunakan uji statistik atau uji z-sum 
# memungkinkan untuk mendapatkan rata-rata lebih dari 25.000 km per tahun.

# B
install.packages("BSDA")
library(BSDA)

mean <- 25000
n <- 100
sample <- 23500
sdpopulasi <- 3000

zsum.test (mean.x=23500, sigma.x = 3000, n.x = 100,  
          alternative = "greater", mu = 25000,
          conf.level = 0.95)

# C
# Nilai p-value = 1
# Yang berarti nilai p-value lebih besar daripada nilai α, nilai α = 0.05
# bukti dari yang di dapatkan menunjukkan bahwa tidak cukup untuk menolak hipotesis nol H0 atau hipotesis alternatif.

# -- NO 3 --

# A 
# H0 : Mean_Bandung = Mean_Bali
# H1 : Mean_Bandung != Mean_Bali

# B
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

# C
install.packages("mosaic")
library(mosaic)

df <- 2
plotDist(dist = 't', df, col = "purple")

# D
nilaiKritis2 <- qt(p = alfa, df = 2 , lower.tail = FALSE)
nilaiKritis2

# E
# p-value = 0.03691
# Nilai tersebut lebih kecil dari 𝛼 = 0.05.
# Maka, Hipotesis Nol H0 ditolak

# F
# Tidak ada bukti yang cukup
# Teterdapat perbedaan antara rata-rata saham di Bandung dan rata-rata saham di Bali


# -- NO 4 --

install.packages("multcompView")
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)

# A
ata <- read_csv("GTL.csv")
head(data)
str(data)

qplot(x = Temp, y = Light, geom = "point", data = data) + facet_grid(.~Glass, labeller = label_both)

# B
data$Glass = as.factor(data$Glass)
data$Temp_Factor = as.factor(data$Temp)
str(data)

anova = aov(Light ~ Glass*Temp_Factor, data = data)
summary(anova)

# C
data_summary = group_by(data, Glass, Temp) %>%
  summarise(mean = mean(Light), sd = sd(Light)) %>%
  arrange(desc(mean))
print(data_summary)
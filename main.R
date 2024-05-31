# Memuat paket
install.packages("tidyverse")
library(tidyverse)

# Membaca dataset CSV
df <- read.csv("E:/analisis-metkuan/starbucks.csv")

view(df)

# Menggabungkan tiga kolom menjadi satu kolom
df <- df %>%
  unite("Beverage_menu", Beverage_category, Beverage, Beverage_prep, sep = "_")

# Menyimpan dataset yang telah dimodifikasi ke CSV
write.csv(df, "E:/analisis-metkuan/starbucks_beverage.csv", row.names = FALSE)

# Membaca dataset CSV yang telah di ekspansi kolom
dt <- read.csv("E:/analisis-metkuan/starbucks_beverage.csv")

view(dt)

# Memuat library yang diperlukan
library(PerformanceAnalytics)
library(car)

# Ambil semua kolom kecuali 5
data_baru = dt[, c(1:4, 6:16)]

str(data_baru)

# Konversi tipe data
data_baru$Total.Fat..g. <- as.numeric(data_baru$Total.Fat..g.)
data_baru$Caffeine..mg. <- as.numeric(data_baru$Caffeine..mg.)
data_baru$Total.Carbohydrates..g. <- as.numeric(data_baru$Total.Carbohydrates..g.)
data_baru$Dietary.Fibre..g. <- as.numeric(data_baru$Dietary.Fibre..g.)
data_baru$Sugars..g. <- as.numeric(data_baru$Sugars..g.)
data_baru$Sodium..mg. <- as.numeric(data_baru$Sodium..mg.)
data_baru$Cholesterol..mg. <- as.numeric(data_baru$Cholesterol..mg.)
data_baru$Calories <- as.numeric(data_baru$Calories)

str(data_baru)

# Ubah satuan dari mg ke g
data_baru$Caffeine..mg. <- data_baru$Caffeine..mg. / 1000
data_baru$Sodium..mg. <- data_baru$Sodium..mg. / 1000
data_baru$Cholesterol..mg. <- data_baru$Cholesterol..mg. / 1000
names(data_baru)[names(data_baru) == "Caffeine..mg."] <- "Caffeine..g."
names(data_baru)[names(data_baru) == "Sodium..mg."] <- "Sodium..g."
names(data_baru)[names(data_baru) == "Cholesterol..mg."] <- "Cholesterol..g."

# analisis pca
# ambil data yang sesuai
data_baru_pca = data_baru[, c(2:10, 15)]

str(data_baru_pca)
view(data_baru_pca)
# Cek nilai tak terhingga (infinite) dan nilai hilang (NA)
sum(is.infinite(data_baru_pca))
sum(is.na(data_baru_pca))

# Ganti nilai tak terhingga dengan NA
data_baru_pca[is.infinite(data_baru_pca)] <- NA

# Pastikan semua kolom bertipe numerik
data_baru_pca <- data.frame(lapply(data_baru_pca, function(x) as.numeric(as.character(x))))

# Periksa nilai tak terhingga dan nilai hilang untuk setiap kolom
sum_infinite <- sapply(data_baru_pca, function(x) sum(is.infinite(x)))
sum_na <- sapply(data_baru_pca, function(x) sum(is.na(x)))

print(sum_infinite)
print(sum_na)

# Tangani nilai hilang dengan menggantinya menggunakan nilai rata-rata dari kolom terkait
for(i in 1:ncol(data_baru_pca)) {
  data_baru_pca[is.na(data_baru_pca[, i]), i] <- mean(data_baru_pca[, i], na.rm = TRUE)
}

# Verifikasi bahwa tidak ada lagi nilai tak terhingga atau hilang
sum(is.na(data_baru_pca))

#-------------------------------------------------------------------

# Visualisasi matriks korelasi antar variabel
chart.Correlation(data_baru_pca)

# Hitung Variance Inflation Factor (VIF) untuk mendeteksi multikolinearitas
vif_values <- vif(lm(data_baru_pca))
print(vif_values)

# Analisis PCA
data_baru_pca = scale(data_baru_pca)
cov_baru_pca = cov(data_baru_pca)
eigen(cov_baru_pca)
data_baru_pca.pca = prcomp(data_baru_pca, center = TRUE, scale. = TRUE)
summary(data_baru_pca.pca)

pcnya = predict(data_baru_pca.pca, newdata = data_baru_pca)
head(pcnya)
chart.Correlation(pcnya)
View(pcnya)

#--------------------------------------------------

# Biplot
library("ggbiplot")
library("ggplot2")
library("factoextra")
kopi.pca = prcomp(data_baru_pca, center = TRUE, scale. = TRUE)
summary(kopi.pca)
kopi.pca

screeplot(kopi.pca, type="line", main="Scree plot")
fviz_eig(kopi.pca,
         addlabels = TRUE,
         ylim = c(0,30),
         main = "Figure 1")

pcnya = predict(kopi.pca, newdata = data_baru_pca)
View((pcnya))

biplot(kopi.pca, scale = 0, choices = c(1,2))

datakopi_scaled = scale(data_baru_pca)
cov(datakopi_scaled)

biplott = ggbiplot(pcobj = kopi.pca,
                   choices = c(1,2),
                   scale = 0,
                   obs.scale = 0, var.scale = 0, 
                   labels = row.names(data),
                   labels.size = 4,
                   varname.size = 5,
                   varname.abbrev = TRUE,
                   var.axes = TRUE,
                   circle = FALSE,
                   ellipse = TRUE)

print(biplott)

print(ggbiplot(kopi.pca))

#----------------------------------------------------

# Install dan load paket yang dibutuhkan
library(cluster)
library(factoextra)

str(data_baru_pca)
view(data_baru_pca)

# Melakukan PCA
data.pca <- prcomp(data_baru_pca, center=TRUE, scale.=TRUE)
df.pca <- as.data.frame(data.pca$x[, 1:2]) 
colnames(df.pca) <- c("PC1", "PC2")  # Memberi nama kolom

# Menampilkan summary dari hasil PCA
summary(data.pca)

# Melakukan klasterisasi k-means pada data asli dengan 3 kluster
set.seed(123)
kmeans.awal <- kmeans(data_baru_pca, centers = 3)

# Melakukan klasterisasi k-means pada data PCA dengan 3 kluster
set.seed(123)
kmeans1 <- kmeans(df.pca, centers = 3)

# Menampilkan hasil klasterisasi
kmeans.awal
kmeans1

# Visualisasi hasil klasterisasi pada data asli
fviz_cluster(kmeans.awal, data = data_baru_pca, geom = "point", ellipse.type = "convex", 
             palette = "jco", ggtheme = theme_minimal())

# Visualisasi hasil klasterisasi pada data PCA
fviz_cluster(kmeans1, data = df.pca, geom = "point", ellipse.type = "convex", 
             palette = "jco", ggtheme = theme_minimal())
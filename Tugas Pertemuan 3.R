#Library yang digunakan
install.packages(c('rgl', 'plot3D', 'plot3Drgl', 'squash', 'mixtools'))
library(rgl)
library(plot3D)
library(plot3Drgl)
library(squash)
library(MASS)
library(mixtools)

#Membuat fungsi garis kontur
buat_kontur <- function(data_simulasi, miu, sigma, level = 0.05) {
  #Plot data
  plot(data_simulasi, xlab="X1", ylab="X2")
  
  #Buat titik tengah
  points(miu[1], miu[2], pch=3, cex=2, col="blue")
  
  #Gambar kontur
  ellipse(mu = miu, sigma = sigma, alpha = 1 - level, col="red")
}


#Membuat fungsi untuk memeriksa titik tertentu
cek_titik <- function(miu, sigma, level = 0.05, titik) {
  #Persamaan kontur
  eq <- t(titik - miu) %*% solve(sigma) %*% (titik-miu)
  
  #Chi-Square
  c_kuadrat <- qchisq(1 - level, 2)
  
  #Menentukan lokasi
  if(eq <= c_kuadrat){
    posisi <- "di dalam"
  } else {
    posisi <- "di luar"
  }
  paste("KESIMPULAN : Titik berada", posisi, "garis kontur.")
}


#Tes fungsi
miu <- c(5, 10)
sigma <- matrix(c(9, 16, 16, 64), byrow = TRUE, nrow = 2)
data <- mvrnorm(500, miu, sigma)
titik1 <- c(10, 20)
titik2 <- c(16, 20)

buat_kontur(data, miu, sigma)
cek_titik(miu, sigma, titik = titik1)
cek_titik(miu, sigma, titik = titik2)
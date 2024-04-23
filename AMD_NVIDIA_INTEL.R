#Wgranie bibliotek
install.packages("tidyverse")
install.packages("tidyquant")
install.packages("knitr")
install.packages("patchwork")
install.packages("moments")
install.packages("rugarch")
library(rugarch)
library(tidyverse)
library(tidyquant)
library(knitr)
library(zoo)
library(tseries)
library(dplyr)
library(moments)
library(patchwork)
#Wgrywanie danych akcji 
#NVIDIA 
# csv_nvidia <- read_csv(url("https://stooq.pl/q/d/l/?s=nvda.us&d1=20090405&d2=20240405&i=d"))
csv_nvidia <- read_csv(url("https://drive.google.com/uc?id=1z2Lo3HoPpOkAMtzzs6a-5AFrlx1x_mJV&export=download"))
head(csv_nvidia)
colnames(csv_nvidia) <- c("data", "open", "high", "low", " close", "volume")
nvidia1 <- csv_nvidia[,c(1,5)]
#AMD 
# fn_amd<- "https://stooq.pl/q/d/l/?s=amd.us&d1=20090405&d2=20240405&i=d"
fn_amd <- "https://drive.google.com/uc?id=1dLH9tH4nR5EuZTd-Wh_3YAxjuFsta3d7&export=download"
csv_amd <- read_csv(fn_amd)
head(csv_amd)
colnames(csv_amd) <- c("data", "open", "high", "low", " close", "volume")
amd1 <- csv_amd[,c(1,5)]
#INTEL
# fn_in <- "https://stooq.pl/q/d/l/?s=intc.us&d1=20090405&d2=20240405&i=d"
fn_in <- "https://drive.google.com/uc?id=1TdBcCrSIOnzPkYKAWGOFg3O0GRXZ1TJu&export=download"
csv_in <- read_csv(fn_in)
colnames(csv_in)<- c("data", "open", "high", "low", " close", "volume")
int1 <- csv_in[,c(1,5)]
#Ocena kompletności danych 
#NVIDIA
print(which(is.na(nvidia1)))
#AMD
print(which(is.na(amd1)))
#INTEL
print(which(is.na(int1)))

#Szeregi czasowe stóp zwrotu
N <- 1250
nvidia_sl <- zoo(nvidia1[,2], order.by = nvidia1$data)
sl_n <- diff(log(nvidia_sl))
sl_n <- tail(sl_n, N)
amd_sl <- zoo(amd1[,2], order.by = amd1$data)
sl_a <-diff(log(amd_sl))
sl_a <- tail(sl_a, N)
int_sl <- zoo(int1[,2], order.by = int1$data)
sl_i <-diff(log(int_sl))
sl_i <- tail(sl_i, N)
#Wyznaczenie średnia, odchylenie standardowe, maksimum, minimum, mediana.
#NVIDIA 
mean(sl_n)
sd(sl_n)
min(sl_n)
max(sl_n)
#AMD 
mean(sl_a)
sd(sl_a)
min(sl_a)
max(sl_a)
#INTELL
mean(sl_i)
sd(sl_i)
min(sl_i)
max(sl_i)
#INTEL 
#Wykresy szeregow czasowych
#NVIDIA 
wykres1 <- ggplot(sl_n, aes(Index)) + 
  theme_bw() + labs(x= "DATA", y= "logarytmiczne")+
  geom_line(aes(y=sl_n),color = "purple")
#AMD
wykres2 <- ggplot(sl_a, aes(Index)) + 
  theme_bw() + labs(x= "DATA", y= "logarytmiczne")+
  geom_line(aes(y=sl_a),color = "blue")
wykres1 + wykres2 
#INTEL
wykres3 <- ggplot(sl_i, aes(Index)) + 
  theme_bw() + labs(x= "DATA", y= "logarytmiczne")+
  geom_line(aes(y=sl_i),color = "red")
wykres1 + wykres2 + wykres3
#korelacja
#AMD i NVIDIA
cor(sl_a, sl_n)
#AMD i INTEL
cor(sl_a,sl_i)
#INTEL i NVIDIA 
cor(sl_i,sl_n)
#Wynaczenie skosnosci i kurtozy
#NVIDIA 
N<- coredata(sl_n)
M1_N <- moment(N, order = 1, central = FALSE, na.rm = TRUE)
M2_N <- moment(N, order = 2, central = FALSE, na.rm = TRUE)
#skośność 
skewness(sl_n)
#kurtoza 
kurtosis(sl_n)
#AMD
A<- coredata(sl_a)
M1_A <- moment(A, order = 1, central = FALSE, na.rm = TRUE)
M2_A <- moment(A, order = 2, central = FALSE, na.rm = TRUE)
#skośność 
skewness(sl_a)
#kurtoza 
kurtosis(sl_a)
#INTEL
I<- coredata(sl_i)
M1_I <- moment(I, order = 1, central = FALSE, na.rm = TRUE)
M2_I <- moment(I, order = 2, central = FALSE, na.rm = TRUE)
#skośność 
skewness(sl_i)
#kurtoza 
kurtosis(sl_i)

# wykresy 
#NVIDIA 
NVIDIA <- (N - M1_N) / sqrt(M2_N)
bwdth <- 0.1
AMD <- (A - M1_A) / sqrt(M2_A)
INTEL <- (I - M1_I) / sqrt(M2_I)
# Porównanie z rokładem normalnym
hist_1<-ggplot(data.frame(NVIDIA), aes(x = NVIDIA)) +
  theme_bw() +
  geom_histogram(binwidth = bwdth, color = "white", fill = "darkgreen", linewidth = 0.1) +
  stat_function(fun = function(x) dnorm(x) * 1250 * bwdth, color = "red", linewidth = 1.5)
hist_2<-ggplot(data.frame(AMD), aes(x = AMD)) +
  theme_bw() +
  geom_histogram(binwidth = bwdth, color = "white", fill = "darkgreen", linewidth = 0.1) +
  stat_function(fun = function(x) dnorm(x) * 1250 * bwdth, color = "red", linewidth = 1.5)
hist_3<-ggplot(data.frame(INTEL), aes(x = INTELL)) +
  theme_bw() +
  geom_histogram(binwidth = bwdth, color = "white", fill = "darkgreen", linewidth = 0.1) +
  stat_function(fun = function(x) dnorm(x) * 1250 * bwdth, color = "red", linewidth = 1.5)
hist_1+ hist_2 + hist_3

setwd ("D:/bagusco/bagusco/Kuliah S2 --- Pemodelan Klasifikasi/Genap 2017 2018")
wine <- read.csv("white_wine2.csv")
colnames(wine)

data <- wine[,c("alcohol", "density", "quality")]
head(data)
data$kelas.kualitas <- ifelse(data$quality > 6, 1, 2)

library(class)
prediksi <- knn(data[,1:2], data[,1:2], data[,4], k = 25)

library(caret)
confusionMatrix(prediksi, data[,4])



########### VALIDASI ##############


acak <- sample(1:nrow(data), 3000)

data.training <- data[acak,]
data.testing <- data[-acak,]

x.training <- data.training[,c(1,2)]
y.training <- data.training[,4]

x.testing <- data.testing[,c(1,2)]
y.testing <- data.testing[,4]


#membakukan data training: dikurangi ratarata dan dibagi stdev
rata <- apply(x.training, 2, mean)
rata.rata <- matrix(rata,nrow(x.training),2, byrow=TRUE)

sb <- apply(x.training, 2, sd)
simpangan.baku <- matrix(sb, nrow(x.training),2, byrow=TRUE)

x.training.baku <- (x.training - rata.rata)/simpangan.baku

#membakukan data testing: dikurangi ratarata dan dibagi stdev
rata.rata <- matrix(rata,nrow(x.testing),2, byrow=TRUE)

simpangan.baku <- matrix(sb, nrow(x.testing),2, byrow=TRUE)

x.testing.baku <- (x.testing - rata.rata)/simpangan.baku

library(class)
prediksi <- knn(x.training.baku, x.testing.baku, y.training, k=31)

library(caret)
confusionMatrix(prediksi, y.testing)




########### VALIDASI : mencari k optimal##############


acak <- sample(1:nrow(data), 3000)

data.training <- data[acak,]
data.testing <- data[-acak,]

x.training <- data.training[,c(1,2)]
y.training <- data.training[,4]

x.testing <- data.testing[,c(1,2)]
y.testing <- data.testing[,4]


#membakukan data training: dikurangi ratarata dan dibagi stdev
rata <- apply(x.training, 2, mean)
rata.rata <- matrix(rata,nrow(x.training),2, byrow=TRUE)

sb <- apply(x.training, 2, sd)
simpangan.baku <- matrix(sb, nrow(x.training),2, byrow=TRUE)

x.training.baku <- (x.training - rata.rata)/simpangan.baku

#membakukan data testing: dikurangi ratarata dan dibagi stdev
rata.rata <- matrix(rata,nrow(x.testing),2, byrow=TRUE)

simpangan.baku <- matrix(sb, nrow(x.testing),2, byrow=TRUE)

x.testing.baku <- (x.testing - rata.rata)/simpangan.baku

library(class)
library(caret)
nilai.k <- seq(1, 100, by=2)
akurasi <- NULL
for (k in nilai.k) {
prediksi <- knn(x.training.baku, x.testing.baku, y.training, k=k)
kinerja <- confusionMatrix(prediksi, y.testing)
akurasi <- c(akurasi, kinerja$overall[1])
}
plot(nilai.k, akurasi,type="b")


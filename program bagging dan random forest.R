library(caret)
library(rpart)

bankloan <- read.csv("D:/bankloan.csv", header=TRUE, sep=",")

set.seed(100)
untuk.testing <- createDataPartition(bankloan$default, p=0.3, list=FALSE)

data.testing <- bankloan[untuk.testing,]  #membuat data testing
data.training <- bankloan[-untuk.testing,]  #membuat data training

nrow(data.training) #banyaknya observasi data training
nrow(data.testing) #banyaknya observasi data testing

set.seed(500)
k<-100
prediksi<-matrix(NA,nrow(data.testing),k)
for(i in 1:k){
  resample <- sample(1:nrow(data.training), replace=TRUE)
  contoh.boot <- data.training[resample,]
  tree <-rpart(default~., data=contoh.boot, method="class")
  prob <-predict(tree, data.testing)[,2]
  prediksi[,i] <-ifelse(prob<0.5, 0, 1)
}

vote1 <-apply(prediksi,1,sum)
prediksi.akhir <- ifelse(vote1 < k/2, 0, 1)

confusionMatrix(prediksi.akhir, data.testing$default, positive="1")



library(randomForest)
#set.seed(100)
model.forest <- randomForest(as.factor(default) ~ age + ed + employ + address
                             + income + debtinc + creddebt + othdebt,
                             data=data.training, importance=TRUE, ntree=2000, mtry=3)
prediksi.rf <- predict(model.forest, data.testing)

confusionMatrix(prediksi.rf, data.testing$default, positive="1")



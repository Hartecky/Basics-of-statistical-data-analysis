### ZADANIE 9
dane = read.table('http://theta.edu.pl/wp-content/uploads/2018/03/dane_kosiarki-1.txt',header=T)
library(MASS)
library(ROCR)
require(ipred)
sampleset = sample(seq(nrow(dane)), size = 18)

train_set = dane[sampleset, ]
test_set = dane[-sampleset, ]

# KWADRATOWA KLASYFIKACJA
dane.qda = qda(owner ~ income + lotsize, data = dane)
dane.qda
# Predykcja przynależności do klas na zbiorze treningowym
pred_train.qda = predict(dane.qda, newdata = train_set)
pred_train.qda

# Predykcja przynależności do klas na zbiorze testowym
pred.qda = predict(dane.qda, newdata = test_set)
pred.qda

# Procent prawidłowo zaklasyfikowanych obserwacji
qda_classify = pred_train.qda$class
qda_classify
qda_classperc = sum(qda_classify==train_set[,3])/18
qda_classperc

# [1] 0.6666667

# Tabela predykcji na zbiorze uczącym
QDA.prediction = table(pred_train.qda$class, train_set$owner)
QDA.prediction


# QDA with CROSS VALIDATION
CV.qda = qda(owner ~ income + lotsize, data = dane, cv=TRUE)
pred.CV = predict(CV.qda, newdata = train_set)
table(train_set$owner, pred.CV$class, dnn = c("True", "Predicted"))

# ZAKLASYFIKOWANIE NOWEGO OBIEKTU
predict(dane.qda, new = data.frame(income=100, lotsize=22.5))

#Procent właściwie zaklasyfikowanych obiektów:
CV.qda.classify = pred.CV$class
CV.qda.classperc = sum(CV.qda.classify==train_set[,3])/18
  
# METODA QDA I LDA Z 10K KROSWALIDEJSZYN
# LDA
dane.lda = lda(owner ~ income + lotsize, data = dane, trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
dane.lda
lda.predict = predict(dane.lda, newdata = train_set)
lda_classify = lda.predict$class
lda_classperc = sum(lda.predict$class==train_set[,3])/18
lda_classperc
table(train_set$owner, lda_classify)

# QDA
qda.10k = qda(dane$owner ~ dane$income + dane$lotsize, dana = dane, trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
qda.10k.pred = predict(qda.10k, newdata = train_set)
qda.10k.classify = qda.10k.pred$class
qda.10k.classperc = sum(qda.10k.classify==train_set[,3])/18

table(dane$owner, qda.10k.classify)

Z = train_set$owner == "1"
X = train_set$owner == "2"

# Z-ROC - QDA 1
z.qda = qda(Z~income + lotsize, data = train_set)
z.predict = predict(z.qda, newdata = train_set)
z.pred = prediction(z.predict$posterior[,2], Z)
z.perf = performance(z.pred, "tpr", "fpr")
plot(z.perf, colorize=FALSE)

# X-ROC - QDA 2
x.qda = qda(X~income + lotsize, data = train_set)
x.predict = predict(x.qda, newdata = train_set)
x.pred = prediction(x.predict$posterior[,1], X)
x.perf = performance(x.pred, "tpr", "fpr")
plot(x.perf, colorize=FALSE)

# Z-ROC-LDA
z.lda = lda(Z~income + lotsize, data = train_set)
z.lda.predict = predict(z.lda, newdata = train_set)
z.lda.pred = prediction(z.lda.predict$posterior[,2], Z)
z.lda.perf = performance(z.lda.pred, "tpr", "fpr")
plot(z.lda.perf, colorize=FALSE)

# X-ROC-LDA
x.lda = lda(X~income + lotsize, data = train_set)
x.lda.predict = predict(x.lda, newdata = train_set)
x.lda.pred = prediction(x.lda.predict$posterior[,1], X)
x.lda.perf = performance(x.lda.pred, "tpr", "fpr")
plot(x.lda.perf, colorize=FALSE)

trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE)


### ------------------------
### ZADANIE 11

library(MASS)
library(ROCR)
require(ipred)

data("Pima.te")
attach(Pima.te)

sampleset = sample(seq(nrow(Pima.te)), size = 249)

train_set = Pima.te[sampleset, ]
test_set = Pima.te[-sampleset, ]

# ANALIZA METODĄ LDA
dane.lda = lda(type ~., data = train_set)
dane.lda

#Predykcja na zbiorze treningowym
pred.lda = predict(dane.lda, newdata = train_set)
LDA.prediction = table(pred.lda$class, train_set$type)

err.lda = (sum(LDA.prediction) - sum(diag(LDA.prediction)))/sum(LDA.prediction)
# [1] 0.2208835

mypredict.lda <- function(object,newdata){
  predict(object, newdata = newdata )$class
  }

errorest(type~., data = train_set, model=lda, estimator = "cv", predict = mypredict.lda)

errorest(type ~ ., data = train_set, model=lda, estimator="boot",
           predict = mypredict.lda)

# ANALIZA METODĄ QDA
dane.qda = qda(type~., data = Pima.te)
dane.qda

pred.qda = predict(dane.qda, newdata = train_set)
QDA.prediction = table(pred.qda$class, train_set$type)
err.qda = (sum(QDA.prediction) - sum(diag(QDA.prediction)))/sum(QDA.prediction)
mypredict.qda <- function(object, newdata){
  predict (object, newdata = newdata)$class
  }
errorest(type~., data = train_set, model=qda, estimator = "cv", predict = mypredict.qda)

errorest(type ~ ., data = train_set, model=qda, estimator="boot",
         predict = mypredict.qda)

# Wyznaczenie czulosci i specyficznosci
# LDA
lda.ACC = 47 / (47+18)
# [1] 0.7230769
lda.SPS = 147 / (147+37)
# [1] 0.798913

#QDA
qda.ACC = 54 / (54 + 17)
# [1] 0.7605634
qda.SPS = 148 / (148+30)
#[1] 0.8314607

# Krzywe ROC

Z = train_set$type == "Yes"
X = train_set$type == "No"

# QDA
z.qda = qda(Z~. -type, data = train_set)
z.predict = predict(z.qda, newdata = train_set)
z.pred = prediction(z.predict$posterior[,2], Z)
z.perf = performance(z.pred, "tpr", "fpr")
plot(z.perf, colorize=FALSE)

x.qda = qda(X~. -type, data = train_set)
x.predict = predict(x.qda, newdata = train_set)
x.pred = prediction(x.predict$posterior[,1], X)
x.perf = performance(x.pred, "tpr", "fpr")
plot(x.perf, colorize=FALSE)

# LDA 

# Z-ROC-LDA
z.lda = lda(Z~. -type, data = train_set)
z.lda.predict = predict(z.lda, newdata = train_set)
z.lda.pred = prediction(z.lda.predict$posterior[,2], Z)
z.lda.perf = performance(z.lda.pred, "tpr", "fpr")
plot(z.lda.perf, colorize=FALSE)

# X-ROC-LDA
x.lda = lda(X~. -type, data = train_set)
x.lda.predict = predict(x.lda, newdata = train_set)
x.lda.pred = prediction(x.lda.predict$posterior[,1], X)
x.lda.perf = performance(x.lda.pred, "tpr", "fpr")
plot(x.lda.perf, colorize=FALSE)

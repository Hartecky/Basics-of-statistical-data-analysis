# ANALIZA DYSKRYMINACYJNA OPARTA NA REGRESJI LOGISTYCZNEJ

### ZADANIE 7 ###
dane = read.csv("http://theta.edu.pl/wp-content/uploads/2018/03/urine.csv", header = F, sep = ",")
colnames(dane) = c("id", "pres", "gravity", "ph", "osmo", "cond", "urea", "calc")

rows = nrow(dane)
cols = ncol(dane)

# Przygotowanie danych - usunięcie wierszy zawierających wartości NA
dane = dane[2:cols]
dane = dane[complete.cases(dane),]

# Podział danych na zbiory uczące i testowe 

sampleset = sample(seq(nrow(dane)), size = 50)

train_set = dane[sampleset, ]
test_set = dane[-sampleset, ]

# Model regresji logistycznej bazując na zbiorze uczącym
model.log = glm(pres ~ ., data = train_set, family = binomial())
summary(model.log)

# Eliminujemy zmienną osmo
model.log = glm(pres ~ . -osmo, data = train_set, family = binomial())
summary(model.log)

# Eliminujemy zmienną osmo, ph
model.log = glm(pres ~ . -ph -osmo, data = train_set, family = binomial())
summary(model.log)


# Klasyfikator w oparciu o próbę uczącą
pred.values_train = predict(model.log, newdata = train_set, type = "response")
pred.values_train[pred.values_train > 0.5] = 1
pred.values_train[pred.values_train <= 0.5] = 0

# Klasyfikator w oparciu o próbę testową
pred.values_test = predict(model.log, newdata = test_set, type = "response")
pred.values_test[pred.values_test > 0.5] = 1
pred.values_test[pred.values_test <= 0.5] = 0

# Tabela klasyfikacji próby uczącej
Train.prediction = table(pred.values_train, train_set$pres)
Train.prediction

# Tabela klasyfikacji próby testowej
Test.prediction = table(pred.values_test, test_set$pres)
Test.prediction

# Błąd klasyfikacji próby uczącej
err1 = (sum(Train.prediction) - sum(diag(Train.prediction)))/sum(Train.prediction)
err1
# [1] 0.24

# Błąd klasyfikacji próby testowej
err2 = (sum(Test.prediction) - sum(diag(Test.prediction)))/sum(Test.prediction)
err2 


# Klasyfikator w oparciu o metodę LDA
library(MASS)
model.lda = lda(pres ~ . -ph -osmo , data = train_set)
model.lda
pred.lda = predict(model.lda, newdata = test_set)$class
pred.lda
# Tabela klasyfikacji w oparciu o metodę LDA
LDA.prediction = table(pred.lda, test_set$pres)
LDA.prediction
#   0  1
# 0 13  4
# 1  1  9

# Błąd klasyfikacji LDA
err.lda = (sum(LDA.prediction) - sum(diag(LDA.prediction)))/sum(LDA.prediction)
  err.lda
# [1] 0.1851852


### ZADANIE 8 ###

dane = read.csv("http://theta.edu.pl/wp-content/uploads/2018/03/puls2.csv", header = TRUE, sep = ";")

# Przekodowanie wartości na 0 i 1 typu factor
dane$TetnoSpocz = ifelse(dane$TetnoSpocz=="Niskie",0,1)
dane$TetnoSpocz = as.factor(dane$TetnoSpocz)
dane$Palacz = as.factor(dane$Palacz)

sampleset = sample(seq(nrow(dane)), size = 69)

#Podział danych na zbiory uczące oraz testowe
train_set = dane[sampleset, ]
test_set = dane[-sampleset, ]

# Model regresji logistycznej
model.log = glm(TetnoSpocz ~ Palacz + Waga, data = train_set, family = binomial())
summary(model.log)

# Eliminujemy zmienną Palacz
model.log = glm(TetnoSpocz ~ Waga, data = train_set, family = binomial())
summary(model.log)

# Model regresji logistycznej z interakcją
model.interakcja = glm(TetnoSpocz ~ Palacz*Waga, data = train_set, family = binomial())
summary(model.interakcja)

# modele zawierające jako zmienne objaśniające następujące zestawy zmiennych
# M1:Palacz,Waga,Palacz*Waga
# M2:Waga,Palacz*Waga
# M3:Palacz,Palacz*Waga
# M4:Palacz*Waga

M1 = glm(TetnoSpocz ~ Palacz + Waga + Palacz:Waga, data=train_set, family = binomial())
summary(M1)

M2 = glm(TetnoSpocz ~ Waga + Palacz:Waga, data = train_set, family = binomial())
summary(M2)

M3 = glm(TetnoSpocz~Palacz + Palacz:Waga, data = train_set, family = binomial())
summary(M3)

# Najbardziej istotny model
M4 = glm(TetnoSpocz ~ Palacz:Waga, data = train_set, family = binomial())
summary(M4)

# Klasyfikatory w oparciu o próbę uczącą 

# Model bez interakcji
# Próba ucząca
pred.log = predict(model.log, newdata = train_set, type = "response")
pred.log[pred.log > 0.5] = 1
pred.log[pred.log <= 0.5] = 0

#Rysunek na próbie uczącej
log.prediction = table(pred.log, train_set$TetnoSpocz)
log.prediction

#Próba testowa
pred.logtest = predict(model.log, newdata = test_set, type = "response")
pred.logtest[pred.logtest > 0.5] = 1
pred.logtest[pred.logtest <= 0.5] = 0

#Rysunek na próbie testowej
logtest.prediction = table(pred.logtest, test_set$TetnoSpocz)
logtest.prediction


# Model z interakcją
# Próba ucząca
pred.interaction = predict(M4, newdata = train_set, type="response")
pred.interaction[pred.interaction > 0.5] = 1
pred.interaction[pred.interaction <= 0.5] = 0

#Rysunek na próbie testowej
inter.prediction = table(pred.interaction, train_set$TetnoSpocz)
inter.prediction

#Próba testowa
pred.interactiontest = predict(M4, newdata = test_set, type="response")
pred.interactiontest[pred.interactiontest > 0.5] = 1
pred.interactiontest[pred.interactiontest <= 0.5] = 0

#Rysunek na próbie uczącej
inter.predictiontest = table(pred.interactiontest, test_set$TetnoSpocz)
inter.predictiontest

# Błąd klasyfikacji na próbie testowej
err.log = (sum(logtest.prediction) - sum(diag(logtest.prediction)))/sum(logtest.prediction)
err.log
err.interaction = (sum(inter.prediction) - sum(diag(inter.prediction)))/sum(inter.prediction)
err.interaction


# Klasyfikatory LDA w oparciu o próbę uczącą - pełny model logistyczny
library(MASS)
modellog.lda = lda(TetnoSpocz ~ Waga, data = train_set)
predlog.lda = predict(modellog.lda, newdata = test_set)$class

# Tabela klasyfikacji w oparciu o metodę LDA
LDAlog.prediction = table(predlog.lda, test_set$TetnoSpocz)
LDAlog.prediction

# Błąd klasyfikacji LDA
errlog.lda = (sum(LDAlog.prediction) - sum(diag(LDAlog.prediction)))/sum(LDAlog.prediction)
errlog.lda

# Klasyfikatory LDA w oparciu o próbę uczącą - model logistyczny z interakcją 
modelint.lda = lda(TetnoSpocz~Waga*Palacz, data = train_set)
predint.lda = predict(modelint.lda, newdata = test_set)$class

# Tabela klasyfikacji
LDAint.prediction = table(predint.lda, test_set$TetnoSpocz)
LDAint.prediction

# Błąd klasyfikacji LDA
errint.lda = (sum(LDAint.prediction) - sum(diag(LDAint.prediction)))/sum(LDAint.prediction)
errint.lda

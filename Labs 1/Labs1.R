data() # Lista wszystkich dostępnych zestawów danych

# Wczytanie dostępnego zestawu danych
library("ISwR")
try(data(package ="ISwR"))
data("kfm")
help("kfm")
attach(kfm)

# Zamiana płci na wartości binarne 1 - boy, 0 - girl
kfm$sex = ifelse(kfm$sex=='boy',1,0)

#Wykres zależności parami
par(mex =0.5)
pairs(kfm , gap=0, cex.labels =0.9)

#Macierz kowariancji całego zbioru danych
cor(kfm)

# Model regresji wielokrotnej
model = lm(dl.milk ~ sex + weight + ml.suppl + mat.weight + mat.height)
summary(model)

# Model regresji wielokrotnej po pierwszym kroku
model = lm(dl.milk ~ sex + weight + ml.suppl + mat.height)
summary(model)

# Model regresji wielokrotnej po drugim kroku
model = lm(dl.milk ~ weight + mat.height + ml.suppl)
summary(model)

# Model regresji wielokrotnej po trzecim kroku
model = lm(dl.milk ~ weight + mat.height)
summary(model)

# Ostateczny model regresji
model = lm(dl.milk ~ weight)
summary(model)

### -----------------------------------------------
### -----------------------------------------------
### -----------------------------------------------

library(datasets)
data(iris)
attach(iris)
Sepal.Width

library(ggplot2)
library(dplyr)

#Histogramy

#Długość kielicha
filter(iris, Species == Species) %>% 
  ggplot(aes(x = Sepal.Length, color = Species, label = Species, fill = Species)) + 
  geom_histogram(binwidth = 0.05) + 
  facet_wrap(~Species, scales = "free_y") +
  labs(x = "Długość kielicha", y = "Liczebność") + 
  theme_bw() 

#Szerokość kielicha
filter(iris, Species == Species) %>% 
  ggplot(aes(x = Sepal.Width, color = Species, label = Species, fill = Species)) + 
  geom_histogram(binwidth = 0.05) + 
  facet_wrap(~Species, scales = "free_y") +
  labs(x = "Długość kielicha", y = "Liczebność") + 
  theme_bw()

#Wyszczególnienie osobnych gatunków kwiatów
iris.versicolor <- iris[iris$Species=="versicolor",1:4]
iris.setosa <- iris[iris$Species=="setosa",1:4]
iris.virginica <- iris[iris$Species=="virginica",1:4]

#Wykres zależności parami dla każdego z gatunków osobno
par(mex =0.2)

pairs(iris.versicolor , gap=0, cex.labels =0.9)
pairs(iris.setosa , gap=0, cex.labels =0.9)
pairs(iris.virginica , gap=0, cex.labels =0.9)

#Model regresji wielokrotnej dla gatunku setosa
model = lm(Petal.Length ~ Petal.Width + Sepal.Length + Sepal.Width, iris.setosa)
summary(model)  

#Model regresji wielokrotnej po pierwszym kroku
model = lm(Petal.Length ~ Petal.Width + Sepal.Length, iris.setosa)
summary(model)    

#Model regresji wielokrotnej po drugim kroku
model = lm(Petal.Length ~ Petal.Width, iris.setosa)
summary(model)

#Korelacja długości płatka i długości kielicha gat. Versicolor
cor.test(iris.versicolor$Sepal.Length, iris.versicolor$Petal.Length)

#Korelacja długości płatka i długości kielicha gat. Setosa
cor.test(iris.setosa$Sepal.Length, iris.setosa$Petal.Length)

#Korelacja długości płatka i długości kielicha gat. Virginica
cor.test(iris.virginica$Sepal.Length, iris.virginica$Petal.Length)


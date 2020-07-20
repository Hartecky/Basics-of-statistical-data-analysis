### EXCERCISES LABS3 FISHER LINEAR DISCRIMINATION

##############################
### ----- ZADANIE 5 -----  ###
##############################
dane = read.table('http://theta.edu.pl/wp-content/uploads/2018/03/dane_kosiarki-1.txt',header=T)

#Przedstawienie danych na wykresie punktowym
plot(dane$income, dane$lotsize, type = 'p')

#Przedstawione dane na rysunku z wykorzystaniem różnych symboli klas
points(dane$income[dane$owner == 1], dane$lotsize[dane$owner == 1], col = 'red')

# Pakiet MASS
library(MASS)
  
# Analiza dyksryminacyjna
dane_lda = lda(dane$owner ~ dane$income + dane$lotsize, data = dane)
dane_lda
# Wartości prawdopodobieństw a priori
# Prior probabilities of groups:
#   1   2 
# 0.5 0.5 

# Średnie wartości zmiennych objaśniających
# Group means:
#   dane$income dane$lotsize
# 1    26.49167    10.133333
# 2    19.13333     8.816667

# Współczynniki funkcji dyskryminacyjnych
# Coefficients of linear discriminants:
#                     LD1
# dane$income  -0.1453404
# dane$lotsize -0.7590457

# Współczynniki funkcji dyskryminacyjnych - SVD
dane_lda$svd
# [1] 5.067684

#Rysunek
LD1 = predict(dane_lda)$x[,1]
LD1

plot(LD1, xlab="Pierwsza zmienna kan", ylab = "Druga zmienna kan", type="n")
text(cbind(LD1), labels=unclass(dane$owner))

# Rysunek cd.

sum(LD1*(dane$owner=="1")) / sum(dane$owner=="1")
# [1] -1.034437

sum(LD1*(dane$owner=="2")) / sum(dane$owner=="2")
# 1.034437

# Predykcja na nowym zbiorze testowym
dane_predict = predict(dane_lda, dane[,1:2])
dane_predict$class
# > dane_predict$class
#  [1] 2 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 1 2 2 2 2 2 2 2
# Levels: 1 2

# Prawdopodobieństwa przynależności do klas (a posteriori)
dane_predict$posterior
#              1          2
# 1  0.217968446 0.78203155
# 2  0.505507885 0.49449211
# 3  0.847632493 0.15236751
# ...

# Współczynniki funkcji dyskryminacyjnych
dane_predict$x
#            LD1
# 1   0.61750748
# 2  -0.01064948
# 3  -0.82951029
# ...

# Klasy do których należą obiekty
dane_classify = dane_predict$class
dane_classify
# > dane_classify
#  [1] 2 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 1 2 2 2 2 2 2 2
# Levels: 1 2

# Poprawność klasyfikacji
dane_classperc = sum(dane_classify==dane[,3])/24
dane_classperc
# [1] 0.875
# Wynik świadczy o tym, że model klasyfikuje 88% obserwacji

# Ocena jakości dokonanej predykcji
table(Original=dane$owner, Predicted = predict(dane_lda)$class)
#         Predicted
# Original  1  2
#        1 11  1
#        2  2 10
# 11 z 12 obserwacji z grupy "1" zostało poprawnie zaklasyfikowane przez model
# 10 z 12 obserwacji z grupy "2" zostało poprawnie zaklasyfikowane przez model
##############################
### ----- ZADANIE 6 -----  ###
##############################
dane = read.table("http://theta.edu.pl/wp-content/uploads/2018/03/dane_wino.txt", header = F, sep = ",")

colnames(dane) = c("crop_type", "Concentration_1", "Concentration_2", "Concentration_3", "Concentration_4", "Concentration_5", "Concentration_6", "Concentration_7", "Concentration_8", "Concentration_9", "Concentration_10", "Concentration_11", "Concentration_12", "Concentration_13")

dane_lda = lda(dane$crop_type ~ .,data = dane)
dane_lda$svd

# Prawdopodobieństwa a priori
# Prior probabilities of groups:
#         1         2         3 
# 0.3314607 0.3988764 0.2696629
dane_lda$means

#Ocena mocy funkcji dyskryminacji
dane_lda$svd
# [1] 28.18958 19.00634

#Rysunek 
LD1 = predict(dane_lda)$x[,1]
LD2 = predict(dane_lda)$x[,2]

plot(LD1, LD2, xlab = "Pierwsza zmienna kanoniczna", ylab = "Druga zmienna kanoniczna", type = "n")
text(cbind(LD1 ,LD2),labels=unclass(dane$crop_type))

sum(LD1*(dane$crop_type=="1")) / sum(dane$crop_type=="1")
# [1] -3.422489
sum(LD2*(dane$crop_type=="1")) / sum(dane$crop_type=="1")
# [1] 1.691674
sum(LD1*(dane$crop_type=="2")) / sum(dane$crop_type=="2")
# [1] -0.07972623
sum(LD2*(dane$crop_type=="2")) / sum(dane$crop_type=="2")
# [1] -2.472656
sum(LD1*(dane$crop_type=="3")) / sum(dane$crop_type=="3")
# [1] 4.324737
sum(LD2*(dane$crop_type=="3")) / sum(dane$crop_type=="3")
# [1] 1.57812

# Predykcja na nowym zbiorze testowym
dane_predict = predict(dane_lda, dane[,2:14])
dane_predict$class

dane_predict$posterior
  
dane_predict$x

dane_classify = dane_predict$class
dane_classify

dane_classperc = sum(dane_classify==dane[,1])/178
dane_classperc

table(Original = dane$crop_type, Predicted = predict(dane_lda)$class)
  

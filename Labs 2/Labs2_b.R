data("stackloss")
attach(stackloss)

#Model z dwoma zmiennymi
model = lm(stack.loss ~ Water.Temp + Acid.Conc., data = stackloss)
summary(model)

# W modelu powinna się znaleźć tylko zmienna Water.Temp
# ponieważ jest wysokoistotna w przeciwieństwie do Acid.Conc.

model = lm(stack.loss ~ Water.Temp)
summary(model)

#Wykres punktowy z dopasowaną prostą
plot(stack.loss ~ Water.Temp)
abline(model)
ggplot(data = stackloss, aes(x = Water.Temp, y = stack.loss)) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle("Wykres punktowy z dopasowaną prostą do modelu") + 
  theme_bw()
#Odległości Cooka
influencePlot(model, id.method="identify", id.n = 1, id.cex=1, id.col=palette()[1], main="Wykres wpływów", sub="Rozmiar okręgów proporcjonalny do odległości Cooka")
# StudRes       Hat      CookD
# 1 1.8759536 0.2221163 0.44361619
# 2 0.6282651 0.2221163 0.05820793
# 3 1.9179109 0.1239276 0.22802446

#Poziom krytyczny - decyzja o usunięciu obserwacji
lev_val_1 = 4 / length(stack.loss)
#0.1904

#Obserwacje 1 i 2 są odstające
influence.measures(model)

sample_size = nrow(stackloss)
odl_cooka = cooks.distance(model)
influential = as.numeric(names(odl_cooka)[(odl_cooka > (4/sample_size))])

fitted_data = stackloss[-influential,]

fitted_plot = ggplot(data=fitted_data, aes(x = Water.Temp, y=stack.loss)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  ggtitle("Wykres po usunięciu wartości odstających") + 
  theme_bw()

fitted_plot
#Dopasowany model
fitted_model = lm(stack.loss~Water.Temp, data = fitted_data)
summary(model)
summary(fitted_model)
#obserwacje odstające korzystając z wektora reszt studenty-zowanych
residuals_student = rstudent(fitted_model)
outlier_stack = residuals_student[which.max(abs(residuals_student))]
outlier_stack
# 2 
# 2.671091 

#Obserwacje odstajace  korzystając z testu Outliertest
outlierTest(fitted_model)
# No Studentized residuals with Bonferroni p < 0.05
# Largest |rstudent|:
#   rstudent unadjusted p-value Bonferroni p
# 2 2.671091           0.016733      0.31793

### ---------------------------------------------------------------------------------
dane = read.table("http://theta.edu.pl/wp-content/uploads/2018/03/DANE_predkosc_reakcji.txt", header = FALSE, sep = " ")
colnames(dane) = c("Prędkość", "Koncentracja")
attach(dane)

#Wykres podstawowy + wykres reszt
model1 = lm(Prędkość~Koncentracja)
ggplot(data = dane, aes(x = Koncentracja, y = Prędkość)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_bw()

plot(Prędkość~Koncentracja)
abline(model1)
plot(model1$fitted.values,model1$residuals)

#Pierwsza transformacja danych + wykres + wykres reszt
logpred = log(Prędkość)
model2 = lm(logpred~Koncentracja)
ggplot(data = dane, aes(x = Koncentracja, y = logpred)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(data = dane, aes(x = model2$fitted.values, model2$residuals)) +
  geom_point() + 
  theme_bw()


#Druga transformacja + wykres + wykres reszt
logkonc = log(Koncentracja)
model3 = lm(logpred~logkonc)
ggplot(data = dane, aes(x = logkonc, y = logpred)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(data = dane, aes(x = model3$fitted.values, model3$residuals)) +
  geom_point() + 
  theme_bw()

#Trzecia transformacja + wykres zależności + wykres reszt
model4 = lm(Prędkość~logkonc)
ggplot(data = dane, aes(x = Prędkość, y = logpred)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(data = dane, aes(x = model4$fitted.values, model4$residuals)) +
  geom_point() + 
  theme_bw()
#Regresja wielomianowa
model_wielomian = lm(Prędkość ~ Koncentracja + I(Koncentracja^2))
summary(model_wielomian)

plot(Prędkość, Koncentracja)
curve(10.69371 + 2.77768 * x - 0.09782*x^2 , add = T)

model_st6 = lm(Prędkość ~ Koncentracja + I(Koncentracja^2) + I(Koncentracja^3) + I(Koncentracja^4) + I(Koncentracja^5) + I(Koncentracja^6))
summary(model_st6)

plot(Prędkość~Koncentracja)
curve(-120.76838 + 325.45467 * x -246.63164 * x^2 + 83.77377 * x^3 - 14.25726*x^4 + 1.18499*x^5 -0.03785*x^6, add = T)


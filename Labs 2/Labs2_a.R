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

#Odległości Cooka
influencePlot(model, id.method="identify", id.n = 1, id.cex=1, id.col=palette()[1], main="Wykres wpływów", sub="Rozmiar okr prop do odl Cooka")
# StudRes       Hat      CookD
# 1 1.8759536 0.2221163 0.44361619
# 2 0.6282651 0.2221163 0.05820793
# 3 1.9179109 0.1239276 0.22802446

#Poziom krytyczny - decyzja o 
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

#Dopasowany model
fitted_model = lm(stack.loss~Water.Temp, data = fitted_data)

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


## Carregar pacotes necessários

install.packages("car")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("lmtest")
install.packages("nortest")
install.packages("plotly")
install.packages("psych")
install.packages("QuantPsyc")
install.packages("rgl")
install.packages("rstatix")
install.packages("scatterplot3d")

library(car)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(lmtest)
library(nortest)
library(plotly)
library(psych)
library(QuantPsyc)
library(rgl)
library(rstatix)
library(scatterplot3d)


# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------


## Dados de Entrada.

# Dados do Exemplo:
dados <- read.csv2('Notas.csv', fileEncoding = "latin1") 
View(dados)                                 
x1 <- dados$Tempo_Rev
x2 <- dados$Tempo_Sono
y <- dados$Notas


# Exemplo Padrão:
set.seed(123)
n <- 1000
x1 <- rnorm(n, mean = 50, sd = 10)
x2 <- x1 #  + rnorm(n, mean = 50, sd = 10)
y  <- -x1 + x2 - 40 + rnorm(n, mean = 0, sd = 5) 
dados <- data.frame(x1, x2, y)

# Exemplo com resíduos dependentes
set.seed(123)
n <- 1000
dias <- 1:n
x1 <- rnorm(n, mean = 50, sd = 10)
x2 <- x1 + rnorm(n, mean = 50, sd = 10)
residuos_dependentes <- arima.sim(model = list(ar = 0.7), n)
y  <- -x1 + x2 - 40 + residuos_dependentes
dados <- data.frame(dias, x1, x2, y)

# Multicolinearidade: x2 <- f(x1) ou x1 <- f(x2) não-linearmente
# Não-normalidade: y depende não linearmente de x1 e/ou x2
# Outliers: y depende não linearmente de x1 e/ou x2 (Obs: Nem sempre)
# Exemplo de heterocedasticidade: sd da variável y dependente de x1 e/ou x2


# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------


## Construção do modelo:
modelo <- lm(y ~ x1 + x2, dados)

# Análise Gráfica: Gráfico de Dispersão vs. Modelo de RLM:
scatterplot3d(x1, x2, y)


# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------


## Multicolinearidade:
# Calcular o Fator de Inflação da Variância (VIF)
vif_values <- vif(modelo)
vif_values

# Verificar multicolinearidade
if (any(vif_values > 10)) {
  print("Há multicolinearidade significativa entre as variáveis independentes.")
} else {
  print("Não há multicolinearidade significativa entre as variáveis independentes.")
}


# Visualização da multicolinearidade
vif_df <- data.frame(Variable = names(vif_values), VIF = vif_values)
ggplot(vif_df, aes(x = Variable, y = VIF)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Fator de Inflação da Variância (VIF)",
       y = "VIF", x = "Variáveis")


# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------


## Normalidade dos resíduos:
residuos <- residuals(modelo)

## Análise Gráfica: 
#Histograma dos resíduos
hist(residuos, breaks = 20, main = "Histograma dos Resíduos", xlab = "Resíduos", col = "skyblue")
# Resíduos vs. Valores ajustados
fitted_values <- fitted(modelo)
plot(fitted_values, residuos, 
     main = "Resíduos vs. Valores Ajustados", 
     xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")


# Teste de Shapiro
shapiro_test <- shapiro.test(residuos)   # Teste de Shapiro-Wilk
p_valor <- shapiro_test$p.value
if (p_valor > 0.05) {
  sprintf("O valor de p é: %g. Portanto, a distribuição é normal.", p_valor)
} else {
  sprintf("O valor de p é: %g. Portanto, a distribuição não é normal.", p_valor)
}


# Teste de Anderson-Darling:
ad_test <- ad.test(residuos)
p_valor <- ad_test$p.value
if (p_valor > 0.05) {
  sprintf("O valor de p é: %g. Portanto, a distribuição é normal.", p_valor)
} else {
  sprintf("O valor de p é: %g. Portanto, a distribuição não é normal.", p_valor)
}


# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------


## Independência dos resíduos:
residuos <- residuals(modelo)


# Análise Gráfica: Gráfico dos resíduos ao longo do tempo.
plot(residuals(modelo), type = "b", main = "Resíduos ao longo do tempo", xlab = "Tempo", ylab = "Resíduos")


# Teste de Durbin-Watson
dw_test <- dwtest(modelo)
dw_test
p_valor <- dw_test$p.value
d_valor <- dw_test$statistic


if (p_valor > 0.05) {
  sprintf("O valor de p é: %g. Portanto, os resíduos são independentes.", p_valor)
} else {
  sprintf("O valor de p é: %g. Portanto, os resíduos não são independentes.", p_valor)
}

sprintf("Valor da Estatística d: %g", d_valor)


# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------


## Existência de outliers:
residuos <- residuals(modelo)


## Análise Gráfica: 
#Boxplot dos resíduos para identificação visual de outliers
ggplot(data.frame(residuos), aes(y = residuos)) + 
  geom_boxplot(fill = "lightblue", color = "black") +
  ggtitle("Boxplot dos Resíduos") +
  ylab("Resíduos")


# Gráfico de resíduos vs. valores ajustados
plot(fitted(modelo), residuos, 
     main = "Resíduos vs. Valores Ajustados", 
     xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")


## Distância de Cook
cooks_dist <- cooks.distance(modelo)
# Plotar a Distância de Cook
plot(cooks_dist, type = "h", main = "Distância de Cook", 
     ylab = "Distância de Cook", xlab = "Índice da Observação")
abline(h = 4/n, col = "red")  # Limite de corte sugerido
text(x = 1:n, y = cooks_dist, labels = ifelse(cooks_dist > 4/n, names(cooks_dist), ""), pos = 4, cex = 0.7)
# Identificar observações influentes
influential_points <- which(cooks_dist > 4/n)
print(unique(influential_points))


# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------


## Homocedasticidade:
residuos <- residuals(modelo)


# Análise Gráfica: Plot dos resíduos vs. valores ajustados
plot(fitted(modelo), residuos, 
     main = "Resíduos vs. Valores Ajustados", 
     xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")


# Teste de Breusch-Pagan:
bp_test <- bptest(modelo)
p_valor <- bp_test$p.value
if (p_valor > 0.05) {
  sprintf("O valor de p é: %g. Portanto, temos homocedasticidade.", p_valor)
} else {
  sprintf("O valor de p é: %g. Portanto, temos heterocedasticidade.", p_valor)
}


# Teste de White:
white_test <- bptest(modelo, ~ fitted(modelo) + I(fitted(modelo)^2))
p_valor <- white_test$p.value
if (p_valor > 0.05) {
  sprintf("O valor de p é: %g. Portanto, temos homocedasticidade.", p_valor)
} else {
  sprintf("O valor de p é: %g. Portanto, temos heterocedasticidade.", p_valor)
}


# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------
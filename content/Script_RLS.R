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
# ------------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------------
## Dados de Entrada (Regressão Linear Simples):

# Dados do Exemplo:
dados <- read.csv2('Vendas_CDs.csv', fileEncoding = "latin1") 
# View(dados)                                 
x <- dados$Publicidade
y <- dados$Vendas


# Dados Sintéticos:
set.seed(123)
n <- 1000 
x <- rnorm(n, mean = 50, sd = 5)
y <- 3*x - 30 + rnorm(n, mean = 0, sd = 4*x)
dados <- data.frame(x, y)


# Exemplo Heterocedasticidade (N pequeno):
x <- c(1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000)   # Renda
y <- c(300, 600, 900, 1600, 2000, 2400, 3500, 4000, 4500, 5000)       # Gasto
dados <- data.frame(x, y)


# Exemplo Heterocedasticidade (N grande):
set.seed(123)
n <- 1000
x <- rnorm(n, mean = 5000, sd = 1500)
summary(x)
for (i in 1:length(x)) {
  if (x[i] < 3500) {
    y[i] <- 0.30 * x[i]
  } else if (x[i] <= 6500) {
    y[i] <- 0.40 * x[i]
  } else {
    y[i] <- 0.50 * x[i]
  }
}
summary(y)
dados <- data.frame(x, y)

# ------------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------------
## Construção do modelo (Regressão Linear Simples):
modelo <- lm(y ~ x, dados)

# Análise Gráfica: Gráfico de Dispersão vs. Modelo de RLS:
plot(x, y)
abline(modelo, col = "blue")
# ------------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------------
## Normalidade dos resíduos:
residuos <- residuals(modelo)

## Análise Gráfica
# Calcular os resíduos padronizados
residuos_padronizados <- rstandard(modelo)
# Histograma dos resíduos padronizados:
hist(residuos, breaks = 20, main = "Histograma dos Resíduos", xlab = "Resíduos Padronizados", col = "skyblue")
# Gráfico Q-Q dos resíduos padronizados:
qqnorm(residuos_padronizados)
qqline(residuos_padronizados, col = "red")


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

# Gráfico Q-Q dos resíduos padronizados
qqnorm(residuos)
qqline(residuos, col = "red")

# Confirmação da Análise Gráfica:
summary(rstandard(modelo))


## Distância de Cook
cooks_dist <- cooks.distance(modelo)
# Plotar a Distância de Cook
plot(cooks_dist, type = "h", main = "Distância de Cook", 
     ylab = "Distância de Cook", xlab = "Índice da Observação")
abline(h = 4/n, col = "red")  # Limite de corte sugerido
text(x = 1:n, y = cooks_dist, labels = ifelse(cooks_dist > 4/n, names(cooks_dist), ""), pos = 4, cex = 0.7)
# Identificar observações influentes
influential_points <- which(cooks_dist > 10/n)
print(unique(influential_points))
# ------------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------------
## Independência dos resíduos:
residuos <- residuals(modelo)

# Análise Gráfica: Gráfico dos resíduos ao longo do tempo.
plot(residuals(modelo), type = "b", main = "Resíduos ao longo do tempo", xlab = "Tempo", ylab = "Resíduos")


# Teste de Durbin-Watson
dw_test <- dwtest(modelo)
dw_test
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
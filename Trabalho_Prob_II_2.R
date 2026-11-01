library(readxl)

#Gols_mandante_x_gols_visitante <- read_excel("C:/Users/lenovo/Downloads/Gols mandante x gols visitante.xlsx")
Gols_mandante_x_gols_visitante <- read_excel("C:/Users/GUILHERM/Downloads/Gols mandante x gols visitante (2).xlsx")
#Gols_mandante_x_gols_visitante <- read_excel("Coloca o seu aqui MIGUEL")

#View(Gols_mandante_x_gols_visitante)

mandantes <- Gols_mandante_x_gols_visitante$`X = gols mandante`
visitantes <- Gols_mandante_x_gols_visitante$`Y = gols visitante`

##### Distribuição conjunta impirica ###############

# A Distribuição conjunta impirica representa a probabilidade do time mandante (X) e o visitante (Y) marcarem determinadas quantidades de gols na mesma partida

x_y <- table("X" = mandantes, "Y" = visitantes)
prob_x_y <- x_y/49

x_y
prob_x_y

prob_x_y_df <- as.data.frame(prob_x_y)

########### Distribuiçao marginal #############

p_x <- tapply(prob_x_y_df$Freq, prob_x_y_df$X, sum)
prob_x <- data.frame("X" = sort(unique(mandantes)), "Probabilidade" = p_x)

p_y <- tapply(prob_x_y_df$Freq, prob_x_y_df$Y, sum)
prob_y <- data.frame("Y" = sort(unique(visitantes)), "Probabilidade" = p_y)

prob_y
prob_x

p_y
p_x

###### Medidas descritivas

e_x <- mean(mandantes)
e_y <- mean(visitantes)

var_x <- var(mandantes)
var_y <- var(visitantes)

cov <- cov(mandantes, visitantes)
cor <- cor(mandantes, visitantes)

medidas_resumo <- data.frame("Variável" = c("X", "Y"), "Média" = c(e_x, e_y), 
                             "Variância" = c(var_x, var_y), "Covariância" = c(cov, "-"), "Correlação" = c(cor, "-"))
medidas_resumo

prob_x
prob_y

lambda_x <- e_x
lambda_y <- e_y

#   A partir das medidas descritivas, é possível observar que a média de ols dos times mandantes foi de aproximadamente 1,51,
# enquanto a dos visitantes foi de 0,94. Isso indica que os mandantes marcaram, em média, mais gols que os visitantes.

#   Em relação à variância, os gols dos mandantes apresentaram valor de 1,38, enquanto os dos visitantes apresentaram 1,02. Isso mostra que
# houve uma variação maior na quantidade de gols marcados pelos mandantes do que os visitantes entre as partidas analisadas.

#   A covariância entre as variáveis foi de aproximadamente 0,053, indicando uma tendência de variação na mesma direção entre os gols dos mandantes e visitantes. 
# Entretanto, como esse valor é muito próximo de zero, essa associação é bastante fraca. Isso também é observado no coeficiente de correlação, que foi de 
# aproximadamente 0,045, indicando uma relação linear positiva muito fraca entre as variáveis.

######### distribuicoes marginais teoricas -  Poisson univariada ##############

library(ggplot2)
#### PARA X

prob_x$Poisson <- dpois(prob_x$X, lambda = lambda_x)

ggplot(prob_x, aes(x = X)) +
  geom_point(aes(y = Probabilidade, color = "Empírica"), size = 2) +
  geom_line(aes(y = Probabilidade, color = "Empírica"), linewidth = 1) +
  geom_point(aes(y = Poisson, color = "Poisson teórica"), size = 2) +
  geom_line(aes(y = Poisson, color = "Poisson teórica"), linewidth = 1,
            linetype = "dashed") +
  labs(
    title = "Distribuição empírica x Poisson teórica",
    x = "X",
    y = "Probabilidade",
    color = "Distribuição"
  ) +
  scale_color_manual(values = c(
    "Empírica" = "steelblue",
    "Poisson teórica" = "red"
  )) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

# PARA Y

prob_y$Poisson <- dpois(prob_y$Y, lambda = lambda_y)

ggplot(prob_y, aes(x = Y)) +
  geom_point(aes(y = Probabilidade, color = "Empírica"), size = 2) +
  geom_line(aes(y = Probabilidade, color = "Empírica"), linewidth = 1) +
  geom_point(aes(y = Poisson, color = "Poisson teórica"), size = 2) +
  geom_line(aes(y = Poisson, color = "Poisson teórica"), linewidth = 1,
            linetype = "dashed") +
  labs(
    title = "Distribuição empírica x Poisson teórica",
    x = "Y",
    y = "Probabilidade",
    color = "Distribuição"
  ) +
  scale_color_manual(values = c(
    "Empírica" = "steelblue",
    "Poisson teórica" = "red"
  )) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

# INTERPRETAÇAO CASO X 

#   O gráfico mostra uma comparação entre a distribuição empírica dos gols dos mandantes nas 49 partidas e a distribuição teórica de Poisson. 
# É possível observar que a maior parte das partidas teve de 1 a 2 gols. As duas distribuições apresentam comportamentos semelhantes, 
# principalmente para 0 e 1 gol. Porém, para 2 gols, a probabilidade observada foi maior do que a estimada pelo modelo. Já para 3 e 4 gols, 
# aconteceu o contrário, pois o modelo estimou probabilidades maiores do que as observadas. Assim, a distribuição de Poisson consegue representar 
# parte do comportamento dos gols dos mandantes, mas apresenta algumas diferenças em relação aos dados das partidas.


# INTERPRETAÇAO CASO Y

#   O gráfico mostra a comparação entre a distribuição empírica dos gols dos visitantes nas 49 partidas e a distribuição teórica de Poisson. 
# Observa-se que as duas distribuições apresentam comportamentos bastante semelhantes, com probabilidades quase que iguais para todas as quantidades de gols . 
# As maiores probabilidades estão concentradas em 0 e 1 gol, enquanto as probabilidades diminuem conforme a quantidade de gols aumenta. Portanto, 
# o modelo de Poisson apresentou um ajuste bom aos dados observados, conseguindo representar a distribuição dos gols dos visitantes de forma satisfatória .



########### Distribuicao conjunta teorica - Poisson bivariada ###########

# Função da Poisson bivariada

poisson_bivariada <- function(x,y, lambda1, lambda2, lambda3) {
  
  k <- 0:min(x,y)
  coef <- (lambda1^(x-k) *lambda2^(y-k)*lambda3^k)/(factorial(x-k)*factorial(y-k)*factorial(k))
  prob <- exp(-(lambda1 + lambda2 + lambda3))*sum(coef)
  
  return(prob)
  
}

#### Estimacao dos parametros 

lambda3 <- cov
lambda2 <- e_y - lambda3
lambda1 <- e_x - lambda3

###### Construção da distribuição conjunta teórica

n <- as.numeric(length(p_x))

dist_teorica <- matrix(0, nrow = n+1, ncol = n+1)

for (x in 0:n) {
  for (y in 0:n) {
    dist_teorica[x+1, y+1] <- poisson_bivariada(
      x, y, lambda1, lambda2, lambda3
    )
  }
}

rownames(dist_teorica) <- paste0("X=", 0:n)
colnames(dist_teorica) <- paste0("Y=", 0:n)

dist_teorica

dist_teorica_df <- as.data.frame(dist_teorica)

#####33 Distribuicoes marginais obtidas pela Poisson bivariada


prob_x_teorica <- rowSums(dist_teorica)
prob_x_teorica

prob_y_teorica <- colSums(dist_teorica)
prob_y_teorica


# A distribuição conjunta teórica foi construída utilizando o modelo de Poisson bivariada. 
# A partir dela, foram obtidas as distribuições marginais dos gols dos mandantes e dos visitantes. 
# Observa-se que o modelo estima maior probabilidade de os mandantes marcarem 1 gol (33,35 %), enquanto que para os visitantes, a maior 
# probabilidade corresponde a 0 gols (39,08 %). Além disso, as probabilidades diminuem à medida em que o número de gols aumenta, 
# indicando que resultados que apresentam muitos gols são menos prováveis.

######## Disttibuicao da s ###############

# SOMA

soma <- mandantes + visitantes

p_soma <- table(soma)
p_soma <- p_soma/length(soma)
p_soma

e_soma <- mean(soma)
e_soma

var_soma <- var(soma)
var_soma

#DIFERENCA

diferenca <- mandantes-visitantes
p_diferenca <- table(diferenca)/length(diferenca)
p_diferenca

e_diferenca <- mean(diferenca)
e_diferenca

var_diferenca <- var(diferenca)
var_diferenca

# Covariancia e correlaçao

cov_sd <- cov(soma, diferenca)
cov_sd

cor_sd <- cor(soma, diferenca)
cor_sd

medidas_resumo_sd <- data.frame("Variável" = c("X+Y", "X-Y"), "Média" = c(e_soma, e_diferenca), 
                             "Variância" = c(var_soma, var_diferenca), "Covariância" = c(cov_sd, "-"), 
                             "Correlação" = c(cor_sd, "-"))
medidas_resumo_sd

# A média da soma dos gols foi de 2,45, indicando que em média, foram marcados aproximadamente 2,45 gols por partida. 
# Já a média da diferença foi de 0,57, mostrando que os mandantes marcaram mais gols que os visitantes. 
# As variâncias foram de 2,50 para a soma e 2,29 para a diferença. Além disso, a covariância foi positiva 0,36 e a 
# correlação foi de 0,15, indicando uma relação positiva, mas fraca, entre a soma e a diferença dos gols.

############### Convergencia em probabilidade ##############

# Caso teórico

medias_49_x <- numeric(100)

for (i in 1:100) {
  
  x_simulados_49 <- rpois(49, lambda = lambda_x)
  
  medias_49_x[i] <- mean(x_simulados_49)
}

medias_100_x <- numeric(100)

for (i in 1:100) {
  
  x_simulados_100 <- rpois(100, lambda = lambda_x)
  
  medias_100_x[i] <- mean(x_simulados_100)
}

medias_1000_x <- numeric(100)

for (i in 1:100) {
  
  x_simulados_1000 <- rpois(1000, lambda = lambda_x)
  
  medias_1000_x[i] <- mean(x_simulados_1000)
}

medias_1000000_x <- numeric(100)

for (i in 1:100) {
  
  x_simulados_1000000 <- rpois(1000000, lambda = lambda_x)
  
  medias_1000000_x[i] <- mean(x_simulados_1000000)
}


prob_x_49 <- sum(abs(medias_49_x - lambda_x) < 0.1) / 100 # 0,35 ou 35% isso significa que 35 das 100 medias ficaram entre 1,41 e 1,61
prob_x_100 <- sum(abs(medias_100_x - lambda_x) < 0.1) / 100
prob_x_1000 <- sum(abs(medias_1000_x - lambda_x) < 0.1) / 100
prob_x_1000000 <- sum(abs(medias_1000000_x - lambda_x) < 0.1) / 100


####### Para y

medias_49_y <- numeric(100)

for (i in 1:100) {
  
  y_simulados_49 <- rpois(49, lambda = lambda_y)
  
  medias_49_y[i] <- mean(y_simulados_49)
}


medias_100_y <- numeric(100)
for (i in 1:100) {
  
  y_simulados_100 <- rpois(100, lambda = lambda_y)
  
  medias_100_y[i] <- mean(y_simulados_100)
}

medias_1000_y <- numeric(100)

for (i in 1:100) {
  
  y_simulados_1000 <- rpois(1000, lambda = lambda_y)
  
  medias_1000_y[i] <- mean(y_simulados_1000)
}


medias_1000000_y <- numeric(100)

for (i in 1:100) {
  
  y_simulados_1000000 <- rpois(1000000, lambda = lambda_y)
  
  medias_1000000_y[i] <- mean(y_simulados_1000000)
}

prob_y_49 <- sum(abs(medias_49_y - lambda_y) < 0.1) / 100 
prob_y_100 <- sum(abs(medias_100_y - lambda_y) < 0.1) / 100
prob_y_1000 <- sum(abs(medias_1000_y - lambda_y) < 0.1) / 100
prob_y_1000000 <- sum(abs(medias_1000000_y - lambda_y) < 0.1) / 100

prob_x_49 
prob_x_100
prob_x_1000
prob_x_1000000 

prob_y_49 
prob_y_100
prob_y_1000
prob_y_1000000 

######### grafico / tabela das convergencia por probabilidade  

####para x

dados_prob_x <- data.frame(
  n = c(49, 100, 1000, 1000000),
  Probabilidade = c(
    prob_x_49,
    prob_x_100,
    prob_x_1000,
    prob_x_1000000
  )
)

ggplot(dados_prob_x, aes(x = n, y = Probabilidade)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "blue", size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgreen") +
  scale_x_log10(breaks = c(49, 100, 1000, 1000000)) +
  labs(
    x = "Tamanho da amostra",
    y = "Probabilidade estimada",
    title = "Convergência em probabilidade – Mandantes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


### para y
dados_prob_y <- data.frame(
  n = c(49, 100, 1000, 1000000),
  Probabilidade = c(
    prob_y_49,
    prob_y_100,
    prob_y_1000,
    prob_y_1000000
  )
)

ggplot(dados_prob_y, aes(x = n, y = Probabilidade)) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(color = "red", size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgreen") +
  scale_x_log10(breaks = c(49, 100, 1000, 1000000)) +
  labs(
    x = "Tamanho da amostra",
    y = "Probabilidade estimada",
    title = "Convergência em probabilidade – Visitantes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

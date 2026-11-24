################# TCL - convergencia por distribuiçao ###############

set.seed(123)

# Para x

x_simulados_20000 <- rpois(20000, lambda = lambda_x)

z_x_49 <- numeric(10000)

for (i in 1:10000) {
  
  x_simulados_49 <- rpois(49, lambda = lambda_x)
  
  media_x_49 <- mean(x_simulados_49)
  
  z_x_49[i] <- (media_x_49 - lambda_x) / sqrt(lambda_x/49)
  
}

z_x_1000 <- numeric(10000)

for (i in 1:10000) {
  
  x_simulados_1000 <- rpois(1000, lambda = lambda_x)
  
  media_x_1000 <- mean(x_simulados_1000)
  
  z_x_1000[i] <- (media_x_1000 - lambda_x) / sqrt(lambda_x/1000)
  
}


z_x_20000 <- numeric(10000)

for (i in 1:10000) {
  
  x_simulados_20000 <- rpois(20000, lambda = lambda_x)
  
  media_x_20000 <- mean(x_simulados_20000)
  
  z_x_20000[i] <- (media_x_20000 - lambda_x) / sqrt(lambda_x/20000)
  
}

#grafico para X

dados_z_x <- data.frame(
  n = rep(c("49", "1000", "20000"), each = 10000),
  z = c(z_x_49, z_x_1000, z_x_20000)
)

dados_z_x$n <- factor(
  dados_z_x$n,
  levels = c("49", "1000", "20000")
)

ggplot(dados_z_x, aes(x = z)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 35,
    fill = "lightblue",
    color = "white",
    alpha = 0.8
  ) +
  geom_density(
    color = "blue",
    linewidth = 1
  ) +
  stat_function(
    fun = dnorm,
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.5, by = 0.1),
    limits = c(0, 0.5)
  )+
  facet_wrap(~ n, nrow = 1) +
  labs(
    title = "Convergência em distribuição - Mandantes",
    x = "Valores padronizados (Z)",
    y = "Densidade"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold")
  )
# Para y

y_simulados_20000 <- rpois(20000, lambda = lambda_y)

z_y_49 <- numeric(10000)

for (i in 1:10000) {
  
  y_simulados_49 <- rpois(49, lambda = lambda_y)
  
  media_y_49 <- mean(y_simulados_49)
  
  z_y_49[i] <- (media_y_49 - lambda_y) / sqrt(lambda_y/49)
  
}

z_y_1000 <- numeric(10000)

for (i in 1:10000) {
  
  y_simulados_1000 <- rpois(1000, lambda = lambda_y)
  
  media_y_1000 <- mean(y_simulados_1000)
  
  z_y_1000[i] <- (media_y_1000 - lambda_y) / sqrt(lambda_y/1000)
  
}


z_y_20000 <- numeric(10000)

for (i in 1:10000) {
  
  y_simulados_20000 <- rpois(20000, lambda = lambda_y)
  
  media_y_20000 <- mean(y_simulados_20000)
  
  z_y_20000[i] <- (media_y_20000 - lambda_y) / sqrt(lambda_y/20000)
  
}

#grafico para Y

dados_z_y <- data.frame(
  n = rep(c("49", "1000", "20000"), each = 10000),
  z = c(z_y_49, z_y_1000, z_y_20000)
)

dados_z_y$n <- factor(
  dados_z_y$n,
  levels = c("49", "1000", "20000")
)

ggplot(dados_z_y, aes(x = z)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 35,
    fill = "lightblue",
    color = "white",
    alpha = 0.8
  ) +
  geom_density(
    color = "blue",
    linewidth = 1
  ) +
  stat_function(
    fun = dnorm,
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.5, by = 0.1),
    limits = c(0, 0.5)
  )+
  facet_wrap(~ n, nrow = 1) +
  labs(
    title = "Convergência em distribuição - Visitantes",
    x = "Valores padronizados (Z)",
    y = "Densidade"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold")
  )


#### Tabela que mostra a convergencia da media e variancia 

#### nova quantidade de amostra para observar a convergencia da media e variancia da N(0,1)

z_x_100000 <- numeric(10000)

for (i in 1:10000) {
  
  x_simulados_100000 <- rpois(100000, lambda = lambda_x)
  
  media_x_100000 <- mean(x_simulados_100000)
  
  z_x_100000[i] <- (media_x_100000 - lambda_x) / sqrt(lambda_x/100000)
  
}

z_y_100000 <- numeric(10000)

for (i in 1:10000) {
  
  y_simulados_100000 <- rpois(100000, lambda = lambda_y)
  
  media_y_100000 <- mean(y_simulados_100000)
  
  z_y_100000[i] <- (media_y_100000 - lambda_y) / sqrt(lambda_y/100000)
  
}

#### para x 

media_x_49 <- mean(z_x_49)
var_x_49 <- var(z_x_49)

media_x_1000 <- mean(z_x_1000)
var_x_1000 <-var(z_x_1000)

media_x_20000 <- mean(z_x_20000)
var_x_20000 <-var(z_x_20000)

media_x_100000 <- mean(z_x_100000)
var_x_100000 <-var(z_x_100000)

#### para y

media_y_49 <- mean(z_y_49)
var_y_49 <-var(z_y_49)

media_y_1000 <- mean(z_y_1000)
var_y_1000 <-var(z_y_1000)

media_y_20000 <- mean(z_y_20000)
var_y_20000 <-var(z_y_20000)

media_y_100000 <- mean(z_y_100000)
var_y_100000 <-var(z_y_100000)

tabela_convergencia <- data.frame(
  Tamanho_amostra = c(49, 1000, 20000, 100000),
  Media_dos_mandantes = c(
    mean(x_simulados_49),
    mean(x_simulados_1000),
    mean(x_simulados_20000),
    mean(x_simulados_100000)
  ),
  Media_dos_visitantes = c(
    mean(y_simulados_49),
    mean(y_simulados_1000),
    mean(y_simulados_20000),
    mean(y_simulados_100000)
  )
)


tabela_convergencia <- data.frame(
  Tamanho_amostra = c(49, 1000, 20000, 100000),
  Media_X = c(media_x_49, media_x_1000, media_x_20000,media_x_100000),
  Variancia_X = c(var_x_49, var_x_1000, var_x_20000, var_x_100000),
  Media_Y = c(media_y_49, media_y_1000, media_y_20000, media_y_100000),
  Variancia_Y = c(var_y_49, var_y_1000, var_y_20000, var_y_100000)
)

library(gridExtra)

tabela_convergencia <- data.frame(
  Tamanho_amostra = c(49, 1000, 20000, 100000),
  Media_X = c(media_x_49, media_x_1000, media_x_20000, media_x_100000),
  Variancia_X = c(var_x_49, var_x_1000, var_x_20000, var_x_100000),
  Media_Y = c(media_y_49, media_y_1000, media_y_20000, media_y_100000),
  Variancia_Y = c(var_y_49, var_y_1000, var_y_20000, var_y_100000)
)

tabela_convergencia[, -1] <- round(tabela_convergencia[, -1], 4)

tabela_convergencia$Tamanho_amostra <- format(
  tabela_convergencia$Tamanho_amostra,
  scientific = FALSE
)

colnames(tabela_convergencia) <- c(
  "Tamanho da amostra",
  "Média de X",
  "Variância de X",
  "Média de Y",
  "Variância de Y"
)

gridExtra::grid.table(tabela_convergencia, rows = NULL)

library(readxl)

#Gols_mandante_x_gols_visitante <- read_excel("C:/Users/lenovo/Downloads/Gols mandante x gols visitante.xlsx")
#Gols_mandante_x_gols_visitante <- read_excel("Gols mandante x gols visitante.xlsx")
Gols_mandante_x_gols_visitante <- read_excel("C:/Users/GUILHERM/Downloads/Gols mandante x gols visitante (2).xlsx")

#View(Gols_mandante_x_gols_visitante)


###########  Modelos de convergencia #####################

##### convergencia quase certa

#### GRAfico distribuiçao empirica 

e_x <- mean(mandantes)
e_y <- mean(visitantes)

media_x <- cumsum(mandantes) / seq_along(mandantes)
media_y <- cumsum(visitantes) / seq_along(visitantes)

dados_medias_longos <- data.frame(
  partidas = rep(seq_along(mandantes), 2),
  Media = c(media_x, media_y),
  Equipe = rep(c("Mandantes", "Visitantes"),
               each = length(mandantes))
)

library(ggplot2)

ggplot(dados_medias_longos, aes(x = partidas, y = Media, color = Equipe)) +
  geom_line(linewidth = 1) +
  geom_text(
    data = dados_medias_longos |>
      dplyr::group_by(Equipe) |>
      dplyr::slice_max(partidas),
    aes(label = round(Media, 2)),
    hjust = -0.3,
    vjust = -0.5,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("Mandantes" = "blue", "Visitantes" = "red")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.10))) +
  labs(
    x = "Número de partidas",
    y = "Média acumulada",
    title = "Convergência das médias de gols",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

##### Grafico distribuiçao teorica 
set.seed(123)

####### Para x

x_simulados_49 <- rpois(49, lambda = lambda_x)

x_simulados_100 <- rpois(100, lambda = lambda_x)

x_simulados_1000 <- rpois(1000, lambda = lambda_x)

x_simulados_1000000 <- rpois(1000000, lambda = lambda_x)

medias_x <- c(mean(x_simulados_49) , mean(x_simulados_100) , mean(x_simulados_1000) , mean(x_simulados_1000000))
medias_x

####### Para Y

y_simulados_49 <- rpois(49, lambda = lambda_y)

y_simulados_100 <- rpois(100, lambda = lambda_y)

y_simulados_1000 <- rpois(1000, lambda = lambda_y)

y_simulados_1000000 <- rpois(1000000, lambda = lambda_y)

medias_y <- c(mean(y_simulados_49) , mean(y_simulados_100) , mean(y_simulados_1000) , mean(y_simulados_1000000))
medias_y

### tabela quase certamnete 

library(gridExtra)

tabela_medias <- data.frame(
  Tamanho_amostra = c(49, 100, 1000, 1000000),
  Média_dos_mandantes = round(medias_x, 4),
  Média_dos_visitantes = round(medias_y, 4)
)

tabela_medias$Tamanho_amostra <- format(tabela_medias$Tamanho_amostra, scientific = FALSE)

colnames(tabela_medias) <- c(
  "Tamanho da amostra",
  "Média dos mandantes",
  "Média dos visitantes"
)

gridExtra::grid.table(tabela_medias, rows = NULL)

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


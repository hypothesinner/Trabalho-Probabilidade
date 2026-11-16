library(readxl)

Gols_mandante_x_gols_visitante <- read_excel("data/Gols mandante x gols visitante.xlsx")

#View(Gols_mandante_x_gols_visitante)

mandantes <- Gols_mandante_x_gols_visitante$`X = gols mandante`
visitantes <- Gols_mandante_x_gols_visitante$`Y = gols visitante`


##### Distribuição conjunta empírica ###############

# A Distribuição conjunta empírica representa a probabilidade do time mandante (X) e o visitante (Y) marcarem
# determinadas quantidades de gols na mesma partida

x_y <- table("X" = mandantes, "Y" = visitantes)
prob_x_y <- x_y/49

x_y
prob_x_y

prob_x_y_df <- as.data.frame(prob_x_y)

########### Distribuição marginal #############

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

# A correlação positiva é um fator que possibilita a modelagem dessa distribuição por uma Poisson bivariada, uma vez que essa apenas admite parâmetros positivos
# Além disso, a baixa magnitude do valor demonstra uma relação muito pequena entre os mandos de campo no número de gols, embora não nula, apresentando um pequeno
# desvio positivo em razão da tendência de placares baixos pela competitividade do campeonato e pelo desgaste do fator fora, relacionado com distâncias e diversidade
# de condições climáticas (geralmente extenunantes ou limitadoras da performance), fazendo com que os visitantes busquem pelo empate e limitem os gols para baixo, 
# fazendo com que mandante e visitante marquem menos.

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
    subtitle = paste0("Comparação das probabilidades observadas com Poisson (λ = ",
                      round(lambda_x, 2), ")"),
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
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey30")
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
    subtitle = paste0("Comparação das probabilidades observadas com Poisson (λ = ",
                      round(lambda_x, 2), ")"),
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


#################### Distribuiçao empirica x Poisson teorica ###############


ggplot(prob_y, aes(x = Y)) +
  geom_col(aes(y = Probabilidade, fill = "Empírica"), alpha = 0.7, width = 0.6) +
  geom_point(aes(y = Poisson, color = "Poisson teórica"), size = 3, shape = 18) +
  geom_line(aes(y = Poisson, color = "Poisson teórica"), linetype = "dashed") +
  scale_fill_manual(values = c("Empírica" = "steelblue")) +
  scale_color_manual(values = c("Poisson teórica" = "firebrick")) +
  scale_x_continuous(breaks = 0:max(prob_x$X)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Distribuição empírica x Poisson teórica",
    subtitle = paste0("Comparação das probabilidades observadas com Poisson (λ = ",
                      round(lambda_x, 2), ")"),
    x = "X", y = "Probabilidade", fill = NULL, color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey30")
  )

# INTERPRETAÇÃO CASO X 

#   O gráfico mostra uma comparação entre a distribuição empírica dos gols dos mandantes nas 49 partidas e a distribuição teórica de Poisson. 
# É possível observar que a maior parte das partidas teve de 1 a 2 gols. As duas distribuições apresentam comportamentos semelhantes, 
# principalmente para 0 e 1 gol. Porém, para 2 gols, a probabilidade observada foi maior do que a estimada pelo modelo. Já para 3 e 4 gols, 
# aconteceu o contrário, pois o modelo estimou probabilidades maiores do que as observadas. Assim, a distribuição de Poisson consegue representar 
# parte do comportamento dos gols dos mandantes, mas apresenta algumas diferenças em relação aos dados das partidas.


# INTERPRETAÇÃO CASO Y

#   O gráfico mostra a comparação entre a distribuição empírica dos gols dos visitantes nas 49 partidas e a distribuição teórica de Poisson. 
# Observa-se que as duas distribuições apresentam comportamentos bastante semelhantes, com probabilidades quase que iguais para todas as quantidades de gols . 
# As maiores probabilidades estão concentradas em 0 e 1 gol, enquanto as probabilidades diminuem conforme a quantidade de gols aumenta. Portanto, 
# o modelo de Poisson apresentou um ajuste bom aos dados observados, conseguindo representar a distribuição dos gols dos visitantes de forma satisfatória .



########### Distribuição conjunta teórica - Poisson bivariada ###########

# Função da Poisson bivariada

poisson_bivariada <- function(x,y, lambda1, lambda2, lambda3) {
  
  k <- 0:min(x,y)
  coef <- (lambda1^(x-k) *lambda2^(y-k)*lambda3^k)/(factorial(x-k)*factorial(y-k)*factorial(k))
  prob <- exp(-(lambda1 + lambda2 + lambda3))*sum(coef)
  
  return(prob)
  
}

#### Estimação dos parâmetros 

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


####### Distribuicoes marginais obtidas pela Poisson bivariada


prob_x_teorica <- rowSums(dist_teorica)
prob_x_teorica

prob_y_teorica <- colSums(dist_teorica)
prob_y_teorica

# Comparação gráfica (Grafico 3D)

library(plotly)
library(mgcv)

prob_x_y_df$X <- as.numeric(as.character(prob_x_y_df$X))
prob_x_y_df$Y <- as.numeric(as.character(prob_x_y_df$Y))

as.numeric(as.character(prob_x_y_df$Y))

modelo1 <- gam(
  Freq ~ s(X, Y, bs = "tp"),
  data = prob_x_y_df,
  method = "REML"
)

dist_teorica_tb <- as.table(dist_teorica)

rownames(dist_teorica_tb) <- 0:6
colnames(dist_teorica_tb) <- 0:6

dist_teorica_tb
dist_teorica_df <- as.data.frame(dist_teorica_tb)
names(dist_teorica_df) <- c("X", "Y", "Prob")

dist_teorica_df$X <- as.numeric(as.character(dist_teorica_df$X))
dist_teorica_df$Y <- as.numeric(as.character(dist_teorica_df$Y))

modelo2 <- gam(
  Prob ~ s(X, Y, bs = "tp"),
  data = dist_teorica_df,
  method = "REML"
)


prev_emp <- predict(modelo1)

prev_teo <- predict(modelo2)

prob_x_y_ajust <- matrix(
  prev_emp,
  nrow = nrow(prob_x_y),
  ncol = ncol(prob_x_y)
)

dist_teorica_ajust <- matrix(
  prev_teo,
  nrow = nrow(dist_teorica_tb),
  ncol = ncol(dist_teorica_tb)
)

library(plotly)

# Paletas personalizadas (baixo -> alto)
# Paletas personalizadas (baixo -> alto)


paleta_emp <- list(
  c(0.00, "#fcfbfd"),
  c(0.25, "#cbc9e2"),
  c(0.50, "#9e9ac8"),
  c(0.75, "#6a51a3"),
  c(1.00, "#3f007d")
)

paleta_teo <- list(
  c(0.00, "#fff7f3"),
  c(0.25, "#fcc5c0"),
  c(0.50, "#f768a1"),
  c(0.75, "#c51b8a"),
  c(1.00, "#7a0177")
)

# Escala z comum, para comparação visual justa entre os dois gráficos

zmax <- max(prob_x_y_ajust, dist_teorica_ajust, na.rm = TRUE)

########### Graficos 3D lado a lado ##################

g1 <- plot_ly(
  x = 0:5, y = 0:5, z = prob_x_y_ajust,
  type = "surface",
  colorscale = paleta_emp,
  cmin = 0, cmax = zmax,
  scene = "scene",
  colorbar = list(
    title = "Prob.<br>Empírica",
    x = 0.44, xanchor = "left",
    y = 0.5, yanchor = "middle",
    len = 0.6, thickness = 15
  )
)

g2 <- plot_ly(
  x = 0:5, y = 0:5, z = dist_teorica_ajust,
  type = "surface",
  colorscale = paleta_teo,
  cmin = 0, cmax = zmax,
  scene = "scene2",
  colorbar = list(
    title = "Prob.<br>Teórica",
    x = 0.98, xanchor = "left",
    y = 0.5, yanchor = "middle",
    len = 0.6, thickness = 15
  )
)

fig_lado_a_lado <- subplot(g1, g2) %>%
  layout(
    title = list(
      text = paste0(
        "<b>Distribuição Empírica x Distribuição Teórica</b>",
        "<br><sup>Comparação das probabilidades conjuntas P(X,Y) para X, Y = 0,…,5</sup>"
      ),
      x = 0.5
    ),
    scene = list(
      domain = list(x = c(0, 0.42), y = c(0, 1)),
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "P(X,Y)", range = c(0, zmax)),
      camera = list(eye = list(x = 1.6, y = -1.6, z = 0.9))
    ),
    scene2 = list(
      domain = list(x = c(0.54, 0.96), y = c(0, 1)),
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "P(X,Y)", range = c(0, zmax)),
      camera = list(eye = list(x = 1.6, y = -1.6, z = 0.9))
    ),
    annotations = list(
      list(text = "<b>Empírica</b>", x = 0.21, y = 0.95,
           xref = "paper", yref = "paper", showarrow = FALSE, font = list(size = 14)),
      list(text = "<b>Teórica</b>", x = 0.75, y = 0.95,
           xref = "paper", yref = "paper", showarrow = FALSE, font = list(size = 14))
    ),
    margin = list(t = 90, r = 80)
  )

fig_lado_a_lado

########### Graficos 3D sobrepostos ##################

# Camera e proporcoes (so para a sobreposta)
cam <- list(eye = list(x = 1.3, y = -1.3, z = 0.7))
asp <- list(x = 1.2, y = 1.2, z = 0.75)

fig_sobreposta <- plot_ly() %>%
  add_surface(
    x = 0:5, y = 0:5, z = prob_x_y_ajust,
    name = "Empírica",
    colorscale = paleta_emp,
    cmin = 0, cmax = 0.14,
    opacity = 1,
    colorbar = list(
      title = list(text = "Prob.<br>Empírica", font = list(size = 14)),
      tickfont = list(size = 13),
      tickmode = "linear", tick0 = 0, dtick = 0.02,
      tickformat = ".2f",
      ticks = "outside", ticklen = 5,
      x = 0.96, xanchor = "left",
      y = 1.0, yanchor = "top",
      len = 0.46, thickness = 22
    )
  ) %>%
  add_surface(
    x = 0:5, y = 0:5, z = dist_teorica_ajust,
    name = "Teórica",
    colorscale = paleta_teo,
    cmin = 0, cmax = 0.14,
    opacity = 0.6,
    colorbar = list(
      title = list(text = "Prob.<br>Teórica", font = list(size = 14)),
      tickfont = list(size = 13),
      tickmode = "linear", tick0 = 0, dtick = 0.02,
      tickformat = ".2f",
      ticks = "outside", ticklen = 5,
      x = 0.96, xanchor = "left",
      y = 0.0, yanchor = "bottom",
      len = 0.46, thickness = 22
    )
  ) %>%
  layout(
    title = list(
      text = paste0(
        "<b>Distribuição Empírica x Distribuição Teórica</b>",
        "<br><sup>Superfícies sobrepostas de P(X,Y) para X, Y = 0,…,5</sup>"
      ),
      x = 0.5
    ),
    scene = list(
      domain = list(x = c(0, 0.95), y = c(0, 1)),
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "P(X,Y)", range = c(0, 0.14), dtick = 0.02),
      aspectmode = "manual", aspectratio = asp,
      camera = cam
    ),
    margin = list(t = 70, r = 0, b = 10, l = 0)
  )

fig_sobreposta


# A distribuição conjunta teórica foi construída utilizando o modelo de Poisson bivariada. 
# A partir dela, foram obtidas as distribuições marginais dos gols dos mandantes e dos visitantes. 
# Observa-se que o modelo estima maior probabilidade de os mandantes marcarem 1 gol (33,35 %), enquanto que para os visitantes, a maior 
# probabilidade corresponde a 0 gols (39,08 %). Além disso, as probabilidades diminuem à medida em que o número de gols aumenta, 
# indicando que resultados que apresentam muitos gols são menos prováveis.

######## Distribuições de transformações ###############

# SOMA

soma <- mandantes + visitantes

p_soma <- table(soma)
p_soma <- p_soma/length(soma)
p_soma

e_soma <- mean(soma)
e_soma

var_soma <- var(soma)
var_soma

#DIFERENÇA

diferenca <- mandantes - visitantes

p_diferenca <- table(diferenca)/length(diferenca)
p_diferenca

e_diferenca <- mean(diferenca)
e_diferenca

var_diferenca <- var(diferenca)
var_diferenca

# MULTIPLICAÇAO

multiplicacao <- (mandantes) * (visitantes) 

p_multiplicacao <- table(multiplicacao) / length(multiplicacao)
p_multiplicacao

e_multiplicacao <- mean(multiplicacao)
e_multiplicacao

var_multiplicacao <- var(multiplicacao)
var_multiplicacao

#razao

dados_dois_gols <- data.frame(
  Mandantes = mandantes,
  Visitantes = visitantes
)

dados_dois_gols <- subset(
  dados_dois_gols,
  Mandantes > 0 & Visitantes > 0
)

razao <- dados_dois_gols$Mandantes / dados_dois_gols$Visitantes

p_razao <- table(razao) / length(razao)

e_razao <- mean(razao)
var_razao <- var(razao)

p_razao
e_razao
var_razao

#### quando um marca e outro nao 

mandante_apenas <- sum(mandantes > 0 & visitantes == 0)

visitante_apenas <- sum(mandantes == 0 & visitantes > 0)

nenhum_marcou <- sum(mandantes == 0 & visitantes == 0)



tabela_gols <- data.frame(
  Situacao = c(
    "Somente mandante marcou",
    "Somente visitante marcou",
    "Nenhum marcou"
  ),
  Frequencia = c(
    mandante_apenas,
    visitante_apenas,
    nenhum_marcou
  )
)

tabela_gols$Proporcao <- round(
  tabela_gols$Frequencia / length(mandantes), 4
)

tabela_gols

# Covariancia e correlação

###### soma e diferença
cov_sd <- cov(soma, diferenca)

cor_sd <- cor(soma, diferenca)

##### multiplicacao e soma 
cov_ms <- cov(multiplicacao, soma)

cor_ms <- cor(multiplicacao, soma)

#### multiplicacao e diferenca
cov_md <- cov(multiplicacao, diferenca)

cor_md <- cor(multiplicacao, diferenca)

### tebelas
medidas_resumo_sd <- data.frame("Variável" = c("X+Y", "X-Y"), "Média" = c(e_soma, e_diferenca), 
                             "Variância" = c(var_soma, var_diferenca), "Covariância" = c(cov_sd, "-"), 
                             "Correlação" = c(cor_sd, "-"))

medidas_resumo_ms <- data.frame("Variável" = c("X*y", "X+Y"), "Média" = c(e_multiplicacao, e_soma), 
                                "Variância" = c(var_multiplicacao, var_soma), "Covariância" = c(cov_ms, "-"), 
                                "Correlação" = c(cor_ms, "-"))

medidas_resumo_md <- data.frame("Variável" = c("X*y", "X-Y"), "Média" = c(e_multiplicacao, e_diferenca), 
                                "Variância" = c(var_multiplicacao, var_diferenca), "Covariância" = c(cov_md, "-"), 
                                "Correlação" = c(cor_md, "-"))

medidas_resumo_sd 
medidas_resumo_ms 
medidas_resumo_md

# A média da soma dos gols foi de 2,45, indicando que em média, foram marcados aproximadamente 2,45 gols por partida. 
# Já a média da diferença foi de 0,57, mostrando que os mandantes marcaram mais gols que os visitantes. 
# As variâncias foram de 2,50 para a soma e 2,29 para a diferença. Além disso, a covariância foi positiva 0,36 e a 
# correlação foi de 0,15, indicando uma relação positiva, mas fraca, entre a soma e a diferença dos gols.


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

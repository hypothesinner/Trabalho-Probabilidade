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

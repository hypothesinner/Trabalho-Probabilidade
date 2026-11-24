#Comparação gráfica (Grafico 3D)

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
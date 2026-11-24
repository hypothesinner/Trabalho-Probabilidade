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

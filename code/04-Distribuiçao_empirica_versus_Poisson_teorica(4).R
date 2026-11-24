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


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

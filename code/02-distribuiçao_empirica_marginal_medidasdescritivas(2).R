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

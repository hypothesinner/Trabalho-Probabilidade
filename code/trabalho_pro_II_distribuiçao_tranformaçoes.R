library(readxl)

Gols_mandante_x_gols_visitante <- read_excel("data/Gols mandante x gols visitante.xlsx")

#View(Gols_mandante_x_gols_visitante)

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


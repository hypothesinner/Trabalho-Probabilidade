
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
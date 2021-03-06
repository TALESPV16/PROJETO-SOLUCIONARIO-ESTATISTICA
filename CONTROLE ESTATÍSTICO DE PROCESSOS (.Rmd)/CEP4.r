##QUESTÃO CEP4
O conteúdo de cobre, em ppm, de um banho de platina é medido três vezes ao dia e os resultados de X-barra e R são apresentados na tabela abaixo. Sabendo-se que os limites de especificação são 6,0 ± 1,0: 

a) Indique se esse processo está sobre controle (Utilize apenas a regra 1: pontos fora dos limites de controle) e 

b) Estime a capabilidade do processo e interprete o resultado.


###Resposta letra a)

```{r}
(dados <- read.delim2("CEP4.txt", header =  TRUE, sep = "\t"))

plot(dados$XBarra, type = "l", col = "grey", xlab ="Dia",
     ylab = "ppm Cu",
     main = "X-Barra ppm Cu")
points(dados$XBarra, pch = 20)
LSC <- lines.default(rep(7, length(dados$XBarra)), col = "red")
LIC <- lines.default(rep(5, length(dados$XBarra)), col = "red")


mediar <- mean(dados$R)
D3 <- 0
D4 <- 2.28
plot(dados$R, type = "l", col = "grey", xlab ="Dia",
     ylab = "ppm Cu", axes = F,
     main = "R-Barra ppm Cu")
axis(1, at = seq(1,length(dados$R), by = 1, pos = 1))
axis(2, at = seq(0,3,by = 0.01))
points(dados$R, pch = 20)
LSC <- lines.default(rep(mediar*D4, length(dados$R)), col = "red")
LIC <- lines.default(rep(mediar*D3, length(dados$R)), col = "red")

mediaXB <-mean(dados$XBarra)

```

\newpage

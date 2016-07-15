##QUESTÃO CEP1
O sistema de controle de qualidade de cinco bombas apresenta as alturas das bases de uma parte da bomba (medidas realizadas em polegadas) como se pode observar na tabela a seguir.

a) Avalie os dados da tabela abaixo e construa o(s) gráfico(s) de controle mais adequado(s). Justificando e indicando as etapas para o seu desenvolvimento.
b) Qual a premissa principal a ser considerada na construção da carta de controle de individuais (X-barra) e de subgrupos (X-barra-barra)?

###Resposta letra a)
```{r}
dataCEP = read.delim2(file = "QCEP1.txt", header = TRUE, sep = "\t", dec = ",")
dataCEP <- as.data.frame(dataCEP)
print(dataCEP)
str(dataCEP)
```
```{r}
summary(dataCEP)              # Estatistica descritiva    
sapply(dataCEP, sd)
lapply(dataCEP, shapiro.test) # Teste Shapiro
lapply(dataCEP, t.test)   # Teste T de Student
sapply(dataCEP, kurtosis) # Curtose
sapply(dataCEP, skewness) # Assimetria
```

* Plotando os gráficos para análise dos dados

1. Para a Bomba 1

```{r, fig.height= 7, fig.width= 11}
par(mfrow = c(2,2))
hist(dataCEP$B1)
boxplot(dataCEP$B1)
plot(density(dataCEP$B1), col="red")
qqnorm(dataCEP$B1)
qqline(dataCEP$B1, col ="red")
```
\newpage
2. Para a Bomba 2

```{r, fig.height= 8, fig.width= 11}
par(mfrow = c(2,2))
hist(dataCEP$B2)
boxplot(dataCEP$B2)
plot(density(dataCEP$B2), col="red")
qqnorm(dataCEP$B2)
qqline(dataCEP$B2, col ="red")
```
\newpage
3. Para a Bomba 3

```{r, fig.height= 8, fig.width= 11}
par(mfrow = c(2,2))
hist(dataCEP$B3)
boxplot(dataCEP$B3)
plot(density(dataCEP$B3), col="red")
qqnorm(dataCEP$B3)
qqline(dataCEP$B3, col ="red")
```
\newpage
4. Para a Bomba 4

```{r, fig.height= 8, fig.width= 11}
par(mfrow = c(2,2))
hist(dataCEP$B4)
boxplot(dataCEP$B4)
plot(density(dataCEP$B4), col="red")
qqnorm(dataCEP$B4)
qqline(dataCEP$B4, col ="red")
```
\newpage
4. Para a Bomba 5

```{r, fig.height= 8, fig.width= 11}
par(mfrow = c(2,2))
hist(dataCEP$B5)
boxplot(dataCEP$B5)
plot(density(dataCEP$B5), col="red")
qqnorm(dataCEP$B5)
qqline(dataCEP$B5, col ="red")
```

\newpage
*Análise estatística dos dados usando CEP
*Plotando as Cartas de Controle individuais

```{r}
plot.xbar = qcc(dataCEP$B1, type="xbar.one")
plot.xbar = qcc(dataCEP$B2, type="xbar.one")
plot.xbar = qcc(dataCEP$B3, type="xbar.one")
plot.xbar = qcc(dataCEP$B4, type="xbar.one")
plot.xbar = qcc(dataCEP$B5, type="xbar.one")
```

*Plotando a Carta de Controle x barra de todas as bombas
```{r}
plot.xbar = qcc(dataCEP, type="xbar")
```

*Plotando a Carta de Controle R barra de todas as bombas 
```{r, fig.height= 7, fig.width= 11}
plot.R = qcc(dataCEP, type="R")
process.capability(object = plot.xbar, spec.limits=c(0.825,0.835))
```

###Resposta letra b)

*Para a construção das carlas de controle x-barra e x-barra-barra (R) deve se admitir que os dados estão distribuidos normalmente e que haja aleatoriedade dos dados*

\newpage

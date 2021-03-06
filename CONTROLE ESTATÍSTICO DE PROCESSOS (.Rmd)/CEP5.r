##QUESTÃO CEP5
Barras de chocolate são produzidas e a cada 15 minutos 3 barras são retiradas da linha e pesadas. Cartas de controle para média e amplitude são construídas para monitorar o peso dessas barras; o tamanho da amostra é 3 e X-barra e R são estimadas a cada 28 amostras, obtendo-se, respectivamente, 170,785g e 0,515g.

a) Estime os limites de controle para X-barra e R.
b) Estime o desvio padrão deste processo. Com base na carta de controle (X-barra) abaixo, verifique se este valor estimado representa a variabiliade do processo (**justifique sua resposta)**.

###Resposta letra a)

Para estimar os limites da carta de controle x-barra e R são usadas as equações abaixo:

$$
\begin{aligned}
UCL = \bar{x} + A_{2} \times \bar{r}
\end{aligned}
$$
$$
\begin{aligned}
LC = \bar{x}
\end{aligned}
$$
$$
\begin{aligned}
LCL = \bar{x} - A_{2} \times \bar{r}
\end{aligned}
$$
$$
\begin{aligned}
UCL = D_{4} \times \bar{r}
\end{aligned}
$$
$$
\begin{aligned}
LC = \bar{r}
\end{aligned}
$$
$$
\begin{aligned}
LCL = D_{3} \times \bar{r}
\end{aligned}
$$

Dado que as informações fornecidas são:

$LC = \bar{x} = 170,785$

$LC = \bar{r} = 0,515$

Outra informação fornecida é que o tamanho dos subgrupos(amostra) é 3, ou seja, n = 3. 

Consultando a tabela fornecida os coeficientes utilizados serão $A_{2} = 1,02$, $D_{3} = 0$ e $D_{4} = 2,28$ para n = 3.

Utilizando as equações acima podemos calcular os limites de controle superior e inferior para a carta X-Barra:

$UCL = 170,785 + 1,02 \times 0,515 = 171,3103$

$LC = 170,785$

$LCL = 170,785 - 1,02 \times 0,515 = 170,2597$

Utilizando as equações para a construção da carta R podemos calcular os limites de controle superior e inferior:

$UCL = 2,28 \times 0,515 = 1,174$

$LC = 0,515$

$LCL = 0 \times 0,515 = 0$

###Resposta letra b)

Consultando a tabela z, considerando que a distribuição é normal, por conta da amostra ser considerada grande, pois de fato foram coletadas 84 barras de chocolate num período de 420 minutos ou, 7 horas, para um nível de confiânça de 95% e de 99% temos:

NC = 95% / z = 1,96

NC = 99% / z = 2,58

podemos estimar então o desvio padrão, entendendo que o erro padrão é:

$$
\begin{aligned}
E = \frac{\sigma}{\sqrt{n}}
\end{aligned}
$$

Se ao multiplicar o erro E pelo valor de z para um nível de confiança de 95%, e para um nível de confiança de 99%, podemos então ter a distância 2 e 3 sigma em relação à média, e igualando à equação para a determinação da linha de controle superior e inferior, para uma distribuição normal, temos:

$$
\begin{aligned}
\frac{\sigma}{\sqrt{n}} \times z = ULC - \bar{x} 
\end{aligned}
$$

$$
\sigma = \frac{(171,3103 - 170,785) \times \sqrt{84}}{1,96} = 2,45635
$$

$$
\begin{aligned}
\frac{\sigma}{\sqrt{n}} \times z = \bar{x} - LCL 
\end{aligned}
$$

$$
\sigma = \frac{(170,785 - 170,2597) \times \sqrt{84}}{1,96} = 2,45635
$$

\newpage

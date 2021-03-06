##QUESTÃO CEP3

Dois gráficos de controle (a) e (b) foram construídos considerando dados de subgrupos de tamanho 5 e 4, respectivamente. 

a) Analise criticamente estes gráficos de controle e discuta se eles poderiam ser utilizados para o acompanhamento de processos.

###Resposta letra a)

Os gráficos de controle em (a) contém um erro no LIC, e a quantidade 2$\sigma$ difere em 0,001 da mesma quantidade do LSC, ainda sim não há significativa diferença ao obsevar que, o valor do coeficiente $A_{2}$ tabelado para n = 5 (tamanho do subgrupo sugerido para a construção dessa carta de controle), que corresponde ao valor de 0,577, ou seja, $A_{2} = 0,577$, não difere muito do valor calculado abaixo, bem como os limites de controle calculados com o valor tabelado não diferem muito dos gráficos apresentados. Visto isso, sejam as equações:

$UCL = \bar{\bar{x}} + A_{2}*\bar{r}$

$LC = \bar{\bar{x}}$

$LCL = \bar{\bar{x}} - A_{2}*\bar{r}$

Elas, definem a linha central, e os limites superior e inferior da carta de controle.

Dado que, para os gráficos em (a) as seguintes informações podem ser coletadas:

$\bar{\bar{x}}=11,530$

$\bar{\bar{r}} = 6,1$

É possível verificar se, ao aplicar o coeficiente $A_{2} = 0,577$, obteremos praticamente os mesmos valores para o LSc e o LIC, para a carta de controle apresentada em (a), nesse caso:

$UCL = 11,530 + 0,577 * 6,1 = 15,0497$

$LC = 11,530$

$LCL = 11,530 - 0,577 * 6,1 = 8,103$

Ao se realizar o cálculo inverso, ou seja, dados os limites de controle 
UCL = 15,049 e LCL = 8,012, podemos obter o coeficiente que foi usado para a construção da carta de controle apresentada e comparar com o encontrado na literatura para n = 5.

$$
\begin{aligned}
A_{2} = \frac{UCL - \bar{x}}{\bar{r}}
\end{aligned}
$$

$$
\begin{aligned}
A_{2} = \frac{\bar{x} - LCL }{\bar{r}}
\end{aligned}
$$

Dado que o valor de UCL e LCL, são respectivamente 15,049 e 8,012, temos que:

$$
\begin{aligned}
A_{2} = \frac{15,049 - 11,530}{6,1} = 0,57688
\end{aligned}
$$

$$
\begin{aligned}
A_{2} = \frac{11,530 - 8,012}{6,1} = 0,57672
\end{aligned}
$$

Outro aspecto importante é que o tamanho da amostra é maior que 30, pois o tamanho do subgrupo multiplicado pela quantidade de amostras do subgrupo dá a quantidade amostrada que é de 125, ou seja N = 125, o que se pode inferir que a distribuição pode ter comportamento normal. Dessa forma, é possível considerar que a média calculada corresponde à média da amostra.

Usando o mesmo raciocínio exposto iremos avaliar a carta apresentada em (b). Primeiramente iremos considerar a mesma tabela de coeficientes disponível na literatura. Para o subgrupo de tamanho 4, ou seja n = 4, o valor do coeficiente $A_{2}$ é como segue $A_{2} = 0,729$ dessa forma podemos considerar os mesmos cálculos realizados anteriormente, considerando que para essa carta:

$\bar{x}=20,154$

$\bar{r} = 2,152$

$UCL = 20,154 + 0,729 * 2,152 = 21,72280$

$LC = 20,154$

$LCL = 20,154 - 0,729 * 2,152 = 18,58519$

Dado que o valor de UCL e LCL, são respectivamente 21,395 e 19,912, temos que:

$$
\begin{aligned}
A_{2} = \frac{21,395 - 20,154}{2,152} = 0,57667
\end{aligned}
$$

$$
\begin{aligned}
A_{2} = \frac{20,154 - 19,912}{2,152} = 0,11245
\end{aligned}
$$

Dessa forma conclui-se que é possível usar a carta de controle em (a), pode ser considerada como instrumento de controle, já a carta em (b), devido às discrepâncias com relação aos valores, apresentados e tabelados, não é possível confiar nessa carta para avaliar estatisticamente o processo.


\newpage

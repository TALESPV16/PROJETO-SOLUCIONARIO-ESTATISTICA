##QUESTÃO QU3
O processo de geração e distribuição de vapor, bem como o sistema de co-geração, de uma determinada indústria podem ser representados pela figura 1. O primeiro controlador (PC-1) controla a pressão do sistema de distribuição de vapor de média pressão (15 kgf/cm2) em uma faixa de modo a evitar que fique abaixo de um valor requerido. Para isto, quando ocorre diminuição da pressão do sistema de distribuição de vapor, o controlador PC-1 comanda a abertura de uma válvula redutora de pressão que injeta vapor diretamente na linha de distribuição, sem passar pela turbina. O segundo controlador (PC-2) controla a pressão do sistema de distribuição de vapor em outra faixa, evitando que a pressão fique acima de um valor definido como seguro para o processo. Para fazer esse controle, o PC-2 comanda a abertura de uma válvula de alívio que lança na atmosfera o excesso de vapor, gerando perdas de vapor e, consequentemente, de água e energia.

A Figura 2 mostra que 40% da perda de vapor pode ser explicada pela relação entre a geração de energia elétrica pelas turbinas, sendo esta e o alívio de vapor as principais fontes de variação a serem analisadas. Na Tabela 1 são apresentados os dados de perda de vapor e consumo de energia elétrica.

a) Estime as estatísticas descritivas de média e desvio padrão dos dados referentes a perda de vapor da Tabela 1 considerando um nível de confiança de 95%. Discuta os resultados considerando as informações apresentadas no Quadro 1 e Figura 1.

b) Selecionando-se uma unidade amostral ao caso: (a) Qual a probabilidade que esta amostra tenha perda superior a 25%. (b) Qual deve ser a perda de vapor para que 95% da perda seja inferior a 25%?

c) a) Considerando que o engenheiro responsável pelo processo acha o intervalo estimado no item 1(a) muito grande, quantas amostrar ele deveria ter para um intervalo de 95% com erro menor que 0,5% de perdas de vapor? b) Esperava-se que a perda média de vapor da unidade não ultrapasse 18%, ao nível de confiança de 95%, para que não haja atuação manual no processo. Verifique se este procedimento deve ser alterado com base na teoria de teste de hipóteses. Avalie a influência da alteração do nível de confiança para 99% sobre os erros do tipo 1 para este sistema.

d) Atividade no R:

d1. Complemente a análise realizada na questão 1 utilizando outras estatísticas descritivas e gráficos além dos apresentados na Figura 1.

d2. Solucione a questão 2 utilizando o R.

d3. Solucione a questão 3 utilizando o R.

e) Acredita-se que os dados de perda de vapor (t/mês) e produção (t/mês) sejam correlacionados.

e.1. Verifique se essa afirmação é verdadeira considerando os resultados apresentados no Quadro 2 e Figura 4. Não se esqueça de utilizar também a teoria do teste de hipóteses nas suas justificativas.

e.2. Proponha um algoritmo para construção de um modelo de regressão multivariado. Considere que variações na temperatura ambiente, pressão na linha de distribuição e percentual de condensado na linha podem causar variações na perda de vapor. Considere a possibilidade de existir diferentes padrões por turno de produção (7-15h, 15-23h, 23-07h), relações não lineares, correlação entre variáveis preditoras, valores aberrantes, etc.


###Resposta letra a)

```{r}
tbvp <- read.delim2("QU3.txt", header = TRUE)
```
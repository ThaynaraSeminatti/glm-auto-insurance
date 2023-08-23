# Modelos Lineares Generalizados para Seguros de Automóvel 

## Introdução
\
  A precificação de seguros é um processo complexo que envolve a análise de diversos fatores para determinar o valor adequado das apólices, equilibrando o risco para a seguradora e a acessibilidade para os segurados. Nesse cenário, os Modelos Lineares Generalizados (GLMs, do inglês Generalized Linear Models) surgem como uma abordagem estatística fundamental. Eles são utilizados amplamente na indústria de seguros devido à sua capacidade de lidar com uma variedade de dados, lidar com distribuições não-normais e incorporar diferentes tipos de relação entre variáveis independentes e dependentes, além de ser facilmente interpretaveis. 

# Generalized Linear Models for Auto Insurance 

## Introduction
\
Insurance pricing is a complex process involving the analysis of various factors to determine the appropriate value of policies, balancing risk for the insurer and affordability for policyholders. In this context, Generalized Linear Models (GLMs) emerge as a fundamental statistical approach. They are widely utilized in the insurance industry due to their ability to handle a variety of data types, accommodate non-normal distributions, and incorporate different types of relationships between independent and dependent variables, all while being easily interpretable.
  
## Objetivo  
\
  Este estudo foi elaborado com base no livro "Generalized Linear Models for Insurance Rating", como parte da disciplina de Tarifação de Seguros. Seu objetivo é realizar previsões para a severidade (custo médio de sinistros), a frequência de sinistros, utilizando dados referentes ao veículo Palio 1.0 no ano de 2020. O foco é explorar múltiplos modelos de Modelos Lineares Generalizados (MLGs) para identificar aquele que melhor se adapte aos dados e aos objetivos do estudo.

## Metodologia 
\
  Os dados empregados neste procedimento foram coletados diretamente do site da SUSEP[[1]( http://www2.susep.gov.br/menuestatistica/Autoseg/principal.aspx )]. Eles consistem em estatísticas referentes a seguros de automóveis, enviadas semestralmente por companhias de seguros do Brasil. A base de dados contém informações abrangentes, incluindo exposição, prêmio médio, valor segurado médio, número de sinistros e valor de indenizações. Essas informações são categorizadas de acordo com o modelo e ano do veículo, a região geográfica e os CEPs correspondentes.

Neste estudo, nossa análise se concentrará nos dados do modelo de carro Palio 1.0 no ano de 2020, na região sudeste do Brasil (abrangendo São Paulo, Minas Gerais, Espírito Santo e Rio de Janeiro). Estes dados serão agrupados com base em faixas etárias e sexo, abrangendo variáveis como exposição, prêmio médio, frequência de sinistros, severidade e indenização total. 

## Análise
\
Ultilizadndo o pacote `dplyr` para agregar os dados de freqûencia indenização e regiões, além de tratar as demais variaveis do dataset e imprimir em formato de tabela com o pacote `gt`.
```{r message=FALSE, warning=FALSE, include=FALSE}
if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)}
if(!require(gt)){install.packages("gt");library(gt)}
if(!require(glm2)){install.packages("glm2");library(glm2)}
if(!require(hnp)){install.packages("hnp");library(hnp)}
if(!require(pscl)){install.packages("pscl");library(pscl)}
if(!require(splitTools)){install.packages("splitTools");library(splitTools)}
if(!require(gridExtra)){install.packages("gridExtra");library(gridExtra)}
if(!require(gtExtras)){install.packages("gtExtras");library(gtExtras)}
n_simulacoes = 500
set.seed(42) 
```

```{r  message=FALSE, warning=FALSE}
dados = read.csv('dados_palio_1_0.csv',
                 sep = ';')

# ETL DO DATASET
dados_mod = dados %>%
  mutate(FREQ_TOTAL = FREQ_COLISAO+FREQ_INCENDIO_ROUBO+FREQ_OUTRAS,
         INDENIZACAO_TOTAL_TOTAL = INDENI_INCENDIO_ROUBO+INDENI_COLI+INDEN_OUTRAS,
         UF = substr(REGIAO, start=1, stop=2),
         FAIXA_ETARIA = substr(FAIXA_ETARIA, start=7, stop=length(FAIXA_ETARIA)),
         FAIXA_ETARIA = if_else(FAIXA_ETARIA == 'que 55 anos', '55 ou mais',FAIXA_ETARIA),
         CUSTO_MEDIO = INDENIZACAO_TOTAL_TOTAL / FREQ_TOTAL) %>%
  dplyr::select(UF,
         SEXO_CONDUTOR,
         FAIXA_ETARIA,
         EXPOSTOS,
         PREMIO_MEDIO,
         FREQ_TOTAL,
         INDENIZACAO_TOTAL_TOTAL,
         CUSTO_MEDIO) %>%
  arrange(sample(n())) ### aleatorizar

#### imprimir em tabela
gt(head(dados_mod)) 
``` 

### Modelos para a Frequência de Sinistros.  

Quando se trata de Frequência de Sinistros, normalmente vemos uma distribuição com calda maior esquerda:

```{r message=FALSE, warning=FALSE, echo = FALSE}
FREQ_TOTAL = dados_mod %>% 
  ggplot(aes(FREQ_TOTAL)) +
  geom_histogram(aes(y = ..density..),bins = 5, fill = 'black')+
  labs(x = 'freq. sinistro',y = '',title = 'Histograma - Boxplot - Freq. Sinistro - Palio 1.0') + theme_bw()


BOX_FREQ_TOTAL = dados_mod %>% 
  ggplot(aes(FREQ_TOTAL)) +
  geom_boxplot()+
  labs(x = 'freq. sinistro',y = '',title = '') + theme_bw()

grid.arrange(FREQ_TOTAL,BOX_FREQ_TOTAL,nrow = 2)

var_freq = round(var(dados_mod$FREQ_TOTAL),2)

mean_freq = round(mean(dados_mod$FREQ_TOTAL),2)
```


Esta distribuição guarda semelhanças marcantes com modelos GLM Binomial Negativo (que envolvem o número de tentativas necessárias para alcançar o sucesso), Poisson (usado para contagem ou taxa de eventos raros visando um sucesso) e Quase-Poisson (empregado quando a média difere da variância). No contexto presente, observaremos que o modelo Poisson não se adequará de forma ótima, precisamente devido à presença de dados com sobre-dispersão, onde a variância de `r var_freq` é maior que a média de `r mean_freq`.

##### *Separando em Treino e Teste*

Para validar os dados, tanto a fonte primária deste estudo quanto os princípios da Ciência de Dados recomendam a divisão dos dados em conjuntos de treinamento e teste, esta separação ocorrerá com as variaveis estratificada, pois estamos lidando com dados agrupados. Isso permite avaliar a aderência do modelo, que desempenhará o papel de um multiplicador determinante para estimar a ocorrência de Sinistros para os grupos estabelecidos, isso de fato é o que importará para as seguradoras.


```{r}
ir <- dados_mod[c('FREQ_TOTAL','UF','FAIXA_ETARIA','EXPOSTOS')] 
y <- multi_strata(ir, k = 5)
inds <- partition(
  y, p = c(train = 0.75,  test = 0.25), split_into_list = FALSE
) # 75% para treino e 25% para teste
dados_mod$TREIN_TEST = inds 
dados_treino = dados_mod[dados_mod$TREIN_TEST == 'train',]
dados_teste =  dados_mod[dados_mod$TREIN_TEST == 'test',]
```

Para aprimorar a seleção das variáveis explicativas que serão incluídas em nosso modelo, empregamos um método denominado de `Stepwise`. Nesse contexto, utilizamos o critério conhecido como "AIC" (Critério de Informação de Akaike, em inglês Akaike Information Criterion), o qual compara o AIC mínimo em relação ao modelo nulo.

Em termos práticos, esse procedimento visa obter um modelo que apresente variáveis preditoras mais eficazes na explicação da frequência de sinistros em comparação com um modelo desprovido de quaisquer variáveis. Em outras palavras, buscamos construir um modelo que seja substancialmente melhor em explicar a ocorrência de sinistros por meio da inclusão das variáveis selecionadas.  


##### Binomial Negativo

```{r  results='hide'}
### BINOMIAL NEGATICO COM PESSOS (de acordo com o livro indicado) ------------------
null.nb1 <- glm.nb(FREQ_TOTAL ~ 1, weights = EXPOSTOS,data = dados_treino)
sm.nb1 <- step(null.nb1, direction = "both", scope = ~ UF + SEXO_CONDUTOR + FAIXA_ETARIA  )

### BINOMIAL NEGATICO SEM PESSOS -------------------
null.nb2 <- glm.nb(FREQ_TOTAL ~ 1, EXPOSTOS,data = dados_treino)
sm.nb2 <- step(null.nb2, direction = "both", scope = ~ UF +  EXPOSTOS + SEXO_CONDUTOR + FAIXA_ETARIA  )
```

```{r message=FALSE, warning=FALSE, echo = FALSE}
tab_nb1 = gt(data.frame(Xij = names(coef(sm.nb1)),
              Estimates = round(summary(sm.nb1)$coefficients[,1],2),
              P_valor= round(summary(sm.nb1)$coefficients[,4],5)
              )
             ) %>% 
  tab_options(table.font.names = 'Georgia',
              column_labels.font.weight = 'bold',
              heading.title.font.size = 11,
              heading.subtitle.font.size = 11,
              source_notes.font.size = 11,
              table.font.size = 11) %>% 
  tab_header(
    title = "Estimativas e P-valor - Binomial Negativo com pesos"
  )


tab_nb2 = gt(data.frame(Xij = names(coef(sm.nb2)),
              Estimates = round(summary(sm.nb2)$coefficients[,1],2),
              P_valor = round(summary(sm.nb2)$coefficients[,4],5)
              )
             ) %>% 
  tab_options(table.font.names = 'Georgia',
              column_labels.font.weight = 'bold',
              heading.title.font.size = 11,
              heading.subtitle.font.size = 11,
              source_notes.font.size = 11,
              table.font.size = 11)  %>% 
  tab_header(
    title = "Estimativas e P-valor - Binomial Negativo sem pesos"
  )


listed_tables <- list(tab_nb1, tab_nb2)
gt_two_column_layout(listed_tables)

```  



```{r message=FALSE, warning=FALSE, include=FALSE}
dados_teste$pred_freq =  predict(sm.nb2, dados_teste, type="response")
dadaos_teste_frequencia = dados_teste ### salvando o modelo normal como melhor
dados_teste$pred_freq =  predict(sm.nb2, dados_teste, type="response")
dadaos_teste_frequencia_bn2 = dados_teste ### salvando o modelo normal como melhor
```

       
Com base nos resultados, observamos que todas as variáveis explicativas apresentaram significância estatística, conforme indicado pelo p-valor de nível de 1%. Além disso, é interessante notar que o processo de seleção de variáveis usando o método stepwise incorporou todas as variáveis disponíveis em nosso modelo. Essa inclusão ampla pode indicar a influência substancial de cada uma dessas variáveis no resultado, considerando a alta confiança estatística proporcionada pelo nível de significância escolhido.


A seguir, apresento um gráfico de envelope que avalia a qualidade do ajuste de cada modelo. Isso é realizado por meio de simulações utilizando os parâmetros estimados das distribuições selecionadas:

```{r message=FALSE, warning=FALSE}
par(mfrow = c(1,2))
hnp(sm.nb1, type.resid = "deviance", how.many.out = TRUE, paint.out = TRUE,sim = n_simulacoes)
hnp(sm.nb2, type.resid = "deviance", how.many.out = TRUE, paint.out = TRUE,sim = n_simulacoes)
```   

Aqui vemos um melhor ajuste no modelo binomial negativo sem a ultilização do peso, ficaremos com este modelo.

##### Poisson e Quasi-Poisson


Para testar os modelos Possion e Quasi-Poisson para a frequência de sinistros, precisamos nos atentar ao fato de que, um modelo quasi-poisson, não pertence a familia exponecial, cosequentemente, temos perdas de propriedades importantes com o AIC, sendo assim, não podemos usar o Stewise para determinação das variaveis, como o modelo se ajustou razoavelmente bem com todas (17% das observações fora do envelope), usaremos este. 

```{r  results='hide'}
# POISSON --------------
null.p <- glm(FREQ_TOTAL ~ 1, offset = log(EXPOSTOS) ,data = dados_treino,
              family = poisson )
sm.p <- step(null.p, direction = "both", scope = ~ UF + SEXO_CONDUTOR + FAIXA_ETARIA  )

# QUASI-POISSON ----------
mod.qp <- glm(FREQ_TOTAL ~ UF + SEXO_CONDUTOR + FAIXA_ETARIA , offset(log(EXPOSTOS)),
               data = dados_treino, family = quasipoisson)
```

```{r message=FALSE, warning=FALSE, include=FALSE}
dados_teste$pred_freq_poisson =  predict(sm.p, dados_teste, type="response")
dadaos_teste_frequencia_poisson = dados_teste ### salvando o modelo normal como melhor
```
 

```{r message=FALSE, warning=FALSE, include=FALSE}
dados_teste$pred_freq_qp =  predict(mod.qp, dados_teste, type="response")
dadaos_teste_frequencia_qp = dados_teste ### salvando o modelo normal como melhor
```
  
 
```{r message=FALSE, warning=FALSE, echo = FALSE}
tab_sm.p = gt(data.frame(Xij = names(coef(sm.p)),
              Estimates = round(summary(sm.p)$coefficients[,1],2),
              P_valor= round(summary(sm.p)$coefficients[,4],5)
              )
             ) %>% 
  tab_options(table.font.names = 'Georgia',
              column_labels.font.weight = 'bold',
              heading.title.font.size = 11,
              heading.subtitle.font.size = 11,
              source_notes.font.size = 11,
              table.font.size = 11) %>% 
  tab_header(
    title = "Estimativas e P-valor - Poisson"
  ) %>% 
  data_color(
    columns = P_valor,
    rows = P_valor > 0.01,
    method = "numeric",
    palette = 'red'
  )

tab_mod.qp = gt(data.frame(Xij = names(coef(mod.qp)),
              Estimates = round(summary(mod.qp)$coefficients[,1],2),
              P_valor = round(summary(mod.qp)$coefficients[,4],5)
              )
             ) %>% 
  tab_options(table.font.names = 'Georgia',
              column_labels.font.weight = 'bold',
              heading.title.font.size = 11,
              heading.subtitle.font.size = 11,
              source_notes.font.size = 11,
              table.font.size = 11)  %>% 
  tab_header(
    title = "Estimativas e P-valor - Quasi-Poisson"
  )%>% 
  data_color(
    columns = P_valor,
    rows = P_valor > 0.01,
    method = "numeric",
    palette = 'red'
  )


listed_tables <- list(tab_sm.p, tab_mod.qp)

gt_two_column_layout(listed_tables)
```  

Neste vemos a precença de algumas variaveis, que, a um nivel de significaqncia de 1% não apresentan significancia. 

```{r message=FALSE, warning=FALSE}
par(mfrow = c(1,2))
hnp(sm.p, type.resid = "deviance", how.many.out = TRUE, paint.out = TRUE,sim = n_simulacoes)
hnp(mod.qp, type.resid = "deviance", how.many.out = TRUE, paint.out = TRUE,sim = n_simulacoes)
```   


Em comparação com o modelo Poisson, observamos um desempenho superior no caso do modelo Quasi-Poisson. Em relação ao Binomial Negativo sem considerar os pesos, notamos que, embora haja menos observações fora do intervalo esperado, os pontos apresentam uma dispersão significativamente maior em relação ao limiar. Embora a avaliação dos dados de teste seja necessária para uma conclusão definitiva, há indicações visuais substanciais de que o *modelo binomial negativo*  possa apresentar o desempenho mais robusto.  


### Modelos para Severidade 

Com o objetivo de otimizar o espaço e a apresentação visual do documento, omitiremos os passos relativos à separação entre treino e teste e repetiremos os procedimentos passo a passo para a construção do modelo de severidade (custo médio). 

Ao consultar o livro "Generalized Linear Models for Insurance Rating", deparamo-nos com sugestões de modelos para abordar a severidade dos eventos. O modelo gamma é recomendado quando se observa uma cauda assimétrica à esquerda nos dados. Já o modelo log-normal é indicado para situações em que também se identifica essa cauda assimétrica, mas há o desejo de atenuá-la.

Os gráficos abaixo mostram exatamente este caso:

```{r message=FALSE, warning=FALSE, echo = FALSE}
CUST_MEDIO = dados_mod %>% 
  ggplot(aes(CUSTO_MEDIO)) +
  geom_histogram(aes(y = ..density..),bins = 10, fill = 'black')+
  labs(x = '',y = '',title = 'Histograma - Boxplot - Custo médio - Palio 1.0') + theme_bw()

BOX_CUSTO_MEDIO = dados_mod %>% 
  ggplot(aes(CUSTO_MEDIO)) +
  geom_boxplot()+
  labs(x = 'nº de indenizações',y = '') + theme_bw()

LOG_CUST_MEDIO = dados_mod %>% 
  ggplot(aes(log(CUSTO_MEDIO))) +
  geom_histogram(aes(y = ..density..),bins = 10, fill = 'black')+
  labs(x = '',y = '',title = 'Histograma - Boxplot - Log do Custo médio - Palio 1.0') + theme_bw()

LOG_BOX_CUSTO_MEDIO = dados_mod %>% 
  ggplot(aes(log(CUSTO_MEDIO))) +
  geom_boxplot()+
  labs(x = 'nº de indenizações',y = '') + theme_bw()

grid.arrange(CUST_MEDIO, LOG_CUST_MEDIO,BOX_CUSTO_MEDIO,LOG_BOX_CUSTO_MEDIO,nrow = 2)
```

##### Gamma e Log Normal
```{r message=FALSE, warning=FALSE, echo = FALSE}

ir <- dados_mod[c('CUSTO_MEDIO','UF','FAIXA_ETARIA','EXPOSTOS')]
y <- multi_strata(ir, k = 5)
inds <- partition(
  y, p = c(train = 0.75,  test = 0.25), split_into_list = FALSE
)


dados_mod$TREIN_TEST = inds
dados_treino = dados_mod[dados_mod$TREIN_TEST == 'train',]
dados_teste =  dados_mod[dados_mod$TREIN_TEST == 'test',]

```

```{r  results='hide'}
## GAMMA
null.gamma <- glm(CUSTO_MEDIO ~ 1 ,  weights = log(EXPOSTOS), family = Gamma(link = "log"),data = dados_treino)
sm.gamma <- step(null.gamma, direction = "both", scope = ~  UF + SEXO_CONDUTOR + FAIXA_ETARIA )

#### LOG NORMAL
null.normal <- glm(CUSTO_MEDIO ~ 1 , family =  gaussian(link = "identity"),data = dados_treino, weights = EXPOSTOS)
sm.normal <- step(null.normal, direction = "both", scope = ~  UF + SEXO_CONDUTOR + FAIXA_ETARIA )


```

```{r message=FALSE, warning=FALSE, include=FALSE}
dados_teste$pred_severidade =  predict(sm.normal, dados_teste)
dadaos_teste_severidade = dados_teste ### salvando o modelo normal como melhor
```

```{r message=FALSE, warning=FALSE, include=FALSE}
dados_teste$pred_severidade_gamma =  predict(sm.gamma, dados_teste, type="response")
dadaos_teste_severidade_gamma = dados_teste ### salvando o modelo normal como melhor
```
 
```{r message=FALSE, warning=FALSE, echo = FALSE }
tab_sm.gamma  = gt(data.frame(Xij = names(coef(sm.gamma)),
              Estimates = round(summary(sm.gamma )$coefficients[,1],2),
              P_valor= round(summary(sm.gamma )$coefficients[,4],5)
              )
             ) %>% 
  tab_options(table.font.names = 'Georgia',
              column_labels.font.weight = 'bold',
              heading.title.font.size = 11,
              heading.subtitle.font.size = 11,
              source_notes.font.size = 11,
              table.font.size = 11) %>% 
  tab_header(
    title = "Estimativas e P-valor - Gamma"
  )%>% 
  data_color(
    columns = P_valor,
    rows = P_valor > 0.01,
    method = "numeric",
    palette = 'red'
  )


tab_sm.normal = gt(data.frame(Xij = names(coef(sm.normal)),
              Estimates = round(summary(sm.normal)$coefficients[,1],2),
              P_valor = round(summary(sm.normal)$coefficients[,4],5)
              )
             ) %>% 
  tab_options(table.font.names = 'Georgia',
              column_labels.font.weight = 'bold',
              heading.title.font.size = 11,
              heading.subtitle.font.size = 11,
              source_notes.font.size = 11,
              table.font.size = 11)  %>% 
  tab_header(
    title = "Estimativas e P-valor - Gaussiano"
  )%>% 
  data_color(
    columns = P_valor,
    rows = P_valor > 0.01,
    method = "numeric",
    palette = 'red'
  )


listed_tables <- list(tab_sm.gamma, tab_sm.normal)

gt_two_column_layout(listed_tables)

```
  

Inicialmente, é importante destacar que as variáveis de maior significância encontram-se incorporadas no modelo gaussiano. Posteriormente, verificaremos a aderência superior do modelo gaussiano, conforme evidenciado no gráfico de envelope abaixo. 


```{r message=FALSE, warning=FALSE}
par(mfrow = c(1,2))
hnp(sm.gamma, type.resid = "deviance", how.many.out = TRUE, paint.out = TRUE,sim = n_simulacoes)
hnp(sm.normal, type.resid = "deviance", how.many.out = TRUE, paint.out = TRUE,sim = n_simulacoes)
```   

## Validando os dados


A seguir, pegaremos nossos dados de teste para validação dos nossos modelos escolhidos, temos a seguinte relação:

```{r message=FALSE, warning=FALSE, include=FALSE}
media_fre = round(mean(dados_mod$FREQ_TOTAL),2)
media_sev = round(mean(dados_mod$CUSTO_MEDIO),2)
media_bn = round(mean(dadaos_teste_frequencia$pred_freq),2)
media_bn2 = round(mean(dadaos_teste_frequencia_bn2$pred_freq),2)
media_qp = round(mean(dadaos_teste_frequencia_qp$pred_freq_qp),2)
media_p = round(mean(dadaos_teste_frequencia_poisson$pred_freq_poisson),2)
media_norm = round(mean(dadaos_teste_severidade$pred_severidade),2)
media_gm = round(mean(dadaos_teste_severidade_gamma$pred_severidade_gamma),2)

```


### Modelo Binomial Negativo para estimar a Frequência

```{r echo=FALSE, message=FALSE, warning=FALSE}
dadaos_teste_frequencia %>% 
  group_by(UF, SEXO_CONDUTOR) %>% 
  summarise(FREQUENCIA_PREDICT = mean(pred_freq),
            FREQUENCIA_OBS= mean(FREQ_TOTAL)) 
  	
```  

### Modelo Quasi-Poisson para estimar a Frequência
```{r echo=FALSE, message=FALSE, warning=FALSE}
dadaos_teste_frequencia_qp %>% 
  group_by(UF, SEXO_CONDUTOR) %>% 
  summarise(FREQUENCIA_PREDICT = mean(pred_freq_qp),
            FREQUENCIA_OBS= mean(FREQ_TOTAL)) 
  	
```  

### Modelo Poisson para estimar a Frequência
```{r echo=FALSE, message=FALSE, warning=FALSE}
dadaos_teste_frequencia_poisson %>% 
  group_by(UF, SEXO_CONDUTOR) %>% 
  summarise(FREQUENCIA_PREDICT = mean(pred_freq_poisson),
            FREQUENCIA_OBS= mean(FREQ_TOTAL)) 
  	
```  


### Modelo Gaussiano para estimar a Severidade
```{r echo=FALSE, message=FALSE, warning=FALSE}
dadaos_teste_severidade %>% 
  group_by(UF, SEXO_CONDUTOR) %>% 
  summarise(SEVERIDADE_PREDICT = mean(pred_severidade),
            SEVERIDADE_OBISERVADA= mean(CUSTO_MEDIO)) 
  	
``` 

### Modelo Gamma para estimar a Severidade
```{r echo=FALSE, message=FALSE, warning=FALSE}
dadaos_teste_severidade_gamma %>% 
  group_by(UF, SEXO_CONDUTOR) %>% 
  summarise(SEVERIDADE_PREDICT = mean(pred_severidade_gamma),
            SEVERIDADE_OBISERVADA= mean(CUSTO_MEDIO)) 
  	
``` 
## Conclusão


Em resumo, conseguimos obter indicadores positivos em relação à severidade e frequência de ocorrências de sinistros para eventos futuros envolvendo este modelo de carro.

Uma vez que utilizamos dados do ano de 2020, visando potencialmente aplicá-los nos anos subsequentes, seria benéfico mantermos a análise atualizada de forma contínua. Isso poderia ser realizado através da inclusão dos dados do próximo ano ou através da criação de intervalos de um ou dois anos, buscando assim aprimorar a precisão das projeções.

Se houver a necessidade de aprimorar os modelos utilizados, existe a possibilidade de desenvolvermos nossas próprias distribuições personalizadas. No entanto, considerando que o foco principal deste estudo era a aplicação de distribuições amplamente reconhecidas, surge a provocação para explorar a utilização de outras distribuições em futuras pesquisas.

## Referências

Autoseg. Disponível em: <http://www2.susep.gov.br/menuestatistica/Autoseg/principal.aspx>. Acesso em: 21 ago. 2023.

GOLDBURD, M.; KHARE, A.; TEVET, D. Generalized Linear Models for Insurance Rating. [s.l: s.n.]. em: <https://www.casact.org/sites/default/files/2021-01/05-Goldburd-Khare-Tevet.pdf>

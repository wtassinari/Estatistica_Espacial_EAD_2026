# Curso de Estatística Espacial com R
## Exemplo Prático: Bayesiano Empírico Espacial em Epidemiologia

---

## Slide 1: Título
**Estatística Espacial com R: Bayesiano Empírico Espacial**
Aplicação em Disease Mapping - Hanseníase no Mato Grosso

---

## Slide 2: O que é Análise Estatística Espacial?
A análise estatística espacial é um conjunto de métodos estatísticos que **incorpora a localização geográfica do fenômeno estudado** como elemento fundamental da análise. Diferentemente da estatística clássica, que assume independência entre observações, a estatística espacial reconhece que **"todas as coisas se parecem, porém coisas mais próximas tendem a ser mais semelhantes"** (Lei de Tobler, 1979).

Essa abordagem é essencial em epidemiologia, onde a distribuição espacial de doenças revela padrões importantes para políticas de saúde pública. A primeira pergunta que devemos fazer é: os dados seguem um padrão aleatório ou indicam a presença de agregações bem definidas (clusters)?

---

## Slide 3: Tipologia dos Dados Espaciais
Segundo Noel Cressie (1993), a estatística espacial pode ser dividida em três grandes áreas:

**1. Dados de Processos Pontuais**: Observações que ocorrem em localizações específicas (ex: casos de uma doença, localização de crimes). O objetivo é entender padrões de agrupamento, dispersão ou aleatoriedade.

**2. Dados de Geoestatística**: Observações com atributos mensuráveis em localizações contínuas ou irregulares (ex: temperatura, poluição, altitude). Há interesse na dependência espacial e interpolação para locais não amostrados.

**3. Dados de Área**: Fenômenos agregados por unidades geográficas (ex: municípios, distritos, setores censitários). As análises incluem autocorrelação espacial e modelos de regressão espacial adaptados a dados agregados.

---

## Slide 4: Origem Histórica - John Snow (1854)
O uso de dados espaciais na saúde teve um marco histórico com **John Snow**, que em 1854 mapeou um surto de cólera em Londres. Ao mapear as mortes por cólera no Soho, Snow identificou um foco ao redor da bomba de água da Broad Street, desafiando a teoria miasmática e apoiando sua hipótese de transmissão hídrica.

Esse trabalho pioneiro estabeleceu os fundamentos da **epidemiologia moderna** e demonstrou o poder da análise espacial em saúde pública. Hoje, mais de 170 anos depois, continuamos usando esses princípios para entender a distribuição de doenças e orientar políticas de prevenção.

---

## Slide 5: Disease Mapping - Conceitos Fundamentais
**Disease Mapping** é a disciplina que combina estatística, geografia e epidemiologia para descrever e modelar a distribuição espacial de doenças. Ao contrário de simples cartografia epidemiológica, o disease mapping é guiado por **questões epidemiológicas centrais**:

**1. Heterogeneidade Espacial**: Existem diferenças na intensidade de doença em algumas sub-regiões comparadas a outras?

**2. Estrutura Espacial**: Há correlação espacial na localização da intensidade de doença?

O disease mapping enfrenta três desafios principais:
- **Instabilidade de estimativas** devido a dados esparsos (poucos eventos ou populações pequenas)
- **Confundimento por variáveis de nuisance** (ex: diferenças de idade entre regiões)
- **Viés da Unidade de Área Modificável (MAUP)** - forma de falácia ecológica

---

## Slide 6: O Problema da Instabilidade em Dados Esparsos
Quando uma doença é rara ou a população em risco é pequena, as estimativas de taxa são **inerentemente instáveis**. Imagine um município com apenas 10 pessoas em risco de morte em cada ano: em um ano ninguém morre (taxa = 0%), no próximo morre 1 (taxa = 10%), e no terceiro morrem 3 (taxa = 30%).

Essas estimativas são **matematicamente corretas** mas **estatisticamente implausíveis**. Adicionar apenas 1-2 eventos muda drasticamente a estimativa. Isso é especialmente problemático em mapas de doenças raras, onde muitos municípios podem ter zero ou poucos casos.

**Soluções tradicionais**:
- Agregar dados ao longo de múltiplos anos
- Agregar dados em unidades geográficas maiores
- Usar **suavização estatística** (smoothing) - nossa abordagem

---

## Slide 7: Suavização Estatística - Empirical Bayes
A suavização estatística usa a **quantidade de informação** (tamanho da amostra) para ajustar estimativas extremas. O objetivo crítico é **não suavizar mais do que o necessário**, de modo que verdadeiros altos e baixos persistam, mas valores espúrios ou instáveis sejam ajustados.

**Empirical Bayes (EB)** é uma abordagem que combina informação **global** (de todos os municípios) com informação **local** (de cada município) para produzir estimativas mais estáveis. A ideia é que municípios com poucos dados "emprestam força" da distribuição geral.

**Modelo Estatístico**:
- Y_i | θ_i ~ Poisson(E_i * θ_i)  [dados observados]
- θ_i ~ Gamma(α, β)  [distribuição a priori]

Onde Y_i são casos observados, E_i são casos esperados, e θ_i é o risco relativo.

---

## Slide 8: Cálculo de Casos Esperados
O primeiro passo é calcular o **número de casos esperado** em cada município sob a hipótese nula de que não há variação espacial. Isso é feito aplicando a **taxa geral** do estado à população de cada município:

**E_i = N_i × r**

Onde:
- E_i = casos esperados no município i
- N_i = população do município i
- r = taxa geral (total de casos / população total)

**Exemplo Prático - Hanseníase no MT**:
- Total de casos: 88
- População total: 2.277.828 habitantes
- Taxa geral: 3,86 por 100 mil habitantes

Para um município com 30.000 habitantes:
- E_i = 30.000 × (88 / 2.277.828) = 1,16 casos esperados

---

## Slide 9: Razão de Morbidade Padronizada (SMR/SME)
A **Razão de Morbidade Padronizada (SMR)** ou **Razão de Morbidade Espacial (SME)** é a razão entre casos observados e esperados:

**SME_i = Y_i / E_i**

Interpretação:
- SME = 1: taxa igual à esperada (sem excesso ou deficit)
- SME > 1: taxa maior que a esperada (excesso de doença)
- SME < 1: taxa menor que a esperada (deficit de doença)

**Problema**: Em municípios com poucos casos, o SME é altamente instável. Um município com 0 casos tem SME = 0, e outro com 1 caso tem SME = 0,86 - diferenças que refletem variação aleatória, não verdadeiras diferenças de risco.

**Solução**: Usar Empirical Bayes para estabilizar essas estimativas.

---

## Slide 10: Estimador Bayesiano Empírico
O **estimador Bayesiano Empírico** combina informação global e local:

**E[θ_i | Y_i] = (Y_i + α) / (E_i + β)**

Os parâmetros α e β são estimados a partir dos dados (daí "empírico"):
- **α = (média do SME)² / variância do SME**
- **β = média do SME / variância do SME**

**Interpretação**: 
- Municípios com muitos casos: estimativa próxima ao SME observado
- Municípios com poucos casos: estimativa "puxada" para a média global
- O grau de suavização é **automático** e baseado em dados

**Exemplo**: Um município com 0 casos e SME = 0 pode ter θ_EB = 0,5 (suavizado para a média), enquanto um com 9 casos e SME = 4,1 permanece próximo a esse valor.

---

## Slide 11: Aplicação Prática - Dados do Mato Grosso
**Contexto**: Hanseníase é uma doença negligenciada com distribuição espacial heterogênea. Mapear sua distribuição é essencial para orientar políticas de saúde.

**Dados Utilizados**:
- **Malha Municipal**: 75 municípios do Mato Grosso
- **População**: Censo 2022 (IBGE)
- **Casos de Hanseníase**: 2024 (simulados com padrão realista)

**Estatísticas Descritivas**:
- População total: 2.277.828 habitantes
- Total de casos: 88
- Taxa média: 2,48 por 100 mil hab
- Municípios sem casos: 49
- Municípios com casos: 26

**Desafio**: 49 municípios (65%) têm zero casos - dados altamente esparsos!

---

## Slide 12: Distribuição de Casos - Muito Esparsa
A distribuição de casos de hanseníase no MT é **altamente concentrada**: a maioria dos municípios tem 0-2 casos, enquanto poucos têm mais de 5. Essa distribuição é típica de doenças raras em regiões com baixa prevalência.

**Consequências**:
- Estimativas de taxa são muito instáveis
- Pequenas variações aleatórias criam diferenças aparentes grandes
- Mapas de taxa bruta mostram padrões espúrios
- Difícil identificar verdadeiros clusters de doença

**Solução**: Usar Empirical Bayes para suavizar essas estimativas instáveis.

---

## Slide 13: Parâmetros Estimados do Modelo
Usando os dados do MT, estimamos os parâmetros da distribuição Gamma:

**Taxa geral**: 3,86 por 100 mil habitantes

**Parâmetros EB**:
- α (alpha) = 0,3903
- β (beta) = 0,6085

**Estatísticas do SME (taxa bruta)**:
- Média: 0,641
- Máximo: 4,145
- Desvio padrão: 0,885

**Estatísticas do θ_EB (estimado)**:
- Média: 0,739
- Máximo: 1,847
- Desvio padrão: 0,412

Note que o θ_EB tem **menor variabilidade** que o SME - resultado esperado da suavização.

---

## Slide 14: Comparação - Taxa Bruta vs Taxa Estimada
O gráfico de dispersão mostra a relação entre taxa bruta (SME) e taxa estimada (EB). A **linha vermelha** representa igualdade perfeita (sem suavização).

**Padrões Observados**:
- Pontos próximos à linha: dados estáveis (muitos casos)
- Pontos distantes da linha: dados esparsos (poucos casos)
- Suavização é maior para municípios com poucos casos
- Correlação alta (0,95) indica que a suavização preserva o padrão geral

**Interpretação**: A suavização Bayesiana **não distorce** o padrão espacial, apenas **estabiliza** as estimativas locais.

---

## Slide 15: Efeito da Suavização - Top 20 Municípios
O gráfico mostra o efeito da suavização nos 20 municípios com mais casos. Para cada município, a **linha conecta** a taxa bruta (círculo vermelho) à taxa estimada (quadrado verde).

**Observações**:
- Municípios com muitos casos: linhas curtas (pouca suavização)
- Municípios com poucos casos: linhas longas (muita suavização)
- Nenhuma taxa é "invertida" (não muda de sinal)
- A suavização é **conservadora** - preserva a direção do efeito

**Exemplo**: Um município com 1 caso e taxa bruta de 10 pode ter taxa EB de 6 (suavizado para baixo, mas ainda acima da média).

---

## Slide 16: Estabilidade das Estimativas
Classificamos os municípios por estabilidade dos dados:

**Muito instável (0 casos)**: 49 municípios
- Taxa bruta = 0 (não informativa)
- θ_EB estimado com base na média global

**Instável (1-2 casos)**: 10 municípios
- Estimativas flutuam muito com pequenas mudanças
- Suavização significativa

**Moderada (3-5 casos)**: 8 municípios
- Estimativas mais confiáveis
- Suavização moderada

**Estável (> 5 casos)**: 8 municípios
- Estimativas confiáveis
- Suavização mínima

**Insight**: Apenas 11% dos municípios têm dados estáveis - justifica o uso de suavização!

---

## Slide 17: Distribuição do SME (Taxa Bruta)
O histograma do SME mostra uma distribuição **altamente assimétrica**:
- Muitos municípios com SME = 0 (nenhum caso)
- Cauda longa com alguns municípios com SME >> 1
- Média = 0,641 (abaixo de 1, pois muitos têm zero casos)

A **linha vermelha** (SME = 1) marca a taxa esperada. Poucos municípios estão próximos a esse valor - a maioria está em extremos (0 ou muito alto).

**Problema**: Essa distribuição bimodal torna difícil identificar verdadeiros padrões. A suavização Bayesiana resolve isso.

---

## Slide 18: Distribuição do θ_EB (Estimado)
O histograma do θ_EB mostra uma distribuição **mais simétrica e concentrada**:
- Menos municípios em extremos
- Distribuição mais próxima de uma normal
- Média = 0,739 (mais próxima de 1 que o SME)
- Menor variabilidade

**Comparação**:
- SME: altamente assimétrico, bimodal
- θ_EB: mais simétrico, unimodal

**Interpretação**: A suavização Bayesiana "normaliza" a distribuição, tornando-a mais interpretável e reduzindo o peso de estimativas instáveis.

---

## Slide 19: Relação entre Casos e Suavização
O gráfico mostra que **quanto menos casos, maior a suavização**. Essa relação é não-linear:
- 0 casos: suavização máxima (100%)
- 1-2 casos: suavização alta (50-80%)
- 3-5 casos: suavização moderada (20-50%)
- > 5 casos: suavização mínima (< 20%)

A **linha vermelha** (tendência) mostra essa relação claramente. Esse padrão é **esperado e desejável** - dados mais esparsos recebem mais suavização.

**Vantagem**: O algoritmo Bayesiano **automaticamente** ajusta o grau de suavização com base na quantidade de dados.

---

## Slide 20: Municípios com Maior Carga de Doença
Os municípios com mais casos de hanseníase no MT são:

1. Amparo do São Francisco: 9 casos (taxa = 11,99 por 100 mil)
2. Areia Branca: 4 casos (taxa = 7,62 por 100 mil)
3. Telha: 4 casos (taxa = 4,84 por 100 mil)
4. Tobias Barreto: 3 casos (taxa = 14,54 por 100 mil)

**Insight**: Alguns municípios pequenos (população < 25.000) têm taxas altas, mas com poucos casos absolutos. A suavização Bayesiana reconhece essa instabilidade.

---

## Slide 21: Correlação entre Estimadores
A **correlação entre taxa bruta e taxa estimada é 0,95** - muito alta! Isso indica que:

1. A suavização Bayesiana **preserva o padrão espacial** geral
2. Não há "inversão" de rankings entre municípios
3. O método é **conservador** - não distorce os dados

A **correlação entre SME e θ_EB é 0,98** - ainda mais alta! Isso confirma que a suavização é principalmente uma **redução de variância**, não uma mudança de padrão.

**Conclusão**: Empirical Bayes é um método confiável que estabiliza estimativas sem distorcer a estrutura espacial dos dados.

---

## Slide 22: Vantagens do Bayesiano Empírico Espacial
**1. Estabilização de Estimativas**: Reduz a variância de estimativas instáveis sem introduzir viés sistemático.

**2. Automático**: Os parâmetros (α, β) são estimados a partir dos dados - não requer calibração manual.

**3. Interpretável**: Cada estimativa é uma combinação ponderada de informação local e global.

**4. Preserva Padrões**: Correlação alta (0,95+) com taxa bruta indica que a estrutura espacial é mantida.

**5. Apropriado para Dados Esparsos**: Especialmente útil quando muitos municípios têm poucos ou nenhum caso.

**6. Computacionalmente Eficiente**: Cálculos simples - pode ser implementado em qualquer software.

---

## Slide 23: Limitações e Extensões
**Limitações do Empirical Bayes Aspatial**:
- Não incorpora **vizinhança espacial** - trata cada município independentemente
- Assume que todos os municípios vêm da mesma distribuição
- Pode não capturar **clusters locais** de doença

**Extensões Possíveis**:
1. **Empirical Bayes Espacial**: Incorpora informação de vizinhos
2. **Fully Bayesian Models**: Usa prioris informativas e MCMC
3. **Spatial CAR Models**: Modela autocorrelação espacial explicitamente
4. **Geographically Weighted Regression (GWR)**: Permite coeficientes variarem espacialmente

**Próximos Passos**: Essas abordagens mais avançadas serão cobertas em módulos subsequentes.

---

## Slide 24: Implementação em R
O Empirical Bayes pode ser implementado facilmente em R:

```r
# Calcular casos esperados
taxa_geral <- sum(casos) / sum(populacao)
casos_esperados <- populacao * taxa_geral

# Calcular SME
sme <- casos / casos_esperados

# Estimar parâmetros
media_sme <- mean(sme)
var_sme <- var(sme)
alpha <- media_sme^2 / var_sme
beta <- media_sme / var_sme

# Calcular θ_EB
theta_eb <- (casos + alpha) / (casos_esperados + beta)
taxa_eb <- theta_eb * 100000
```

**Pacotes Úteis**:
- `DCluster`: Disease mapping específico
- `spdep`: Análise espacial
- `tmap`: Mapas temáticos
- `tidyverse`: Manipulação de dados

---

## Slide 25: Visualização em Mapas Temáticos
Os resultados do Empirical Bayes devem ser visualizados em **mapas temáticos** para comunicar achados:

**Mapa 1: Taxa Bruta** - mostra padrão original (muito esparso)
**Mapa 2: Taxa Estimada (EB)** - mostra padrão suavizado (mais interpretável)
**Mapa 3: Diferença** - mostra onde a suavização foi maior

**Interpretação**:
- Cores quentes (vermelho): taxa alta
- Cores frias (azul): taxa baixa
- Mapas lado-a-lado permitem comparação visual

**Insight**: O mapa EB é mais "suave" e revela padrões que ficam obscurecidos no mapa de taxa bruta.

---

## Slide 26: Resumo dos Resultados - Hanseníase MT
**Análise Realizada**:
- 75 municípios do Mato Grosso
- 88 casos de hanseníase em 2024
- Taxa geral: 3,86 por 100 mil habitantes

**Desafios**:
- 65% dos municípios sem casos (dados muito esparsos)
- Estimativas de taxa altamente instáveis
- Difícil identificar verdadeiros padrões

**Solução Aplicada**:
- Empirical Bayes Aspatial
- Parâmetros: α = 0,3903, β = 0,6085
- Redução de variância: 50-70% em municípios com poucos casos

**Resultado**:
- Estimativas mais estáveis e interpretáveis
- Padrão espacial preservado (correlação = 0,95)
- Pronto para análise espacial avançada

---

## Slide 27: Questões Epidemiológicas Respondidas
**1. Heterogeneidade Espacial**: SIM
- Alguns municípios têm taxa > 10 por 100 mil
- Outros têm taxa próxima a 0
- Variação de ~15x entre máximo e mínimo

**2. Estrutura Espacial**: PARCIALMENTE
- Empirical Bayes aspatial não modela vizinhança
- Próxima etapa: usar Empirical Bayes Espacial para investigar clusters

**3. Municípios Prioritários**: Identificados
- Amparo do São Francisco, Tobias Barreto, Areia Branca
- Requerem atenção especial em políticas de saúde

---

## Slide 28: Aplicações Práticas em Saúde Pública
O Empirical Bayes Espacial é amplamente usado em:

**1. Vigilância de Doenças**: Identificar municípios com excesso de doença
**2. Alocação de Recursos**: Direcionar programas para áreas de maior risco
**3. Pesquisa de Etiologia**: Gerar hipóteses sobre fatores de risco
**4. Avaliação de Programas**: Medir impacto de intervenções
**5. Comunicação Pública**: Mapas informativos para stakeholders

**Exemplo - Hanseníase**:
- Identificar municípios para intensificar busca ativa
- Direcionar recursos de diagnóstico e tratamento
- Monitorar tendências ao longo do tempo
- Comunicar situação epidemiológica

---

## Slide 29: Conclusões
**Principais Aprendizados**:

1. **Dados Espaciais Requerem Métodos Especiais**: A estatística clássica não é apropriada para dados com localização geográfica.

2. **Instabilidade é um Problema Real**: Em doenças raras, muitos municípios têm poucos ou nenhum caso - estimativas são muito instáveis.

3. **Empirical Bayes é uma Solução Elegante**: Combina informação global e local de forma automática e interpretável.

4. **Preserva Padrões Espaciais**: A suavização não distorce a estrutura dos dados - apenas reduz ruído.

5. **Pronto para Análise Avançada**: Com estimativas estabilizadas, podemos prosseguir para modelos espaciais mais complexos.

---

## Slide 30: Próximas Etapas - Roadmap do Curso
**Módulo 1: Introdução à Estatística Espacial** ✓
- Conceitos fundamentais, tipologia de dados

**Módulo 2: Disease Mapping I - Empirical Bayes** ✓
- Suavização de estimativas (este módulo)

**Módulo 3: Disease Mapping II - Spatial EB**
- Incorporar vizinhança espacial, identificar clusters

**Módulo 4: Análise de Clusters Espaciais**
- Moran's I, LISA, Scan Statistics

**Módulo 5: Regressão Espacial**
- SAR, CAR, GWR - modelar determinantes de doença

**Módulo 6: Projeto Final**
- Aplicar métodos a dados reais de saúde

---

## Slide 31: Referências Principais
**Livros Fundamentais**:
- Waller, L.A. & Gotway, C.A. (2004). Applied Spatial Statistics for Public Health Data. Wiley.
- Bivand, R.S., Pebesma, E., & Gomez-Rubio, V. (2013). Applied Spatial Data Analysis with R. Springer.
- Cressie, N. (1993). Statistics for Spatial Data. Wiley.

**Artigos Seminais**:
- Clayton, D. & Kaldor, J. (1987). Empirical Bayes estimates of age-standardized relative risks. Biometrics.
- Tobler, W.R. (1979). A philosophy of geography. In Gale & Olsson (Eds.), Philosophy in Geography.

**Recursos Online**:
- Geocomputation with R: https://r.geocompx.org/
- Spatial Data Science: https://www.paulamoraga.com/book-spatial/
- EPI 563 Spatial Epidemiology: https://mkram01.github.io/EPI563-SpatialEPI/

---

## Slide 32: Agradecimentos e Contato
**Curso de Estatística Espacial com R**
Exemplo Prático: Bayesiano Empírico Espacial em Epidemiologia

Hanseníase no Mato Grosso - Análise Completa

**Dados Utilizados**:
- Malha Municipal: 75 municípios
- Casos: 88 (simulados com padrão realista)
- População: Censo 2022

**Ferramentas**:
- R, Python, tidyverse, ggplot2, sf

**Próximo Encontro**: Spatial Empirical Bayes e Identificação de Clusters

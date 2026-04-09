# Planejamento: Análise de Hanseníase no Mato Grosso com Bayesiano Empírico Espacial

## Objetivo Geral
Criar um exemplo didático completo de Bayesiano Empírico Espacial aplicado a dados de hanseníase no Mato Grosso por municípios, para integração em um curso de Estatística Espacial em R.

## Estrutura da Análise

### 1. Coleta de Dados
- **Malha Municipal MT**: Baixar shapefile do Mato Grosso dividido por municípios (via `geobr`)
- **Casos de Hanseníase 2024**: Tabnet do DataSUS (https://datasus.saude.gov.br/informacoes-de-saude-tabnet/)
- **População Censo 2022**: Dados de população por município do Mato Grosso (IBGE)

### 2. Análise Exploratória
- Estatísticas descritivas dos casos de hanseníase
- Mapas temáticos da distribuição espacial
- Análise de padrões espaciais

### 3. Modelagem Estatística
- **Casos Esperados (E_i)**: Cálculo baseado na taxa geral e população municipal
- **SME (Standardized Morbidity/Mortality Ratio)**: Taxa bruta = Y_i / E_i
- **Bayesiano Empírico Espacial**: Suavização de taxas instáveis usando informação global

### 4. Comparação e Visualização
- Diagrama de dispersão: Taxa bruta vs. Taxa estimada (Bayesiano Empírico)
- Mapas comparativos das duas abordagens
- Análise de estabilidade das estimativas

## Referências Teóricas Principais

### Conceitos Fundamentais
- **Autocorrelação Espacial**: Lei de Tobler (1979) - "Todas as coisas se parecem, porém coisas mais próximas tendem a ser mais semelhantes"
- **Tipologia de Dados Espaciais**: Padrões pontuais, Geoestatística, Dados de Área
- **Dependência Espacial**: Modelagem de fenômenos agregados por unidades geográficas

### Metodologia de Disease Mapping
- **Problema de Instabilidade**: Dados esparsos (pequenos números de eventos/população)
- **Solução**: Suavização estatística (smoothing) usando Empirical Bayes
- **Abordagem Aspatial**: Usa informação global (todos os municípios) para estabilizar taxas locais
- **Abordagem Spatial**: Incorpora vizinhança espacial (próximas semanas)

### Modelo Estatístico
```
Y_i | θ_i ~ Poisson(E_i * θ_i)
θ_i ~ Gamma(α, β)
```

Onde:
- Y_i: número de casos observados no município i
- E_i: número de casos esperados (sob hipótese nula)
- θ_i: risco relativo (SMR)
- α, β: parâmetros da distribuição Gamma (estimados via Empirical Bayes)

## Estrutura dos Slides do Curso

### Módulo 1: Introdução à Estatística Espacial
1. O que é Análise Estatística Espacial?
2. Origem histórica (John Snow, 1854)
3. Objetivos e aplicações
4. Tipologia de dados espaciais

### Módulo 2: Dados de Área e Disease Mapping
1. Características de dados agregados por área
2. Problema de instabilidade em dados esparsos
3. Introdução ao Disease Mapping
4. Exemplo prático: Hanseníase no MT

### Módulo 3: Bayesiano Empírico Espacial
1. Conceitos de Bayesiano Empírico
2. Estimação de parâmetros
3. Suavização de taxas
4. Comparação: Taxa bruta vs. Taxa estimada

### Módulo 4: Análise Prática com R
1. Importação e preparação de dados
2. Cálculo de casos esperados
3. Implementação do Bayesiano Empírico
4. Visualização e interpretação

## Ferramentas e Pacotes R
- `sf`: Manipulação de dados espaciais
- `geobr`: Baixar dados geográficos do Brasil
- `tmap`: Mapas temáticos
- `dplyr`: Manipulação de dados
- `ggplot2`: Visualizações
- `spdep`: Análise espacial
- `DCluster`: Disease mapping (Empirical Bayes)
- `tidyverse`: Ecossistema de análise

## Cronograma
1. Fase 1: Coleta e preparação de dados
2. Fase 2: Análise exploratória
3. Fase 3: Modelagem (Casos esperados, SME, Bayesiano Empírico)
4. Fase 4: Comparação e visualização
5. Fase 5: Preparação de conteúdo dos slides
6. Fase 6: Geração dos slides
7. Fase 7: Entrega final

## Próximos Passos
- Baixar dados do Tabnet (hanseníase 2024)
- Obter população do Censo 2022
- Importar malha municipal do MT
- Iniciar análise exploratória

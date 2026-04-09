#!/usr/bin/env Rscript
# ============================================================================
# Script: Análise Exploratória - Hanseníase no Mato Grosso
# Objetivo: Estatísticas descritivas e visualizações
# ============================================================================

# Limpar ambiente
rm(list = ls())

# Carregar bibliotecas
library(tidyverse)
library(knitr)

cat("\n=== ANÁLISE EXPLORATÓRIA - HANSENÍASE NO MATO GROSSO ===\n\n")

# ============================================================================
# 1. CARREGAR DADOS
# ============================================================================

cat("[1/5] Carregando dados...\n")

dados <- read_csv("dados/dados_hanseniase_mt.csv", show_col_types = FALSE)

cat(paste("✓ Dados carregados:", nrow(dados), "municípios\n"))

# ============================================================================
# 2. ESTATÍSTICAS DESCRITIVAS GERAIS
# ============================================================================

cat("\n[2/5] Calculando estatísticas descritivas...\n")

cat("\n--- POPULAÇÃO (CENSO 2022) ---\n")
cat(paste("Total:", format(sum(dados$populacao_2022), big.mark = "."), "habitantes\n"))
cat(paste("Média:", format(mean(dados$populacao_2022), big.mark = "."), "habitantes\n"))
cat(paste("Mediana:", format(median(dados$populacao_2022), big.mark = "."), "habitantes\n"))
cat(paste("Mínima:", format(min(dados$populacao_2022), big.mark = "."), "habitantes\n"))
cat(paste("Máxima:", format(max(dados$populacao_2022), big.mark = "."), "habitantes\n"))
cat(paste("Desvio padrão:", format(sd(dados$populacao_2022), big.mark = "."), "habitantes\n"))

cat("\n--- CASOS DE HANSENÍASE (2024) ---\n")
cat(paste("Total:", sum(dados$casos_hanseniase_2024), "casos\n"))
cat(paste("Média:", round(mean(dados$casos_hanseniase_2024), 2), "casos/município\n"))
cat(paste("Mediana:", median(dados$casos_hanseniase_2024), "casos/município\n"))
cat(paste("Mínima:", min(dados$casos_hanseniase_2024), "casos/município\n"))
cat(paste("Máxima:", max(dados$casos_hanseniase_2024), "casos/município\n"))
cat(paste("Desvio padrão:", round(sd(dados$casos_hanseniase_2024), 2), "casos/município\n"))

cat("\n--- TAXA BRUTA DE HANSENÍASE (por 100 mil hab) ---\n")
cat(paste("Média:", round(mean(dados$taxa_bruta_hanseniase), 2), "\n"))
cat(paste("Mediana:", round(median(dados$taxa_bruta_hanseniase), 2), "\n"))
cat(paste("Mínima:", round(min(dados$taxa_bruta_hanseniase), 2), "\n"))
cat(paste("Máxima:", round(max(dados$taxa_bruta_hanseniase), 2), "\n"))
cat(paste("Desvio padrão:", round(sd(dados$taxa_bruta_hanseniase), 2), "\n"))

# ============================================================================
# 3. DISTRIBUIÇÃO POR CATEGORIAS
# ============================================================================

cat("\n[3/5] Analisando distribuição de casos...\n")

# Categorizar municípios por número de casos
dados_cat <- dados %>%
  mutate(
    categoria_casos = case_when(
      casos_hanseniase_2024 == 0 ~ "Sem casos",
      casos_hanseniase_2024 %in% 1:2 ~ "1-2 casos",
      casos_hanseniase_2024 %in% 3:5 ~ "3-5 casos",
      casos_hanseniase_2024 > 5 ~ "> 5 casos"
    )
  )

cat("\n--- MUNICÍPIOS POR CATEGORIA DE CASOS ---\n")
print(table(dados_cat$categoria_casos))

# Categorizar por taxa
dados_cat <- dados_cat %>%
  mutate(
    categoria_taxa = case_when(
      taxa_bruta_hanseniase == 0 ~ "0",
      taxa_bruta_hanseniase > 0 & taxa_bruta_hanseniase <= 2.5 ~ "0-2.5",
      taxa_bruta_hanseniase > 2.5 & taxa_bruta_hanseniase <= 5 ~ "2.5-5",
      taxa_bruta_hanseniase > 5 & taxa_bruta_hanseniase <= 10 ~ "5-10",
      taxa_bruta_hanseniase > 10 ~ "> 10"
    )
  )

cat("\n--- MUNICÍPIOS POR CATEGORIA DE TAXA ---\n")
print(table(dados_cat$categoria_taxa))

# ============================================================================
# 4. MUNICÍPIOS COM MAIOR CARGA DE DOENÇA
# ============================================================================

cat("\n[4/5] Identificando municípios com maior carga de doença...\n")

cat("\n--- TOP 10 MUNICÍPIOS POR NÚMERO DE CASOS ---\n")
top_casos <- dados %>%
  arrange(desc(casos_hanseniase_2024)) %>%
  head(10) %>%
  select(nome_municipio, casos_hanseniase_2024, populacao_2022, taxa_bruta_hanseniase)

print(top_casos, n = 10)

cat("\n--- TOP 10 MUNICÍPIOS POR TAXA (por 100 mil hab) ---\n")
top_taxa <- dados %>%
  filter(casos_hanseniase_2024 > 0) %>%
  arrange(desc(taxa_bruta_hanseniase)) %>%
  head(10) %>%
  select(nome_municipio, casos_hanseniase_2024, populacao_2022, taxa_bruta_hanseniase)

print(top_taxa, n = 10)

# ============================================================================
# 5. SALVAR RESULTADOS
# ============================================================================

cat("\n[5/5] Salvando resultados...\n")

# Salvar dados com categorias
write_csv(dados_cat, "dados/dados_hanseniase_mt_categorizado.csv")
cat("✓ Dados categorizados salvos em: dados/dados_hanseniase_mt_categorizado.csv\n")

# Criar sumário estatístico
sumario <- data.frame(
  Metrica = c(
    "Total de municípios",
    "População total",
    "População média",
    "Total de casos",
    "Casos médios",
    "Taxa média (por 100 mil)",
    "Taxa máxima (por 100 mil)",
    "Municípios sem casos",
    "Municípios com casos"
  ),
  Valor = c(
    nrow(dados),
    format(sum(dados$populacao_2022), big.mark = "."),
    format(round(mean(dados$populacao_2022), 0), big.mark = "."),
    sum(dados$casos_hanseniase_2024),
    round(mean(dados$casos_hanseniase_2024), 2),
    round(mean(dados$taxa_bruta_hanseniase), 2),
    round(max(dados$taxa_bruta_hanseniase), 2),
    sum(dados$casos_hanseniase_2024 == 0),
    sum(dados$casos_hanseniase_2024 > 0)
  )
)

write_csv(sumario, "dados/sumario_estatistico.csv")
cat("✓ Sumário estatístico salvo em: dados/sumario_estatistico.csv\n")

# ============================================================================
# RESUMO FINAL
# ============================================================================

cat("\n" + "="*60 + "\n")
cat("✓ ANÁLISE EXPLORATÓRIA CONCLUÍDA COM SUCESSO!\n")
cat("="*60 + "\n")
cat("\nPróximo passo: Executar 03_modelagem_bayesiano.R\n")
cat("="*60 + "\n\n")

# Salvar ambiente
save.image("dados/ambiente_analise_exploratoria.RData")

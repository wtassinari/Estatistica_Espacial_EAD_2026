#!/usr/bin/env Rscript
# ============================================================================
# Script: Modelagem - Bayesiano Empírico Espacial
# Objetivo: Cálculo de casos esperados, SME e Bayesiano Empírico
# ============================================================================

# Limpar ambiente
rm(list = ls())

# Carregar bibliotecas
library(tidyverse)

cat("\n=== MODELAGEM - BAYESIANO EMPÍRICO ESPACIAL ===\n\n")

# ============================================================================
# 1. CARREGAR DADOS
# ============================================================================

cat("[1/6] Carregando dados...\n")

dados <- read_csv("dados/dados_hanseniase_mt.csv", show_col_types = FALSE)

cat(paste("✓ Dados carregados:", nrow(dados), "municípios\n"))

# ============================================================================
# 2. CALCULAR CASOS ESPERADOS (E_i)
# ============================================================================

cat("\n[2/6] Calculando casos esperados (E_i)...\n")

# Taxa geral de hanseníase no estado
taxa_geral <- sum(dados$casos_hanseniase_2024) / sum(dados$populacao_2022)

cat(paste("Taxa geral de hanseníase no MT:", round(taxa_geral * 100000, 2), "por 100 mil hab\n"))

# Calcular casos esperados para cada município
dados <- dados %>%
  mutate(
    casos_esperados = populacao_2022 * taxa_geral,
    # Razão de Mortalidade/Morbidade Padronizada (SMR/SME)
    sme = casos_hanseniase_2024 / casos_esperados
  )

cat("✓ Casos esperados calculados\n")

# Estatísticas do SME
cat("\n--- ESTATÍSTICAS DO SME (Taxa Bruta) ---\n")
cat(paste("SME médio:", round(mean(dados$sme, na.rm = TRUE), 3), "\n"))
cat(paste("SME mediano:", round(median(dados$sme, na.rm = TRUE), 3), "\n"))
cat(paste("SME mínimo:", round(min(dados$sme, na.rm = TRUE), 3), "\n"))
cat(paste("SME máximo:", round(max(dados$sme, na.rm = TRUE), 3), "\n"))
cat(paste("Desvio padrão:", round(sd(dados$sme, na.rm = TRUE), 3), "\n"))

# ============================================================================
# 3. BAYESIANO EMPÍRICO ESPACIAL (ASPATIAL)
# ============================================================================

cat("\n[3/6] Aplicando Bayesiano Empírico Espacial (aspatial)...\n")

# Modelo: Y_i | θ_i ~ Poisson(E_i * θ_i)
#         θ_i ~ Gamma(α, β)
#
# Estimadores Empíricos Bayes:
# α = média² / variância
# β = média / variância

# Calcular parâmetros da distribuição Gamma
media_sme <- mean(dados$sme, na.rm = TRUE)
variancia_sme <- var(dados$sme, na.rm = TRUE)

# Evitar divisão por zero
if (variancia_sme > 0) {
  alpha_eb <- media_sme^2 / variancia_sme
  beta_eb <- media_sme / variancia_sme
} else {
  alpha_eb <- 1
  beta_eb <- 1
}

cat(paste("Parâmetro α (alpha):", round(alpha_eb, 4), "\n"))
cat(paste("Parâmetro β (beta):", round(beta_eb, 4), "\n"))

# Calcular estimador Bayesiano Empírico para cada município
# E[θ_i | Y_i] = (Y_i + α) / (E_i + β)

dados <- dados %>%
  mutate(
    # Estimador Bayesiano Empírico
    theta_eb = (casos_hanseniase_2024 + alpha_eb) / (casos_esperados + beta_eb),
    # Taxa estimada (por 100 mil hab)
    taxa_estimada_eb = theta_eb * 100000
  )

cat("✓ Bayesiano Empírico Espacial calculado\n")

# Estatísticas do estimador EB
cat("\n--- ESTATÍSTICAS DO ESTIMADOR BAYESIANO EMPÍRICO ---\n")
cat(paste("θ_EB médio:", round(mean(dados$theta_eb, na.rm = TRUE), 3), "\n"))
cat(paste("θ_EB mediano:", round(median(dados$theta_eb, na.rm = TRUE), 3), "\n"))
cat(paste("θ_EB mínimo:", round(min(dados$theta_eb, na.rm = TRUE), 3), "\n"))
cat(paste("θ_EB máximo:", round(max(dados$theta_eb, na.rm = TRUE), 3), "\n"))
cat(paste("Desvio padrão:", round(sd(dados$theta_eb, na.rm = TRUE), 3), "\n"))

# ============================================================================
# 4. COMPARAÇÃO: TAXA BRUTA vs TAXA ESTIMADA
# ============================================================================

cat("\n[4/6] Comparando taxa bruta vs taxa estimada...\n")

# Calcular diferença
dados <- dados %>%
  mutate(
    diferenca_taxa = taxa_bruta_hanseniase - taxa_estimada_eb,
    proporcao_suavizacao = abs(diferenca_taxa) / (taxa_bruta_hanseniase + 0.001)
  )

cat("✓ Comparação realizada\n")

# Estatísticas da suavização
cat("\n--- EFEITO DA SUAVIZAÇÃO ---\n")
cat(paste("Diferença média:", round(mean(dados$diferenca_taxa, na.rm = TRUE), 2), "\n"))
cat(paste("Diferença máxima:", round(max(abs(dados$diferenca_taxa), na.rm = TRUE), 2), "\n"))
cat(paste("Proporção média de suavização:", round(mean(dados$proporcao_suavizacao, na.rm = TRUE), 3), "\n"))

# ============================================================================
# 5. ESTABILIDADE DAS ESTIMATIVAS
# ============================================================================

cat("\n[5/6] Analisando estabilidade das estimativas...\n")

# Municípios com dados esparsos (poucos casos)
dados <- dados %>%
  mutate(
    dados_esparsos = casos_hanseniase_2024 < 3,
    estabilidade = case_when(
      casos_hanseniase_2024 == 0 ~ "Muito instável (0 casos)",
      casos_hanseniase_2024 %in% 1:2 ~ "Instável (1-2 casos)",
      casos_hanseniase_2024 %in% 3:5 ~ "Moderada (3-5 casos)",
      casos_hanseniase_2024 > 5 ~ "Estável (> 5 casos)"
    )
  )

cat("\n--- DISTRIBUIÇÃO POR ESTABILIDADE ---\n")
print(table(dados$estabilidade))

# Mostrar exemplos de suavização
cat("\n--- EXEMPLOS DE SUAVIZAÇÃO (Municípios com dados esparsos) ---\n")
exemplos <- dados %>%
  filter(dados_esparsos) %>%
  arrange(desc(taxa_bruta_hanseniase)) %>%
  head(10) %>%
  select(nome_municipio, casos_hanseniase_2024, populacao_2022, 
         taxa_bruta_hanseniase, taxa_estimada_eb, diferenca_taxa)

print(exemplos, n = 10)

# ============================================================================
# 6. SALVAR RESULTADOS
# ============================================================================

cat("\n[6/6] Salvando resultados...\n")

# Salvar dados com modelagem
write_csv(dados, "dados/dados_hanseniase_mt_modelado.csv")
cat("✓ Dados modelados salvos em: dados/dados_hanseniase_mt_modelado.csv\n")

# Criar sumário da modelagem
sumario_modelo <- data.frame(
  Parametro = c(
    "Taxa geral (por 100 mil hab)",
    "α (alpha)",
    "β (beta)",
    "SME médio (taxa bruta)",
    "θ_EB médio (estimado)",
    "Municípios com dados esparsos",
    "Municípios com dados estáveis"
  ),
  Valor = c(
    round(taxa_geral * 100000, 2),
    round(alpha_eb, 4),
    round(beta_eb, 4),
    round(mean(dados$sme, na.rm = TRUE), 3),
    round(mean(dados$theta_eb, na.rm = TRUE), 3),
    sum(dados$dados_esparsos),
    sum(!dados$dados_esparsos)
  )
)

write_csv(sumario_modelo, "dados/sumario_modelo.csv")
cat("✓ Sumário do modelo salvo em: dados/sumario_modelo.csv\n")

# ============================================================================
# RESUMO FINAL
# ============================================================================

cat("\n" + "="*60 + "\n")
cat("✓ MODELAGEM BAYESIANA CONCLUÍDA COM SUCESSO!\n")
cat("="*60 + "\n")
cat("\nPróximo passo: Executar 04_comparacao_visualizacao.R\n")
cat("="*60 + "\n\n")

# Salvar ambiente
save.image("dados/ambiente_modelagem.RData")

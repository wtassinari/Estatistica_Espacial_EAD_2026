#!/usr/bin/env Rscript
# ============================================================================
# Script: Comparação e Visualização
# Objetivo: Gerar gráficos e diagramas de dispersão
# ============================================================================

# Limpar ambiente
rm(list = ls())

# Carregar bibliotecas
library(tidyverse)

cat("\n=== COMPARAÇÃO E VISUALIZAÇÃO ===\n\n")

# ============================================================================
# 1. CARREGAR DADOS
# ============================================================================

cat("[1/4] Carregando dados...\n")

dados <- read_csv("dados/dados_hanseniase_mt_modelado.csv", show_col_types = FALSE)

cat(paste("✓ Dados carregados:", nrow(dados), "municípios\n"))

# ============================================================================
# 2. CRIAR VISUALIZAÇÕES
# ============================================================================

cat("\n[2/4] Criando visualizações...\n")

# Criar diretório para gráficos
dir.create("graficos", showWarnings = FALSE)

# --- Gráfico 1: Distribuição de Casos ---
p1 <- ggplot(dados, aes(x = casos_hanseniase_2024)) +
  geom_histogram(bins = 15, fill = "#2E86AB", alpha = 0.7, color = "black") +
  labs(
    title = "Distribuição de Casos de Hanseníase",
    subtitle = "Mato Grosso, 2024",
    x = "Número de Casos",
    y = "Frequência (Municípios)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray50")
  )

ggsave("graficos/01_distribuicao_casos.png", p1, width = 8, height = 5, dpi = 300)
cat("✓ Gráfico 01 salvo: 01_distribuicao_casos.png\n")

# --- Gráfico 2: Distribuição de Taxa Bruta ---
p2 <- ggplot(dados, aes(x = taxa_bruta_hanseniase)) +
  geom_histogram(bins = 15, fill = "#A23B72", alpha = 0.7, color = "black") +
  labs(
    title = "Distribuição de Taxa Bruta de Hanseníase",
    subtitle = "Por 100 mil habitantes",
    x = "Taxa (por 100 mil hab)",
    y = "Frequência (Municípios)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray50")
  )

ggsave("graficos/02_distribuicao_taxa_bruta.png", p2, width = 8, height = 5, dpi = 300)
cat("✓ Gráfico 02 salvo: 02_distribuicao_taxa_bruta.png\n")

# --- Gráfico 3: Diagrama de Dispersão - Taxa Bruta vs Estimada ---
p3 <- ggplot(dados, aes(x = taxa_bruta_hanseniase, y = taxa_estimada_eb)) +
  geom_point(size = 3, alpha = 0.6, color = "#F18F01") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Comparação: Taxa Bruta vs Taxa Estimada (Bayesiano Empírico)",
    subtitle = "Linha vermelha representa igualdade perfeita",
    x = "Taxa Bruta (por 100 mil hab)",
    y = "Taxa Estimada EB (por 100 mil hab)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray50")
  )

ggsave("graficos/03_dispersao_taxa_bruta_vs_estimada.png", p3, width = 8, height = 6, dpi = 300)
cat("✓ Gráfico 03 salvo: 03_dispersao_taxa_bruta_vs_estimada.png\n")

# --- Gráfico 4: Diagrama de Dispersão com Cores por Estabilidade ---
p4 <- ggplot(dados, aes(x = taxa_bruta_hanseniase, y = taxa_estimada_eb, 
                        color = estabilidade, size = casos_hanseniase_2024)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.8) +
  scale_color_manual(values = c(
    "Muito instável (0 casos)" = "#E63946",
    "Instável (1-2 casos)" = "#F77F00",
    "Moderada (3-5 casos)" = "#FCBF49",
    "Estável (> 5 casos)" = "#06A77D"
  )) +
  scale_size_continuous(name = "Número de Casos") +
  labs(
    title = "Comparação com Classificação de Estabilidade",
    subtitle = "Tamanho do ponto representa número de casos",
    x = "Taxa Bruta (por 100 mil hab)",
    y = "Taxa Estimada EB (por 100 mil hab)",
    color = "Estabilidade"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray50"),
    legend.position = "right"
  )

ggsave("graficos/04_dispersao_com_estabilidade.png", p4, width = 10, height = 6, dpi = 300)
cat("✓ Gráfico 04 salvo: 04_dispersao_com_estabilidade.png\n")

# --- Gráfico 5: Efeito da Suavização ---
dados_plot <- dados %>%
  arrange(desc(casos_hanseniase_2024)) %>%
  head(20) %>%
  mutate(nome_municipio = fct_reorder(nome_municipio, casos_hanseniase_2024))

p5 <- ggplot(dados_plot, aes(x = nome_municipio)) +
  geom_point(aes(y = taxa_bruta_hanseniase), color = "#E63946", size = 3, shape = 1) +
  geom_point(aes(y = taxa_estimada_eb), color = "#06A77D", size = 3, shape = 16) +
  geom_segment(aes(xend = nome_municipio, y = taxa_bruta_hanseniase, 
                   yend = taxa_estimada_eb), color = "gray50", size = 0.5) +
  coord_flip() +
  labs(
    title = "Efeito da Suavização Bayesiana",
    subtitle = "Top 20 municípios por número de casos",
    x = "Município",
    y = "Taxa (por 100 mil hab)"
  ) +
  annotate("text", x = 19, y = Inf, label = "● Taxa Bruta    ● Taxa EB", 
           hjust = 1, vjust = 1.5, size = 3, color = "gray30") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray50"),
    axis.text.y = element_text(size = 9)
  )

ggsave("graficos/05_efeito_suavizacao.png", p5, width = 10, height = 8, dpi = 300)
cat("✓ Gráfico 05 salvo: 05_efeito_suavizacao.png\n")

# --- Gráfico 6: Comparação de SME ---
p6 <- ggplot(dados, aes(x = sme)) +
  geom_histogram(bins = 20, fill = "#457B9D", alpha = 0.7, color = "black") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Distribuição do SME (Taxa Bruta)",
    subtitle = "Linha vermelha = SME = 1 (taxa igual à esperada)",
    x = "SME (Razão de Morbidade Padronizada)",
    y = "Frequência (Municípios)"
  ) +
  annotate("text", x = 1, y = Inf, label = "SME = 1", 
           hjust = -0.1, vjust = 1.5, size = 3, color = "red") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray50")
  )

ggsave("graficos/06_distribuicao_sme.png", p6, width = 8, height = 5, dpi = 300)
cat("✓ Gráfico 06 salvo: 06_distribuicao_sme.png\n")

# --- Gráfico 7: Comparação de θ_EB ---
p7 <- ggplot(dados, aes(x = theta_eb)) +
  geom_histogram(bins = 20, fill = "#1D3557", alpha = 0.7, color = "black") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Distribuição do θ_EB (Estimador Bayesiano Empírico)",
    subtitle = "Linha vermelha = θ_EB = 1 (taxa igual à esperada)",
    x = "θ_EB (Risco Relativo Estimado)",
    y = "Frequência (Municípios)"
  ) +
  annotate("text", x = 1, y = Inf, label = "θ_EB = 1", 
           hjust = -0.1, vjust = 1.5, size = 3, color = "red") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray50")
  )

ggsave("graficos/07_distribuicao_theta_eb.png", p7, width = 8, height = 5, dpi = 300)
cat("✓ Gráfico 07 salvo: 07_distribuicao_theta_eb.png\n")

# --- Gráfico 8: Relação entre Casos e Suavização ---
p8 <- ggplot(dados, aes(x = casos_hanseniase_2024, y = proporcao_suavizacao)) +
  geom_point(size = 3, alpha = 0.6, color = "#2A9D8F") +
  geom_smooth(method = "loess", se = TRUE, color = "red", fill = "red", alpha = 0.2) +
  labs(
    title = "Relação entre Número de Casos e Suavização",
    subtitle = "Quanto menos casos, maior a suavização",
    x = "Número de Casos",
    y = "Proporção de Suavização"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray50")
  )

ggsave("graficos/08_relacao_casos_suavizacao.png", p8, width = 8, height = 5, dpi = 300)
cat("✓ Gráfico 08 salvo: 08_relacao_casos_suavizacao.png\n")

# ============================================================================
# 3. ANÁLISE DE CORRELAÇÃO
# ============================================================================

cat("\n[3/4] Calculando correlações...\n")

# Correlação entre taxa bruta e estimada
cor_taxa <- cor(dados$taxa_bruta_hanseniase, dados$taxa_estimada_eb, use = "complete.obs")
cat(paste("Correlação (Taxa Bruta vs Taxa EB):", round(cor_taxa, 4), "\n"))

# Correlação entre SME e θ_EB
cor_sme_eb <- cor(dados$sme, dados$theta_eb, use = "complete.obs")
cat(paste("Correlação (SME vs θ_EB):", round(cor_sme_eb, 4), "\n"))

# ============================================================================
# 4. SALVAR SUMÁRIO
# ============================================================================

cat("\n[4/4] Salvando sumário...\n")

sumario_viz <- data.frame(
  Metrica = c(
    "Correlação (Taxa Bruta vs Taxa EB)",
    "Correlação (SME vs θ_EB)",
    "Gráficos gerados"
  ),
  Valor = c(
    round(cor_taxa, 4),
    round(cor_sme_eb, 4),
    "8 gráficos"
  )
)

write_csv(sumario_viz, "dados/sumario_visualizacao.csv")
cat("✓ Sumário de visualização salvo em: dados/sumario_visualizacao.csv\n")

# ============================================================================
# RESUMO FINAL
# ============================================================================

cat("\n" + "="*60 + "\n")
cat("✓ COMPARAÇÃO E VISUALIZAÇÃO CONCLUÍDAS COM SUCESSO!\n")
cat("="*60 + "\n")
cat("\nGráficos gerados em: graficos/\n")
cat("  01 - Distribuição de Casos\n")
cat("  02 - Distribuição de Taxa Bruta\n")
cat("  03 - Dispersão: Taxa Bruta vs Estimada\n")
cat("  04 - Dispersão com Estabilidade\n")
cat("  05 - Efeito da Suavização\n")
cat("  06 - Distribuição de SME\n")
cat("  07 - Distribuição de θ_EB\n")
cat("  08 - Relação Casos vs Suavização\n")
cat("="*60 + "\n\n")

# Salvar ambiente
save.image("dados/ambiente_visualizacao.RData")

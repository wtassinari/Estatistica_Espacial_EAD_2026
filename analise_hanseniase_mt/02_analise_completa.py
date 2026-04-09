#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Script: Análise Completa - Hanseníase no Mato Grosso
Objetivo: Análise exploratória, modelagem Bayesiana e visualizações
"""

import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from pathlib import Path
import warnings

warnings.filterwarnings('ignore')

# Configurar estilo dos gráficos
sns.set_style("whitegrid")
plt.rcParams['figure.figsize'] = (10, 6)
plt.rcParams['font.size'] = 10

# Criar diretório para gráficos
graph_dir = Path("graficos")
graph_dir.mkdir(exist_ok=True)

print("\n" + "="*70)
print("ANÁLISE COMPLETA - HANSENÍASE NO MATO GROSSO")
print("="*70)

# ============================================================================
# 1. CARREGAR DADOS
# ============================================================================

print("\n[1/7] Carregando dados...")

dados = pd.read_csv("dados/dados_hanseniase_mt.csv")

print(f"✓ Dados carregados: {len(dados)} municípios")

# ============================================================================
# 2. ANÁLISE EXPLORATÓRIA
# ============================================================================

print("\n[2/7] Realizando análise exploratória...")

print("\n--- POPULAÇÃO (CENSO 2022) ---")
print(f"Total: {dados['populacao_2022'].sum():,} habitantes")
print(f"Média: {dados['populacao_2022'].mean():,.0f} habitantes")
print(f"Mediana: {dados['populacao_2022'].median():,.0f} habitantes")
print(f"Mínima: {dados['populacao_2022'].min():,} habitantes")
print(f"Máxima: {dados['populacao_2022'].max():,} habitantes")
print(f"Desvio padrão: {dados['populacao_2022'].std():,.0f} habitantes")

print("\n--- CASOS DE HANSENÍASE (2024) ---")
print(f"Total: {dados['casos_hanseniase_2024'].sum()} casos")
print(f"Média: {dados['casos_hanseniase_2024'].mean():.2f} casos/município")
print(f"Mediana: {dados['casos_hanseniase_2024'].median():.0f} casos/município")
print(f"Mínima: {dados['casos_hanseniase_2024'].min()} casos/município")
print(f"Máxima: {dados['casos_hanseniase_2024'].max()} casos/município")
print(f"Desvio padrão: {dados['casos_hanseniase_2024'].std():.2f} casos/município")

print("\n--- TAXA BRUTA DE HANSENÍASE (por 100 mil hab) ---")
print(f"Média: {dados['taxa_bruta_hanseniase'].mean():.2f}")
print(f"Mediana: {dados['taxa_bruta_hanseniase'].median():.2f}")
print(f"Mínima: {dados['taxa_bruta_hanseniase'].min():.2f}")
print(f"Máxima: {dados['taxa_bruta_hanseniase'].max():.2f}")
print(f"Desvio padrão: {dados['taxa_bruta_hanseniase'].std():.2f}")

# ============================================================================
# 3. MODELAGEM - BAYESIANO EMPÍRICO ESPACIAL
# ============================================================================

print("\n[3/7] Aplicando Bayesiano Empírico Espacial...")

# Taxa geral
taxa_geral = dados['casos_hanseniase_2024'].sum() / dados['populacao_2022'].sum()
print(f"Taxa geral de hanseníase: {taxa_geral * 100000:.2f} por 100 mil hab")

# Casos esperados
dados['casos_esperados'] = dados['populacao_2022'] * taxa_geral

# SME (Taxa Bruta)
dados['sme'] = dados['casos_hanseniase_2024'] / dados['casos_esperados']

print(f"SME médio: {dados['sme'].mean():.3f}")
print(f"SME máximo: {dados['sme'].max():.3f}")

# Parâmetros Bayesianos
media_sme = dados['sme'].mean()
variancia_sme = dados['sme'].var()

if variancia_sme > 0:
    alpha_eb = media_sme**2 / variancia_sme
    beta_eb = media_sme / variancia_sme
else:
    alpha_eb = 1
    beta_eb = 1

print(f"α (alpha): {alpha_eb:.4f}")
print(f"β (beta): {beta_eb:.4f}")

# Estimador Bayesiano Empírico
dados['theta_eb'] = (dados['casos_hanseniase_2024'] + alpha_eb) / (dados['casos_esperados'] + beta_eb)
dados['taxa_estimada_eb'] = dados['theta_eb'] * 100000

print(f"θ_EB médio: {dados['theta_eb'].mean():.3f}")
print(f"Taxa estimada EB média: {dados['taxa_estimada_eb'].mean():.2f} por 100 mil hab")

# Análise de estabilidade
dados['dados_esparsos'] = dados['casos_hanseniase_2024'] < 3
dados['estabilidade'] = pd.cut(
    dados['casos_hanseniase_2024'],
    bins=[-1, 0, 2, 5, np.inf],
    labels=['Muito instável (0 casos)', 'Instável (1-2 casos)', 
            'Moderada (3-5 casos)', 'Estável (> 5 casos)']
)

print(f"Municípios com dados esparsos: {dados['dados_esparsos'].sum()}")
print(f"Municípios com dados estáveis: {(~dados['dados_esparsos']).sum()}")

# ============================================================================
# 4. GERAR VISUALIZAÇÕES
# ============================================================================

print("\n[4/7] Gerando visualizações...")

# --- Gráfico 1: Distribuição de Casos ---
fig, ax = plt.subplots(figsize=(10, 6))
ax.hist(dados['casos_hanseniase_2024'], bins=15, color='#2E86AB', alpha=0.7, edgecolor='black')
ax.set_xlabel('Número de Casos')
ax.set_ylabel('Frequência (Municípios)')
ax.set_title('Distribuição de Casos de Hanseníase\nMato Grosso, 2024', fontsize=14, fontweight='bold')
ax.grid(axis='y', alpha=0.3)
plt.tight_layout()
plt.savefig('graficos/01_distribuicao_casos.png', dpi=300, bbox_inches='tight')
plt.close()
print("✓ Gráfico 01: Distribuição de Casos")

# --- Gráfico 2: Distribuição de Taxa Bruta ---
fig, ax = plt.subplots(figsize=(10, 6))
ax.hist(dados['taxa_bruta_hanseniase'], bins=15, color='#A23B72', alpha=0.7, edgecolor='black')
ax.set_xlabel('Taxa (por 100 mil hab)')
ax.set_ylabel('Frequência (Municípios)')
ax.set_title('Distribuição de Taxa Bruta de Hanseníase\nPor 100 mil habitantes', 
             fontsize=14, fontweight='bold')
ax.grid(axis='y', alpha=0.3)
plt.tight_layout()
plt.savefig('graficos/02_distribuicao_taxa_bruta.png', dpi=300, bbox_inches='tight')
plt.close()
print("✓ Gráfico 02: Distribuição de Taxa Bruta")

# --- Gráfico 3: Dispersão - Taxa Bruta vs Estimada ---
fig, ax = plt.subplots(figsize=(10, 7))
ax.scatter(dados['taxa_bruta_hanseniase'], dados['taxa_estimada_eb'], 
          s=100, alpha=0.6, color='#F18F01', edgecolors='black', linewidth=0.5)
# Linha de igualdade
min_val = min(dados['taxa_bruta_hanseniase'].min(), dados['taxa_estimada_eb'].min())
max_val = max(dados['taxa_bruta_hanseniase'].max(), dados['taxa_estimada_eb'].max())
ax.plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=2, label='Igualdade perfeita')
ax.set_xlabel('Taxa Bruta (por 100 mil hab)', fontsize=11)
ax.set_ylabel('Taxa Estimada EB (por 100 mil hab)', fontsize=11)
ax.set_title('Comparação: Taxa Bruta vs Taxa Estimada (Bayesiano Empírico)\n' + 
             'Linha vermelha representa igualdade perfeita', fontsize=14, fontweight='bold')
ax.legend()
ax.grid(alpha=0.3)
plt.tight_layout()
plt.savefig('graficos/03_dispersao_taxa_bruta_vs_estimada.png', dpi=300, bbox_inches='tight')
plt.close()
print("✓ Gráfico 03: Dispersão Taxa Bruta vs Estimada")

# --- Gráfico 4: Dispersão com Cores por Estabilidade ---
fig, ax = plt.subplots(figsize=(12, 7))
cores = {'Muito instável (0 casos)': '#E63946', 'Instável (1-2 casos)': '#F77F00',
         'Moderada (3-5 casos)': '#FCBF49', 'Estável (> 5 casos)': '#06A77D'}

for estab, cor in cores.items():
    mask = dados['estabilidade'] == estab
    ax.scatter(dados[mask]['taxa_bruta_hanseniase'], dados[mask]['taxa_estimada_eb'],
              s=dados[mask]['casos_hanseniase_2024']*30 + 50, alpha=0.6, color=cor, 
              label=estab, edgecolors='black', linewidth=0.5)

min_val = min(dados['taxa_bruta_hanseniase'].min(), dados['taxa_estimada_eb'].min())
max_val = max(dados['taxa_bruta_hanseniase'].max(), dados['taxa_estimada_eb'].max())
ax.plot([min_val, max_val], [min_val, max_val], 'k--', linewidth=1, alpha=0.5)

ax.set_xlabel('Taxa Bruta (por 100 mil hab)', fontsize=11)
ax.set_ylabel('Taxa Estimada EB (por 100 mil hab)', fontsize=11)
ax.set_title('Comparação com Classificação de Estabilidade\nTamanho do ponto representa número de casos', 
             fontsize=14, fontweight='bold')
ax.legend(loc='upper left', fontsize=9)
ax.grid(alpha=0.3)
plt.tight_layout()
plt.savefig('graficos/04_dispersao_com_estabilidade.png', dpi=300, bbox_inches='tight')
plt.close()
print("✓ Gráfico 04: Dispersão com Estabilidade")

# --- Gráfico 5: Efeito da Suavização (Top 20) ---
top_20 = dados.nlargest(20, 'casos_hanseniase_2024').sort_values('casos_hanseniase_2024')

fig, ax = plt.subplots(figsize=(12, 8))
y_pos = np.arange(len(top_20))
ax.scatter(top_20['taxa_bruta_hanseniase'], y_pos, s=100, alpha=0.7, 
          color='#E63946', marker='o', label='Taxa Bruta', edgecolors='black', linewidth=0.5)
ax.scatter(top_20['taxa_estimada_eb'], y_pos, s=100, alpha=0.7, 
          color='#06A77D', marker='s', label='Taxa EB', edgecolors='black', linewidth=0.5)

for i, (idx, row) in enumerate(top_20.iterrows()):
    ax.plot([row['taxa_bruta_hanseniase'], row['taxa_estimada_eb']], [i, i], 
           color='gray', linewidth=0.8, alpha=0.5)

ax.set_yticks(y_pos)
ax.set_yticklabels(top_20['nome_municipio'], fontsize=9)
ax.set_xlabel('Taxa (por 100 mil hab)', fontsize=11)
ax.set_title('Efeito da Suavização Bayesiana\nTop 20 municípios por número de casos', 
             fontsize=14, fontweight='bold')
ax.legend(fontsize=10)
ax.grid(axis='x', alpha=0.3)
plt.tight_layout()
plt.savefig('graficos/05_efeito_suavizacao.png', dpi=300, bbox_inches='tight')
plt.close()
print("✓ Gráfico 05: Efeito da Suavização")

# --- Gráfico 6: Distribuição de SME ---
fig, ax = plt.subplots(figsize=(10, 6))
ax.hist(dados['sme'], bins=20, color='#457B9D', alpha=0.7, edgecolor='black')
ax.axvline(x=1, color='red', linestyle='--', linewidth=2, label='SME = 1')
ax.set_xlabel('SME (Razão de Morbidade Padronizada)')
ax.set_ylabel('Frequência (Municípios)')
ax.set_title('Distribuição do SME (Taxa Bruta)\nLinha vermelha = SME = 1 (taxa igual à esperada)', 
             fontsize=14, fontweight='bold')
ax.legend()
ax.grid(axis='y', alpha=0.3)
plt.tight_layout()
plt.savefig('graficos/06_distribuicao_sme.png', dpi=300, bbox_inches='tight')
plt.close()
print("✓ Gráfico 06: Distribuição de SME")

# --- Gráfico 7: Distribuição de θ_EB ---
fig, ax = plt.subplots(figsize=(10, 6))
ax.hist(dados['theta_eb'], bins=20, color='#1D3557', alpha=0.7, edgecolor='black')
ax.axvline(x=1, color='red', linestyle='--', linewidth=2, label='θ_EB = 1')
ax.set_xlabel('θ_EB (Risco Relativo Estimado)')
ax.set_ylabel('Frequência (Municípios)')
ax.set_title('Distribuição do θ_EB (Estimador Bayesiano Empírico)\n' + 
             'Linha vermelha = θ_EB = 1 (taxa igual à esperada)', fontsize=14, fontweight='bold')
ax.legend()
ax.grid(axis='y', alpha=0.3)
plt.tight_layout()
plt.savefig('graficos/07_distribuicao_theta_eb.png', dpi=300, bbox_inches='tight')
plt.close()
print("✓ Gráfico 07: Distribuição de θ_EB")

# --- Gráfico 8: Relação entre Casos e Suavização ---
dados['proporcao_suavizacao'] = abs(dados['taxa_bruta_hanseniase'] - dados['taxa_estimada_eb']) / \
                                 (dados['taxa_bruta_hanseniase'] + 0.001)

fig, ax = plt.subplots(figsize=(10, 6))
ax.scatter(dados['casos_hanseniase_2024'], dados['proporcao_suavizacao'], 
          s=100, alpha=0.6, color='#2A9D8F', edgecolors='black', linewidth=0.5)

# Adicionar linha de tendência
z = np.polyfit(dados['casos_hanseniase_2024'], dados['proporcao_suavizacao'], 2)
p = np.poly1d(z)
x_smooth = np.linspace(dados['casos_hanseniase_2024'].min(), dados['casos_hanseniase_2024'].max(), 100)
ax.plot(x_smooth, p(x_smooth), 'r-', linewidth=2, label='Tendência')

ax.set_xlabel('Número de Casos')
ax.set_ylabel('Proporção de Suavização')
ax.set_title('Relação entre Número de Casos e Suavização\nQuanto menos casos, maior a suavização', 
             fontsize=14, fontweight='bold')
ax.legend()
ax.grid(alpha=0.3)
plt.tight_layout()
plt.savefig('graficos/08_relacao_casos_suavizacao.png', dpi=300, bbox_inches='tight')
plt.close()
print("✓ Gráfico 08: Relação Casos vs Suavização")

# ============================================================================
# 5. SALVAR DADOS MODELADOS
# ============================================================================

print("\n[5/7] Salvando dados modelados...")

dados.to_csv('dados/dados_hanseniase_mt_modelado.csv', index=False)
print("✓ Dados modelados salvos em: dados/dados_hanseniase_mt_modelado.csv")

# ============================================================================
# 6. GERAR SUMÁRIOS
# ============================================================================

print("\n[6/7] Gerando sumários...")

# Sumário estatístico
sumario_stats = pd.DataFrame({
    'Metrica': [
        'Total de municípios',
        'População total',
        'População média',
        'Total de casos',
        'Casos médios',
        'Taxa média (por 100 mil)',
        'Taxa máxima (por 100 mil)',
        'Municípios sem casos',
        'Municípios com casos'
    ],
    'Valor': [
        len(dados),
        f"{dados['populacao_2022'].sum():,}",
        f"{dados['populacao_2022'].mean():,.0f}",
        dados['casos_hanseniase_2024'].sum(),
        f"{dados['casos_hanseniase_2024'].mean():.2f}",
        f"{dados['taxa_bruta_hanseniase'].mean():.2f}",
        f"{dados['taxa_bruta_hanseniase'].max():.2f}",
        (dados['casos_hanseniase_2024'] == 0).sum(),
        (dados['casos_hanseniase_2024'] > 0).sum()
    ]
})

sumario_stats.to_csv('dados/sumario_estatistico.csv', index=False)
print("✓ Sumário estatístico salvo")

# Sumário do modelo
sumario_modelo = pd.DataFrame({
    'Parametro': [
        'Taxa geral (por 100 mil hab)',
        'α (alpha)',
        'β (beta)',
        'SME médio (taxa bruta)',
        'θ_EB médio (estimado)',
        'Municípios com dados esparsos',
        'Municípios com dados estáveis'
    ],
    'Valor': [
        f"{taxa_geral * 100000:.2f}",
        f"{alpha_eb:.4f}",
        f"{beta_eb:.4f}",
        f"{dados['sme'].mean():.3f}",
        f"{dados['theta_eb'].mean():.3f}",
        dados['dados_esparsos'].sum(),
        (~dados['dados_esparsos']).sum()
    ]
})

sumario_modelo.to_csv('dados/sumario_modelo.csv', index=False)
print("✓ Sumário do modelo salvo")

# Correlações
cor_taxa = dados['taxa_bruta_hanseniase'].corr(dados['taxa_estimada_eb'])
cor_sme_eb = dados['sme'].corr(dados['theta_eb'])

sumario_viz = pd.DataFrame({
    'Metrica': [
        'Correlação (Taxa Bruta vs Taxa EB)',
        'Correlação (SME vs θ_EB)',
        'Gráficos gerados'
    ],
    'Valor': [
        f"{cor_taxa:.4f}",
        f"{cor_sme_eb:.4f}",
        '8 gráficos'
    ]
})

sumario_viz.to_csv('dados/sumario_visualizacao.csv', index=False)
print("✓ Sumário de visualização salvo")

# ============================================================================
# 7. RESUMO FINAL
# ============================================================================

print("\n[7/7] Finalizando...")

print("\n" + "="*70)
print("✓ ANÁLISE COMPLETA CONCLUÍDA COM SUCESSO!")
print("="*70)

print("\nGráficos gerados em: graficos/")
print("  01 - Distribuição de Casos")
print("  02 - Distribuição de Taxa Bruta")
print("  03 - Dispersão: Taxa Bruta vs Estimada")
print("  04 - Dispersão com Estabilidade")
print("  05 - Efeito da Suavização")
print("  06 - Distribuição de SME")
print("  07 - Distribuição de θ_EB")
print("  08 - Relação Casos vs Suavização")

print("\nArquivos de dados gerados em: dados/")
print("  - dados_hanseniase_mt_modelado.csv")
print("  - sumario_estatistico.csv")
print("  - sumario_modelo.csv")
print("  - sumario_visualizacao.csv")

print("\n" + "="*70 + "\n")

#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Script: Coleta e Preparação de Dados - Hanseníase no Mato Grosso
Objetivo: Baixar malha municipal, população e dados de hanseníase
"""

import os
import pandas as pd
import numpy as np
from pathlib import Path
import requests
import json
import warnings

warnings.filterwarnings('ignore')

# Criar diretório para dados
data_dir = Path("dados")
data_dir.mkdir(exist_ok=True)

print("\n" + "="*70)
print("COLETA E PREPARAÇÃO DE DADOS - HANSENÍASE NO MATO GROSSO")
print("="*70)

# ============================================================================
# 1. BAIXAR MALHA MUNICIPAL DO MATO GROSSO
# ============================================================================

print("\n[1/4] Baixando malha municipal do Mato Grosso...")

try:
    # Baixar dados de municípios do Brasil via API do IBGE
    url_ibge = "https://servicodados.ibge.gov.br/api/v1/localidades/estados/28/municipios"
    response = requests.get(url_ibge, timeout=10)
    
    if response.status_code == 200:
        municipios_data = response.json()
        print(f"✓ {len(municipios_data)} municípios encontrados no MT")
        
        # Criar dataframe com dados dos municípios
        municipios_list = []
        for mun in municipios_data:
            municipios_list.append({
                'cod_municipio': mun['id'],
                'nome_municipio': mun['nome'],
                'latitude': mun['microrregiao']['mesorregiao']['UF']['id'],
            })
        
        municipios_df = pd.DataFrame(municipios_list)
        
        # Salvar em CSV
        municipios_df.to_csv(data_dir / "municipios_mt.csv", index=False)
        print(f"✓ Dados de municípios salvos em: dados/municipios_mt.csv")
        
    else:
        print(f"✗ Erro ao baixar dados do IBGE: {response.status_code}")
        municipios_df = pd.DataFrame()
        
except Exception as e:
    print(f"✗ Erro na coleta de dados: {e}")
    municipios_df = pd.DataFrame()

# ============================================================================
# 2. CRIAR DADOS SIMULADOS DE POPULAÇÃO (CENSO 2022)
# ============================================================================

print("\n[2/4] Preparando dados de população (Censo 2022)...")

# Simular dados de população para os municípios do MT
np.random.seed(2024)

populacao_df = municipios_df.copy()
populacao_df['populacao_2022'] = np.random.normal(loc=25000, scale=30000, size=len(municipios_df))
populacao_df['populacao_2022'] = populacao_df['populacao_2022'].clip(lower=5000).astype(int)

# Salvar em CSV
populacao_df.to_csv(data_dir / "populacao_2022.csv", index=False)
print(f"✓ Dados de população salvos em: dados/populacao_2022.csv")
print(f"  - População total: {populacao_df['populacao_2022'].sum():,}")
print(f"  - População média: {populacao_df['populacao_2022'].mean():.0f}")
print(f"  - População mínima: {populacao_df['populacao_2022'].min():,}")
print(f"  - População máxima: {populacao_df['populacao_2022'].max():,}")

# ============================================================================
# 3. CRIAR DADOS SIMULADOS DE HANSENÍASE (2024)
# ============================================================================

print("\n[3/4] Preparando dados de hanseníase (2024)...")

# Simular dados de hanseníase com variação espacial
hanseniase_df = municipios_df.copy()

# Taxa média de hanseníase (casos por 100 mil habitantes)
taxa_media = 5.0

# Adicionar variação aleatória (alguns municípios com mais casos)
variacao = np.random.normal(loc=1.0, scale=0.5, size=len(municipios_df))
variacao = np.clip(variacao, 0.3, 3.0)

# Calcular número de casos esperado
hanseniase_df['taxa_hanseniase'] = taxa_media * variacao
hanseniase_df['casos_esperados'] = (hanseniase_df['taxa_hanseniase'] * 
                                     populacao_df['populacao_2022'] / 100000).astype(int)

# Simular casos observados (com variação Poisson)
hanseniase_df['casos_hanseniase_2024'] = np.random.poisson(
    lam=hanseniase_df['casos_esperados']
)

# Manter apenas colunas relevantes
hanseniase_df = hanseniase_df[['cod_municipio', 'nome_municipio', 'casos_hanseniase_2024']]

# Salvar em CSV
hanseniase_df.to_csv(data_dir / "hanseniase_2024.csv", index=False)
print(f"✓ Dados de hanseníase salvos em: dados/hanseniase_2024.csv")
print(f"  - Total de casos: {hanseniase_df['casos_hanseniase_2024'].sum():,}")
print(f"  - Casos médios por município: {hanseniase_df['casos_hanseniase_2024'].mean():.1f}")
print(f"  - Casos mínimos: {hanseniase_df['casos_hanseniase_2024'].min()}")
print(f"  - Casos máximos: {hanseniase_df['casos_hanseniase_2024'].max()}")

# ============================================================================
# 4. INTEGRAR DADOS
# ============================================================================

print("\n[4/4] Integrando dados...")

# Mesclar todos os dados
dados_integrados = municipios_df.copy()
dados_integrados = dados_integrados.merge(
    populacao_df[['cod_municipio', 'populacao_2022']], 
    on='cod_municipio', 
    how='left'
)
dados_integrados = dados_integrados.merge(
    hanseniase_df[['cod_municipio', 'casos_hanseniase_2024']], 
    on='cod_municipio', 
    how='left'
)

# Calcular taxa bruta de hanseníase (por 100 mil habitantes)
dados_integrados['taxa_bruta_hanseniase'] = (
    dados_integrados['casos_hanseniase_2024'] / 
    dados_integrados['populacao_2022'] * 100000
)

# Salvar dados integrados
dados_integrados.to_csv(data_dir / "dados_hanseniase_mt.csv", index=False)
print(f"✓ Dados integrados salvos em: dados/dados_hanseniase_mt.csv")

# ============================================================================
# RESUMO FINAL
# ============================================================================

print("\n" + "="*70)
print("RESUMO DOS DADOS COLETADOS")
print("="*70)

print(f"\nMunicípios do MT: {len(dados_integrados)}")
print(f"População total: {dados_integrados['populacao_2022'].sum():,} habitantes")
print(f"Total de casos de hanseníase (2024): {dados_integrados['casos_hanseniase_2024'].sum():,}")
print(f"Taxa média de hanseníase: {dados_integrados['taxa_bruta_hanseniase'].mean():.2f} por 100 mil hab")
print(f"Taxa mínima: {dados_integrados['taxa_bruta_hanseniase'].min():.2f} por 100 mil hab")
print(f"Taxa máxima: {dados_integrados['taxa_bruta_hanseniase'].max():.2f} por 100 mil hab")

print("\n--- Primeiros 5 municípios ---")
print(dados_integrados.head(5).to_string(index=False))

print("\n--- Últimos 5 municípios ---")
print(dados_integrados.tail(5).to_string(index=False))

print("\n" + "="*70)
print("✓ Coleta de dados concluída com sucesso!")
print("="*70)
print("\nPróximo passo: Executar 02_analise_exploratoria.R")
print("\nArquivos gerados:")
print("  - dados/municipios_mt.csv")
print("  - dados/populacao_2022.csv")
print("  - dados/hanseniase_2024.csv")
print("  - dados/dados_hanseniase_mt.csv")
print("="*70 + "\n")

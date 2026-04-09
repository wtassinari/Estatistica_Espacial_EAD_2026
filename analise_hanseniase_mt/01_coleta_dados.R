# ============================================================================
# Script: Coleta e Preparação de Dados - Hanseníase no Mato Grosso
# Objetivo: Baixar malha municipal, população e dados de hanseníase
# ============================================================================

# Limpar ambiente
rm(list = ls())

# Carregar bibliotecas
library(geobr)      # Dados geográficos do Brasil
library(sf)         # Manipulação de dados espaciais
library(dplyr)      # Manipulação de dados
library(readr)      # Leitura de arquivos
library(tidyr)      # Transformação de dados

# Criar diretório para dados
dir.create("dados", showWarnings = FALSE)

# ============================================================================
# 1. BAIXAR MALHA MUNICIPAL DO MATO GROSSO
# ============================================================================

cat("\n=== Baixando malha municipal do Mato Grosso ===\n")

# Baixar municípios do Brasil
municipios_br <- read_municipality(year = 2022)

# Filtrar apenas Mato Grosso (código UF = 28)
mt_municipios <- municipios_br %>%
  filter(code_state == 28)

cat(paste("Total de municípios no MT:", nrow(mt_municipios), "\n"))

# Salvar em formato shapefile
st_write(mt_municipios, 
         "dados/mt_municipios.shp", 
         delete_layer = TRUE, 
         quiet = TRUE)

cat("Malha municipal salva em: dados/mt_municipios.shp\n")

# ============================================================================
# 2. BAIXAR POPULAÇÃO DO CENSO 2022 (IBGE)
# ============================================================================

cat("\n=== Preparando dados de população do Censo 2022 ===\n")

# Nota: Os dados de população do Censo 2022 devem ser obtidos do IBGE
# Aqui criamos um template para que o usuário possa preencher com os dados reais
# Você pode baixar de: https://www.ibge.gov.br/

# Criar um dataframe template com os municípios do MT
populacao_template <- mt_municipios %>%
  st_drop_geometry() %>%
  select(code_muni, name_muni) %>%
  rename(cod_municipio = code_muni,
         nome_municipio = name_muni) %>%
  mutate(populacao_2022 = NA_real_)

# Salvar template
write_csv(populacao_template, "dados/populacao_template.csv")

cat("Template de população salvo em: dados/populacao_template.csv\n")
cat("IMPORTANTE: Preencha este arquivo com os dados de população do Censo 2022 do IBGE\n")

# ============================================================================
# 3. PREPARAR TEMPLATE PARA DADOS DE HANSENÍASE
# ============================================================================

cat("\n=== Preparando template para dados de hanseníase ===\n")

# Criar template para dados de hanseníase
hanseniase_template <- mt_municipios %>%
  st_drop_geometry() %>%
  select(code_muni, name_muni) %>%
  rename(cod_municipio = code_muni,
         nome_municipio = name_muni) %>%
  mutate(casos_hanseniase_2024 = NA_integer_)

# Salvar template
write_csv(hanseniase_template, "dados/hanseniase_template.csv")

cat("Template de hanseníase salvo em: dados/hanseniase_template.csv\n")
cat("IMPORTANTE: Preencha este arquivo com os dados do TabNet (DataSUS)\n")

# ============================================================================
# 4. CRIAR EXEMPLO COM DADOS SIMULADOS (para testes)
# ============================================================================

cat("\n=== Criando dados simulados para teste ===\n")

# Simular dados de população (baseado em distribuição realista)
set.seed(2024)
populacao_simulada <- populacao_template %>%
  mutate(populacao_2022 = round(rnorm(n(), mean = 25000, sd = 30000))) %>%
  mutate(populacao_2022 = pmax(populacao_2022, 5000))  # Mínimo de 5000

# Simular dados de hanseníase (com variação espacial)
hanseniase_simulada <- hanseniase_template %>%
  mutate(
    # Taxa média de hanseníase (casos por 100 mil habitantes)
    taxa_media = 5,
    # Adicionar variação aleatória
    casos_hanseniase_2024 = rpois(n(), lambda = taxa_media * populacao_simulada$populacao_2022 / 100000)
  ) %>%
  select(-taxa_media)

# Salvar dados simulados
write_csv(populacao_simulada, "dados/populacao_2022.csv")
write_csv(hanseniase_simulada, "dados/hanseniase_2024.csv")

cat("Dados simulados salvos para teste\n")
cat("- dados/populacao_2022.csv\n")
cat("- dados/hanseniase_2024.csv\n")

# ============================================================================
# 5. INTEGRAR DADOS ESPACIAIS COM ATRIBUTOS
# ============================================================================

cat("\n=== Integrando dados espaciais com atributos ===\n")

# Juntar dados de população e hanseníase
dados_atributos <- populacao_simulada %>%
  left_join(hanseniase_simulada, by = c("cod_municipio", "nome_municipio"))

# Juntar com geometria
mt_dados <- mt_municipios %>%
  st_drop_geometry() %>%
  select(code_muni, name_muni) %>%
  rename(cod_municipio = code_muni,
         nome_municipio = name_muni) %>%
  left_join(dados_atributos, by = c("cod_municipio", "nome_municipio")) %>%
  left_join(mt_municipios %>% st_drop_geometry() %>% select(code_muni, geometry = geom),
            by = c("cod_municipio" = "code_muni"))

# Converter para sf
mt_dados_sf <- st_as_sf(mt_dados)

# Salvar dados integrados
st_write(mt_dados_sf, 
         "dados/mt_hanseniase_populacao.shp", 
         delete_layer = TRUE, 
         quiet = TRUE)

write_csv(mt_dados %>% st_drop_geometry(), 
          "dados/mt_hanseniase_populacao.csv")

cat("Dados integrados salvos em:\n")
cat("- dados/mt_hanseniase_populacao.shp\n")
cat("- dados/mt_hanseniase_populacao.csv\n")

# ============================================================================
# 6. RESUMO DOS DADOS
# ============================================================================

cat("\n=== RESUMO DOS DADOS ===\n")
cat(paste("Total de municípios no MT:", nrow(mt_dados), "\n"))
cat(paste("População total:", format(sum(mt_dados$populacao_2022, na.rm = TRUE), big.mark = "."), "\n"))
cat(paste("Total de casos de hanseníase:", sum(mt_dados$casos_hanseniase_2024, na.rm = TRUE), "\n"))
cat(paste("Taxa média (por 100 mil hab):", 
          round(sum(mt_dados$casos_hanseniase_2024, na.rm = TRUE) / 
                sum(mt_dados$populacao_2022, na.rm = TRUE) * 100000, 2), "\n"))

# Estatísticas descritivas
cat("\n--- Estatísticas de População ---\n")
print(summary(mt_dados$populacao_2022))

cat("\n--- Estatísticas de Casos de Hanseníase ---\n")
print(summary(mt_dados$casos_hanseniase_2024))

cat("\n=== Coleta de dados concluída! ===\n")
cat("Próximo passo: Executar 02_analise_exploratoria.R\n")

# Salvar ambiente para uso posterior
save.image("dados/ambiente_coleta.RData")

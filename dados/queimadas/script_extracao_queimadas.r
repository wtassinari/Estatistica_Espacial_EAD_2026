# Instalar remotes (se necessário)
install.packages("remotes")

# Instalar o pacote 
remotes::install_github("wtassinari/queimadasR", force = TRUE)

library(queimadasR)

estados <- c("RIO DE JANEIRO")

dados <- download_focos_anual_periodo(
  data_inicio_str = "01/08/2025",
  data_fim_str    = "31/08/2025",
  estados_alvo    = "RIO DE JANEIRO",
  satelites_alvo  = NULL,   # Todos os satélites
  timeout         = 300,
  deduplicar_final = TRUE
)

head(dados_norte)
summary(dados_norte)
nrow(dados_norte)


library(dplyr)
dados_pontos <- dados |> select(latitude, longitude)
dados_geo <- dados |> select(latitude, longitude, frp)


# Exportar
library(writexl)
write_xlsx(dados_pontos, "queimadas_pontos.xlsx")
write_xlsx(dados_geo, "queimadas_geo.xlsx")

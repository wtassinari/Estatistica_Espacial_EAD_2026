# Limpa o diretório temporário
unlink(tempdir(), recursive = TRUE)

# Se tiver pasta de cache do knitr, delete também
unlink("index_cache", recursive = TRUE)
unlink("index_files", recursive = TRUE)


# Atualizando os pacotes envolvidos
update.packages(ask = FALSE)
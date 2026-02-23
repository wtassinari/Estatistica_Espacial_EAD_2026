## Aplicação v: Geoestatística com dados de pluviosidade na cidade do Rio de Janeiro/RJ

### Biliotecas do R que serão utilizadas

```{r, echo=T}
library(automap)
library(gstat)

library(sf)
library(sp)
library(dplyr)
library(lattice)
library(raster)
```


### Importando a tabela com a chuva acumulada média de 7 dias das últimas 24hs e 96hs das estações pluviométricas da cidde do Rio de Janeiro

[Descrição das Estações (Alerta Rio)](http://www.sistema-alerta-rio.com.br/dados-meteorologicos/info-estacoes/)

[Download Dados Pluviométricos (Alerta Rio)](http://www.sistema-alerta-rio.com.br/dados-meteorologicos/download/dados-pluviometricos/)

```{r, echo=T}
pluvio <- read.csv("dados/chuva_rio/pluviosidade.csv", sep=";")
```

### Análise Gráfica Descritiva 

```{r, echo=T}
quantil <- quantile(pluvio$acumulado_24h, seq(0,1,0.2))
quantil
```

### Transformar os dados em um objeto espacial do R

* x - Longitude
* y - Latitude

```{r, echo=T}
sp::coordinates(pluvio) = ~ x + y
```

### Análise Gráfica Descritiva com os dados espaciais

```{r, echo=T,  out.width="90%"}
# Bubble plot
bubble(pluvio, "acumulado_24h", key.entries = quantil, pch=19, col="blue")
```

```{r, echo=T,  out.width="90%"}
# Point plot
spplot(pluvio['acumulado_24h'], scales=list(draw=T), key.space="right", colorkey=T)
```

### Modelando o variograma experimental (ou empírico)

- **width** - Distância média entre amostras ou distância dos lags
- **cutoff** - Máxima distância

```{r, echo=T}
variogram.emp = gstat::variogram(acumulado_24h ~x+y, pluvio, width=1000, cutoff=20000)
variogram.emp
```

```{r, echo=T}
# Variogram plot
plot(variogram.emp, main = "Empirical variogram", pch = 19, col = "darkblue")
```



#### Ajustando o semivariograma teórico

- **Sill** - Semivariância estrutural ou contribuição (ponto máximo que chega ao plato no eixo de y)
- **Range** - Alcance (ponto máximo em x)
- **Nugget** - Efeito pepita, quando (y, 0)

```{r echo=F, fig.align="center", out.width= "70%", fig.show='hold'}
include_graphics('figuras/variograma_model.png')
```
[Fonte do gráfico](https://vsp.pnnl.gov/help/Vsample/Kriging_Variogram_Model.htm)


```{r, echo=T}
## 1) Modelo Linear
lin.fit  = fit.variogram(variogram.emp, 
                         model = vgm(psill = 240, model = "Lin",
                                     range = 5000, nugget = 10))

## 2) Modelo exponencial
exp.fit  = fit.variogram(variogram.emp, 
                         model = vgm(psill = 240, model = "Exp",
                                     range = 5000, nugget = 10))

## 3) Modelo gaussiano
gau.fit  = fit.variogram(variogram.emp, 
                         model = vgm(psill = 240, model = "Gau",
                                     range = 5000, nugget = 10))

## 3) Modelo wave
wav.fit  = fit.variogram(variogram.emp, 
                         model = vgm(psill = 240, model = "Wav",
                                     range = 5000, nugget = 10))
```

```{r, echo=T, out.width= "100%"}
plot.lin <- plot(variogram.emp, lin.fit, main = "Modelo Linear")
plot.exp <- plot(variogram.emp, exp.fit, main = "Modelo Exponencial")
plot.gau <- plot(variogram.emp, gau.fit, main = "Modelo Gaussiano")
plot.wav <- plot(variogram.emp, wav.fit, main = "Modelo Wave")
```


#### Validação cruzada 

- **RMSE** (root mean squared error): é a medida que calcula "a raiz quadrática média" dos erros entre valores observados (reais) e predições (hipóteses).

```{r, echo=T}
## 1) Modelo Linear
cv.lin <- krige.cv(acumulado_24h ~x+y, locations = pluvio, model = lin.fit)
summary(cv.lin)
plot(cv.lin$var1.pred ~ cv.lin$observed, cex = 1.2, lwd = 2)
abline(0, 1, col = "lightgrey", lwd = 2)
lm_lin <- lm(cv.lin$var1.pred ~ cv.lin$observed)
abline(lm_lin, col = "red", lwd = 2)
r2_lin = summary(lm_lin)$r.squared
rmse_lin = hydroGOF::rmse(cv.lin$var1.pred, cv.lin$observed)

## 2) Modelo exponencial
cv.exp <- krige.cv(acumulado_24h ~x+y, locations = pluvio, model = exp.fit)
summary(cv.exp)
plot(cv.exp$var1.pred ~ cv.exp$observed, cex = 1.2, lwd = 2)
abline(0, 1, col = "lightgrey", lwd = 2)
lm_exp <- lm(cv.exp$var1.pred ~ cv.exp$observed)
abline(lm_exp, col = "red", lwd = 2)
r2_exp = summary(lm_exp)$r.squared
rmse_exp = hydroGOF::rmse(cv.exp$var1.pred, cv.exp$observed)

## 3) Modelo Gaussiano
cv.gau <- krige.cv(acumulado_24h ~x+y, locations = pluvio, model = gau.fit)
summary(cv.gau)
plot(cv.gau$var1.pred ~ cv.gau$observed, cex = 1.2, lwd = 2)
abline(0, 1, col = "lightgrey", lwd = 2)
lm_gau <- lm(cv.gau$var1.pred ~ cv.gau$observed)
abline(lm_gau, col = "red", lwd = 2)
r2_gau = summary(lm_gau)$r.squared
rmse_gau = hydroGOF::rmse(cv.gau$var1.pred, cv.gau$observed)

## 4) Modelo Wave
cv.wav <- krige.cv(acumulado_24h ~x+y, locations = pluvio, model = wav.fit)
summary(cv.wav)
plot(cv.wav$var1.pred ~ cv.wav$observed, cex = 1.2, lwd = 2)
abline(0, 1, col = "lightgrey", lwd = 2)
lm_wav <- lm(cv.wav$var1.pred ~ cv.wav$observed)
abline(lm_wav, col = "red", lwd = 2)
r2_wav = summary(lm_wav)$r.squared
rmse_wav = hydroGOF::rmse(cv.wav$var1.pred, cv.wav$observed)
```

```{r, echo=T}
# Criando uma tabela das estatística de R2 e RMSE
df.r2 <- data.frame(r2_lin, r2_exp, r2_gau, r2_wav)
df.rmse <- data.frame(rmse_lin, rmse_exp, rmse_gau, rmse_wav)
tabela <- data.frame(cbind(t(df.r2), t(df.rmse)))
colnames(tabela) <- c('R2', 'RMSE')
rnames <- gsub('r2_','', rownames(tabela)) # remove o prefixo r2 dos nomes das linhas
rownames(tabela) <- rnames # substitui o nome das linhas simplificadas na tab original
tabela
```

#### Criando os grids do contorno da cidade do Rio de Janeiro para intermpolação

```{r, echo=T}
# Importando o contorno do Rio
contorno.rio <- shapefile('dados/chuva_rio/MUNIC_2K_2022_IPP_SIRGAS.shp')
```

```{r, echo=T}
# Criando grade para interpolacao com a  resolucao de 50m
r <- raster(contorno.rio, res = 50)

# Criando um objeto formato raster
rp <- rasterize(contorno.rio, r, 0) 

# Trasnsformando o objeto raster no formato SpatialPixelsDataFrame
grid <- as(rp, "SpatialPixelsDataFrame") 
sp::plot(grid)
```

#### Krigagem

```{r, echo=T}
# Colocando os dados de chuva e o grid na mesma projecao
sp::proj4string(pluvio) = CRS(proj4string(contorno.rio))

mapa_chuva_lin <- krige(acumulado_24h ~1, pluvio, grid, model =lin.fit)
mapa_chuva_exp <- krige(acumulado_24h ~1, pluvio, grid, model =exp.fit)
mapa_chuva_gau <- krige(acumulado_24h ~1, pluvio, grid, model =gau.fit)
mapa_chuva_wav <- krige(acumulado_24h ~1, pluvio, grid, model =wav.fit)
```

```{r, echo=T, out.width= "100%"}
plot.lin <- plot(mapa_chuva_lin, main = "Modelo Linear")
plot.exp <- plot(mapa_chuva_exp, main = "Modelo Exponencial")
plot.gau <- plot(mapa_chuva_gau, main = "Modelo Gaussiano")
plot.wav <- plot(mapa_chuva_wav, main = "Modelo Wave")

```

#### Auto Krige 

```{r, echo=T}
# Modelando
auto.krige = autoKrige(acumulado_24h~x+y, pluvio, grid, model = 'Exp')
summary(auto.krige)
```

```{r, echo=T}
# Validação cruzada
auto.krige.cv <- autoKrige.cv(acumulado_24h~x+y, pluvio, model = 'Exp')
summary(auto.krige.cv)
```

#### Convertendo para o formato raster - auto krige

```{r, echo=T}
raster_chuva <- raster(auto.krige$krige_output)
plot(raster_chuva)
```

- Caso queira salvar a imagem raster em um arquivo formato geotiff para ler em algum SIG por exemplo:
  
  ```{r, echo=T}
# Exportando o objeto da imagem
writeRaster(raster_chuva,
            filename = 'dados/chuva_rio/chuva_auto.tiff',
            format = 'GTiff',
            overwrite = T)

# Importando de volta para o R
raster_chuva <- raster("dados/chuva_rio/chuva_auto.tiff")
```

#### Fazendo o mapa interativo com as estações

```{r, echo=T}
# Importando os dados referente as estatções pluviométricas
estacoes.sf <- read_sf('dados/chuva_rio/Estac_C3_B5es_Alerta_Rio.shp')

# Convertendo UTM para Lat Long das estacoes
estacoes.longlat <- st_transform(estacoes.sf, "+proj=longlat +ellps=WGS84 +datum=WGS84")
estacoes.longlat$coords <- st_coordinates(estacoes.longlat)
estacoes.longlat$X <- estacoes.longlat$coords[,1]
estacoes.longlat$Y <- estacoes.longlat$coords[,2]
```

```{r, echo=T}
# Importando a malha de bairros
bairros.sf <- read_sf('dados/chuva_rio/BAIRROS_2K_2022_IPP_SIRGAS.shp')

# Convertendo UTM para Lat Long a malha dos bairros
bairros.longlat <- st_transform(bairros.sf, "+proj=longlat +ellps=WGS84 +datum=WGS84")
```

```{r, echo=T}
# Convertendo o raster da chuva para lat long 
raster_chuva_longlat <- projectRaster(raster_chuva, crs = CRS("+proj=longlat +datum=WGS84"))
```

```{r, echo=T}
library(leaflet)

# Definindo Paleta de cores da superfície interpolada da chuva
pal <- colorNumeric(c("#000066", "#00c8f8", "#F0E68C","#FFFF00", "#FF8C00"), values(raster_chuva_longlat), na.color = "transparent", reverse = T)
```

```{r, echo=T, out.width= "100%"}
# Construindo o mapa interativo via leaflet

leaflet(data = estacoes.longlat, options = leafletOptions(
  attributionControl=FALSE)) |> 
  # addTiles() |>
  addProviderTiles("CartoDB.Positron", group = "Ruas") |>
  addProviderTiles("Esri.WorldImagery", options = providerTileOptions(opacity = 0.7), group = "Satélite") |> 
  addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") |>
  addProviderTiles(providers$Stadia.StamenToner, group = "Toner") |>
  setView(lng = -43.42, lat = -22.90, zoom = 10.4) |>
  # addProviderTiles(providers$CartoDB.Voyager) |>
  addMarkers(~X, ~Y, popup = ~as.character(est), label = ~as.character(est),
             group = "Estações")  |>
  ############## Polígonos dos Bairros ################
addPolygons(data=bairros.longlat,
            weight = 3,
            color = "darkblue",
            smoothFactor = 1,
            fill = FALSE,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px",
              direction = "auto"), 
            group = "Bairros") |>
  ########## Adicionando o raster  #########################
addRasterImage(raster_chuva_longlat, colors = pal, opacity = 0.8,
               group = "Chuva: 1 semana") %>%
  leaflet::addLegend(pal = pal, values = values(raster_chuva_longlat),
                     title = "Chuva Acumulada - 1 semana", group = "Chuva: 1 semana") |>
  ############## Controle das layers (botoes) ################
addLayersControl(
  baseGroups = c("Voyager", "Ruas", "Satélite", "Toner"),
  overlayGroups = c("Estações", "Bairros",  "Chuva: 1 semana"),
  options = layersControlOptions(collapsed = FALSE),
  position = "bottomleft") |>
  ########## Desabilitando os grupos ################
hideGroup(group = c("Bairros"))

```
## ---------------------------------------------------------------------------------------------
#| message: false

library(dplyr) # Manejo de datos
library(sf) # Manejo de datos vectoriales
library(stars) # Manejo de datos raster
library(ggplot2) # Graficos en general y mapas
# remotes::install_github('r-tmap/tmap')
library(tmap) # Mapas temáticos
library(rgee) # GEE en R
source('src/s2_clean.R') # Funcion para limpiar nubes


## ---------------------------------------------------------------------------------------------
datosRto <- st_read('data/rto_mz_loteA.gpkg')


## ---------------------------------------------------------------------------------------------
datosRto


## ---------------------------------------------------------------------------------------------
poligonoLote <-
  concaveman::concaveman(datosRto,
                         concavity = 2.5,
                         length_threshold = 0)
plot(poligonoLote)


## ---------------------------------------------------------------------------------------------
area <- st_area(poligonoLote)
# round(units::set_units(area, 'ha'), 0)


## ---------------------------------------------------------------------------------------------
ggplot(datosRto)


## ---------------------------------------------------------------------------------------------
ggplot(datosRto, aes(x = Masa_de_re))


## ---------------------------------------------------------------------------------------------
ggplot(datosRto) +
  geom_histogram(aes(x = Masa_de_re))


## ---------------------------------------------------------------------------------------------
datosRto <- 
  datosRto |> 
  filter(Masa_de_re < 10)


## ---------------------------------------------------------------------------------------------
ggplot(datosRto) +
  geom_sf()


## ---------------------------------------------------------------------------------------------
ggplot(datosRto) +
  geom_sf(aes(color = Masa_de_re))


## ---------------------------------------------------------------------------------------------
ggplot(datosRto) +
  geom_sf(aes(color = Masa_de_re),
          size = 0.5)


## ---------------------------------------------------------------------------------------------
tm_shape(datosRto) +
  tm_dots()


## ---------------------------------------------------------------------------------------------
tm_shape(datosRto) +
  tm_dots(fill = "Masa_de_re")


## ---------------------------------------------------------------------------------------------
tm_shape(datosRto) +
  tm_dots(fill = "Masa_de_re",
          fill.scale = tm_scale_continuous())


## ---------------------------------------------------------------------------------------------
tmap_mode('view')

tm_shape(poligonoLote) +
  tm_polygons(col = 'red')

tmap_mode('plot')


## ---------------------------------------------------------------------------------------------
tm_shape(datosRto) +
  tm_dots(fill = 'Masa_de_re',
          fill.scale = tm_scale_continuous(values = "brewer.greens"),
          fill.legend = tm_legend(title = "Rendimiento\nkg/ha",
                                  frame = FALSE)) +
tm_shape(poligonoLote) +
  tm_polygons(fill = NA,
              col = "red",
              lwd = 2)


## ---------------------------------------------------------------------------------------------
datosRto <- st_transform(datosRto, 32720)
poligonoLote <- st_transform(poligonoLote, 32720)


## ---------------------------------------------------------------------------------------------
#| include: false
library(googledrive)

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)



## ---------------------------------------------------------------------------------------------
#| label: InitializeRgee
rgee::ee_Initialize(drive = TRUE)


## ---------------------------------------------------------------------------------------------
#| message: false
poligonoLote_ee <- rgee::sf_as_ee(poligonoLote)


## ---------------------------------------------------------------------------------------------
disponible <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
  filterDate("2023-09-10","2023-09-20")$
  filterBounds(poligonoLote_ee$geometry())

misImagenes <- ee_get_date_ic(disponible)
misImagenes


## ---------------------------------------------------------------------------------------------

miImages <- ee$Image(misImagenes[1,'id'])

viz = list(min = 0,
           max = 13292,
           bands = c('B4', 'B3', 'B2'),
           gamma = 1.75)

Map$centerObject(eeObject = miImages, zoom = 9)
Map$addLayer(eeObject = miImages, visParams = viz) +
Map$addLayer(eeObject = poligonoLote_ee, visParams = list(color = 'red'))


## ---------------------------------------------------------------------------------------------
# Filtra por si hay superposición de imágenes
coveringFilter = ee$Filter$contains(
  leftField = '.geo',
  rightValue = poligonoLote_ee$geometry()
)



## ---------------------------------------------------------------------------------------------
start <- rgee::rdate_to_eedate(as.Date("2021-03-10"))
end <- rgee::rdate_to_eedate(as.Date("2021-03-30"))

sat_data_s2  <-
  ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
  filterBounds(poligonoLote_ee$geometry())$
  filter(ee$Filter$lte("CLOUD_COVERAGE_ASSESSMENT",
                       20))$
  filter(ee$Filter$date(start, end))$
  map(s2_clean)$
  filter(coveringFilter)$
  map(function(image) {

    ndvi = image$normalizedDifference(c('B8', 'B4'))$rename('NDVI')
   
    newBands = ee$Image(list(ndvi))

    image$addBands(newBands)
  })$
  select(list(
    'B3', 'B4', 'B8', 'B11', 'B12',
    'NDVI'
    ))$
  map(function(img) {
    img$float()
  })


## ---------------------------------------------------------------------------------------------
#| results: asis
bandNames <- sat_data_s2$first()$bandNames()$getInfo()
cat("Nombre de bandas: ", paste(bandNames, collapse = ", "), '\n')


## ---------------------------------------------------------------------------------------------
#| results: asis
count <- sat_data_s2$size()
cat("Cantidad de fechas: ", count$getInfo(), '\n')


## ---------------------------------------------------------------------------------------------
sat_data <- sat_data_s2$toBands()

sat_data_stars <- rgee::ee_as_stars(
  sat_data,
  region = poligonoLote_ee$geometry(),
  scale = 10,
  via = "drive"
)

sat_data_stars


## ---------------------------------------------------------------------------------------------
plot(sat_data_stars)


## ---------------------------------------------------------------------------------------------
sat_data_stars <- st_crop(sat_data_stars, poligonoLote)


## ---------------------------------------------------------------------------------------------
#| warning: false


nombre_bandas <- st_get_dimension_values(sat_data_stars, 'band')
bandas_ndvi <- which(grepl("NDVI", nombre_bandas))

tm_shape(st_as_stars(sat_data_stars[,,,bandas_ndvi])) +
  tm_raster()


## ---------------------------------------------------------------------------------------------
sat_data_sf <- st_as_sf(sat_data_stars,
                        as_points = TRUE)


## ---------------------------------------------------------------------------------------------
plot(sat_data_sf)


## ---------------------------------------------------------------------------------------------
cluster <- paar::kmspc(
  sat_data_sf,
  variables = colnames(st_drop_geometry(sat_data_sf)),
  number_cluster = 2:4)


## ---------------------------------------------------------------------------------------------
cluster$summaryResults


## ---------------------------------------------------------------------------------------------
cluster$indices


## ---------------------------------------------------------------------------------------------
cluster_data <- cbind(sat_data_sf, cluster$cluster)


## ---------------------------------------------------------------------------------------------
tm_shape(cluster_data) +
  tm_dots(fill  = 'Cluster_2',
          fill.scale = tm_scale_categorical())


## ---------------------------------------------------------------------------------------------
cluster_data_poly <- st_join(
  st_as_sf(sat_data_stars,
           as_points = FALSE),
  cluster_data[, c("Cluster_2", "Cluster_3", "Cluster_4")]
  )


## ---------------------------------------------------------------------------------------------

tmap_arrange(
  tm_shape(cluster_data_poly) +
    tm_polygons(fill  = 'Cluster_2',
                fill.scale = tm_scale_categorical()),
  
  tm_shape(datosRto) +
    tm_dots(fill  = 'Masa_de_re',
            fill.scale = tm_scale_continuous()),
  ncol = 2
)
  


## ---------------------------------------------------------------------------------------------
tmap_mode('view')
tm_basemap("Esri.WorldImagery") +
 tm_shape(cluster_data_poly) +
    tm_polygons(fill  = 'Cluster_2',
                fill.scale = tm_scale_categorical())
 


## ---------------------------------------------------------------------------------------------
datos_rendimiento_zonas <-
  st_join(datosRto[, "Masa_de_re"],
          cluster_data_poly[, "Cluster_2"],
          left = FALSE)


## ---------------------------------------------------------------------------------------------
datos_rendimiento_zonas |>
  mutate(mediaGral = mean(Masa_de_re, na.rm = TRUE)) |>
  group_by(Cluster_2) |>
  reframe(
    n = n(),
    media = mean(Masa_de_re, na.rm = TRUE),
    CV = sd(Masa_de_re) / media * 100,
    mediaGeneral = unique(mediaGral),
    media_ratio = media/unique(mediaGral))


## ---------------------------------------------------------------------------------------------
ggplot(datos_rendimiento_zonas,
       aes(as.factor(Cluster_2), Masa_de_re)) +
  stat_summary(fun.data = mean_se,
               geom = "bar") +
  labs(y = "Rendimiento",
       x = "Clúster")


## ---------------------------------------------------------------------------------------------
start_gif <- "2020-12-01"
end_gif <- "2021-05-01"
ndvi_s2 <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
  filterBounds(poligonoLote_ee$geometry())$
  filter(ee$Filter$lte("CLOUD_COVERAGE_ASSESSMENT",
                       1))$
  filter(ee$Filter$date(start_gif, end_gif))$
  map(s2_clean)$
  filter(coveringFilter)$
  map(function(image) {
    ndvi = image$normalizedDifference(c('B8', 'B4'))$rename('NDVI')
    image$addBands(ndvi)
  })$
  select('NDVI')$
  map(function(img) {
    img$float()
  })
distinctDOY <- ndvi_s2$filterDate(start_gif, end_gif)
# rgeeExtra::ee_maxValue(comp$toBands())
# rgeeExtra::ee_minValue(comp$toBands())
visParams = list(
  min = 0.0,
  max = 1,
  bands = "NDVI",
  palette = c(
    'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
    '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
    '012E01', '011D01', '011301'
  )
)

region <- poligonoLote_ee$geometry()$bounds()


rgbVis <- ndvi_s2$map(function(img) {
  do.call(img$visualize, visParams) %>%
    ee$Image$clip(poligonoLote_ee)
})


gifParams <- list(
  region = region,
  dimensions = 800,
  crs = 'EPSG:32720',
  framesPerSecond = 1.2
)


dates_mabbr <- distinctDOY %>%
  ee_get_date_ic %>% # Get Image Collection dates
  '[['("time_start") %>% # Select time_start column
  format("%d-%m-%Y") # Get the month component of the datetime



## ---------------------------------------------------------------------------------------------
#| label: gif_sentinel
#| message: false


animation <- rgeeExtra::ee_utils_gif_creator(rgbVis, gifParams, mode = "wb")
animation %>%
  rgeeExtra::ee_utils_gif_annotate(
    text = "NDVI: Sentinel 2",
    size = 15,
    color = "white",
    location = "+10+10"
  ) %>%
  rgeeExtra::ee_utils_gif_annotate(
    text = dates_mabbr,
    size = 15,
    location = "+10+30",
    color = "white",
    font = "arial",
    boxcolor = "#000000"
  ) # -> animation_wtxt

# rgeeExtra::ee_utils_gif_save(animation_wtxt, path = "raster_as_ee.gif")


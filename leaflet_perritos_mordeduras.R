library(sf)
library(readxl)
library(dplyr)
library(leaflet)
library(reshape2)
library(htmlwidgets)
library(leaflet.extras)
library(leaflet.extras2)
library(fields)   # Creo que no se ocupa para la paleta

setwd("C:/Users/SIGEH/Desktop/Lalo/Gob/Proyectos/Mordeduras_perritos/")

clues= readxl::read_excel("Datos/CLUES_gastrointestinales/morbi general  X unidad sectorial 2023 (1).xlsx")
clues_c_perros = clues|> 
  dplyr::filter(grepl("perro",Padecimiento))  
clues_total_pad=clues|>
  dplyr::filter(grepl("Total",`Unidad médica`))|>
  dplyr::mutate(`Unidad médica`=sub("^Total ", "", `Unidad médica`))|>
  dplyr::select(-Padecimiento)


clues_c_perros_y_total = merge(x = clues_c_perros, y = clues_total_pad |> dplyr::select(CLUES,Casos)|> dplyr::mutate(Casos_total = Casos)|> dplyr::select(-Casos), by='CLUES', all.y = T)
clues_c_perros_y_total$porc_casos_perros = clues_c_perros_y_total$Casos/clues_c_perros_y_total$Casos_total*100
clues_shp = sf::read_sf("Datos/CLUES_gastrointestinales/clues_gastrointestinales_shapefile.shp")
clues_c_perros_y_total = merge(x = clues_c_perros_y_total, y = clues_shp|> dplyr::select(CLUES, mun, geometry), by = 'CLUES')
clues_c_perros_y_total = clues_c_perros_y_total |> st_as_sf()

clues_c_perros_y_total_p = clues_c_perros_y_total[order(clues_c_perros_y_total$porc_casos_perros, decreasing = TRUE), ]
clues_c_perros_y_total_n = clues_c_perros_y_total[order(clues_c_perros_y_total$Casos, decreasing = TRUE), ]

pal_perros = colorNumeric(c("yellow","red"),domain = clues_c_perros_y_total_p$porc_casos_perros,na.color = "grey")
pal_perros_n = colorNumeric(c("yellow","red"),domain = clues_c_perros_y_total_n$Casos,na.color = "grey")


### HeatMap
# Porcentaje de casos
clues_c_perros_y_total_p = clues_c_perros_y_total_p |> dplyr::filter(!is.na(porc_casos_perros))
coordenadas_porcentaje = sf::st_coordinates(clues_c_perros_y_total_p )
longitud_porcentaje = coordenadas_porcentaje[,1]
latitud_porcentaje = coordenadas_porcentaje[,2]

clues_c_perros_y_total_p$latitud = latitud_porcentaje
clues_c_perros_y_total_p$longitud = longitud_porcentaje


# Numero de casos de Mordedura
clues_c_perros_y_total_n = clues_c_perros_y_total_n |> dplyr::filter(!is.na(Casos))
coordenadas_numero = sf::st_coordinates(clues_c_perros_y_total_n)
longitud_numero = coordenadas_numero[,1]
latitud_numero = coordenadas_numero[,2]

clues_c_perros_y_total_n$latitud = latitud_numero
clues_c_perros_y_total_n$longitud = longitud_numero

# Paleta de colores
paleta = colorRampPalette(c( "cyan",  "yellow", "red"))(3)



perros = leaflet()|>
  addTiles()|>
  addCircles(data = clues_c_perros_y_total_p |> as("Spatial"), color = pal_perros(clues_c_perros_y_total_p$porc_casos_perros), 
             radius = clues_c_perros_y_total_p$porc_casos_perros*200, 
             label = ~paste(Municipio, "-",clues_c_perros_y_total_p$`Unidad médica`),
             opacity = 0.5, 
             fillOpacity = 0.5,
             popup = ~paste("Municipio:", clues_c_perros_y_total_p$mun,
                            "<br> Nombre: ",clues_c_perros_y_total_p$`Unidad médica`,
                            "<br> Mordeduras de perros: ",clues_c_perros_y_total_p$Casos,
                            "<br> Total de casos en el establecimiento: ",clues_c_perros_y_total_p$Casos_total,
                            "<br> % de casos de mordeduras: ",clues_c_perros_y_total_p$porc_casos_perros|>round(2),"%"
             ),group = "Porcentaje de casos de mordeduras"
             )|>
  addCircles(data = clues_c_perros_y_total_n |> as("Spatial"), color = pal_perros_n(clues_c_perros_y_total_n$Casos), 
             radius = clues_c_perros_y_total_n$Casos*10, 
             opacity = 0.5,fillOpacity = 0.5,
             popup = ~paste("Municipio:", clues_c_perros_y_total_n$mun,
                            "<br> Nombre: ",clues_c_perros_y_total_n$`Unidad médica`,
                            "<br> Mordeduras de perros: ",clues_c_perros_y_total_n$Casos,
                            "<br> Total de casos en el establecimiento: ",clues_c_perros_y_total_n$Casos_total,
                            "<br> % de casos de mordeduras: ",clues_c_perros_y_total_n$porc_casos_perros|>round(2),"%"
              ),group = "Número de casos de mordeduras"
             )|>
  addHeatmap(data = clues_c_perros_y_total_p |> as("Spatial"), lng = clues_c_perros_y_total_p$longitud, lat = clues_c_perros_y_total_p$latitud, blur= 5, max = 1, radius = 40, group = "Porcentaje de casos de mordeduras", gradient = "Reds") |>
  addHeatmap(data = clues_c_perros_y_total_n |> as("Spatial"), lng = clues_c_perros_y_total_n$longitud, lat = clues_c_perros_y_total_n$latitud, blur = 5, max = 1, radius = 40, group = "Número de casos de mordeduras", gradient = "Reds") |>
  addSearchFeatures(targetGroups = "Porcentaje de casos de mordeduras",
                    options = searchFeaturesOptions(
                      zoom = 12, 
                      openPopup = F,
                      firstTipSubmit =F,
                      hideMarkerOnCollapse =T))|>
  addLayersControl(
    baseGroups = c("Porcentaje de casos de mordeduras", "Número de casos de mordeduras"),
    options = layersControlOptions(collapsed = FALSE,)
  )|>
  hideGroup(c("Número de casos de mordeduras"))|>
  addControl(
    html = "<h2 style='color: darkblue;text-align: center;'>Casos de Mordeduras por Perros a nivel de CLUES</h2>",
    position = "bottomleft"
  )

perros
library(htmlwidgets)
saveWidget(perros, "mapa_mordedura_perros.html",selfcontained = T)



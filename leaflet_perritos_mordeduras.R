library(readxl)
library(dplyr)
library(leaflet)
library(reshape2)
library(htmlwidgets)
library(leaflet.extras)
clues=read_excel("rasters/comedores/Documentacion/inputs/CLUES_gastrointestinales/morbi general  X unidad sectorial 2023 (1).xlsx")
clues_c_perros=clues|>
  dplyr::filter(grepl("perro",Padecimiento))  
clues_total_pad=clues|>
  dplyr::filter(grepl("Total",`Unidad médica`))|>
  dplyr::mutate(`Unidad médica`=sub("^Total ", "", `Unidad médica`))|>
  dplyr::select(-Padecimiento)


clues_c_perros_y_total=merge(x=clues_c_perros,y=clues_total_pad|>dplyr::select(CLUES,Casos)|>dplyr::mutate(Casos_total=Casos)|>dplyr::select(-Casos),by='CLUES',all.y=T)
clues_c_perros_y_total$porc_casos_perros=clues_c_perros_y_total$Casos/clues_c_perros_y_total$Casos_total*100
clues_shp=read_sf("rasters/comedores/Documentacion/inputs/CLUES_gastrointestinales/clues_gastrointestinales_shapefile.shp")
clues_c_perros_y_total=merge(x=clues_c_perros_y_total,y=clues_shp|>dplyr::select(CLUES,mun,geometry),by='CLUES')
clues_c_perros_y_total=clues_c_perros_y_total|>st_as_sf()

pal_perros=colorNumeric(c("yellow","red"),domain = clues_c_perros_y_total_p$porc_casos_perros,na.color = "grey")
pal_perros_n=colorNumeric(c("yellow","red"),domain = clues_c_perros_y_total_n$Casos,na.color = "grey")


clues_c_perros_y_total_p <- clues_c_perros_y_total[order(clues_c_perros_y_total$porc_casos_perros, decreasing = TRUE), ]
clues_c_perros_y_total_n <- clues_c_perros_y_total[order(clues_c_perros_y_total$Casos, decreasing = TRUE), ]
perros=leaflet()|>
  addTiles()|>
  addCircles(data=clues_c_perros_y_total_p|>as("Spatial"), color = pal_perros(clues_c_perros_y_total_p$porc_casos_perros), 
             radius = clues_c_perros_y_total_p$porc_casos_perros*200, 
             label = ~paste(Municipio, "-",clues_c_perros_y_total_p$`Unidad médica`),
             opacity = 0.5,fillOpacity = 0.5,
             popup = ~paste("Municipio:", clues_c_perros_y_total_p$mun,
                            "<br> Nombre: ",clues_c_perros_y_total_p$`Unidad médica`,
                            "<br> Mordeduras de perros: ",clues_c_perros_y_total_p$Casos,
                            "<br> Total de casos en el establecimiento: ",clues_c_perros_y_total_p$Casos_total,
                            "<br> % de casos de mordeduras: ",clues_c_perros_y_total_p$porc_casos_perros|>round(2),"%"
             ),group = "Porcentaje de casos de mordeduras"
             )|>
  addCircles(data=clues_c_perros_y_total_n|>as("Spatial"), color = pal_perros_n(clues_c_perros_y_total_n$Casos), 
             radius = clues_c_perros_y_total_n$Casos*10, 
             opacity = 0.5,fillOpacity = 0.5,
             popup = ~paste("Municipio:", clues_c_perros_y_total_n$mun,
                            "<br> Nombre: ",clues_c_perros_y_total_n$`Unidad médica`,
                            "<br> Mordeduras de perros: ",clues_c_perros_y_total_n$Casos,
                            "<br> Total de casos en el establecimiento: ",clues_c_perros_y_total_n$Casos_total,
                            "<br> % de casos de mordeduras: ",clues_c_perros_y_total_n$porc_casos_perros|>round(2),"%"
             ),group = "Número de casos de mordeduras"
             )|>
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
saveWidget(perros, "Perros y gatos/Mapas Web/Casos de Mordeduras por Perros/mapa_mordedura_perros.html",selfcontained = T)



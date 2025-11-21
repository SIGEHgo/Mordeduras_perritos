library(sf)
library(readxl)
library(dplyr)
library(reshape2)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(leafem)
library(htmlwidgets)
library(htmltools)


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
clues_c_perros_y_total = clues_c_perros_y_total |> sf::st_as_sf()

clues_c_perros_y_total_p = clues_c_perros_y_total[order(clues_c_perros_y_total$porc_casos_perros, decreasing = TRUE), ]
clues_c_perros_y_total_n = clues_c_perros_y_total[order(clues_c_perros_y_total$Casos, decreasing = TRUE), ]

pal_perros = colorNumeric(c("yellow","red"),domain = clues_c_perros_y_total_p$porc_casos_perros, na.color = "grey")
pal_perros_n = colorNumeric(c("yellow","red"),domain = clues_c_perros_y_total_n$Casos, na.color = "grey")


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





info.box <- HTML(paste0(
  HTML(
    '<div class="modal fade" id="infobox" role="dialog"><div class="modal-dialog"><!-- Modal content--><div class="modal-content"><div class="modal-header"><button type="button" class="close" data-dismiss="modal">&times;</button>'
  ),
  
  # Header / Title
  HTML(
    '</div><div class="modal-body">'
  ),
  
  # Body
  HTML('<h4>Información Adicional:</h4>
<p> El mapa web muestra información por cada CLUES (Clave Única de Establecimientos de Salud) mediante dos capas: </p>
<ul>
  <li><b>Porcentaje de casos de mordeduras:</b> Cada CLUES aparece como un círculo cuyo tamaño depende del porcentaje de mordeduras de perro registradas ahí, en comparación con el total de casos atendidos en ese mismo establecimiento.</li>
  
  <li><b>Número de casos de mordeduras:</b> De forma similar, cada CLUES se representa con un círculo, pero en este caso el tamaño corresponde al número total de mordeduras registradas.</li>
</ul>
<p> Además, en cada capa se incluye un mapa de calor que ayuda a visualizar mejor cómo se distribuyen los casos de mordeduras. Este mapa de calor se oculta automáticamente cuando se hace demasiado zoom, para evitar que se confunda con la visualización de los datos detallados.</p>'),
  
  # Closing divs
  HTML('</div><div class="modal-footer"><button type="button" class="btn btn-default" data-dismiss="modal">Cerrar</button></div></div>')
))





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
    position = "bottomright"
  ) |> 
  addLogo(img ="img/fondo_transparent.png",  position ="bottomleft" , width = "350", height = "175", offset.x = 10, offset.y = -30) |> 
  addLegend(
    position = "bottomright",
    colors = c("yellow", "red"),
    labels = c("Menor intensidad", "Mayor intensidad"),
    title = "Porcentaje de mordeduras",
    group = "Porcentaje de casos de mordeduras",
    layerId = "leyenda_porcentaje",
    opacity = 1,
  )  |> 
  addLegend(
    position = "bottomright",
    colors = c("yellow", "red"), # 'transparent' for the circle icon
    labels = c("Menor intensidad", "Mayor intensidad"),
    title = "Número de mordeduras", # More descriptive title
    group = "Número de casos de mordeduras",
    layerId = "leyenda_numero",
    opacity = 1
  )  |> 
  htmlwidgets::onRender("
    function(el, x) {
      var map = this;
      
      

      function actualizarLeyendas() {
        var baseLayers = document.querySelectorAll('.leaflet-control-layers-base input[type=radio]');
        var leyendas = document.getElementsByClassName('info legend leaflet-control');
        
        baseLayers.forEach(function(input) {
          if (input.checked) {
            var activa = input.nextSibling.textContent.trim();
      
            Array.from(leyendas).forEach(function(leyendaEl) {
              var textoLeyenda = leyendaEl.children[0]?.children[0]?.innerHTML?.trim();
      
              if (activa === 'Porcentaje de casos de mordeduras') {
      
                if (textoLeyenda !== 'Número de mordeduras') {
                  leyendaEl.style.display = 'block';   // activar esta
                } else {
                  leyendaEl.style.display = 'none';    // ocultar las demás
                }
      
              } else {
      
                if (textoLeyenda !== 'Porcentaje de mordeduras') {
                  leyendaEl.style.display = 'block';   // activar otras leyendas
                } else {
                  leyendaEl.style.display = 'none';    // ocultar solo esa
                }
      
              }
            });
          }
        });
      }
      
      // Asignar el evento a cada input tipo radio
      var baseLayers = document.querySelectorAll('.leaflet-control-layers-base input[type=radio]');
      baseLayers.forEach(function(input) {
        input.addEventListener('change', actualizarLeyendas);
      });
      
      actualizarLeyendas();
      
      


      
     // Intento de funcion optima, pero no funciono :(, pero ahi esta la idea checar despues
     //  function actualizarLeyendas(legend) {
     //   if (!legend) return;
     //    const swatches = legend.children;
     //    console.log(swatches);
     //    
     //    const aplicarEstilos = (elemento, colorFondo, colorBorde) => {
     //      elemento.style.borderRadius = '50%';
     //      elemento.style.width = '12px';
     //      elemento.style.height = '12px';
     //      elemento.style.backgroundColor = colorFondo;
     //      elemento.style.border = `2px solid ${colorBorde}`;
     //    };
     //  
     //    aplicarEstilos(swatches[1], 'rgba(243, 233, 103, 0.3)', '#f0e00d');
     //    aplicarEstilos(swatches[3], 'rgba(214, 104, 79, 0.3)', '#e83024');
     //  };
     // 
     // function actualizarLeyendas(document.getElementsByClassName('info legend leaflet-control')[0]);
     // function actualizarLeyendas(document.getElementsByClassName('info legend leaflet-control')[1]);
     
    
      
      //Forma no optima pero funciona 
      
      var legend1 = document.getElementsByClassName('info legend leaflet-control')[0];
      var firstSwatch = legend1.children[1];
      firstSwatch.style.borderRadius = '50%'; // Makes it a perfect circle
      firstSwatch.style.width = '12px';      // Adjust size as desired
      firstSwatch.style.height = '12px';     // Keep width and height equal for a circle
      firstSwatch.style.backgroundColor = 'rgba(243, 233, 103, 0.3)';
      firstSwatch.style.border = '2px solid #f0e00d';
      
      var thirdSwatch = legend1.children[3];
      thirdSwatch.style.borderRadius = '50%';
      thirdSwatch.style.width = '12px';
      thirdSwatch.style.height = '12px';
      thirdSwatch.style.backgroundColor = 'rgba(214, 104, 79, 0.3)';
      thirdSwatch.style.border = '2px solid #e83024';
      
      var legend2 = document.getElementsByClassName('info legend leaflet-control')[1];
      var firstSwatch = legend2.children[1];
      firstSwatch.style.borderRadius = '50%'; // Makes it a perfect circle
      firstSwatch.style.width = '12px';      // Adjust size as desired
      firstSwatch.style.height = '12px';     // Keep width and height equal for a circle
      firstSwatch.style.backgroundColor = 'rgba(243, 233, 103, 0.3)';
      firstSwatch.style.border = '2px solid #f0e00d';
      
      var thirdSwatch = legend2.children[3];
      thirdSwatch.style.borderRadius = '50%';
      thirdSwatch.style.width = '12px';
      thirdSwatch.style.height = '12px';
      thirdSwatch.style.backgroundColor = 'rgba(214, 104, 79, 0.3)';
      thirdSwatch.style.border = '2px solid #e83024';
      
    }
  ") |> 
  addBootstrapDependency()  |> 
  addEasyButton(easyButton(
    icon = "fa-info-circle", title = "Map Information",
    onClick = JS("function(btn, map){ $('#infobox').modal('show'); }")
  ))  |> 
  htmlwidgets::appendContent(info.box)

perros



htmlwidgets::saveWidget(perros, "mapa_mordedura_perros.html",selfcontained = T, title = "Accidentes fauna feral")



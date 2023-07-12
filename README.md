# Proyecto_F

---
title: "Presencia de mamiferos en regiones socioeconomicas en Costa Rica"
author: "Ferdy Salazar y Daniela Hidalgo"
format: 
 html:
    page-layout: custom
    theme: "Lux"    
    margin-top: 40px
    margin-left: 40px
    margin-bottom: 15px        
    margin-right: 15px
server: shiny
lang: es
---

```{r}
#| label: carga-paquetes
#| context: setup
#| warning: false
#| message: false

library(tidyverse)
library(DT)
library(plotly)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(shiny)
library(rsconnect)
library(quarto)
library(viridisLite)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(readr)
library(sf)
library(terra)
library(raster)
library(rgdal)
```

```{r}
#| label: carga-datos
#| context: data
#| warning: false
#| message: false

# carga de datos

# carga regiones socioeconomicas
regiones <- 
  st_read("regiones.geojson",
          quiet = TRUE) |> 
  st_transform(4326)

# Carga de datos de mamiferos de Costa Rica
mamiferos <-
  st_read(
    "mamiferos.csv.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude", 
      "Y_POSSIBLE_NAMES=decimalLatitude"  
    ),
    quiet = TRUE
  )

# WGS84 de mamiferos  
st_crs(mamiferos) <- 4326

# union regiones y mamiferos 

mamiferos_union_regiones <-
  st_join(
    x = mamiferos,
    y = dplyr::select(regiones, region), # selección de columna cod_canton,
    join = st_within
  )

# registros presencia 

registros_presencia<-
  mamiferos_union_regiones |>
  st_drop_geometry() |>
  group_by(region) |>
  summarise(registros_presencia = n_distinct(gbifID, na.rm = TRUE)) # con n se cuentan (especie)

# union de registros y regiones

regiones_union_registros <-
  left_join(
    x = regiones,
    y = registros_presencia,
    by = "region"
  ) |>
  replace_na(list(registros_presencia = 0))

# mamiferos dentro de regiones 

mamiferos_dentro_regiones <-
  st_filter(
    x = mamiferos,
    y = regiones, 
    .predicate = st_within
  ) 

# Conversión de tipos de datos
mamiferos_union_regiones <-
	mamiferos_union_regiones |>
	mutate(
		year = as.integer(year),
		month = as.integer(month),
		day = as.integer(day)
	)

# Paleta de colores de riqueza de especies
colores_registros_presencia <-
  colorNumeric(
    palette = "Reds",
    domain = regiones_union_registros$registros_presencia,
    na.color = "transparent"
  )

# Paleta de colores de las especies
colores_mapa <- colorFactor(
  palette = viridis(length(unique(mamiferos$gbifID))), 
  domain = mamiferos$gbifID
)
```



```{r}
#| label: panel-widgets-entrada
#| panel: sidebar
#| warning: false
#| message: false

# Panel de entrada widget

# Barra lateral

# Lista de las  especies

lista_especies <- unique(mamiferos_union_regiones$species) 
lista_especies <- sort(lista_especies) #
lista_especies <- c("Todas", lista_especies) 

# Selector de especies
selectInput(
  inputId = "especie",
  label = "Especie",
  choices = lista_especies,
  selected = "Todas"
)


lista_meses <- unique(mamiferos_union_regiones$month) 
lista_meses <- sort(lista_meses) #
lista_meses <- c("Todos", lista_meses) 

#lista de meses 
lista_meses <- unique(mamiferos_union_regiones$month) 
lista_meses <- sort(lista_meses) #
lista_meses <- c("Todos", lista_meses) 

# Selector de meses

sliderInput(
	inputId = "meses_slider",
	label = "Meses",
	min = min(mamiferos_union_regiones$month, na.rm = TRUE),
	max = max(mamiferos_union_regiones$month, na.rm = TRUE),
	value = (c(3, 6)
))


lista_regiones <- unique(mamiferos_union_regiones$region) 
lista_regiones <- sort(lista_regiones) #
lista_regiones <- c("Todas", lista_regiones) 


radioButtons("region", "Selecciona una región socioeconómica:",
                   choices = lista_regiones,
                   selected = "Todas")
```

```{r}
#| label: panel-widgets-salida
#| panel: center

#mapa 
h3("Mapa de registros de presencia de especies")
# Línea vacía
HTML("<p>")

# Widget de tabla interactiva
leafletOutput(
	outputId =  "mapa" # identificador del widget
)

# tabla
# Encabezado de nivel 3
h3("Registros de presencia de especies")
# Línea vacía
HTML("<p>")

# Widget de tabla interactiva
dataTableOutput(
	outputId =  "tabla" # identificador del widget
)

h3("Gráfico de cantidad de registros de presencia en las Regiones Socioeconómicas")
HTML("<p>")

# Salida de gráfico interactivo
plotlyOutput(
	outputId =  "grafico" # identificador del widget
)
```

```{r}
#| label: servidor
#| context: server

  
# Servidor

output$mapa <- renderLeaflet({
leaflet() |>
  setView(
    lng = -84.19452,
    lat = 9.572735,
    zoom = 7) |>
  addTiles(group = "Mapa general (OpenStreetMap)") |>
  addProviderTiles(
    providers$Esri.WorldImagery, 
    group = "Imágenes satelitales (ESRI World Imagery)"
  ) |> 
  addPolygons(
    data = regiones_union_registros,
    fillColor = ~ colores_registros_presencia(regiones_union_registros$registros_presencia),
    fillOpacity = 0.7,
    color = "blue",
    stroke = TRUE,
    weight = 1.0,
    popup = paste(
      paste("<strong>Region:</strong>", regiones_union_registros$region),
      paste("<strong>Registros de presencia de especies:</strong>", regiones_union_registros$registros_presencia),
      sep = '<br/>'
    ),
    group = "Registros de presencia coropletas"
  ) |>
  addScaleBar(
    position = "bottomleft", 
    options = scaleBarOptions(imperial = FALSE)
  ) |>    
  addLegend(
    position = "bottomleft",
    pal = colores_registros_presencia,
    values = regiones_union_registros$registros_presencia,
    group = "Registros de presencia coropletas",
    title = "Registros de presencia coropletas"
  ) |>
  addCircleMarkers(
    data = mamiferos_dentro_regiones,
    stroke = F,
    radius = 4,
    fillColor = ~colores_mapa(mamiferos_dentro_regiones$gbifID),
    fillOpacity = 1.0,
    popup = paste(
      paste0("<strong>Especie: </strong>", mamiferos_dentro_regiones$species),
      paste0("<strong>Localidad: </strong>", mamiferos_dentro_regiones$locality),
      paste0("<strong>Fecha: </strong>", mamiferos_dentro_regiones$eventDate),
      paste0("<strong>Fuente: </strong>", mamiferos_dentro_regiones$institutionCode),
      paste0("<a href='", mamiferos_dentro_regiones$occurrenceID, "'>Más información</a>"),
      sep = '<br/>'
    ),    
    group = "Registros de presencia"
  ) |>  
  addLayersControl(
    baseGroups = c(
      "Mapa general (OpenStreetMap)", 
      "Imágenes satelitales (ESRI World Imagery)"
    ),
    overlayGroups = c(
      "Registros de presencia coropletas",
      "Registros de presencia de especies"
    )
  ) 
  
})


filtrar_mamiferos_union_region <- reactive({
  
  mamiferos_union_regiones_filtrados <- mamiferos_union_regiones
  
  # Filtro por especie
  if (input$especie != "Todas") {
    mamiferos_union_regiones_filtrados <-
      mamiferos_union_regiones_filtrados |>
      filter(species == input$especie)
  }
  
  # Filtro por region
  if (input$region != "Todas") {
  	mamiferos_union_regiones_filtrados <-
  		mamiferos_union_regiones_filtrados |>
  		filter(region == input$region)
  }
  
   mamiferos_union_regiones_filtrados <-
  	mamiferos_union_regiones_filtrados |>
  	filter(month == input$meses_slider)

  return(mamiferos_union_regiones_filtrados)
}) 


output$grafico <- renderPlotly({
	
  mamiferos_union_regiones <- filtrar_mamiferos_union_region()
  
	
	cantidad_registros_region <-
	  mamiferos_union_regiones |>
	  st_drop_geometry() |>
	  group_by(region) |>
	  summarize(cantidad_registros = n()) |> 
		drop_na(region) 
  
	
	# tabla 
output$tabla <- renderDataTable({
  
  
	cantidad_registros_region <-
	  mamiferos_union_regiones |>
	  st_drop_geometry() |>
	  group_by(region) |>
	  summarize(cantidad_registros = n()) |> 
		drop_na(region) 

  cantidad_registros_region |>
    dplyr::select(region, cantidad_registros) |>
    datatable(
      colnames = c("Region", "Registros de presencia de especies"),
      options = list(
        pageLength = 5,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
      )
    )
})
	
	 # Gráfico ggplot2
	grafico_ggplot2 <-
	  cantidad_registros_region |>
	  ggplot(
	    aes(
	      x = reorder(region, -cantidad_registros), 
	      y = cantidad_registros
	    )
	  ) +
	  geom_col(
	    aes(
	      text = paste0(
	        "Region", ": ", region, "\n",
	        "Cantidad de registros: ", cantidad_registros
	      )
	    )    
	  ) +
	  xlab("Region") +
	  ylab("Cantidad de registros de presencia") +  
	  theme_bw() +
	  theme(axis.text.x = element_text(angle = 45, hjust = 1))
	
# Gráfico plotly
ggplotly(grafico_ggplot2, tooltip = "text") |> 
  config(locale = 'es')	

})
```

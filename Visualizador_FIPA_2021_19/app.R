# Plataforma de visualizacion historica y actual para Lobo fino de Juan Fernandez (Arctocephalus philippii) -----
# Proyecto FIPA 2021-19 
# Autor del codigo: Jose A. Lastra
# Laboratorio Geo-Informacion y Percepción Remota PUCV (https://www.labgrs.pucv.cl/)
# 
# __________________________________________________________________________________#
# Inicio de la aplicacion ----
# __________________________________________________________________________________#

# 1.- Librerias y funciones auxiliares -----
library(shiny) 
library(shinyjs)
library(leaflet)
library(tidyverse)
library(leaflegend)
library(shinythemes)
library(shinyalert)
library(DT)
library(leafpop)
options("rgdal_show_exportToProj4_warnings" = "none")
library(rgdal)
library(raster)
library(plotly)
library(sf)
library(shinyWidgets)
library(shinybusy)
library(dqshiny)
source("RFUN/pieChart.R") # funcion para pieChart datos actuales
source("RFUN/tableChart.R")# funcion para creacion de tabla datos actuales
source("RFUN/growth_rate.R") #funcion para aplicacion de modelos
source("RFUN/get_params.R") #funcion complementaria
source('RFUN/growth_plot.R') #funcion construccion de plot de modelos
# __________________________________________________________________________________#
# 2.- Carga de datos base ----
shp <- read_sf("archivos/comuna JF.gpkg") #comunas
lobos_data_hist <- st_read('archivos/Censo_historico_8_20221114.gpkg') #data historica playas
islas_data <- read_sf("archivos/Island_historic_8_20221114.gpkg",layer = 'Capa unida') # data historica islas
labels_map <- read_sf('archivos/labels.gpkg') %>% #etiquetas para playas 
  mutate(x = st_coordinates(.)[1:41,1],y = st_coordinates(.)[1:41,2])
nombres <- shp$ISLA %>% unique() # nombres de islas
load("archivos/tablaPie_2021.RData") # tabla datos 2021
b64 <- base64enc::dataURI(file = "description.html", mime = "text/html") #encoding info del proyecto

# __________________________________________________________________________________#
# 3.- Interfaz de usuario -----
ui <- navbarPage(
  title = '',
  windowTitle = "FIPA 2021-19",
  theme = shinytheme("cosmo"),
## 3.1.- Ventana informacion general del proyecto ----
  tabPanel(
    "Información del proyecto",
    tags$iframe(
      src = b64,
      width = "100%", height = "950px",
      frameborder = 0, scrolling = "auto"
    )
  ),
## 3.2.- Ventana informacion historica (mapa) ----
  tabPanel(
    "Registros históricos",
    add_loading_state(".leaflet",
      text = "Loading datasets...",
      svgColor = "steelblue", timeout = 5500
    ),
    add_loading_state("#series",
      text = "Loading time series plot...",
      svgColor = "steelblue", timeout = 1500
    ),
    tags$style(
      "#map { cursor: crosshair;}
                                     #controls {background-color: white;padding: 0 20px 20px 20px;
                                     cursor: move; opacity: 0.75; zoom: 0.9;transition: opacity 500ms 2s;}
                                      #controls:hover {opacity: 0.95;  transition-delay: 0;}"
    ),
    div(
      class = "outer",
      tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0;
                                                right: 0; bottom: 0; overflow: hidden; padding: 0}"),
      leafletOutput(outputId = "map", width = "100%", height = "100%"),
      fixedPanel(top = 40, right = 2,
                 actionButton('plotBtn', 'Mostrar panel', "data-toggle"='collapse', "data-target"='#controls',
                              style="z-index:500; opacity: .80; color: #fff; background-color: #a662e3; border-color: #a153e5")),
      absolutePanel(
        id = "controls",
        style = "z-index:500;overflow-y: scroll;",
        class = "panel panel-default",
        fixed = T,
        draggable = F, top = 90, left = "auto",
        right = 1, bottom = 'auto',
        width = 500, height = "auto",
        ## user's controls
        h4(strong("Opciones del mapa")),
        useShinyjs(),
        pickerInput(
          inputId = "data",
          label = "Información a desplegar",
          choices = c("Lobera", "Isla"), width = "95%", selected = "Lobera",
          options = list(
            title = "Seleccione sus datos"
          )
        ),
        pickerInput(
          inputId = "sites",
          label = "Islas",
          choices = nombres, selected = "Robinson Crusoe",
          width = "95%",
          options = list(
            title = "Elija una de las islas"
          )
        ),
        pickerInput(
          inputId = "indiv",
          label = "Seleccione una categoría para visualizar",
          choices = list(
            "Total individuos" = "total",
            "Total adultos" = "total_adultos",
            "Total cachorros" = "total_cachorros"
          ),
          multiple = TRUE, width = "95%",
          options = list(
            title = "Elija una opción de la lista",
            style = "btn-primary"
          )
        ), hr(),
        prettySwitch(
          inputId = "curvas",
          label = "Agregar curvas",
          status = "success",value = F,
          fill = TRUE
        ),
        checkboxGroupButtons(
          inputId = "modelos",
          label = "Modelos disponibles", width = "95%",
          choices = c(
            "Exponencial",
            "Competencia"), 
          checkIcon = list(
            yes = tags$i(
              class = "fa fa-check-square",
              style = "color: steelblue"
            ),
            no = tags$i(
              class = "fa fa-square-o",
              style = "color: steelblue"
            )
          )
        ), hr(),
        conditionalPanel
        (
          condition = "0==1",
          sliderInput("dummyslider", "", min = 0, max = 1, value = 0)
        ),
        uiOutput("years_list"),
        hr(),
        h6(strong(a("Desarrollado por José A. Lastra LabGRS - PUCV",
                         href = "https://www.pucv.cl/uuaa/site/edic/base/port/labgrs.html"))),
      ),
      # fixedPanel(top = 52.5,bottom = 'auto',left = 45,width = 375,
      #            HTML('<img height="100"  src="logos.png" asp="1" class="img-responsive" align="left">'))
      div(id = "cp1", conditionalPanel(
        condition = "input.map_click != 0",
        absolutePanel(
          id = "tSeries",
          style = "z-index:500;background-color: transparent;
                         opacity: 1;margin: auto;border-color: transparent;padding-bottom: 2mm;
                                                         padding-top: 1mm;",
          class = "panel panel-default",
          fixed = F, draggable = F, top = "auto", left = 5,
          right = 5, bottom = 5, width = "100%", height = "auto",
          actionBttn(
            inputId = "clse",
            label = "Close",
            style = "unite", size = "sm",
            color = "danger"
          ),
          plotlyOutput("series", width = "auto", height = 250)
        )
      )),
      fixedPanel(top = 52,left = 45,right = 'auto',bottom = 'auto',
                 img(src = "fipa_costa.png", style = "class = img-responsive", height = 80))
    ),
    
    useShinyalert(force = T)
  ),
## 3.3.- Ventana registros actuales ----
  tabPanel(
    "Registros actuales",
    sidebarLayout(
      sidebarPanel(
        # select spatial aggregation
        awesomeRadio(
          inputId = "scaleSelect",
          label = "Seleccione agregación espacial",
          choices = c("Lobera", "Islas"),
          selected = "Islas",
          inline = TRUE,
          checkbox = TRUE
        ),
        pickerInput(
          inputId = "sitios",
          label = "Seleccione isla",
          choices = nombres, selected = "Robinson Crusoe",
          options = list(
            title = "Elija una de las islas"
          )
        ),
        uiOutput("beachList"),
        pickerInput( # field choices
          inputId = "plotChoices",
          label = "Seleccione campos a visualizar",
          choices = names(tabla_pie_2021)[3:8], width = "95%",
          options = list(
            title = "Elija una opción de la lista", `selected-text-format` = "count > 1",
            multiple = T
          ),
          multiple = TRUE,
          choicesOpt = list(content = paste0(
            "<div style='font-weight: bold;'>",
            names(tabla_pie_2021)[3:8], "</div>"
          ))
        ), hr(), HTML('<div style="text-align: center;">
          <img src="fipa_grey.png" alt="logo" style="class = img-responsive; height: 150px;">
          </div>'),
        width = 2
      ),
      mainPanel(
      dataTableOutput("tablePlot"),hr(), 
      column(plotlyOutput(outputId = "piePlot"),width = 5,offset = 1),
      div(conditionalPanel(condition = "input.plotChoices != 0",
        column(HTML('<div style="text-align: center;">
          <img src="lobos_fino.JPG" alt="logo" style="class = img-responsive; height: 300px;">
          </div>'),width = 2, offset = 1)))
        
      )
    )
  ),
## 3.4.- Ventana interacciones lobo-humano ----
  tabPanel("Lobo-Humano",
    tags$iframe("Lobo-Humano",
      src = 'lobo_humano.html',
      width = "100%", height = "900px",
      frameborder = 0, scrolling = "auto"
    )
  ),
## 3.5.- Menu ventanas adicionales ----
  navbarMenu(
    title = "Más información",
### 3.5.1.- Ventana equipo y colaboradores ----
    tabPanel("Equipo y colaboradores",
             tags$iframe(
               src = 'equipo.html',
               width = "100%", height = "900px",
               frameborder = 0, scrolling = "auto"
             )),
### 3.5.2.- Ventana documentos adicionales ----
    tabPanel("Documentos adicionales")
  ),tags$script(HTML("var header = $('.navbar> .container-fluid');
                       header.append('<div style=\"float:right;color: white\"><a href=https://github.com/JoseLastra><h6>Desarrollado por: José A. Lastra</h6></a></div>');
                       console.log(header)"))
)

# __________________________________________________________________________________#
# 4.- Servidor -----
server <- function(input, output, session) {

# __________________________________________________________________________________#
## 4.1.- Panel mapa reactivo base -----
### 4.1.1.- Render mapa base -----
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark Matter") %>%
      addProviderTiles(providers$OpenStreetMap, group = "Open Street Map") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri TopoMap") %>%
      addProviderTiles(providers$Esri.WorldImagery,
        group = "Esri Imagery"
      ) %>%
      addProviderTiles(providers$CartoDB.PositronOnlyLabels,
        group = "Esri Imagery"
      ) %>%
      addLayersControl(
        position = "topleft",
        baseGroups = c("Esri TopoMap", "CartoDB Dark Matter", "Open Street Map", "Esri Imagery")
      ) %>%
      fitBounds(lng1 = -78.9, lng2 = -78.6, lat1 = -33.6, lat2 = -33.7) %>% 
      addLabelOnlyMarkers(data = labels_map,lng = ~x,lat =~y , label =  ~as.character(lobera), 
                          labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T),group = 'labels')
  })


### 4.1.2.- Extent reactivo mapa base -----
  observeEvent(c(input$sites, input$data), {
    proxyMap <- leafletProxy("map")
    if (input$data == "Lobera") {
      area_ext <- shp %>%
        filter(ISLA == input$sites) %>%
        st_bbox() %>%
        as.vector()

      proxyMap <- proxyMap %>%  
      fitBounds(lng1 = area_ext[1], lng2 = area_ext[3], lat1 = area_ext[4], lat2 = area_ext[2])
    }
    if (input$data == "Isla") {
      area_ext <- shp %>%
        st_bbox() %>%
        as.vector()
      proxyMap <- proxyMap %>% setView(lng = -79.83, lat = -33.70, zoom = 10)
    }
    proxyMap 
  })

## 4.2.- Esconder/mostrar opciones segun seleccion de usuario -----
### 4.2.1.- Esconder/mostrar loberas -----
  observeEvent(input$data, {
    if (input$data == "Isla") {
      shinyjs::hide(id = "sites")
    } else {
      shinyjs::show(id = "sites")
    }
  })

### 4.2.2.- Esconder/mostrar opcion de aplicar modelos -----
  observeEvent(input$data, {
    if (input$data == "Isla") {
      shinyjs::show(id = "curvas")
    } else {
      shinyjs::hide(id = "curvas")
    }
  })
### 4.2.3.- Esconder/mostrar loberas -----
  observeEvent(input$curvas, {
    if (isFALSE(input$curvas)) {
      shinyjs::hide(id = "modelos")
    } else {
      shinyjs::show(id = "modelos")
    }
  })
  # __________________________________________________________________________________#
## 4.3.- Ajustar temporalidad de datos -----

  anios <- reactive({
    if(input$data == 'Lobera'){
      load(file = 'archivos/yearsObjects_loberas.RData')
    }
    
    if(input$data == 'Isla'){
      load(file = 'archivos/yearsObjects_islas.RData')
    }
    
    if (input$sites == "Santa Clara") {
      uni_yy <- clara_yy
    }
    if (input$sites == "Robinson Crusoe") {
      uni_yy <- robinson_yy
    }
    if (input$sites == "Alejandro Selkirk") {
      uni_yy <- selkirk_yy
    }

    return(uni_yy)
  })

  output$years_list <- renderUI({
    uni_yy <- anios()

    sliderTextInput(
      inputId = "years", label = "Seleccione un año", choices = uni_yy,
      selected = head(rev(uni_yy), 1), grid = T, force_edges = T, width = "95%"
    )
  })
  
## 4.4.- Listado de loberas disponibles -----
  output$beachList <- renderUI({
    loberas <- tabla_pie_2021 %>% filter(isla == input$sitios)
    loberas_list <- loberas$lobera %>% unique()

    pickerInput(
      inputId = "beach",
      label = "Seleccione lobera",
      choices = loberas_list,
      options = list(
        title = "Elija una opción de la lista"
      ),
      choicesOpt = list(content = paste0(
        "<div style='font-weight: bold;'>",
        loberas_list, "</div>"
      ))
    )
  })
  # __________________________________________________________________________________#
## 4.5.- Filtro de datos segun seleccion en UI -----
### 4.5.1.- Filtro escala playas ----
  filter_seals <- eventReactive(c(input$sites, input$indiv), {
    lista_selected <- input$indiv %>% unlist()
    nombres <- names(lista_selected)

    shp_filtered <- lobos_data_hist %>%
      filter(isla == input$sites) %>%
      select(all_of(c("lobera", "tipologia", "codigo", "importancia", "year", lista_selected)))

    names(shp_filtered) <- c("Lobera", "Tipologia", "Codigo", "Importancia", "Year", lista_selected, "geom")

    shp_filtered <- shp_filtered %>%
      reshape2::melt(id.vars = c("Year", "Codigo", "Tipologia", "Lobera", "Importancia", "geom")) %>%
      st_as_sf()
    return(shp_filtered)
  })
  
### 4.5.2.- Filtro escala Islas ----
  filter_island <- eventReactive(c(input$sites, input$indiv), {
    lista_selected <- input$indiv %>% unlist()
    nombres <- names(lista_selected)

    shp_filtered <- islas_data %>%
      select(all_of(c("Island", "year", lista_selected)))

    names(shp_filtered) <- c("Isla", "Year", lista_selected, "geom")

    shp_filtered <- shp_filtered %>%
      reshape2::melt(id.vars = c("Isla", "Year", "geom")) %>%
      st_as_sf()
    return(shp_filtered)
  })
  # __________________________________________________________________________________#
## 4.6.- filtro por categoria seleccionada -----

### 4.6.1.- Cachorros -----
  cachorros <- eventReactive(c(input$years, input$sites), {
    req(filter_seals())
    req(input$years)
    req(!is.null(input$indiv[[1]]))

    yy <- anios()[which(anios() >= input$years)][1] %>% as.numeric()
    yy_filter_seal <- filter_seals() %>%
      filter(variable == "total_cachorros" & Year == yy)
    return(yy_filter_seal)
  })
  islas_cachorros <- eventReactive(c(input$years, input$sites), {
    req(filter_island())
    req(!is.null(input$indiv[[1]]))
    req(input$years)

    yy <- anios()[which(anios() >= input$years)][1] %>% as.numeric()

    yy_filter_island <- filter_island() %>%
      filter(Year == yy & variable == "total_cachorros")
    return(yy_filter_island)
  })

### 4.6.2.- Adultos -----
  adultos <- eventReactive(c(input$years, input$sites), {
    req(filter_seals())
    req(!is.null(input$indiv[[1]]))
    req(input$years)

    yy <- anios()[which(anios() >= input$years)][1] %>% as.numeric()
    yy_filter_seal <- filter_seals() %>%
      filter(variable == "total_adultos" & Year == yy)
    return(yy_filter_seal)
  })
  islas_adultos <- eventReactive(c(input$years, input$sites), {
    req(filter_island())
    req(!is.null(input$indiv[[1]]))
    req(input$years)

    yy <- anios()[which(anios() >= input$years)][1] %>% as.numeric()

    yy_filter_island <- filter_island() %>%
      filter(Year == yy & variable == "total_adultos")
    return(yy_filter_island)
  })
### 4.6.3.- Total individuos -----
  total <- eventReactive(c(input$years, input$sites), {
    req(filter_seals())
    req(!is.null(input$indiv[[1]]))
    req(input$years)

    yy <- anios()[which(anios() >= input$years)][1] %>% as.numeric()

    yy_filter_seal <- filter_seals() %>%
      filter(Year == yy & variable == "total")
    return(yy_filter_seal)
  })

  islas_total <- eventReactive(c(input$years, input$sites), {
    req(filter_island())
    req(!is.null(input$indiv[[1]]))
    req(input$years)

    yy <- anios()[which(anios() >= input$years)][1] %>% as.numeric()

    yy_filter_island <- filter_island() %>%
      filter(Year == yy & variable == "total")
    return(yy_filter_island)
  })

# __________________________________________________________________________________#
## 4.7.- Creacion serie de tiempo -----
### 4.7.1.- construccion de tabla ----
  data <- eventReactive(c(input$map_marker_click, input$indiv, input$sites, input$data), {
    req(!is.null(input$indiv[[1]]))

    click <- input$map_marker_click %>% unlist()

    dts <- st_as_sf(data.frame(lng = click[4], lat = click[3]),
      coords = c("lng", "lat"),
      crs = 4326
    )
    lista_variable <- input$indiv %>% unlist()
    if (length(lista_variable) == 1) {
      lista_selected <- input$indiv %>% unlist()
    }

    if (length(lista_variable) > 1 & length(lista_variable) <= 2) {
      check_list <- input$indiv %>%
        unlist() %>%
        as.vector()

      if (identical(check_list, c("total_cachorros", "total_adultos")) | identical(check_list, c("total_adultos", "total_cachorros"))) {
        lista_selected <- input$indiv %>% unlist()
      }

      if (identical(check_list, c("total_cachorros", "total")) | identical(check_list, c("total", "total_cachorros"))) {
        lista_selected <- unlist(input$indiv)[which(unlist(input$indiv) == "total")]
      }

      if (identical(check_list, c("total", "total_adultos")) | identical(check_list, c("total_adultos", "total"))) {
        lista_selected <- unlist(input$indiv)[which(unlist(input$indiv) == "total")]
      }
    }

    if (length(lista_variable) > 2) {
      lista_selected <- unlist(input$indiv)[which(unlist(input$indiv) == "total")]
    }

    if (input$data == "Lobera") {
      fila <- st_nearest_feature(dts, filter_seals())

      code.select <- filter_seals()[fila, ] %>%
        as.data.frame() %>%
        select(Codigo, -geom) %>%
        unlist() %>%
        as.vector()
      data.select <- filter_seals() %>%
        filter(Codigo == code.select & variable %in% lista_selected) %>%
        as.data.frame() %>%
        select(-geom) %>%
        select(all_of(c("Year", "Codigo", "Tipologia", "Lobera", "variable", "value")))

      names(data.select) <- c("Year", "Codigo", "Tipologia", "Lobera", "variable", "value")
    }
    if (input$data == "Isla") {
      fila <- st_nearest_feature(dts, filter_island())

      code.select <- filter_island()[fila, ] %>%
        as.data.frame() %>%
        select(Isla, -geom) %>%
        unlist() %>%
        as.vector()
      data.select <- filter_island() %>%
        filter(Isla == code.select & variable %in% lista_selected) %>%
        as.data.frame() %>%
        select(-geom) %>%
        select(all_of(c("Isla", "Year", "variable", "value")))

      names(data.select) <- c("Isla", "Year", "variable", "value")
    }
  
    data.select
  })
  
### 4.7.2.- Render de serie de tiempo ----
  output$series <- renderPlotly({
    req(input$map_marker_click)
    req(!is.null(input$indiv[[1]]))

    lista_indiv <- input$indiv %>%
      unlist() %>%
      as.vector()
    
    #reactive functions plot
    ## R curves
    if(input$curvas == TRUE & length(input$modelos) >= 1 & 'total' %in% lista_indiv){
      plot_serie <- growth_plot(tabla = data(), modelo = isolate(input$modelos))
    }
    
    if(input$curvas == TRUE & length(input$modelos) >= 1 & !'total' %in% lista_indiv){
      plot_serie <- growth_plot(tabla = data(), modelo = isolate(input$modelos))
    }
    
    if(input$curvas == F | (length(input$modelos) == 0 & input$curvas == T)){
    ## absolute numbers series
    plot.data <- data()

    g <- ggplot(data = plot.data, aes(x = Year, y = value, fill = variable)) +
      geom_bar(stat = "identity") +
      theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90)
      ) +
      ylab("N° de individuos") +
      scale_x_continuous(name = "Años", breaks = plot.data$Year %>% unique())

    if (length(lista_indiv) == 1) {
      if (lista_indiv == "total") {
        h <- g + scale_fill_manual(values = "red")
      }
      if (lista_indiv == "total_adultos") {
        h <- g + scale_fill_manual(values = "yellow")
      }
      if (lista_indiv == "total_cachorros") {
        h <- g + scale_fill_manual(values = "blue")
      }
    }
    if (length(lista_indiv) > 1) {
      if (identical(lista_indiv, c("total_cachorros", "total_adultos")) | identical(lista_indiv, c("total_adultos", "total_cachorros")) & !"total" %in% lista_indiv) {
        h <- g + scale_fill_manual(values = c("yellow", "blue"))
      }
      if ("total" %in% lista_indiv) {
        h <- g + scale_fill_manual(values = "red")
      }
    }

    plot_serie <- ggplotly(h, tooltip = c("Year", "value"))
    }
    
    plot_serie
  })

### 4.7.3.- Remover panel de ts ----
  observeEvent(input$map_marker_click, {
    shinyjs::show("cp1")
    shinyjs::show("clse")
  })
  observeEvent(c(input$sites, input$data, input$clse), {
    shinyjs::hide("cp1")
  })

# __________________________________________________________________________________#

## 4.8.- Mapa reactivo para islas y loberas ----

  observeEvent(c(input$sites, input$indiv, input$years, input$data), {
    proxyMap <- leafletProxy("map") %>%
      clearControls() %>%
      clearShapes() %>%
      clearMarkers() %>% 
      addLabelOnlyMarkers(data = labels_map,lng = ~x,lat =~y , label =  ~as.character(lobera), 
                          labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T),group = 'labels')


    if (length(unlist(input$indiv)) > 0) {
      check_data <- input$indiv %>%
        unlist() %>%
        as.vector()

      if (length(check_data) == 1) {
        capa_select <- input$indiv %>% unlist()

        if ("total" == capa_select) {
          # total
          if (input$data == "Isla") {
            tabla_total <- islas_total() %>% as.data.frame()
            shp_map <- islas_total()
            popSelect <- c(1, 2, 4)
          }
          if (input$data == "Lobera") {
            tabla_total <- total() %>% as.data.frame()
            shp_map <- total()
            popSelect <- c(1:5, 7)
          }
          data_columna <- tabla_total %>%
            select(value, -geom) %>%
            unlist() %>%
            na.omit() %>% 
            as.numeric()

          symbols <- makeSymbolsSize(
            values = data_columna,
            shape = "circle",
            color = "red",
            fillColor = "red",
            opacity = .8,
            baseSize = 10
          )
### 4.8.1.- Total individuos -----
          proxyMap <- proxyMap %>%
            clearGroup(group = c("Adultos", "Cachorros")) %>%
            addMarkers(data = shp_map, group = "Total", icon = symbols, popup = popupTable(shp_map, row.numbers = F, feature.id = F, zcol = popSelect)) %>%
            addLegendSize(
              values = data_columna,
              color = "red",
              fillColor = "red",
              opacity = .8,
              title = "Número de individuos",
              shape = "circle",
              orientation = "vertical",
              breaks = 5, group = 'Leyenda', baseSize = 10
            ) %>%
            addLayersControl(
              baseGroups = c("Esri TopoMap", "CartoDB Dark Matter", "Open Street Map", "Esri Imagery"),
              overlayGroups = c("Total"), position = "topleft"
            )
          
        }
        if ("total_adultos" == capa_select | "total_cachorros" == capa_select) {
          if ("total_adultos" == capa_select) {
            
### 4.8.2.- Total adultos ----
            if (input$data == "Isla") {
              tabla_adultos <- islas_adultos() %>% as.data.frame()
              shp_map <- islas_adultos()
              popSelect <- c(1, 2, 4)
            }
            if (input$data == "Lobera") {
              tabla_adultos <- adultos() %>% as.data.frame()
              shp_map <- adultos()
              popSelect <- c(1:5, 7)
            }
            data_columna2 <- tabla_adultos %>%
              select(value, -geom) %>%
              unlist() %>%
              na.omit() %>% 
              as.numeric()

            symbols2 <- makeSymbolsSize(
              values = data_columna2,
              shape = "circle",
              color = "yellow",
              fillColor = "yellow",
              opacity = .8,
              baseSize = 10
            )
            proxyMap <- proxyMap %>%
              clearGroup(c("Total")) %>%
              addMarkers(data = shp_map, group = "Adultos", icon = symbols2, popup = popupTable(shp_map, row.numbers = F, feature.id = F, zcol = popSelect)) %>%
              addLegendSize(
                values = data_columna2,
                color = "yellow",
                fillColor = "yellow",
                opacity = .8,
                title = "Número de adultos",
                shape = "circle",
                orientation = "vertical",
                breaks = 5, group = 'Leyenda', baseSize = 10
              ) %>%
              addLayersControl(
                baseGroups = c("Esri TopoMap", "CartoDB Dark Matter", "Open Street Map", "Esri Imagery"),
                overlayGroups = c("Adultos"), position = "topleft"
              )
            if (length(unique(data_columna2)) == 1) {
                clearGroup(c("Total")) %>%
                addCircleMarkers(data = shp_map, group = "Adultos", popup = popupTable(shp_map, row.numbers = F, feature.id = F, zcol = popSelect), color = "yellow", fillOpacity = 0, weight = 0.5) %>%
                addLayersControl(
                  baseGroups = c("Esri TopoMap", "CartoDB Dark Matter", "Open Street Map", "Esri Imagery"),
                  overlayGroups = c("Adultos"), position = "topleft"
                ) %>%
                addLegend(position = "topleft", colors = "yellow", labels = "Ningún individuo",group = 'Leyenda')
            }
          }
          if ("total_cachorros" == capa_select) {
            
### 4.8.3.- Total cachorros -----
            if (input$data == "Isla") {
              tabla_cachorros <- islas_cachorros() %>% as.data.frame()
              shp_map <- islas_cachorros()
              popSelect <- c(1, 2, 4)
            }
            if (input$data == "Lobera") {
              tabla_cachorros <- cachorros() %>% as.data.frame()
              shp_map <- cachorros()
              popSelect <- c(1:5, 7)
            }

            data_columna3 <- tabla_cachorros %>%
              select(value, -geom) %>%
              unlist() %>%
              na.omit() %>% 
              as.numeric()

            if (length(unique(data_columna3)) > 1) {
              symbols3 <- makeSymbolsSize(
                values = data_columna3,
                shape = "circle",
                color = "blue",
                fillColor = "blue",
                opacity = .8,
                baseSize = 10
              )

              proxyMap <- proxyMap %>%
                clearGroup(c("Total")) %>%
                addMarkers(data = shp_map, group = "Cachorros", icon = symbols3, popup = popupTable(shp_map, row.numbers = F, feature.id = F, zcol = popSelect)) %>%
                addLegendSize(
                  values = data_columna3,
                  color = "blue",
                  fillColor = "blue",
                  opacity = .8,
                  title = "Número de cachorros",
                  shape = "circle",
                  orientation = "vertical",
                  breaks = 5, group = 'Leyenda', baseSize = 10
                ) %>%
                addLayersControl(
                  baseGroups = c("Esri TopoMap", "CartoDB Dark Matter", "Open Street Map", "Esri Imagery"),
                  overlayGroups = c("Cachorros"), position = "topleft"
                )
            }
            if (length(unique(data_columna3)) == 1) {
              proxyMap <- proxyMap %>%
                clearGroup(c("Total")) %>%
                addCircleMarkers(data = shp_map, group = "Cachorros", popup = popupTable(shp_map, row.numbers = F, feature.id = F, zcol = popSelect), color = "blue", fillOpacity = 0, weight = 0.5) %>%
                addLayersControl(
                  baseGroups = c("Esri TopoMap", "CartoDB Dark Matter", "Open Street Map", "Esri Imagery"),
                  overlayGroups = c("Cachorros"), position = "topleft"
                ) %>%
                addLegend(position = "topleft", colors = "blue", labels = "Ningún individuo",group = 'Leyenda')
            }
          }
        }
      }
      if (length(check_data) > 1 & length(check_data) <= 3) {
        capa_select <- input$indiv %>%
          unlist() %>%
          as.vector()
        if (identical(capa_select, c("total_cachorros", "total_adultos")) | identical(capa_select, c("total_adultos", "total_cachorros")) & !"total" %in% capa_select) {
### 4.8.4.- Total cachorros y adultos -----
#### 4.8.4.1.- Adultos ----
          if (input$data == "Isla") {
            tabla_adultos <- islas_adultos() %>% as.data.frame()
            shp_mapA <- islas_adultos()
            popSelectA <- c(1, 2, 4)
          }
          if (input$data == "Lobera") {
            tabla_adultos <- adultos() %>% as.data.frame()
            shp_mapA <- adultos()
            popSelectA <- c(1:5, 7)
          }
          data_columna2 <- tabla_adultos %>%
            select(value, -geom) %>%
            unlist() %>%
            na.omit() %>% 
            as.numeric()

          symbols2 <- makeSymbolsSize(
            values = data_columna2,
            shape = "circle",
            color = "yellow",
            fillColor = "yellow",
            opacity = .8,
            baseSize = 10
          )
          proxyMap <- proxyMap %>%
            clearGroup(c("Total", "Cachorros", "Adultos")) %>%
            clearControls() %>%
            addMarkers(data = shp_mapA, group = "Adultos", icon = symbols2, popup = popupTable(shp_mapA, row.numbers = F, feature.id = F, zcol = popSelectA)) %>%
            addLegendSize(
              values = data_columna2,
              color = "yellow",
              fillColor = "yellow",
              opacity = .8,
              title = "Número de Adultos",
              shape = "circle",
              orientation = "vertical",
              breaks = 5, group = 'Leyenda', baseSize = 10
            )
#### 4.8.4.2.- Cachorros ----
          if (input$data == "Isla") {
            tabla_cachorros <- islas_cachorros() %>% as.data.frame()
            shp_mapC <- islas_cachorros()
            popSelectC <- c(1, 2, 4)
          }
          if (input$data == "Lobera") {
            tabla_cachorros <- cachorros() %>% as.data.frame()
            shp_mapC <- cachorros()
            popSelectC <- c(1:5, 7)
          }
          data_columna3 <- tabla_cachorros %>%
            select(value, -geom) %>%
            unlist() %>%
            na.omit() %>% 
            as.numeric()


          if (length(unique(data_columna3)) > 1) {
            symbols3 <- makeSymbolsSize(
              values = data_columna3,
              shape = "circle",
              color = "blue",
              fillColor = "blue",
              opacity = .8,
              baseSize = 10
            )

            proxyMap <- proxyMap %>%
              clearGroup(c("Total")) %>%
              addMarkers(data = shp_mapC, group = "Cachorros", icon = symbols3, popup = popupTable(shp_mapC, row.numbers = F, feature.id = F, zcol = popSelectC)) %>%
              addLegendSize(
                values = data_columna3,
                color = "blue",
                fillColor = "blue",
                opacity = .8,
                title = "Número de cachorros",
                shape = "circle",
                orientation = "vertical",
                breaks = 5, group = 'Leyenda', baseSize = 10
              ) %>%
              addLayersControl(
                baseGroups = c("Esri TopoMap", "CartoDB Dark Matter", "Open Street Map", "Esri Imagery"),
                overlayGroups = c("Adultos", "Cachorros"), position = "topleft"
              )
          }
          if (length(unique(data_columna3)) == 1 & length(unique(data_columna2)) == 1) {
            shp_noData <- lobos_data_hist %>% filter(isla == input$sites)
            popSelectA <-c(1:4)
            proxyMap <- proxyMap %>%
              clearGroup(c("Total")) %>%
              addCircleMarkers(data = shp_mapC, group = "Cachorros", popup = popupTable(shp_mapC, row.numbers = F, feature.id = F, zcol = popSelectC), color = "blue") %>%
              addCircleMarkers(data = shp_noData, group = "Adultos", popup = popupTable(shp_noData, row.numbers = F, feature.id = F, zcol = popSelectA), color = "yellow") %>% 
              
              addLayersControl(
                baseGroups = c("Esri TopoMap", "CartoDB Dark Matter", "Open Street Map", "Esri Imagery"),
                overlayGroups = c("Adultos", "Cachorros"), position = "topleft"
              ) %>%
              addLegend(position = "topleft", colors = c("blue", 'yellow'), labels = c("Ningún cachorro", 'Ningún Adulto'),group = 'Leyenda')
          }
          
        }
        if ("total" %in% capa_select | length(capa_select) == 3) {
### 4.8.5.- Mostrar solo total individuos si hay 3 selecciones ----
          if (input$data == "Isla") {
            tabla_total <- islas_total() %>% as.data.frame()
            shp_map <- islas_total()
            popSelect <- c(1, 2, 4)
          }
          if (input$data == "Lobera") {
            tabla_total <- total() %>% as.data.frame()
            shp_map <- total()
            popSelect <- c(1:5, 7)
          }
          data_columna <- tabla_total %>%
            select(value, -geom) %>%
            unlist() %>%
            na.omit() %>% 
            as.numeric()

          symbols <- makeSymbolsSize(
            values = data_columna,
            shape = "circle",
            color = "red",
            fillColor = "red",
            opacity = .8,
            baseSize = 10
          )

          proxyMap <- proxyMap %>%
            clearGroup(group = c("Adultos", "Cachorros")) %>%
            addMarkers(data = shp_map, group = "Total", icon = symbols, popup = popupTable(shp_map, row.numbers = F, feature.id = F, zcol = popSelect)) %>%
            addLegendSize(
              values = data_columna,
              color = "red",
              fillColor = "red",
              opacity = .8,
              title = "Número de individuos",
              shape = "circle",
              orientation = "vertical",
              breaks = 5, group = 'Leyenda', baseSize = 10
            ) %>%
            addLayersControl(
              baseGroups = c("Esri TopoMap", "CartoDB Dark Matter", "Open Street Map", "Esri Imagery"),
              overlayGroups = c("Total"), position = "topleft"
            )
        }
      }
### 4.8.7.- cuando no hay selecciones ----
    }
    if (is.null(unlist(input$indiv))) {
      proxyMap <- proxyMap %>%
        clearMarkers() %>%
        clearControls()%>% 
        addLabelOnlyMarkers(data = labels_map,lng = ~x,lat =~y , label =  ~as.character(lobera), 
                            labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T),group = 'labels')
      
    }
    proxyMap
  })

#_______________________________________________________________________________________________#
### Anexo Pop up con informacion -----
  shinyalert(
    text = paste(
      '<h4 style="text-align:justify; color: black">',
      "Bienvenido/as! <br> <hr>El objetivo de la siguiente plataforma es entregar información dinámica actual e histórica de la población de lobo fino de Juan Fernández (<i>Arctocephalus philippii</i>). Específicamente, la página entrega registro histórico de la totalidad de los censos realizados en el archipiélago por diferentes autores, los que pueden disgregarse por isla, lobera y año de muestreo dependiendo del interés del usuario/a. También, entregamos algunas gráficas específicas al censo 2021 y algunos de los resultados de las evaluaciones de interacciones entre el lobo fino y la población humana en el archipiélago. El proyecto y la construcción de la presente plataforma fueron realizados bajo el financiamiento <b>FIPA 2021-19 “Censo de lobo fino de Juan Fernández (<i>Arctocephalus philippii</i>) en el archipiélago de Juan Fernández”</b> liderado por la ONG Costa Humboldt. La pestaña colaboradores contiene la lista completa de colaboradores. Para consultas escribirnos a: <a href ='mailto:Lobofinojf@costahumboldt.org'>Lobofinojf@costahumboldt.org</a>", "</h4>", "</br>"
    ),
    type = "info", html = T,
    animation = "slide-from-bottom", size = "m"
  )

# _____________________________________________________________________________________________________#
## 4.9.- Panel Registros actuales -----
### 4.9.1. - Esconder/mostrar opciones de seleccion ----
  observeEvent(input$scaleSelect, {
    if (input$scaleSelect == "Islas") {
      shinyjs::hide(id = "beachList")
    }
    if (input$scaleSelect == "Lobera") {
      shinyjs::show(id = "beachList")
    }
  })

### 4.9.2.- Seleccion de parametros y creacion de grafico ----
  plotData <- eventReactive(c(input$scaleSelect, input$sitios, input$beach, input$plotChoices), {
    req(length(input$plotChoices) > 0)
    if (input$scaleSelect == "Lobera" & length(input$beach) > 0) {
      tabla <- tabla_pie_2021 %>% filter(isla == input$sitios & lobera == input$beach)
      field.list <- input$plotChoices
      scale <- input$scaleSelect

      grafico <- plot_pie(tabla = tabla, field.list = field.list, scale = scale)
    }
    if (input$scaleSelect == "Islas") {
      tabla <- tabla_pie_2021 %>%
        filter(isla == input$sitios) %>%
        dplyr::select(-lobera)
      field.list <- input$plotChoices
      scale <- input$scaleSelect

      grafico <- plot_pie(tabla = tabla, field.list = field.list, scale = scale)
    }


    return(grafico)
  })

### 4.9.3.- Seleccion de parametros y filtro de tabla ----
  tablaData <- eventReactive(c(input$scaleSelect, input$sitios, input$beach, input$plotChoices), {
    req(length(input$plotChoices) > 0)
    if (input$scaleSelect == "Lobera" & length(input$beach) > 0) {
      tabla <- tabla_pie_2021 %>% filter(isla == input$sitios & lobera == input$beach)
      field.list <- input$plotChoices
      scale <- input$scaleSelect

      tabla2 <- tablaPie(tabla = tabla, field.list = field.list, scale = scale)
    }
    if (input$scaleSelect == "Islas") {
      tabla <- tabla_pie_2021 %>%
        filter(isla == input$sitios) %>%
        dplyr::select(-lobera)
      field.list <- input$plotChoices
      scale <- input$scaleSelect

      tabla2 <- tablaPie(tabla = tabla, field.list = field.list, scale = scale)
    }

    return(tabla2)
  })

### 4.9.4.- Renderizar tabla -----

  output$tablePlot <- renderDataTable(
    {

      req(length(input$plotChoices) > 0)

      datatable(data = tablaData(), rownames = F)
    },
    server = T
  )

### 4.9.5.- Render grafico -----

  output$piePlot <- renderPlotly({
    req(length(input$plotChoices) > 0)
    plotData()
  })

}

shinyApp(ui, server)

# Fin de la aplicacion -----

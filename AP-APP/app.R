# Carga de librerías
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(shinyWidgets)
library(lubridate)
library(plotly)


# Carga de datos
load("DatosMap.RData") #Mapa visualización
load("PlotPred.RData") #Gráficas predicción
choices_month <- format(seq.Date(from = as.Date("2019-01-01"), by = "month", length.out = 24), "%Y-%m")
load("DatosMapG.RData") #Mapa agrupación
load("Grupos.RData") #Información sobre los grupos

# Define UI
ui <- dashboardPage(
    dashboardHeader(title = "MoviMed"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Visualización", tabName = "Visual", icon = icon("history")),
            menuItem("Predicción", icon = icon("forward"), tabName = "Pred"),
            menuItem("Agrupamiento", icon = icon("layer-group"), tabName = "Groups")
        ),
        textOutput("res")
    ),
    dashboardBody(
        tabItems(
            # Modificación de la pestaña de visualización
            tabItem("Visual", 
                    h2("Accidentalidad en Medellín 2014-2018"),
                    sidebarPanel(# Ventana de tiempo
                                h4("Ventana de tiempo"),
                                p("Seleccione la ventana de tiempo que desea visualizar:"),
                                dateRangeInput("Rango", NULL, start = "2014-01-01", end = "2018-12-31",
                                               min = "2014-01-01", max = "2018-12-31", format = "yyyy-mm-dd",
                                               startview = "month", language = "es", separator = " hasta ",
                                               width = NULL, autoclose = TRUE),
                                # Selección del tipo
                                h4("Tipo"),
                                p("Seleccione el tipo de accidente que desea visualizar:"),
                                radioButtons ("Tipo", NULL,
                                                   choices = list("Tipo I" = "Tipo II", "Tipo II" = "Tipo I", "Todos" = "-"),
                                                   selected = "Tipo II"),
                                # Botón de activación
                                actionButton("Button", "Cargar visualización", icon("sync-alt"), 
                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                # Help
                                helpText(p(strong("Los accidentes Tipo I:"), "son choques de baja gravedad que no requiren atención de emergencia.", br(),
                                           strong("Los accidentes Tipo II:"),"son aquellos que por su clase o gravedad requieren el apoyo de vehículos y/o equipos de emergencia.")
                                )),
                    mainPanel(# Mapa Histórico
                             h3("Mapa Histórico"),
                             textOutput("Selected_TipoRango"),
                             leafletOutput(outputId = "MapaHist")
                             ),
                    p("Las cifras de accidentes en la ventana de tiempo seleccionada fueron:", align = "left"),
                    fluidRow(# Datos adicionales
                            valueBoxOutput("Total"),
                            valueBoxOutput("TipoI"),
                            valueBoxOutput("TipoII")
                            )
                    ),
            
            # Modificación de la pestaña de predicción
            tabItem("Pred", 
                    fluidPage(
                        titlePanel(h1("Predicción Accidentalidad en Medellín 2019-2020")),
                        fluidRow(# Resolución temporal
                                column(3, wellPanel(selectInput("input_type", "Resolución Temporal",
                                                                c("Diaria", "Mensual", "Anual")
                                                                ),
                                                    h6(strong("Nota:"), "define el tipo de gráfico y el tipo de ventana temporal permitida")
                                                    )
                                       ),
                                # Ventana temporal
                                column(5, wellPanel(uiOutput("ui")
                                                    )
                                       ),
                                # Tipo
                                column(3, wellPanel(selectInput("Tipo2", "Tipo",
                                                                c("Tipo I" = "Tipo II", "Tipo II" = "Tipo I", "Ambos" = "-"), 
                                                                selected = "-"
                                                                ),
                                                    actionButton("Button2", "Cargar visualización", icon("sync-alt"), 
                                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                    )
                                       )
                                )
                        ),
                    fluidRow(# Gráficos
                             column(8, wellPanel(plotlyOutput("Graphics"))),
                             # Datos adicionales
                             column(3, wellPanel(valueBoxOutput("Total2", width = NULL),
                                                 valueBoxOutput("TipoI2", width = NULL),
                                                 valueBoxOutput("TipoII2", width = NULL)
                                                 )
                                    )
                    )
                    ),
            # Modificación de la pestaña de agrupamiento
            tabItem("Groups",
                    h1("Barrios de Medellín según su accidentalidad"),
                    sidebarPanel(#Selección de Barrio
                                h3("Barrio"),
                                p("Seleccione el barrio de su interés:"),
                                selectizeInput("Barrio",NULL,sort(unique(DataMap@data$NOMBRE)),selected="La Candelaria"),
                                box(title = "Información Barrio", status ="primary", solidHeader = TRUE,
                                    htmlOutput("Comuna"),
                                    htmlOutput("Mensual"),
                                    htmlOutput("Estrato"),
                                    width = NULL
                                    ),
                                box(title = "Información Grupo", status ="warning", solidHeader = TRUE,
                                    h4(htmlOutput("Grupo")),
                                    htmlOutput("PMensual"),
                                    htmlOutput("PDiario"),
                                    htmlOutput("PDiaEsp"),
                                    htmlOutput("PDiaPost"),
                                    htmlOutput("Mortal"),
                                    width = NULL
                                    )
                                ),
                    mainPanel(# Mapa Histórico
                             h3("Mapa Agrupamiento por Barrios"),
                             leafletOutput(outputId = "MapaGroups", width = "100%", height= 550)
                             ),
                    )
        )
    )
)

server <- function(input, output, session) {
    
    # Mapa Histórico
    output$MapaHist <- renderLeaflet({
        input$Button
        DatosMapV <- filter(DatosMap, FECHA >= isolate(input$Rango[1])  & FECHA <= isolate(input$Rango[2]) & TIPO != isolate(input$Tipo))
        Coordenadas <- data.frame(lat= DatosMapV$LATITUD, lng = DatosMapV$LONGITUD)
        PopUpMap <- paste(sep="<br/>",
                          paste("Fecha:",DatosMapV$FECHA),
                          paste("Tipo:",DatosMapV$TIPO),
                          paste("Clase:",DatosMapV$CLASE),
                          paste("Gravedad:",DatosMapV$GRAVEDAD))
        leaflet(Coordenadas)%>% 
            addTiles() %>%
            addMarkers(popup = PopUpMap,
                       clusterOptions = markerClusterOptions()
            ) %>%
            addMiniMap(tiles = providers$Esri.WorldStreetMap,
                       toggleDisplay = TRUE)
    })

    output$Selected_TipoRango <- renderText({
        input$Button
        paste("En este mapa puede visualizar los accidentes en Medellín de",
              isolate(input$Rango[1]), "a",isolate(input$Rango[2]),
              "del o de los tipos seleccionados:")
    })
    
    # SubItem
    output$res <- renderText({
        req(input$sidebarItemExpanded)
        paste("Expanded menuItem:", input$sidebarItemExpanded)
    })
    
    # ValueBox Total
    output$Total <- renderValueBox({
        input$Button
        DatosMapV <- filter(DatosMap, FECHA >= isolate(input$Rango[1])  & FECHA <= isolate(input$Rango[2]))
        valueBox(
            value = format(sum(dim(DatosMapV)[1]), big.mark = ".", decimal.mark = ","),
            subtitle = strong("Total Accidentes"),
            icon = icon("car-crash"),
            color = "blue"
        )
    })
    
    # ValueBox Total Tipo I
    output$TipoI <- renderValueBox({
        input$Button
        DatosMapV <- filter(DatosMap, FECHA >= isolate(input$Rango[1])  & FECHA <= isolate(input$Rango[2]) & TIPO == "Tipo I")
        valueBox(
            value = format(sum(dim(DatosMapV)[1]), big.mark = ".", decimal.mark = ","),
            subtitle = "Total Accidentes Tipo I",
            icon = icon("handshake"),
            color = "green"
        )
    })
    
    # ValueBox Total Tipo II
    output$TipoII <- renderValueBox({
        input$Button
        DatosMapV <- filter(DatosMap, FECHA >= isolate(input$Rango[1])  & FECHA <= isolate(input$Rango[2]) & TIPO == "Tipo II")
        valueBox(
            value = format(sum(dim(DatosMapV)[1]), big.mark = ".", decimal.mark = ","),
            subtitle = "Total Accidentes Tipo II",
            icon = icon("ambulance"),
            color = "yellow"
        )
    })
    
    # Rango 2
    output$ui <- renderUI({
        if (is.null(input$input_type))
            return()
        
        switch(input$input_type,
               input$Button2,
               "Anual" = radioButtons("dynamic", "Indique el año",
                                     choices = c("2019" = "2020",
                                                 "2020" = "2019",
                                                 "Ambos" = ""),
                                     selected = ""),
               "Mensual" = sliderTextInput("dynamic", "Ventana Temporal",
                                           choices = choices_month, selected = c(choices_month[2],choices_month[10]), 
                                           grid = FALSE,  dragRange = TRUE),
               "Diaria" = dateRangeInput("dynamic", "Ventana Temporal", start = "2019-01-01", end = "2019-06-30",
                                         min = "2019-01-01", max = "2020-12-31", format = "yyyy-mm-dd",
                                         startview = "month", language = "es", separator = " hasta ",
                                         width = NULL, autoclose = TRUE
                                         )
        )
    })
    

    # Gráficas
    output$Graphics <- renderPlotly({
        input$Button2
        if (is.null(input$input_type))
            return()
        
        switch(input$input_type,
               
               "Anual" = plot_ly(filter(DAnual, AÑO != input$dynamic, TIPO != isolate(input$Tipo2)), 
                                 x=~AÑO, y=~Y, type="bar", color = ~TIPO, colors = c("#3171B7","#F09E38")) %>%
                         layout(plot_bgcolor  = "F3F3F3",paper_bgcolor = "#F3F3F3",
                                yaxis = list(title = "Cantidad")), 
               "Mensual" = plot_ly(filter(DMensual, MES >= input$dynamic[1], MES <= input$dynamic[2], TIPO != isolate(input$Tipo2)),
                                   x=~MES, y=~Y, type="bar", color = ~TIPO, colors = c("#3171B7","#F09E38"))%>%
                          layout(plot_bgcolor  = "F3F3F3",paper_bgcolor = "#F3F3F3",
                                 yaxis = list(title = "Cantidad")), 
               "Diaria" = plot_ly(filter(DDiaria, DIA >= input$dynamic[1], DIA <= input$dynamic[2], TIPO != isolate(input$Tipo2)),
                                  x=~DIA, y=~Y, type="scatter", mode = "lines" ,color = ~TIPO, colors = c("#3171B7","#F09E38"))%>%
                   layout(plot_bgcolor  = "F3F3F3",paper_bgcolor = "#F3F3F3",
                          yaxis = list(title = "Cantidad"), xaxis = list(type = "date", tickformat = "%d-%m<br>%Y"))
               )
    })
    
    # ValueBox Total2
    output$Total2 <- renderValueBox({
        input$Button2
        if (is.null(input$input_type))
            return()
        
        switch(input$input_type,
               "Anual" = valueBox(value = sum(filter(DAnual, AÑO != input$dynamic, TIPO != isolate(input$Tipo2))[2]),
                                  subtitle = strong("Total Accidentes"),
                                  icon = icon("car-crash"),
                                  color = "green"
                                  ),
               "Mensual" = valueBox(value = sum(filter(DMensual, MES>= input$dynamic[1], MES <= input$dynamic[2], TIPO != isolate(input$Tipo2))[2]),
                                    subtitle = strong("Total Accidentes"),
                                    icon = icon("car-crash"),
                                    color = "green"
                                    ),
               "Diaria" = valueBox(value = sum(filter(DDiaria, DIA >= input$dynamic[1], DIA <= input$dynamic[2], TIPO != isolate(input$Tipo2))[2]),
                                    subtitle = strong("Total Accidentes"),
                                    icon = icon("car-crash"),
                                    color = "green"
                                   )
               )
    })
    
    # ValueBox Total2 Tipo I
    output$TipoI2 <- renderValueBox({
        input$Button2
        
        switch(input$input_type,
               "Anual" = valueBox(value = sum(filter(filter(DAnual, AÑO != input$dynamic, TIPO != isolate(input$Tipo2)), TIPO == "Tipo I")[[2]]),
                                  subtitle = "Total Accidentes Tipo I",
                                  icon = icon("handshake"),
                                  color = "blue"
               ),
               "Mensual" = valueBox(value = sum(filter(filter(DMensual, MES>= input$dynamic[1], MES <= input$dynamic[2], TIPO != isolate(input$Tipo2)), TIPO == "Tipo I")[[2]]),
                                    subtitle = "Total Accidentes Tipo I",
                                    icon = icon("handshake"),
                                    color = "blue"
               ),
               "Diaria" = valueBox(value = sum(filter(filter(DDiaria, DIA>= input$dynamic[1], DIA <= input$dynamic[2], TIPO != isolate(input$Tipo2)), TIPO == "Tipo I")[[2]]),
                                   subtitle = "Total Accidentes Tipo I",
                                   icon = icon("handshake"),
                                   color = "blue"
                                   )
               )
    })

    # ValueBox Total2 Tipo II
    output$TipoII2 <- renderValueBox({
        input$Button2
        if (is.null(input$input_type))
            return()
        
        switch(input$input_type,
               "Anual" = valueBox(value = sum(filter(filter(DAnual, AÑO != input$dynamic, TIPO != isolate(input$Tipo2)), TIPO == "Tipo II")[[2]]),
                                  subtitle = "Total Accidentes Tipo II",
                                  icon = icon("ambulance"),
                                  color = "yellow"
               ),
               "Mensual" = valueBox(value = sum(filter(filter(DMensual, MES>= input$dynamic[1], MES <= input$dynamic[2], TIPO != isolate(input$Tipo2)), TIPO == "Tipo II")[[2]]),
                                    subtitle = "Total Accidentes Tipo II",
                                    icon = icon("ambulance"),
                                    color = "yellow"
               ),
               "Diaria" = valueBox(value = sum(filter(filter(DDiaria, DIA>= input$dynamic[1], DIA <= input$dynamic[2], TIPO != isolate(input$Tipo2)), TIPO == "Tipo II")[[2]]),
                                   subtitle = "Total Accidentes Tipo II",
                                   icon = icon("ambulance"),
                                   color = "yellow"
               )
        )
    })
    
    # Mapa Agrupamiento
    output$MapaGroups <- renderLeaflet({
        leaflet(DataMap)%>% 
            addTiles() %>%
            addPolygons(popup = paste(sep="<br/>",paste("Barrio:",DataMap@data$NOMBRE),
                                      paste("Comuna:",DataMap@data$NOMBRE_COM),
                                      paste("Promedio mensual accidentes:",round(DataMap@data$`Promedio mensual`,0)),
                                      paste("Grupo:",DataMap@data$grupo)),
                        stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.75,layerId = DataMap@data$OBJECTID,
                        fillColor=~factpal(grupo))%>%
            addLegend(pal=factpal,values = c("7. Sin info","6. Muy bajo","5. Bajo","4. Medio","3. Medio alto","2. Alto","1. Muy alto"),
                      opacity = 0.7, title = "Grupo", position = "topright")
    })
    
    observeEvent(input$MapaGroups_shape_click, {
        
        event <- input$MapaGroups_shape_click
        updateSelectInput(session, inputId = "Barrio", selected = DataMap@data$NOMBRE[DataMap@data$OBJECTID==event$id])
    })
    
    # Información Barrio
    ## Comuna
    output$Comuna <- renderText({
        paste("<B>Comuna:</B>",DataMap@data$NOMBRE_COM[DataMap@data$NOMBRE==input$Barrio])
    })
    
    ## Promedio Mensual
    output$Mensual <- renderText({
        paste("<B>Promedio mensual Accidentes:</B>",round(DataMap@data$`Promedio mensual`[DataMap@data$NOMBRE==input$Barrio]))
    })
    
    ## Estrato
    output$Estrato <- renderText({
        paste("<B>Estrato:</B>",DataMap@data$estrato[DataMap@data$NOMBRE==input$Barrio])
    })
    
    # Información Grupo
    ## Grupo
    output$Grupo <- renderText({
        paste("<b><font color='#F09E38'>Grupo: </font><b/>",DataMap@data$grupo[DataMap@data$NOMBRE==input$Barrio])
    })
    
    output$PMensual <- renderText({
        a = as.character(DataMap@data$grupo[DataMap@data$NOMBRE==input$Barrio])
        paste("<B>Promedio mensual de accidentes:</B>",ResumenGrupos$Promedio_mensual[ResumenGrupos$Grupo==a])
    })
    
    output$PDiario <- renderText({
        a = as.character(DataMap@data$grupo[DataMap@data$NOMBRE==input$Barrio])
        paste("<B>Promedio diario de accidentes:</B>",ResumenGrupos$Promedio_diario[ResumenGrupos$Grupo==a])
    })
    
    output$PDiaEsp <- renderText({
        a = as.character(DataMap@data$grupo[DataMap@data$NOMBRE==input$Barrio])
        paste("<B>Promedio de accidentes en días especiales:</B>",ResumenGrupos$Dias_especiales[ResumenGrupos$Grupo==a])
    })
    
    output$PDiaPost <- renderText({
        a = as.character(DataMap@data$grupo[DataMap@data$NOMBRE==input$Barrio])
        paste("<B>Promedio de accidentes en días Post-Quincena:</B>",ResumenGrupos$Dias_postquincena[ResumenGrupos$Grupo==a])
    })
    
    output$Mortal <- renderText({
        a = as.character(DataMap@data$grupo[DataMap@data$NOMBRE==input$Barrio])
        paste("<B>Promedio accidentes fatales:</B>",ResumenGrupos$Accidentes_fatales[ResumenGrupos$Grupo==a])
    })

}

options(shiny.sanitize.errors = TRUE)

shinyApp(ui, server)

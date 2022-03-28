#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(kableExtra)
library(labelled)

endh <- readRDS("endh2020.rds")

endh <- endh %>% rename(indice_tortura_p28 = indice_tortura) %>%
    mutate(p64 = case_when(p64 == 1 ~ "Creyente practicante",
                           p64 == 2 ~ "Creyente no practicante",
                           p64 == 3 ~ "No creyente"),
           p64 = factor(p64, levels = c("Creyente practicante",
                                        "Creyente no practicante",
                                        "No creyente")),
           politica = na_if(politica, 9))

etiquetas <- look_for(endh)

etiquetas <- etiquetas %>% as_tibble()

etiquetas <- etiquetas %>%
    filter(!variable %in% c("sexo", "region", "comuna", "edad", "politica", "p64", "tramo_edad", "nse_3", "nacionalidad"))

etiquetas <- etiquetas %>% 
    filter(col_type == "dbl+lbl") %>%
    select(label, variable)

# Define UI for application that draws a histogram
shinyUI(navbarPage(

    # Nombre de la aplicación
    title = strong("Encuesta Nacional de Derechos Humanos"),
    
    tags$script(HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><a href=\"https://github.com/mau-carrasco/endh\"><img src=\"https://cdn-icons-png.flaticon.com/512/25/25231.png\" alt=\"alt\" style=\"float:right;width:33px;height:41px;padding-top:10px;\"> </a></div>');
    console.log(header)")
    ),
    
    # Tema
    theme = shinytheme("simplex"),
    tabPanel("Inicio",
             h4(strong("Bienvenidos y bienvenidas a la app web de la Encuesta Nacional de Derechos Humanos del INDH")),
             br(),
             p("En este sitio web podrás visualizar y analizar los resultados de la última Encuesta Nacional de Derechos Humanos (ENDH), realizada por el Instituto Nacional de Derechos Humanos de Chile (INDH) durante el año 2020, en pleno contexto de pandemia."),
             p("El objetivo general de la ENDH es medir las percepciones, opiniones y experiencias de la población mayor de 14 años residente en Chile en relación a los derechos humanos. Para cumplir con este objetivo, el INDH aplica cada dos años una encuesta presencial y en hogares a cerca de 7000 personas, seleccionadas de manera aleatoria y probabilística en cada una de las regiones del país. En la sección Datos, encontrarás las bases de datos de todas las ENDH realizadas entre los años 2010 y 2020, con sus respectivos cuestionarios, manuales e informes metodológicos."),
             p("El siguiente video te explica cómo puedes visualizar, describir y comparar los resultados de la última ENDH, realizada entre los meses de mayo y septiembre de 2020:"),
             br(),
             fluidRow(column(12,
                             align = "center",
                             HTML('<iframe width="80%" height = "500" src="https://www.youtube.com/embed/brXGmEdVXAI" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                             )),
             br()
             ),
    tabPanel("Encuesta",
             h4(strong("Preguntas de la encuesta")),
             sidebarPanel(
                 selectInput("pregunta_encuesta",
                        label = strong("Pregunta"),
                        choices = unique(etiquetas$label)
                        ),
                 selectInput("grupos0",
                             label = strong("Grupos de contraste"),
                             choices = list("Sexo" = "sexo",
                                            "Edad" = "tramo_edad",
                                            "Nivel Socio Económico (NSE)" = "nse_3",
                                            "Nacionalidad" = "nacionalidad",
                                            "Creencia religiosa" = "p64",
                                            "Posición política" = "politica"),
                             selected = "sexo"),
             selectInput("regiones0",
                         label = strong("Región(es)"),
                         choices = endh$nombre_region,
                         selected = c("Arica y Parinacota", "Tarapaca", "Antofagasta", "Atacama", "Coquimbo", "Valparaiso", "Metropolitana de Santiago", "Libertador General Bernardo OHiggins", "Maule", "Nuble", "Biobio", "La Araucania", "Los Rios", "Los Lagos", "Aysen del General Carlos Ibanez del Campo", "Magallanes y de la Antartica Chilena"),
                         multiple = T)
             ),
             mainPanel(
                 fluidRow(
                     column(6, 
                            align = "center",
                            textOutput("titulo0"),
                            br(),
                            tableOutput("tabla0")),
                     column(6,
                            align = "center",
                            textOutput("titulo0a"),
                            plotOutput("plot0"))
                 ),
                 fluidRow(
                     column(6, 
                            align = "center",
                            textOutput("titulo00"),
                            br(),
                            tableOutput("tabla00")),
                     column(6,
                            align = "center",
                            textOutput("titulo00a"),
                            plotOutput("plot00"))
                 )
                 )
             ),
    # Sidebar with a slider input for number of bins
    tabPanel("Índices",
             h4(strong("Índices de Derechos Humanos")),
             sidebarPanel(
                 selectInput("indices", 
                              label = strong("Índice"),
                              choices = list("Permisividad hacia la violencia contra las mujeres" = "p20",
                                             "Apoyo al ejercicio de derechos humanos en grupos de especial protección" = "p19",
                                             "Justificación de la violencia policial" = "p38",
                                             "Justificación de los métodos de protesta" = "p37",
                                             "Justificiación de la tortura" = "p28"), 
                              selected = "p20"),
                 selectInput("grupos",
                             label = strong("Grupos de contraste"),
                             choices = list("Sexo" = "sexo",
                                            "Edad" = "tramo_edad",
                                            "Nivel Socio Económico (NSE)" = "nse_3",
                                            "Nacionalidad" = "nacionalidad",
                                            "Creencia religiosa" = "p64",
                                            "Posición política" = "politica"),
                             selected = "sexo"),
                 selectInput("regiones",
                             label = strong("Región(es)"),
                             choices = endh$nombre_region,
                             selected = c("Arica y Parinacota", "Tarapaca", "Antofagasta", "Atacama", "Coquimbo", "Valparaiso", "Metropolitana de Santiago", "Libertador General Bernardo OHiggins", "Maule", "Nuble", "Biobio", "La Araucania", "Los Rios", "Los Lagos", "Aysen del General Carlos Ibanez del Campo", "Magallanes y de la Antartica Chilena"),
                             multiple = T)
             ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Preguntas" , 
                                 align = "center",
                                 textOutput("titulo5"),
                                 br(),
                                 tableOutput("tabla5"),
                                 br(),
                                 textOutput("titulo5a"),
                                 br(),
                                 tableOutput("tabla6")
                        ),
                        tabPanel("Puntajes",
                                 fluidRow(
                            column(6,
                                   align = "center",
                                   textOutput("titulo3"),
                                   br(),
                                   tableOutput("tabla3")
                                   ),
                            column(6,
                                   align = "center",
                                   textOutput("titulo3a"),
                                   plotOutput("plot3")
                                   )
                        ),
                        fluidRow(
                            column(6,
                                   align = "center",
                                   textOutput("titulo4"),
                                   br(),
                                   tableOutput("tabla4")
                            ),
                            column(6,
                                   align = "center",
                                   textOutput("titulo4a"),
                                   plotOutput("plot4")
                            )
                        )
                        ),
                        tabPanel("Niveles",
                                 fluidRow(
                                     column(6,
                                            align = "center",
                                            textOutput("titulo"),
                                            br(),
                                            tableOutput("tabla1")
                                     ),
                                     column(6, 
                                            align = "center",
                                            textOutput("titulo1"),
                                            plotOutput("plot1")
                                     )
                                 ),
                                 fluidRow(
                                     column(6,
                                            align = "center",
                                            textOutput("titulo2"),
                                            br(),
                                            tableOutput("tabla2")
                                     ),
                                     column(6, 
                                            align = "center",
                                            textOutput("titulo2a"),
                                            plotOutput("plot2")
                                     )
                                 )
                                 )
            )
        )
    ),
    tabPanel("Datos",
             h4(strong("Informes y Bases de datos")),
             p("En esta sección encontrarás los manuales y las bases de datos de todas las versiones de la Encuesta Nacional de Derechos Humanos, realizadas por el INDH de Chile entre 2010 y 2020."),
             fluidRow(
                 column(4,
                        h5(strong("Sexta Encuesta Nacional de Derechos Humanos - 2020: ")),
                        strong("Base de datos:"),
                        p(downloadLink("download2020_sav", "SPSS"), " | ", downloadLink("download2020_dta", "STATA"), " | ", downloadLink("download2020_rds", "R")),
                        strong("Documentos: "),
                        p(downloadLink("cuestionario2020", "Cuestionario"), "|", downloadLink("informe2020", "Informe"), " | ", downloadLink("codigos2020", "Libro de códigos")),
                        ),
                 column(4,
                        h5(strong("Quinta Encuesta Nacional de Derechos Humanos - 2018: ")),
                        strong("Base de datos: "),
                        p(downloadLink("download2018_sav", "SPSS"), " | ", downloadLink("download2018_dta", "STATA"), " | ", downloadLink("download2018_rds", "R")),
                        strong("Documentos: "),
                        p(downloadLink("cuestionario2018", "Cuestionario"), "|", downloadLink("informe2018", "Informe"), " | ", downloadLink("codigos2018", "Libro de códigos")),
                        ),
                 column(4,
                        h5(strong("Cuarta Encuesta Nacional de Derechos Humanos - 2016: ")),
                        )
             ),
             br(),
             p(strong("Tercera Encuesta Nacional de Derechos Humanos - 2014: ")),
             br(),
             p(strong("Segunda Encuesta Nacional de Derechos Humanos - 2012: ")),
             br(),
             p(strong("Primera Encuesta Nacional de Derechos Humanos - 2010: ")),
             br()
             )
)
)

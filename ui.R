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

endh <- endh %>%
    mutate(p64 = case_when(p64 == 1 ~ "Creyente practicante",
                           p64 == 2 ~ "Creyente no practicante",
                           p64 == 3 ~ "No creyente"),
           p64 = factor(p64, levels = c("Creyente practicante",
                                        "Creyente no practicante",
                                        "No creyente")))

# Define UI for application that draws a histogram
shinyUI(navbarPage(

    # Nombre de la aplicación
    title = strong("Encuesta Nacional de Derechos Humanos"),
    
    # Tema
    theme = shinytheme("simplex"),

    # Sidebar with a slider input for number of bins
    tabPanel("Índices",
             h4(strong("Índices de Derechos Humanos")),
             sidebarPanel(
                 selectInput("indices", 
                              label = strong("Índice de actitudes"),
                              choices = list("Permisividad hacia la violencia contra las mujeres" = "p20",
                                             "Apoyo al ejercicio de derechos humanos en grupos de especial protección" = "p19",
                                             "Justificación de la violencia policial" = "p38",
                                             "Justificación de los métodos de protesta" = "p37"), 
                              selected = "p20"),
                 selectInput("regiones",
                             label = strong("Región(es)"),
                             choices = endh$nombre_region,
                             selected = c("Arica y Parinacota", "Tarapaca", "Antofagasta", "Atacama", "Coquimbo", "Valparaiso", "Metropolitana de Santiago", "Libertador General Bernardo OHiggins", "Maule", "Nuble", "Biobio", "La Araucania", "Los Rios", "Los Lagos", "Aysen del General Carlos Ibanez del Campo", "Magallanes y de la Antartica Chilena"),
                             multiple = T),
                 radioButtons("grupos",
                             label = strong("Grupos de contraste"),
                             choices = list("Sexo" = "sexo",
                                            "Edad" = "tramo_edad",
                                            "Nivel Socio Económico (NSE)" = "nse_3",
                                            "Nacionalidad" = "nacionalidad",
                                            "Creencia religiosa" = "p64"),
                             selected = "sexo")
             ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Gráficos", plotOutput("plot")),
                        tabPanel("Tablas" , p(tableOutput("tabla1"), tableOutput("tabla2"))),
                        tabPanel("Reactivos" , p(tableOutput("tabla3")))
            )
        )
    ),
    tabPanel("Datos",
             h4(strong("Informes y Bases de datos")),
             p("En esta sección encontrarás los manuales y las bases de datos de todas las versiones de la Encuesta Nacional de Derechos Humanos, realizadas por el INDH de Chile entre 2010 y 2020."),
             p(strong("Quinta Encuesta Nacional de Derechos Humanos - 2020: "))
             )
))

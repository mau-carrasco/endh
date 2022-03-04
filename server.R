#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$plot <- renderPlot({
        
        variable <- c("p19", "p20", "p37", "p38")
        titulo <- c("Apoyo a los DDHH de grupos de especial protección",
                    "Permisividad hacia la violencia contra las mujeres",
                    "Justificación de los métodos de protesta",
                    "Justificación de la violencia policial")
        titulos <- tibble(variable, titulo)
        
        titulo <- titulos %>% filter(variable == input$indices) %>% select(titulo)
        
        variable2 <- c("sexo", "tramo_edad", "nse_3", "nacionalidad", "p64")
        titulo2 <- c("Sexo", "Edad", "NSE", "Nacionalidad", "Creencia religiosa")
        
        titulos2 <- tibble(variable2, titulo2)
        
        titulo2 <- titulos2 %>% filter(variable2 == input$grupos) %>% select(titulo2)

        endh_grafico <- endh %>% 
            select(folio, contains(input$indices), nombre_region, input$grupos) %>%
            mutate(nombre_region = as_factor(nombre_region)) %>%
            filter(nombre_region %in% c(input$regiones))
        
        # generate bins based on input$bins from ui.R
        datos_grafico1 <- endh_grafico %>% select(contains("suma")) %>% drop_na()
        
        # draw the histogram with the specified number of bins
        datos_grafico1 <- datos_grafico1 %>% select(contains("suma")) %>% drop_na() %>%
            select(variable = 1)

        # generate bins based on input$bins from ui.R
        datos_grafico2 <- endh_grafico %>% select(contains("indice")) %>% drop_na()
        
        # draw the chart with the specified indicator of ui.R
        datos_grafico2 <- datos_grafico2 %>% select(contains("indice")) %>% drop_na() %>%
            select(variable = 1) %>%
            mutate(variable = as_factor(variable))
        
        datos_grafico3 <- endh_grafico %>% select(contains("suma"), input$grupos) %>% drop_na()
        
        datos_grafico3 <- datos_grafico3 %>% select(contains("suma"), input$grupos) %>% drop_na() %>%
            select(variable = 1,
                   grupos = 2) %>%
            mutate(variable = as.numeric(variable),
                   grupos = as_factor(grupos))
        
        datos_grafico4 <- endh_grafico %>% select(contains("indice"), input$grupos) %>% drop_na()
        
        niveles = c("Alto", "Medio", "Bajo")
        
        datos_grafico4 <- datos_grafico4 %>% select(contains("indice"), input$grupos) %>% drop_na() %>%
            select(variable = 1,
                   grupos = 2) %>%
            mutate(variable = as_factor(variable),
                   variable = factor(variable, levels = niveles),
                   grupos = as_factor(grupos))
        
        paleta3 <- RColorBrewer::brewer.pal(n_distinct(datos_grafico3$grupos), "Pastel1")
        
        paleta4 <- RColorBrewer::brewer.pal(n_distinct(datos_grafico4$variable), "Blues")
        
        par(mfrow = c(2, 2))
        
        hist(as.numeric(datos_grafico1$variable),
             main = paste0("Puntajes: ", titulo),
             xlab = "Puntaje",
             ylab = "Frecuencia",
             breaks=7,
             col = RColorBrewer::brewer.pal(n_distinct(datos_grafico1$variable), "Blues"))
        
        plot(datos_grafico2$variable,
             main = paste0("Niveles: ", titulo),
             xlab = "Niveles",
             ylab = "Frecuencia",
             col = RColorBrewer::brewer.pal(n_distinct(datos_grafico2$variable), "Blues"))
        
        plot(datos_grafico3$grupos,
             datos_grafico3$variable,
             main = paste0(titulo, " vs ", titulo2),
             ylab = "Puntaje",
             xlab = paste0(titulo2),
             col = paleta3)
        
        plot(datos_grafico4$grupos,
             datos_grafico4$variable,
             main = paste0(titulo, " vs ", titulo2),
             ylab = "Niveles",
             xlab = paste0(titulo2),
             col = paleta4)
        
    })
    
    output$tabla1 <- function(){
        variable <- c("p19", "p20", "p37", "p38")
        titulo <- c("Apoyo a los DDHH de grupos de especial protección",
                    "Permisividad hacia la violencia contra las mujeres",
                    "Justificación de los métodos de protesta",
                    "Justificación de la violencia policial")
        titulos <- tibble(variable, titulo)
        
        titulo <- titulos %>% filter(variable == input$indices) %>% select(titulo)
        
        variable2 <- c("sexo", "tramo_edad", "nse_3", "nacionalidad", "p64")
        titulo2 <- c("Sexo", "Edad", "NSE", "Nacionalidad", "Creencia religiosa")
        
        titulos2 <- tibble(variable2, titulo2)
        
        titulo2 <- titulos2 %>% filter(variable2 == input$grupos) %>% select(titulo2)
        
        endh_tabla <- endh %>% 
            select(folio, contains(input$indices), nombre_region, input$grupos) %>%
            mutate(nombre_region = as_factor(nombre_region)) %>%
            filter(nombre_region %in% c(input$regiones))
        
        # generate bins based on input$bins from ui.R
        datos_tabla1 <- endh_tabla %>% select(contains("indice"), input$grupos) %>% drop_na()
        
        # draw the histogram with the specified number of bins
        datos_tabla1 <- datos_tabla1 %>% select(contains("indice"), input$grupos) %>% drop_na() %>%
            select(Niveles = 1,
                   Grupo = 2)
        
        datos_tabla1 %>%
            group_by(as_factor(Niveles)) %>%
            summarise(n = n()) %>%
            mutate(prop = n/sum(n)) %>%
            janitor::adorn_totals("row") %>%
            kable(digits = 2,
                  col.names = c(" ", "n", "Prop"),
                  caption = paste0("Niveles del Índice de ", titulo)) %>%
            kable_styling("striped", full_width = F)
    }
    
    output$tabla2 <- function(){
        variable <- c("p19", "p20", "p37", "p38")
        titulo <- c("Apoyo a los DDHH de grupos de especial protección",
                    "Permisividad hacia la violencia contra las mujeres",
                    "Justificación de los métodos de protesta",
                    "Justificación de la violencia policial")
        titulos <- tibble(variable, titulo)
        
        titulo <- titulos %>% filter(variable == input$indices) %>% select(titulo)
        
        variable2 <- c("sexo", "tramo_edad", "nse_3", "nacionalidad", "p64")
        titulo2 <- c("Sexo", "Edad", "NSE", "Nacionalidad", "Creencia religiosa")
        
        titulos2 <- tibble(variable2, titulo2)
        
        titulo2 <- titulos2 %>% filter(variable2 == input$grupos) %>% select(titulo2)
        
        endh_tabla <- endh %>% 
            select(folio, contains(input$indices), nombre_region, input$grupos) %>%
            mutate(nombre_region = as_factor(nombre_region)) %>%
            filter(nombre_region %in% c(input$regiones))
        
        # generate bins based on input$bins from ui.R
        datos_tabla2 <- endh_tabla %>% select(contains("indice"), input$grupos) %>% drop_na()
        
        # draw the histogram with the specified number of bins
        datos_tabla2 <- datos_tabla2 %>% select(contains("indice"), input$grupos) %>% drop_na() %>%
            select(variable = 1,
                   grupos = 2) %>%
            mutate(variable = as_factor(variable),
                   grupos = as_factor(grupos))
        
        table(as_factor(datos_tabla2$variable),
              as_factor(datos_tabla2$grupos)) %>% 
            prop.table(2) %>%
            kable(digits = 2,
                  caption = paste0("Niveles de ", titulo, " vs ", titulo2)) %>%
            kable_styling("striped", full_width = F)
    }
    
    output$tabla3 <- function() {
        # generar datos para tabla en base a los input de ui.R
        
        endh_tabla <- endh %>% 
            select(folio, contains(input$indices), nombre_region) %>%
            mutate(nombre_region = as_factor(nombre_region)) %>%
            filter(nombre_region %in% c(input$regiones))
        
        etiquetas <- look_for(endh)
        
        datos_tabla <- endh_tabla %>% 
            select(!contains("suma")) %>% 
            select(!contains("indice")) %>%
            pivot_longer(cols = contains(input$indices), names_to = "preguntas", values_to = "respuestas") %>%
            mutate(respuestas = as_factor(respuestas)) %>%
            left_join(etiquetas %>% select(preguntas = variable, etiqueta = label)) %>%
            separate(etiqueta, c("pregunta", "encabezado"), sep= "([.?:])")
        
        
        variable <- c("p19", "p20", "p37", "p38")
        titulo <- c("¿Qué tan de acuerdo o en desacuerdo se encuentra usted con las siguientes afirmaciones?",
                    "¿Qué tan de acuerdo o en desacuerdo se encuentra usted con las siguientes afirmaciones?",
                    "Me gustaría preguntarle sobre acciones que la gente hace para protestar contra algo que le parece injusto. ¿Usted justificaría las siguientes acciones?",
                    "Ahora, me gustaría preguntarle sobre conductas de Carabineros. ¿Con qué frecuencia usted justificaría o no las siguientes acciones para controlar los actos de violencia cometidos por manifestantes?")
        titulos <- tibble(variable, titulo)
        titulo <- titulos %>% filter(variable == input$indices) %>% select(titulo)
        
        datos_tabla %>%
            group_by(pregunta, respuestas) %>%
            filter(!respuestas == "Ns (no leer)",
                   !respuestas == "Nr (no leer)",
                   !respuestas == "Ns-Nr (no leer)") %>%
            summarise(frecuencia = n_distinct(folio)) %>%
            mutate(porcentaje = round(frecuencia/sum(frecuencia)*100,1)) %>%
            select(!frecuencia) %>%
            pivot_wider(names_from = "respuestas", values_from = "porcentaje") %>%
            kable("html",
                  booktabs = T,
                  caption = titulo) %>%
            kable_styling("striped", full_width = F) %>%
            footnote(general = "ENDH - INDH, 2020", general_title = "Fuente:")
    }

})

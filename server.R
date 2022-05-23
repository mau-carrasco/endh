library(shiny)
library(shinythemes)
library(tidyverse)
library(kableExtra)
library(labelled)

endh <- readRDS("endh2020.rds")

endh2018 <- haven::read_sav("endh2018.sav")

endh2015 <- haven::read_sav("endh2015.sav")

endh2013 <- haven::read_sav("endh2013.sav")

endh2011 <- haven::read_sav("endh2011.sav")

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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$plot1 <- renderPlot({
        
        variable <- c("p19", "p20", "p37", "p38", "p28")
        titulo <- c("Apoyo a los DDHH de grupos de especial protección",
                    "Permisividad hacia la violencia contra las mujeres",
                    "Justificación de los métodos de protesta",
                    "Justificación de la violencia policial",
                    "Justificación de la tortura")
        titulos <- tibble(variable, titulo)
        
        titulo <- titulos %>% filter(variable == input$indices) %>% select(titulo)
        
        endh_grafico <- endh %>% 
            select(folio, contains(input$indices), nombre_region, input$grupos) %>%
            mutate(nombre_region = as_factor(nombre_region)) %>%
            filter(nombre_region %in% c(input$regiones))
        
        datos_grafico <- endh_grafico %>% select(contains("indice")) %>% drop_na()
        
        datos_grafico <- datos_grafico %>% select(contains("indice")) %>% drop_na() %>%
            select(variable = 1) %>%
            mutate(variable = as_factor(variable))
        
        plot(datos_grafico$variable,
             ylab = "Frecuencia",
             col = RColorBrewer::brewer.pal(n_distinct(datos_grafico$variable), "Blues"))
    }, res = 96)
    
    output$plot2 <- renderPlot({
        
        variable <- c("p19", "p20", "p37", "p38", "p28")
        titulo <- c("Apoyo a los DDHH de grupos de especial protección",
                    "Permisividad hacia la violencia contra las mujeres",
                    "Justificación de los métodos de protesta",
                    "Justificación de la violencia policial",
                    "Justificación de la tortura")
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
        
        niveles = c("Alto", "Medio", "Bajo")
        
        
        datos_grafico <- endh_grafico %>% select(contains("indice"), input$grupos) %>% drop_na()
        
        paleta4 <- RColorBrewer::brewer.pal(n_distinct(datos_grafico$variable), "Blues")
        
        datos_grafico <- datos_grafico %>%
            select(variable = 1,
                   grupos = 2) %>%
            mutate(variable = as_factor(variable),
                   variable = factor(variable, levels = niveles),
                   grupos = as_factor(grupos))
        
        plot(datos_grafico$grupos,
             datos_grafico$variable,
             ylab = "Niveles",
             xlab = paste0(titulo2),
             col = paleta4)
    }, res = 96)
    
    output$plot3 <- renderPlot({

        endh_grafico <- endh %>% 
            select(folio, contains(input$indices), nombre_region, input$grupos) %>%
            mutate(nombre_region = as_factor(nombre_region)) %>%
            filter(nombre_region %in% c(input$regiones))
        
        # generate bins based on input$bins from ui.R
        datos_grafico1 <- endh_grafico %>% select(contains("suma")) %>% drop_na()
        
        # draw the histogram with the specified number of bins
        datos_grafico1 <- datos_grafico1 %>% 
            select(contains("suma")) %>% 
            drop_na() %>%
            select(variable = 1) %>%
            mutate(variable = as.numeric(variable))
        
        hist(datos_grafico1$variable,
             xlab = "Puntaje",
             ylab = "Frecuencia",
             breaks=7,
             main = NULL,
             col = RColorBrewer::brewer.pal(n_distinct(datos_grafico1$variable), "Blues"))
    }, res = 96)
    
    output$plot4 <- renderPlot({
        
        endh_grafico <- endh %>% 
            select(folio, contains(input$indices), nombre_region, input$grupos) %>%
            mutate(nombre_region = as_factor(nombre_region)) %>%
            filter(nombre_region %in% c(input$regiones))
        
        # generate bins based on input$bins from ui.R
        datos_grafico1 <- endh_grafico %>% select(contains("suma"), input$grupos) %>% drop_na()
        
        # draw the histogram with the specified number of bins
        datos_grafico1 <- datos_grafico1 %>% 
            select(contains("suma"), input$grupos) %>% 
            drop_na() %>%
            select(variable = 1,
                   grupos = 2) %>%
            mutate(variable = as.numeric(variable),
                   grupos = as_factor(grupos))
        
        plot(datos_grafico1$grupos,
             datos_grafico1$variable,
             ylab = "Puntaje",
             xlab = NULL,
             breaks=7,
             main = NULL,
             col = RColorBrewer::brewer.pal(n_distinct(datos_grafico1$variable), "Blues"))
    }, res = 96)
    
    output$tabla1 <- renderTable({
        
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
            mutate(Niveles = as_factor(Niveles)) %>%
            group_by(Niveles) %>%
            summarise(n = n()) %>%
            mutate(prop = n/sum(n)) %>%
            janitor::adorn_totals("row")
    })
    
    output$tabla2 <- renderTable({
        
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
        
        datos_tabla2 %>% select(Niveles = variable, grupos) %>%
            group_by(grupos, Niveles) %>%
            summarise(n = n()) %>%
            pivot_wider(names_from = "grupos", values_from = "n") %>%
            janitor::adorn_totals("row")
    })
    
    output$tabla3 <- renderTable({
        
        endh_grafico <- endh %>% 
            select(folio, contains(input$indices), nombre_region, input$grupos) %>%
            mutate(nombre_region = as_factor(nombre_region)) %>%
            filter(nombre_region %in% c(input$regiones))
        
        datos_tabla3 <- endh_grafico %>% 
            select(contains("suma"), input$grupos) %>%
            drop_na() %>%
            select(variable = 1,
                   grupos = 2) %>%
            mutate(variable = as.numeric(variable),
                   grupos = as_factor(grupos))
        
        datos_tabla3 %>%
            summarise(n = n(),
                      prom = mean(variable),
                      med = median(variable),
                      de = sd(variable),
                      min = min(variable),
                      max = max(variable))
    })
    
    output$tabla4 <- renderTable({
        endh_grafico <- endh %>% 
            select(folio, contains(input$indices), nombre_region, input$grupos) %>%
            mutate(nombre_region = as_factor(nombre_region)) %>%
            filter(nombre_region %in% c(input$regiones))
        
        datos_tabla3 <- endh_grafico %>% 
            select(contains("suma"), input$grupos) %>%
            drop_na() %>%
            select(variable = 1,
                   grupos = 2) %>%
            mutate(variable = as.numeric(variable),
                   grupos = as_factor(grupos))
        
        datos_tabla3 %>%
            group_by(grupos) %>%
            summarise(n = n(),
                      prom = mean(variable),
                      med = median(variable),
                      de = sd(variable),
                      min = min(variable),
                      max = max(variable))
    })
    
    output$tabla5 <- renderTable({
        # generar datos para tabla en base a los input de ui.R
        
        endh_tabla <- endh %>% 
            select(folio, contains(input$indices), nombre_region, input$grupos) %>%
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
        
        
        variable <- c("p19", "p20", "p37", "p38", "p28")
        titulo <- c("¿Qué tan de acuerdo o en desacuerdo se encuentra usted con las siguientes afirmaciones?",
                    "¿Qué tan de acuerdo o en desacuerdo se encuentra usted con las siguientes afirmaciones?",
                    "Me gustaría preguntarle sobre acciones que la gente hace para protestar contra algo que le parece injusto. ¿Usted justificaría las siguientes acciones?",
                    "Ahora, me gustaría preguntarle sobre conductas de Carabineros. ¿Con qué frecuencia usted justificaría o no las siguientes acciones para controlar los actos de violencia cometidos por manifestantes?",
                    "¿Qué tan de acuerdo o en desacuerdo se encuentra usted con las siguientes afirmaciones?")
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
            pivot_wider(names_from = "respuestas", values_from = "porcentaje") 
    })
    
    output$tabla6 <- renderTable({
        # generar datos para tabla en base a los input de ui.R
        
        endh_tabla <- endh %>% 
            select(folio, contains(input$indices), nombre_region, grupos = input$grupos) %>%
            mutate(nombre_region = as_factor(nombre_region)) %>%
            filter(nombre_region %in% c(input$regiones))
        
        etiquetas <- look_for(endh)
        
        datos_tabla <- endh_tabla %>% 
            select(!contains("suma")) %>% 
            select(!contains("indice")) %>%
            pivot_longer(cols = contains(input$indices), names_to = "preguntas", values_to = "respuestas") %>%
            left_join(etiquetas %>% select(preguntas = variable, etiqueta = label)) %>%
            separate(etiqueta, c("pregunta", "encabezado"), sep= "([.?:])")
        
        
        datos_tabla %>%
            mutate(respuestas = case_when(respuestas %in% c(3,4,5) ~ "De acuerdo",
                                          respuestas %in% c(1,2) ~ "En desacuerdo")) %>%
            mutate(grupos = as_factor(grupos),
                   respuestas = as_factor(respuestas)) %>%
            group_by(grupos, pregunta, respuestas) %>%
            filter(!respuestas == "Ns (no leer)",
                   !respuestas == "Nr (no leer)",
                   !respuestas == "Ns-Nr (no leer)") %>%
            drop_na(grupos) %>%
            summarise(frecuencia = n_distinct(folio)) %>%
            mutate(porcentaje = round(frecuencia/sum(frecuencia)*100,1)) %>%
            filter(respuestas == "De acuerdo") %>%
            select(!frecuencia) %>%
            pivot_wider(names_from = "grupos", values_from = "porcentaje") %>%
            select(!respuestas)
    })
    
    output$titulo <- renderText({
        variable <- c("p19", "p20", "p37", "p38", "p28")
        titulo <- c("Apoyo a los DDHH de grupos de especial protección",
                    "Permisividad hacia la violencia contra las mujeres",
                    "Justificación de los métodos de protesta",
                    "Justificación de la violencia policial",
                    "Justificación de la tortura")
        titulos <- tibble(variable, titulo)
        
        titulo <- titulos %>% filter(variable == input$indices) %>% select(titulo)
        
        paste0("Niveles de ",titulo)
    })
    
    output$titulo1 <- renderText({
        variable <- c("p19", "p20", "p37", "p38", "p28")
        titulo <- c("Apoyo a los DDHH de grupos de especial protección",
                    "Permisividad hacia la violencia contra las mujeres",
                    "Justificación de los métodos de protesta",
                    "Justificación de la violencia policial",
                    "Justificación de la tortura")
        titulos <- tibble(variable, titulo)
        
        titulo <- titulos %>% filter(variable == input$indices) %>% select(titulo)
        
        paste0("Niveles de ",titulo)
    })
    
    output$titulo2 <- renderText({
        variable <- c("p19", "p20", "p37", "p38", "p28")
        titulo <- c("Apoyo a los DDHH de grupos de especial protección",
                    "Permisividad hacia la violencia contra las mujeres",
                    "Justificación de los métodos de protesta",
                    "Justificación de la violencia policial",
                    "Justificación de la tortura")
        titulos <- tibble(variable, titulo)
        
        titulo <- titulos %>% filter(variable == input$indices) %>% select(titulo)
        
        variable2 <- c("sexo", "tramo_edad", "nse_3", "nacionalidad", "p64", "politica")
        titulo2 <- c("Sexo", "Edad", "NSE", "Nacionalidad", "Creencia religiosa", "Posición política")
        
        titulos2 <- tibble(variable2, titulo2)
        
        titulo2 <- titulos2 %>% filter(variable2 == input$grupos) %>% select(titulo2)
        
        paste0("Niveles de ",titulo, " según ", titulo2)
    })
    
    output$titulo2a <- renderText({
        variable <- c("p19", "p20", "p37", "p38", "p28")
        titulo <- c("Apoyo a los DDHH de grupos de especial protección",
                    "Permisividad hacia la violencia contra las mujeres",
                    "Justificación de los métodos de protesta",
                    "Justificación de la violencia policial",
                    "Justificación de la tortura")
        titulos <- tibble(variable, titulo)
        
        titulo <- titulos %>% filter(variable == input$indices) %>% select(titulo)
        
        variable2 <- c("sexo", "tramo_edad", "nse_3", "nacionalidad", "p64", "politica")
        titulo2 <- c("Sexo", "Edad", "NSE", "Nacionalidad", "Creencia religiosa", "Posición política")
        
        titulos2 <- tibble(variable2, titulo2)
        
        titulo2 <- titulos2 %>% filter(variable2 == input$grupos) %>% select(titulo2)
        
        paste0("Niveles de ",titulo, " según ", titulo2)
    })
    
    output$titulo3 <- renderText({
        variable <- c("p19", "p20", "p37", "p38", "p28")
        titulo <- c("Apoyo a los DDHH de grupos de especial protección",
                    "Permisividad hacia la violencia contra las mujeres",
                    "Justificación de los métodos de protesta",
                    "Justificación de la violencia policial",
                    "Justificación de la tortura")
        titulos <- tibble(variable, titulo)
        
        titulo <- titulos %>% filter(variable == input$indices) %>% select(titulo)
        
        paste0("Estadísticos descriptivos del Índice de ",titulo, " (Puntaje)")
    })
    
    output$titulo3a <- renderText({
        variable <- c("p19", "p20", "p37", "p38", "p28")
        titulo <- c("Apoyo a los DDHH de grupos de especial protección",
                    "Permisividad hacia la violencia contra las mujeres",
                    "Justificación de los métodos de protesta",
                    "Justificación de la violencia policial",
                    "Justificación de la tortura")
        titulos <- tibble(variable, titulo)
        
        titulo <- titulos %>% filter(variable == input$indices) %>% select(titulo)
        
        paste0("Historiograma de los puntajes obtenidos en el Índice de ",titulo)
    })
    
    output$titulo4 <- renderText({
        variable <- c("p19", "p20", "p37", "p38", "p28")
        titulo <- c("Apoyo a los DDHH de grupos de especial protección",
                    "Permisividad hacia la violencia contra las mujeres",
                    "Justificación de los métodos de protesta",
                    "Justificación de la violencia policial",
                    "Justificación de la tortura")
        titulos <- tibble(variable, titulo)
        
        titulo <- titulos %>% filter(variable == input$indices) %>% select(titulo)
        
        variable2 <- c("sexo", "tramo_edad", "nse_3", "nacionalidad", "p64", "politica")
        titulo2 <- c("Sexo", "Edad", "NSE", "Nacionalidad", "Creencia religiosa", "Posición política")
        
        titulos2 <- tibble(variable2, titulo2)
        
        titulo2 <- titulos2 %>% filter(variable2 == input$grupos) %>% select(titulo2)
        
        paste0("Descriptivos del Índice de ",titulo, " según ", titulo2, " (Puntajes)")
    })
    
    output$titulo4a <- renderText({
        variable <- c("p19", "p20", "p37", "p38", "p28")
        titulo <- c("Apoyo a los DDHH de grupos de especial protección",
                    "Permisividad hacia la violencia contra las mujeres",
                    "Justificación de los métodos de protesta",
                    "Justificación de la violencia policial",
                    "Justificación de la tortura")
        titulos <- tibble(variable, titulo)
        
        titulo <- titulos %>% filter(variable == input$indices) %>% select(titulo)
        
        variable2 <- c("sexo", "tramo_edad", "nse_3", "nacionalidad", "p64", "politica")
        titulo2 <- c("Sexo", "Edad", "NSE", "Nacionalidad", "Creencia religiosa", "Posición política")
        
        titulos2 <- tibble(variable2, titulo2)
        
        titulo2 <- titulos2 %>% filter(variable2 == input$grupos) %>% select(titulo2)
        
        paste0("Bloxplot con los puntajes obtenidos en el Índice de ",titulo, " según ", titulo2)
    })
    
    output$titulo5 <- renderText({
        variable <- c("p19", "p20", "p37", "p38", "p28")
        titulo <- c("¿Qué tan de acuerdo o en desacuerdo se encuentra usted con las siguientes afirmaciones?",
                    "¿Qué tan de acuerdo o en desacuerdo se encuentra usted con las siguientes afirmaciones?",
                    "Me gustaría preguntarle sobre acciones que la gente hace para protestar contra algo que le parece injusto. ¿Usted justificaría las siguientes acciones?",
                    "Ahora, me gustaría preguntarle sobre conductas de Carabineros. ¿Con qué frecuencia usted justificaría o no las siguientes acciones para controlar los actos de violencia cometidos por manifestantes?",
                    "¿Qué tan de acuerdo o en desacuerdo se encuentra usted con las siguientes afirmaciones?")
        titulos <- tibble(variable, titulo)
        titulo <- titulos %>% filter(variable == input$indices) %>% select(titulo)
        
        paste0(titulo, " (Porcentaje de respuestas)")
    })
    
    output$titulo5a <- renderText({
        variable <- c("p19", "p20", "p37", "p38", "p28")
        titulo <- c("¿Qué tan de acuerdo o en desacuerdo se encuentra usted con las siguientes afirmaciones?",
                    "¿Qué tan de acuerdo o en desacuerdo se encuentra usted con las siguientes afirmaciones?",
                    "Me gustaría preguntarle sobre acciones que la gente hace para protestar contra algo que le parece injusto. ¿Usted justificaría las siguientes acciones?",
                    "Ahora, me gustaría preguntarle sobre conductas de Carabineros. ¿Con qué frecuencia usted justificaría o no las siguientes acciones para controlar los actos de violencia cometidos por manifestantes?",
                    "¿Qué tan de acuerdo o en desacuerdo se encuentra usted con las siguientes afirmaciones?")
        titulos <- tibble(variable, titulo)
        titulo <- titulos %>% filter(variable == input$indices) %>% select(titulo)
        
        variable2 <- c("sexo", "tramo_edad", "nse_3", "nacionalidad", "p64", "politica")
        titulo2 <- c("Sexo", "Edad", "NSE", "Nacionalidad", "Creencia religiosa", "Posición política")
        
        titulos2 <- tibble(variable2, titulo2)
        
        titulo2 <- titulos2 %>% filter(variable2 == input$grupos) %>% select(titulo2)
        
        paste0(titulo, " Porcentaje de personas que reponden de acuerdo o muy de acuerdo, o siempre, casi siempre o a veces, según ", titulo2)
    })
    
    output$tabla0 <- renderTable({
        
        var <- etiquetas %>% 
            filter(label == input$pregunta_encuesta) %>%
            select(variable)
        
        var <- as.character(var)
        
        endh %>% 
            select(var, nombre_region) %>%
            filter(nombre_region %in% c(input$regiones0)) %>%
            select(respuesta = 1) %>%
            mutate(respuesta = na_if(respuesta, 99)) %>%
            drop_na() %>%
            mutate(respuesta = as_factor(respuesta)) %>%
            count(respuesta) %>%
            mutate(prop = n/sum(n)) %>%
            janitor::adorn_totals("row")
            
    })
    
    output$tabla00 <- renderTable({
        
        var <- etiquetas %>% 
            filter(label == input$pregunta_encuesta) %>%
            select(variable)
        
        var <- as.character(var)
        
        niveles <- names(attr(endh %>% select(var), "labels"))
        niveles
        
        endh %>% 
            select(var, input$grupos0, nombre_region) %>%
            filter(nombre_region %in% c(input$regiones0)) %>%
            select(respuesta = 1,
                   grupos = 2) %>%
            mutate(grupos = as_factor(grupos),
                   respuesta = na_if(respuesta, 99)) %>%
            drop_na() %>%
            mutate(respuesta = as_factor(respuesta, levels = niveles)) %>%
            group_by(grupos, respuesta) %>%
            summarise(n = n()) %>%
            pivot_wider(names_from = "grupos",
                        values_from = "n") %>%
            arrange(respuesta) %>%
            janitor::adorn_totals("row")
        
    })
    
    output$plot0 <- renderPlot({
        
        var <- etiquetas %>% 
            filter(label == input$pregunta_encuesta) %>%
            select(variable)
        
        var <- as.character(var)
        
        datos <- endh %>% 
            filter(nombre_region %in% c(input$regiones0)) %>%
            select(var) %>%
            select(respuesta = 1) %>%
            mutate(respuesta = na_if(respuesta, 99)) %>%
            drop_na() %>%
            mutate(respuesta = as_factor(respuesta))
        
        colores <- RColorBrewer::brewer.pal(n_distinct(datos$respuesta, na.rm = T), "RdBu")
        
        plot(datos$respuesta,
             col = colores)
        
    })
    
    output$plot00 <- renderPlot({
        
        variable2 <- c("sexo", "tramo_edad", "nse_3", "nacionalidad", "p64", "politica")
        titulo2 <- c("Sexo", "Edad", "NSE", "Nacionalidad", "Creencia religiosa", "Posición política")
        
        titulos2 <- tibble(variable2, titulo2)
        
        titulo2 <- titulos2 %>% filter(variable2 == input$grupos0) %>% select(titulo2)
        
        var <- etiquetas %>% 
            filter(label == input$pregunta_encuesta) %>%
            select(variable)
        
        var <- as.character(var)
        
        niveles <- names(attr(endh %>% select(var), "labels"))
        
        datos <- endh %>% 
            filter(nombre_region %in% c(input$regiones0)) %>%
            select(var, input$grupos0) %>%
            select(respuesta = 1,
                   grupos = 2) %>%
            mutate(grupos = as_factor(grupos),
                   respuesta = na_if(respuesta, 99),
                   respuesta = na_if(respuesta, 88)) %>%
            drop_na() %>%
            mutate(respuesta = as_factor(respuesta, levels = niveles)) %>%
            drop_na()
        
        colores <- RColorBrewer::brewer.pal(n_distinct(datos$respuesta, na.rm = T), "RdBu")
        
        barplot(
            table(datos$respuesta, datos$grupos),
            legend.text = TRUE,
            col = colores,
            args.legend = list(x = "top", ncol = 2, cex=0.7, title = "Respuesta"),
            ylim = c(0, nrow(datos)*1.2)
            )
        
    })
    
    output$titulo0 <- renderText({
        
        var <- etiquetas %>% 
            filter(label == input$pregunta_encuesta) %>%
            select(label)
        
        var <- as.character(var)
        
        paste0(var)
        
    })
    
    output$titulo0a <- renderText({
        
        var <- etiquetas %>% 
            filter(label == input$pregunta_encuesta) %>%
            select(label)
        
        var <- as.character(var)
        
        paste0(var)
        
    })
    
    output$titulo00 <- renderText({
        
        var <- etiquetas %>% 
            filter(label == input$pregunta_encuesta) %>%
            select(label)
        
        var <- as.character(var)
        
        variable2 <- c("sexo", "tramo_edad", "nse_3", "nacionalidad", "p64", "politica")
        titulo2 <- c("Sexo", "Edad", "NSE", "Nacionalidad", "Creencia religiosa", "Posición política")
        
        titulos2 <- tibble(variable2, titulo2)
        
        titulo2 <- titulos2 %>% filter(variable2 == input$grupos0) %>% select(titulo2)
        
        
        paste0(var, " Respuesta según ", titulo2)
        
    })
    
    output$titulo00a <- renderText({
        
        var <- etiquetas %>% 
            filter(label == input$pregunta_encuesta) %>%
            select(label)
        
        var <- as.character(var)
        
        variable2 <- c("sexo", "tramo_edad", "nse_3", "nacionalidad", "p64", "politica")
        titulo2 <- c("Sexo", "Edad", "NSE", "Nacionalidad", "Creencia religiosa", "Posición política")
        
        titulos2 <- tibble(variable2, titulo2)
        
        titulo2 <- titulos2 %>% filter(variable2 == input$grupos0) %>% select(titulo2)
        
        
        paste0(var, " Respuesta según ", titulo2)
        
    })
    
    output$download2020_sav <- downloadHandler(
        filename = function() {
            paste0("ENDH-2020.sav")
        },
        content = function(file) {
            haven::write_sav(endh, file)
        })
    
    output$download2020_dta <- downloadHandler(
        filename = function() {
            paste0("ENDH-2020.dta")
        },
        content = function(file) {
            haven::write_dta(endh, file)
        })
    
    output$download2020_rds <- downloadHandler(
        filename = function() {
            paste0("ENDH-2020.rds")
        },
        content = function(file) {
            readr::write_rds(endh, file)
        })
    
    output$cuestionario2020 <- downloadHandler(
        filename = "Cuestionario ENDH - 2020.pdf",
        content = function(file) {
            file.copy("www/cuestionario2020.pdf", file)
        })
    
    output$informe2020 <- downloadHandler(
        filename = "Informe ENDH - 2020.pdf",
        content = function(file) {
            file.copy("www/informe2020.pdf", file)
        })
    
    output$codigos2020 <- downloadHandler(
        filename = "Libro de codigos ENDH - 2020.pdf",
        content = function(file) {
            file.copy("www/codigos2020.pdf", file)
        })
    
    
    output$download2018_dta <- downloadHandler(
        filename = function() {
            paste0("ENDH-2018.dta")
        },
        content = function(file) {
            haven::write_dta(endh2018, file)
        })
    
    output$download2018_sav <- downloadHandler(
        filename = function() {
            paste0("ENDH-2018.sav")
        },
        content = function(file) {
            haven::write_sav(endh2018, file)
        })
    
    output$download2018_rds <- downloadHandler(
        filename = function() {
            paste0("ENDH-2018.rds")
        },
        content = function(file) {
            readr::write_rds(endh2018, file)
        })
    
    output$cuestionario2018 <- downloadHandler(
        filename = "Cuestionario ENDH - 2018.pdf",
        content = function(file) {
            file.copy("www/cuestionario2018.pdf", file)
        })
    
    output$informe2018 <- downloadHandler(
        filename = "Informe ENDH - 2018.pdf",
        content = function(file) {
            file.copy("www/informe2018.pdf", file)
        })
    
    output$codigos2018 <- downloadHandler(
        filename = "Libro de codigos ENDH - 2018.pdf",
        content = function(file) {
            file.copy("www/codigos2018.pdf", file)
        })
    
    output$download2015_dta <- downloadHandler(
        filename = function() {
            paste0("ENDH-2015.dta")
        },
        content = function(file) {
            haven::write_dta(endh2015, file)
        })
    
    output$download2015_sav <- downloadHandler(
        filename = function() {
            paste0("ENDH-2015.sav")
        },
        content = function(file) {
            haven::write_sav(endh2015, file)
        })
    
    output$download2015_rds <- downloadHandler(
        filename = function() {
            paste0("ENDH-2015.rds")
        },
        content = function(file) {
            readr::write_rds(endh2015, file)
        })
    
    output$cuestionario2015 <- downloadHandler(
        filename = "Cuestionario ENDH - 2015.pdf",
        content = function(file) {
            file.copy("www/cuestionario2015.pdf", file)
        })
    
    output$informe2015 <- downloadHandler(
        filename = "Informe ENDH - 2015.pdf",
        content = function(file) {
            file.copy("www/informe2015.pdf", file)
        })
    
    output$codigos2015 <- downloadHandler(
        filename = "Libro de codigos ENDH - 2015.pdf",
        content = function(file) {
            file.copy("www/codigos2015.pdf", file)
        })
    
    output$download2013_dta <- downloadHandler(
        filename = function() {
            paste0("ENDH-2013.dta")
        },
        content = function(file) {
            haven::write_dta(endh2013, file)
        })
    
    output$download2013_sav <- downloadHandler(
        filename = function() {
            paste0("ENDH-2013.sav")
        },
        content = function(file) {
            haven::write_sav(endh2013, file)
        })
    
    output$download2013_rds <- downloadHandler(
        filename = function() {
            paste0("ENDH-2013.rds")
        },
        content = function(file) {
            readr::write_rds(endh2013, file)
        })
    
    output$cuestionario2013 <- downloadHandler(
        filename = "Cuestionario ENDH - 2013.pdf",
        content = function(file) {
            file.copy("www/cuestionario2013.pdf", file)
        })
    
    output$informe2013 <- downloadHandler(
        filename = "Informe ENDH - 2013.pdf",
        content = function(file) {
            file.copy("www/informe2013.pdf", file)
        })
    
    output$codigos2013 <- downloadHandler(
        filename = "Libro de codigos ENDH - 2013.pdf",
        content = function(file) {
            file.copy("www/codigos2013.pdf", file)
        })
    
    output$download2011_dta <- downloadHandler(
        filename = function() {
            paste0("ENDH-2011.dta")
        },
        content = function(file) {
            haven::write_dta(endh2011, file)
        })
    
    output$download2011_sav <- downloadHandler(
        filename = function() {
            paste0("ENDH-2011.sav")
        },
        content = function(file) {
            haven::write_sav(endh2011, file)
        })
    
    output$download2011_rds <- downloadHandler(
        filename = function() {
            paste0("ENDH-2011.rds")
        },
        content = function(file) {
            readr::write_rds(endh2011, file)
        })
    
    output$cuestionario2011 <- downloadHandler(
        filename = "Cuestionario ENDH - 2011.pdf",
        content = function(file) {
            file.copy("www/cuestionario2011.pdf", file)
        })
    
    output$informe2011 <- downloadHandler(
        filename = "Informe ENDH - 2011.pdf",
        content = function(file) {
            file.copy("www/informe2011.pdf", file)
        })
    
    output$codigos2011 <- downloadHandler(
        filename = "Libro de codigos ENDH - 2011.pdf",
        content = function(file) {
            file.copy("www/codigos2011.pdf", file)
        })

})

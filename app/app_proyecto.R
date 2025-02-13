library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)
library(stringr)
library(bslib)
library(bsicons)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(lubridate)
library(syuzhet)
library(igraph)
library(e1071)
library(shinycssloaders)
library(arules)
library(mice)
library(arulesViz)

tweets_df <- read.csv("../data/tweets_analysis_final.csv", header = TRUE)
dataset_completo <- read.csv("../data/dataset_f1.csv")
dataset <- read.csv("../data/dataset_f1_con_meteo.csv")
circuitos <- read.csv("../data/circuits.csv")
carreras <- read.csv("../data/races.csv")
datos_reglas <- read.csv("../data/reglas_de_asoc_f1.csv")
datos_mapa_circuitos_21 <- read.csv("../data/mapa1_21.csv")
datos_mapa_circuitos_22 <- read.csv("../data/mapa1_22.csv")
datos_mapa_carrera_tweets <- read.csv("../data/mapa2.csv")
datos_mapa_paises_pilotos <- read.csv("../data/mapa3.csv")
datos_mapa_tiempos_tweets <- read.csv("../data/mapa4.csv")
datos_mapa_circuitos_f1 <- read.csv("../data/mapa5.csv")
datos_mapa_boxes_posicion_final <- read.csv("../data/mapa6.csv")

check_columns <- function(dataset, cols) {
  all(cols %in% colnames(dataset))
}

clasificaciones_app <- function() {
  required_columns_main <- c("año", "circuitId", "pos_en_carrera", "referencia")
  required_columns_carreras <- c("circuitId", "name")
    
  if (!check_columns(dataset_completo, required_columns_main)) {
    stop("El dataset principal no contiene las columnas necesarias: ", paste(required_columns_main, collapse = ", "))
  }
  if (!check_columns(carreras, required_columns_carreras)) {
    stop("El dataset de carreras no contiene las columnas necesarias: ", paste(required_columns_carreras, collapse = ", "))
  }
  
  dataset_completo <- dataset_completo %>%
    left_join(carreras %>% dplyr::select(circuitId, name), by = "circuitId")
  
  ui <- fluidPage(
    titlePanel(title = "Clasificaciones F1"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("anio", "Seleccione el Año:", choices = unique(dataset_completo$año)),
        selectInput("circuito", "Seleccione el Circuito:", choices = unique(dataset_completo$name))
      ),
      mainPanel(
        uiOutput("tabla_output")
      )
    )
  )
  
  server <- function(input, output, session) {
    datos_filtrados <- reactive({
      dataset_completo %>%
        filter(año == input$anio, name == input$circuito)
    })
    
    output$tabla_output <- renderUI({
      datos <- datos_filtrados()
      
      if (nrow(datos) == 0) {
        showModal(
          modalDialog(
            title = "Mensaje de Error",
            "No hay datos disponibles para esta carrera en el año seleccionado.",
            easyClose = TRUE
          )
        )
      } else {
        renderDataTable({
          datos %>%
            distinct(pos_en_carrera, .keep_all = TRUE) %>%
            arrange(pos_en_carrera) %>%
            dplyr::select(Posición = pos_en_carrera, Piloto = referencia)
        }, options = list(paging = FALSE, rownames = FALSE))
      }
    })
  }
  
  shinyApp(ui = ui, server = server)
}


grafo_correlaciones <- function() {
  ui <- fluidPage(
    titlePanel("Visualización de Correlaciones entre Variables", windowTitle = "Correlaciones"),
    br(),
    sidebarLayout(
      sidebarPanel(
        selectInput("referencia", "Selecciona la Referencia del Piloto:",
                    choices = unique(dataset$referencia)),
        sliderInput("umbral", "Selecciona el Umbral de Correlación:",
                    min = 0, max = 1, value = 0.3, step = 0.05),
        br(),
        tags$p(tags$strong("Leyenda:")),
        tags$div(
          style = "padding-left: 20px;",
          tags$p(
            HTML("<span style='color:red;'>Rojo:</span> Correlación Negativa"),
            HTML("<span style='color:blue;'>Azul:</span> Correlación Positiva")
          )
        )
      ),
      mainPanel(
        plotOutput("correlation_graph", width = "100%", height = "800px")
      )
    )
  )
  
  
  server <- function(input, output, session) {    
    observe({
      dataset_filtered <- dataset_completo[dataset_completo$referencia == input$referencia, ]
      output$correlation_graph <- renderPlot({
        num_data <- scale(dataset_filtered[, sapply(dataset_filtered, is.numeric)])
        correlation_matrix <- cor(num_data)
        correlation_matrix[is.na(correlation_matrix)] <- 0
        threshold <- input$umbral
        
        graph <- graph.empty()
        for (i in colnames(correlation_matrix)) {
          graph <- add_vertices(graph, 1, name = i)
        }
        for (i in 1:(ncol(correlation_matrix) - 1)) {
          for (j in (i + 1):ncol(correlation_matrix)) {
            if (abs(correlation_matrix[i, j]) > threshold) {
              graph <- add_edges(graph, c(i, j), weight = round(correlation_matrix[i, j], 3))
            }
          }
        }
        graph <- delete.vertices(graph, which(degree(graph) == 0))
        
        vertex_positions <- layout_in_circle(graph, order = 1:vcount(graph))
        
        
        edge_colors <- colorRampPalette(c("red", "white", "blue"))(100)
        
        
        plot(graph, layout = vertex_positions, vertex.size = 5, vertex.label.cex = 1, 
             vertex.label.color = "black", vertex.label.dist = -1, edge.arrow.size = 0, 
             edge.color = edge_colors[findInterval(E(graph)$weight, seq(-1, 1, length.out = 101))], 
             main = "Visualización de Correlaciones entre Variables", 
             cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
      })
    })
  }
  
  
  shinyApp(ui = ui, server = server)
}

visualizacion_correlaciones <- function() {
  ui <- fluidPage(
    titlePanel("Visualización de correlaciones entre variables"),
    sidebarLayout(
      sidebarPanel(
        selectInput("referencia", "Selecciona la referencia del piloto:",
                    choices = unique(dataset_completo$referencia)),
        selectInput("variable", "Selecciona la variable para correlacionar:",
                    choices = setdiff(names(dataset_completo), c("driverId", "raceId", "año", "circuitId", "fecha", "hora", "pos_en_carrera", "fecha_nac", "referencia", "nacionalidad")))
      ),
      mainPanel(
        plotOutput("correlation_plot"),
        textOutput("warning_message")
      )
    )
  )
  
  
  server <- function(input, output, session) {    
    observe({
      dataset_filtered <- dataset_completo[dataset_completo$referencia == input$referencia, ]
      output$correlation_plot <- renderPlot({
        num_data <- scale(dataset_filtered[, sapply(dataset_filtered, is.numeric)])
        correlation <- cor(num_data)
        colnames(correlation) <- colnames(num_data)
        variable <- input$variable
        colors <- ifelse(seq_along(colnames(correlation)) > (ncol(correlation) - 10), "red", "blue")
        
        
        plot(correlation[input$variable, ], main = paste("Correlación de '", input$variable, "' con otras variables"),
             xlab = "", ylab = "Correlación", ylim = c(-1, 1), col = colors, pch = 16, xaxt = "n") 
        
        
        abline(h = c(0), col = "black", lty = 2)
        
        
        text_labels <- text(1:ncol(correlation), par("usr")[3] - 0.05, labels = colnames(correlation), srt = 45, adj = 1, xpd = TRUE, cex = 0.8, col = colors)
        
        
        segments(1:ncol(correlation), par("usr")[3] - 0.05, 1:ncol(correlation), correlation[input$variable, ], col = colors, lty = "dashed")
        
        
        points(seq_along(correlation[input$variable, ]), correlation[input$variable, ], col = "black", pch = 16)
      })
    })
    
    output$warning_message <- renderText({
      "Recuerda que para algunos pilotos pueden haber datos faltantes que afecten a la obtención de la correlación. Por ejemplo para un piloto que rara vez participa en las últimas rondas de clasificación (q2,q3), no se obtendrá correlación con otras variables. Lo mismo ocurre con las variables meteorológicas."
    })
  }
  
  
  shinyApp(ui = ui, server = server)
}

reglas_app <- function() {
  ui <- fluidPage(
    titlePanel("Análisis de Reglas de Asociación"),
    sidebarLayout(
      sidebarPanel(
        radioButtons("rhs_choice", "Selecciona RHS",
                     choices = list("Ganar" = "ganador=Si", "Puntuar" = "puntua=Si")),
        selectInput("referencia", "Selecciona referencia", choices = NULL),
        selectInput("ordenar_por", "Ordenar reglas por",
                    choices = list("Soporte" = "support", "Confianza" = "confidence", "Lift" = "lift")),
        actionButton("analizar", "Analizar"),
        plotOutput("grafico_barras", height = "600px")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Reglas", verbatimTextOutput("reglas_output")),
          tabPanel("Gráfico", plotOutput("reglas_plot"))
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    observe({
      if (input$rhs_choice == "ganador=Si") {
        pilotos <- datos_reglas %>% filter(ganador == "Si") %>% count(raceId, name = "veces_ganado")
        updateSelectInput(session, "referencia", choices = unique(pilotos$raceId))
      } else {
        pilotos <- datos_reglas %>% filter(puntua == "Si") %>% count(raceId, name = "veces_puntuado")
        updateSelectInput(session, "referencia", choices = unique(pilotos$raceId))
      }
    })
    
    observeEvent(input$analizar, {
      req(input$referencia)
      
      filtered_data <- datos_reglas %>% filter(raceId == input$referencia)
      
      if (input$rhs_choice == "ganador=Si") {
        reglas_data <- filtered_data %>% dplyr::select(-puntua, -raceId)
      } else {
        reglas_data <- filtered_data %>% dplyr::select(-ganador, -raceId)
      }
      
      # Convert data to transactions
      transacciones <- as(reglas_data, "transactions")
      
      reglas <- apriori(transacciones, parameter = list(minlen = 3, supp = 0.004, conf = 0.8), 
                        appearance = list(rhs = input$rhs_choice), control = list(verbose = FALSE))
      
      reglas_no_redundantes <- reglas[!is.redundant(reglas)]
      ordenadas <- arules::sort(reglas_no_redundantes, by = input$ordenar_por)
      quality(ordenadas) <- round(quality(ordenadas), digits = 3)
      
      output$reglas_output <- renderPrint({
        if (length(ordenadas) > 0) {
          inspect(ordenadas)
        } else {
          "No se generaron reglas de asociación."
        }
      })
      
      output$reglas_plot <- renderPlot({
        if (length(ordenadas) > 0) {
          plot(ordenadas, method = "graph", control = list(engine = "igraph"))
        } else {
          plot.new()
          text(0.5, 0.5, "No se generaron reglas de asociación.", cex = 1.5)
        }
      })
    })
    
    output$grafico_barras <- renderPlot({
      if (input$rhs_choice == "ganador=Si") {
        pilotos_mas_frecuentes <- datos_reglas %>% filter(ganador == "Si") %>% count(raceId, name = "freq") %>% arrange(desc(freq)) %>% head(10)
        ylim <- c(0, 100)
      } else {
        pilotos_mas_frecuentes <- datos_reglas %>% filter(puntua == "Si") %>% count(raceId, name = "freq") %>% arrange(desc(freq)) %>% head(10)
        ylim <- c(0, 300)
      }
      
      barplot(pilotos_mas_frecuentes$freq, names.arg = pilotos_mas_frecuentes$raceId,
              main = "Carreras más frecuentes", xlab = "Carrera", ylab = "Frecuencia",
              col = "skyblue", ylim = ylim, cex.names = 0.8, las = 2)
    })
  }
  
  shinyApp(ui = ui, server = server)
}

pca_app <- function() {
  ui <- fluidPage(
    titlePanel("PCA Fórmula 1"),
    fluidRow(
      column(width = 6,
             h3("Modelo PCA general para el dataset completo"),
             tabsetPanel(
               tabPanel("Resumen de las variables", verbatimTextOutput("generalSummary")),
               tabPanel("General PCA Scree Plot", plotOutput("generalScreePlot")),
               tabPanel("General PCA Variable Plot (1,2)", plotOutput("generalPcaVarPlot1")),
               tabPanel("General PCA Variable Plot (1,3)", plotOutput("generalPcaVarPlot2"))
             )
      ),
      column(width = 6,
             h3("Análisis por piloto"),
             sidebarPanel(
               selectInput("referencia", "Reference", choices = unique(dataset_completo$referencia)),
               actionButton("update", "Update")
             ),
             tabsetPanel(
               tabPanel("Summary", verbatimTextOutput("summary")),
               tabPanel("PCA Scree Plot", plotOutput("screePlot")),
               tabPanel("SCR Plot", plotOutput("scrPlot")),
               tabPanel("PCA Variable Plot (1,2)", plotOutput("pcaVarPlot1")),
               tabPanel("PCA Variable Plot (1,3)", plotOutput("pcaVarPlot2"))
             )
      )
    ),
    tags$head(tags$script(HTML("
      Shiny.addCustomMessageHandler('show_error', function(message) {
        alert(message);
      });
    ")))
  )
  
  server <- function(input, output, session) {
    observeEvent(input$update, {
      tryCatch({
        alo <- subset(dataset_completo, referencia == input$referencia)
        
        desc_alo <- data.frame(
          "variable" = colnames(alo),
          "tipo" = c(rep('id', 4), 'date', rep('numerical', 3), 'date', rep('categorical', 2), rep('numerical', 7), rep('date', 3), rep('numerical', 6)),
          stringsAsFactors = FALSE
        )
        
        nombres_numericos <- desc_alo$variable[desc_alo$tipo == "numerical"]
        data_numericas <- alo[, nombres_numericos]
        
        if (any(!complete.cases(data_numericas))) {
          stop("Este piloto tiene demasiados datos faltantes en algunas variables para elaborar el modelo.")
        }
        
        data_numericas <- dplyr::select(data_numericas, -pos_en_carrera, -acumulado_victorias, -hora)
        datos_ae <- scale(data_numericas)
        
        res.pca <- FactoMineR::PCA(datos_ae, scale.unit = FALSE, graph = FALSE, ncp = 5)
        
        X <- as.matrix(datos_ae)
        misScores <- res.pca$ind$coord[, 1:3]
        misLoadings <- sweep(res.pca$var$coord[, 1:3], 2, sqrt(res.pca$eig[1:3, 1]), FUN = "/")
        
        myE <- X - misScores %*% t(misLoadings)
        mySCR <- rowSums(myE^2)
        
        g <- var(mySCR) / (2 * mean(mySCR))
        h <- (2 * mean(mySCR)^2) / var(mySCR)
        chi2lim <- g * qchisq(0.95, df = h)
        chi2lim99 <- g * qchisq(0.99, df = h)
        
        moderada <- which(mySCR > chi2lim)
        
        output$summary <- renderPrint({
          summary(data_numericas)
        })
        
        output$screePlot <- renderPlot({
          eig.val <- get_eigenvalue(res.pca)
          VPmedio <- 100 * (1/nrow(eig.val))
          fviz_eig(res.pca, addlabels = TRUE) +
            geom_hline(yintercept = VPmedio, linetype = 2, color = "red")
        })
        
        output$scrPlot <- renderPlot({
          ggplot() +
            geom_label(data = data.frame(x = 1:length(mySCR), y = mySCR)[moderada,], 
                       aes(x = x, y = y, label = x)) +
            geom_point(data = data.frame(x = 1:length(mySCR), y = mySCR)[-moderada,], 
                       aes(x = x, y = y)) +
            geom_text(check_overlap = TRUE) +
            geom_hline(yintercept = chi2lim, col = "orange", linewidth = 1) +
            geom_hline(yintercept = chi2lim99, col = "red3", linewidth = 1) +
            ggtitle("SCR") +
            theme_bw()
        })
        
        output$pcaVarPlot1 <- renderPlot({
          fviz_pca_var(res.pca, axes = c(1, 2), repel = TRUE, col.var = "contrib",
                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
        })
        
        output$pcaVarPlot2 <- renderPlot({
          fviz_pca_var(res.pca, axes = c(1, 3), repel = TRUE, col.var = "contrib",
                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
        })
      }, error = function(e) {
        session$sendCustomMessage(type = 'show_error', message = "Error: Missing data in the selected reference.")
      })
    })
    
    
    general_data_numericas <- dataset_completo %>%
      dplyr::select(where(is.numeric)) %>%
      dplyr::select(-pos_en_carrera, -acumulado_victorias, -hora)
    general_datos_ae <- scale(general_data_numericas)
    general_datos_ae_imp <- mice(general_datos_ae, m = 1)
    general_datos_ae_imp <- mice::complete(general_datos_ae_imp)
    
    general_res.pca <- FactoMineR::PCA(general_datos_ae_imp, scale.unit = FALSE, graph = FALSE, ncp = 5)
    
    output$generalSummary <- renderPrint({
      summary(general_data_numericas)
    })
    
    output$generalScreePlot <- renderPlot({
      eig.val <- get_eigenvalue(general_res.pca)
      VPmedio <- 100 * (1/nrow(eig.val))
      fviz_eig(general_res.pca, addlabels = TRUE) +
        geom_hline(yintercept = VPmedio, linetype = 2, color = "red")
    })
    
    output$generalPcaVarPlot1 <- renderPlot({
      fviz_pca_var(general_res.pca, axes = c(1, 2), repel = TRUE, col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
    })
    
    output$generalPcaVarPlot2 <- renderPlot({
      fviz_pca_var(general_res.pca, axes = c(1, 3), repel = TRUE, col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
    })
  }
  
  shinyApp(ui = ui, server = server)
}

tweets_viz <- function() {
  mentions_list <- tweets_df %>%
    filter(!is.na(mentions)) %>%
    separate_rows(mentions, sep = ",\\s*") %>%
    distinct(mentions) %>%
    pull(mentions)

  ui <- fluidPage(
    titlePanel("Análisis de Tweets de F1"),
    sidebarLayout(
      sidebarPanel(
        selectInput("selected_year", "Selecciona el Año:", choices = unique(year(tweets_df$date))),
        selectInput("selected_circuit", "Selecciona Circuitos:", choices = unique(tweets_df$name), multiple = TRUE),
        selectInput("selected_accounts", "Selecciona Cuentas de Twitter:", choices = mentions_list, multiple = TRUE),
        actionButton("update", "Actualizar")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Análisis Exploratorio", 
                  withSpinner(plotOutput("tweets_anio_plot")),
                  withSpinner(plotOutput("tweets_circuito_plot")),
                  withSpinner(plotOutput("cuentas_mas_mencionadas_plot")),
                  withSpinner(plotOutput("hashtags_mas_usados_plot"))),
          tabPanel("AFC Menciones", 
                  uiOutput("chi2_info_menciones"),
                  withSpinner(plotOutput("afc_menciones_plot"))),
          tabPanel("AFC Circuitos", 
                  uiOutput("chi2_info_circuitos"),
                  withSpinner(plotOutput("afc_circuitos_plot")),
                  withSpinner(plotOutput("alternative_plot")))
        )
      )
    )
  )

  server <- function(input, output) {
    observeEvent(input$update, {
      if (length(input$selected_accounts) < 2) {
        showNotification("Selecciona al menos 2 pilotos.", type = "error")
        return(NULL)
      }
      
      if (length(input$selected_circuit) < 2) {
        showNotification("Selecciona al menos 2 circuitos.", type = "error")
        return(NULL)
      }
      
      showNotification("Actualizando gráficos, por favor espere...", type = "message")
      
      selected_year <- input$selected_year
      selected_circuit <- input$selected_circuit
      selected_accounts <- input$selected_accounts
      
      filtered_data <- tweets_df %>%
        filter(year(date) == selected_year & name %in% selected_circuit)
      
      if (nrow(filtered_data) == 0) {
        showNotification("No hay datos para los filtros seleccionados.", type = "error")
        return(NULL)
      }
      
      tweets_df_expanded <- filtered_data %>%
        separate_rows(mentions, sep = ",\\s*")
      
      if (length(selected_accounts) > 0) {
        tweets_df_expanded <- tweets_df_expanded %>%
          filter(mentions %in% selected_accounts)
      }
      
      if (nrow(tweets_df_expanded) == 0) {
        showNotification("No hay datos para las cuentas seleccionadas.", type = "error")
        return(NULL)
      }
      
      # Análisis Exploratorio
      output$tweets_anio_plot <- renderPlot({
        tweets_por_anio <- tweets_df %>%
          mutate(año = year(date)) %>%
          group_by(año) %>%
          summarise(num_tweets = n(), .groups = 'drop') %>%
          arrange(desc(num_tweets))
        
        ggplot(tweets_por_anio, aes(x = reorder(año, num_tweets), y = num_tweets, fill = as.factor(año))) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_minimal() +
          labs(title = "Número de Tweets por Año", x = "Año", y = "Número de Tweets") +
          theme(legend.position = "none")
      })
      
      output$tweets_circuito_plot <- renderPlot({
        tweets_por_circuito <- tweets_df %>%
          group_by(name) %>%
          summarise(num_tweets = n(), .groups = 'drop') %>%
          arrange(desc(num_tweets)) %>%
          slice_head(n = 15)
        
        ggplot(tweets_por_circuito, aes(x = reorder(name, num_tweets), y = num_tweets, fill = name)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_minimal() +
          labs(title = "Número de Tweets por Circuito", x = "Circuito", y = "Número de Tweets") +
          theme(legend.position = "none")
      })
      
      output$cuentas_mas_mencionadas_plot <- renderPlot({
        mentions_df <- tweets_df %>%
          filter(mentions != "") %>%
          separate_rows(mentions, sep = ",\\s*") %>%
          count(mentions, name = "count", sort = TRUE) %>%
          top_n(15, count)
        
        ggplot(mentions_df, aes(x = reorder(mentions, count), y = count)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          coord_flip() +
          theme_minimal() +
          labs(title = "Top 15 Cuentas de Twitter Más Mencionadas",
              x = "Cuenta de Twitter",
              y = "Número de Menciones")
      })
      
      output$hashtags_mas_usados_plot <- renderPlot({
        hashtags_df <- tweets_df %>%
          filter(hashtags != "", !is.na(hashtags)) %>%
          filter(str_detect(hashtags, "\\[.*?\\]")) %>%
          filter(hashtags != "[]") %>%
          mutate(hashtags = str_extract_all(hashtags, "\\w+")) %>%
          unnest(hashtags) %>%
          count(hashtags, name = "frecuencia", sort = TRUE) %>%
          top_n(15, frecuencia)
        
        ggplot(hashtags_df, aes(x = reorder(hashtags, frecuencia), y = frecuencia)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          coord_flip() +
          theme_minimal() +
          labs(title = "Top 10 Hashtags Más Usados",
              x = "Hashtags",
              y = "Frecuencia")
      })
      
      # AFC Menciones
      output$afc_menciones_plot <- renderPlot({
        tweets_df_expanded <- filtered_data %>%
          separate_rows(mentions, sep = ",\\s*") %>%
          filter(mentions %in% selected_accounts)
        
        frequency_table <- tweets_df_expanded %>%
          count(mentions, sentiment_category) %>%
          spread(key = sentiment_category, value = n, fill = 0)
        
        frequency_table <- as.data.frame(frequency_table)
        rownames(frequency_table) <- frequency_table$mentions
        frequency_table <- select(frequency_table, -mentions)
        
        chi2_test <- chisq.test(frequency_table)
        p_value <- chi2_test$p.value
        
        if (p_value > 0.5) {
          showNotification("El p-value es mayor a 0.5. No tiene sentido realizar el AFC.", type = "error")
          return(NULL)
        }
        
        chi2_color <- if (p_value < 0.05) {
          "red"
        } else if (p_value < 0.1) {
          "yellow"
        } else {
          "green"
        }
        
        output$chi2_info_menciones <- renderUI({
          div(
            style = paste("background: linear-gradient(", chi2_color, ", white); color: black; padding: 20px; margin: 20px; border-radius: 10px;"),
            h4("P-Value del Test Chi2"),
            p(format(round(p_value, 5), nsmall = 5))
          )
        })
        
        afc <- FactoMineR::PCA(frequency_table, graph = FALSE)
        
        if(is.null(nrow(afc$row$coord)) || is.null(ncol(afc$row$coord)) || nrow(afc$row$coord) < 2 || ncol(afc$row$coord) < 2){
          showNotification("No hay suficientes dimensiones para mostrar el gráfico de biplot.", type = "warning")
          return(NULL)
        }
        
        fviz_ca_biplot(afc, repel = TRUE, 
                      label = "all",
                      col.row = "cos2",
                      col.col = "contrib",
                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      ggtheme = theme_minimal()) +
          labs(title = "Gráfico Biplot",
              x = "Dimensión 1", y = "Dimensión 2")
      })
      
      # AFC Circuitos
      output$afc_circuitos_plot <- renderPlot({
        frequency_table_circuits <- filtered_data %>%
          count(name, sentiment_category) %>%
          spread(key = sentiment_category, value = n, fill = 0)
        
        frequency_table_circuits <- as.data.frame(frequency_table_circuits)
        rownames(frequency_table_circuits) <- frequency_table_circuits$name
        frequency_table_circuits <- select(frequency_table_circuits, -name)
        
        chi2_test <- chisq.test(frequency_table_circuits)
        p_value <- chi2_test$p.value
        
        if (p_value > 0.5) {
          showNotification("El p-value es mayor a 0.5. No tiene sentido realizar el AFC.", type = "error")
          return(NULL)
        }
        
        chi2_color <- if (p_value < 0.05) {
          "red"
        } else if (p_value < 0.1) {
          "yellow"
        } else {
          "green"
        }
        
        output$chi2_info_circuitos <- renderUI({
          div(
            style = paste("background: linear-gradient(", chi2_color, ", white); color: black; padding: 20px; margin: 20px; border-radius: 10px;"),
            h4("P-Value del Test Chi2"),
            p(format(round(p_value, 5), nsmall = 5))
          )
        })
        
        afc <- FactoMineR::PCA(frequency_table_circuits, graph = FALSE)
        
        if(is.null(nrow(afc$row$coord)) || is.null(ncol(afc$row$coord)) || nrow(afc$row$coord) < 2 || ncol(afc$row$coord) < 2){
          showNotification("No hay suficientes dimensiones para mostrar el gráfico de biplot.", type = "warning")
          return(NULL)
        }
        
        fviz_ca_biplot(afc, repel = TRUE, 
                      label = "all",
                      col.row = "cos2",
                      col.col = "contrib",
                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      ggtheme = theme_minimal()) +
          labs(title = "Gráfico Biplot",
              x = "Dimensión 1", y = "Dimensión 2")
      })
    })
  }

  shinyApp(ui = ui, server = server)
}

mapa_popularidad_circuitos_año <- function() {
  ui <- fluidPage(
    titlePanel("Visualización de popularidad de los circuitos por año"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("dataset", "Selecciona el año:",
          choices = c("2021" = "result_21", "2022" = "result_22"))
      ),
      mainPanel(
        plotlyOutput("popularityPlot")
      )
    )
  )

  server <- function(input, output) {
    output$popularityPlot <- renderPlotly({
      dataset <- switch(input$dataset,
                        "result_21" = datos_mapa_circuitos_21,
                        "result_22" = datos_mapa_circuitos_22)
      year <- switch(input$dataset,
                    "result_21" = "2021",
                    "result_22" = "2022")
      
      g <- list(
        scope = 'world',
        projection = list(type = 'orthographic'),
        showland = TRUE,
        landcolor = 'rgb(217, 217, 217)',
        showocean = TRUE,
        oceancolor = 'rgb(204, 255, 255)',
        subunitwidth = 1,
        countrywidth = 1,
        subunitcolor = 'rgb(255, 255, 255)',
        countrycolor = 'rgb(255, 255, 255)'
      )
      
      fig <- plot_geo(dataset, locationmode = 'ISO-3', sizes = c(1, 250))
      fig <- fig %>% add_markers(
        x = ~lon, y = ~lat, size = ~porc_positiva, marker = list(sizeref = 0.05, sizemode = "area"), color = 'pop_positiva',
        hoverinfo = "text",
        mode = 'text',
        text = ~competicion,
        textfont = list(
          size = 10,  
          color = 'black'
        )) %>%
        add_markers(
          x = ~lon, y = ~lat, size = ~porc_negativa, marker = list(sizeref = 0.05, sizemode = "area"), color = 'pop_negativa', 
          hoverinfo = "text",
          mode = 'text',
          text = ~competicion,
          textfont = list(
            size = 10, 
            color = 'black'
          )) %>%
        add_markers(
          x = ~lon, y = ~lat, size = ~porc_neutra, marker = list(sizeref = 0.05, sizemode = "area"), color = 'pop_neutra', 
          hoverinfo = "text",
          mode = 'text',
          text = ~competicion,
          textfont = list(
            size = 10,  
            color = 'black'
          ))
      
      fig <- fig %>% layout(title = paste('Popularidad en', year), geo = g)
      
      fig
    })
  }

  shinyApp(ui, server)
}

mapa_visualización_carrera_tweets <- function() {
  ui <- fluidPage(
    titlePanel("Visualización de popularidad de la carrera según tweets publicados por horas"),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )

  server <- function(input, output) {
    output$distPlot <- renderPlot({
      ggplot(datos_mapa_carrera_tweets, aes(x = time, y = conteo, color = sentimientos, group = sentimientos)) +
        geom_line(size = 1) +
        labs(x = "Hora de publicación", y = "Número de Tweets", color = "Sentimiento") +
        theme_minimal() +
        ggtitle("Popularidad de la carrera según tweets publicados por horas")
    })
  }

  shinyApp(ui, server)
}

mapa_paises_mas_pilotos <- function(){
  ui <- fluidPage(
    titlePanel("Visualización de países con más pilotos"),
    mainPanel(
      plotlyOutput("plot")
    )
  )

  server <- function(input, output) {
    output$plot <- renderPlotly({
      g <- list(
        scope = 'world',
        projection = list(type = 'orthographic'),
        showland = TRUE,
        landcolor = ('rgb(217, 217, 217)'),
        showocean = TRUE,
        oceancolor = 'rgb(204, 255, 255)',
        subunitwidth = 1,
        countrywidth = 1,
        subunitcolor = 'rgb(255,255,255)',
        countrycolor = 'rgb(255,255,255)'
      )
      
      fig <- plot_geo(datos_mapa_paises_pilotos, locationmode = 'ISO-3', sizes = c(1, 250))
      
      fig <- fig %>% add_markers(
        x = ~long, y = ~lat, size = ~porc_baja, marker = list(sizeref = 0.05, sizemode = "area"), color = 'poco',
        hoverinfo = "text",
        mode = 'text',
        text = ~nacionalidad,
        textfont = list(
          size = 10,  # Tamaño del texto
          color = 'black'
        )) %>%
        add_markers(
          x = ~long, y = ~lat, size = ~porc_media, marker = list(sizeref = 0.05, sizemode = "area"), color = 'medio', 
          hoverinfo = "text",
          mode = 'text',
          text = ~nacionalidad,
          textfont = list(
            size = 10,  # Tamaño del texto
            color = 'black'
          )) %>%
        add_markers(
          x = ~long, y = ~lat, size = ~porc_alta, marker = list(sizeref = 0.05, sizemode = "area"), color = 'alto', 
          hoverinfo = "text",
          mode = 'text',
          text = ~nacionalidad,
          textfont = list(
            size = 10,  # Tamaño del texto
            color = 'black'
          ))
      
      fig <- fig %>% layout(title = 'Países con más pilotos', geo = g)
      
      fig
    })
  }

  shinyApp(ui = ui, server = server)
}

mapa_tiempos_realizados_tweets <- function(){
  ui <- fluidPage(
    titlePanel("Relación de tiempos realizados con tweets"),
    sidebarLayout(
      sidebarPanel(
        selectInput("competicion", "Seleccionar competición:",
                    choices = NULL) # No incluimos las opciones aquí
      ),
      mainPanel(
        plotOutput("grafico")
      )
    )
  )

  server <- function(input, output, session) {
    data <- reactiveValues(datos_mapa_tiempos_tweets = NULL)
    
    observe({
      data$result <- datos_mapa_tiempos_tweets
      updateSelectInput(session, "competicion", choices = unique(datos_mapa_tiempos_tweets$competicion))
    })
    
    output$grafico <- renderPlot({
      req(data$result) # Asegurarse de que 'result' no sea NULL
      df <- subset(data$result, competicion == input$competicion)

      grafico_barras <- ggplot(df, aes(x = competicion, y = recuento, fill = tipo_sent)) +
        geom_bar(position = 'dodge', stat = 'identity') +  # Barras laterales
        labs(title = "Relación de Tiempos Realizados con Tweets",
            x = "Competición", y = "Tiempos Realizados") +
        scale_fill_manual(values = c("positivo" = "green", "negativo" = "red", "neutro" = "grey")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      grafico_barras
    })
  }

  shinyApp(ui = ui, server = server)
}

mapa_circuitos_f1 <- function() {
  ui <- fluidPage(
    titlePanel("Visualización de Circuitos de F1"),
    mainPanel(
      plotlyOutput("plot")
    )
  )

  server <- function(input, output) {
    output$plot <- renderPlotly({
      g <- list(
        scope = 'world',
        projection = list(type = 'orthographic'),
        showland = TRUE,
        landcolor = 'rgb(217, 217, 217)',
        showocean = TRUE,
        oceancolor = 'rgb(204, 255, 255)',
        subunitwidth = 1,
        countrywidth = 1,
        subunitcolor = 'rgb(255,255,255)',
        countrycolor = 'rgb(255,255,255)'
      )
      
      fig <- plot_geo(datos_mapa_circuitos_f1, locationmode = 'ISO-3', sizes = c(1, 250))
      fig <- fig %>%
        add_text(
          x = ~lon, y = ~lat, text = ~name, marker = list(sizeref = 0.05, sizemode = "area"),
          textfont = list(size = 10, color = 'black'),
          hoverinfo = "text",
          hovertext = ~paste(
            'Circuito: ', name, '<br>',
            'País: ', country, '<br>',
            'Localización: ', location, '<br>',
            'Número de carreras realizadas: ', num_carreras, '<br>',
            'Número de pilotos que han participado: ', num_pilotos
          )
        )
      
      fig <- fig %>% layout(title = '', geo = g)
      fig
    })
  }

  shinyApp(ui = ui, server = server)
}

mapa_boxes_posicion_final <- function() {
  ui <- fluidPage(
    titlePanel("Paradas en boxes y posición final"),
    sidebarLayout(
      sidebarPanel(
        selectInput("piloto", "Seleccione el Piloto:", choices = unique(datos_mapa_boxes_posicion_final$pilotos)),
        selectInput("año", "Seleccione el Año:", choices = unique(datos_mapa_boxes_posicion_final$año))
      ),
      mainPanel(
        plotOutput("boxStopsPlot")
      )
    )
  )

  server <- function(input, output) {
    output$boxStopsPlot <- renderPlot({
      plot_box_stops_and_position(input$piloto, input$año)
    })
    
    plot_box_stops_and_position <- function(piloto_seleccionado, año_seleccionado) {
      datos_filtrados <- datos_mapa_boxes_posicion_final %>%
        filter(pilotos == piloto_seleccionado & año == año_seleccionado) %>%
        group_by(competicion) %>%
        summarise(pos_final = mean(pos_final), para_box = mean(para_box)) %>%
        ungroup()
      
      competiciones_piloto <- datos_filtrados$competicion
      
      datos_completos <- datos_filtrados
      
      ggplot(datos_completos, aes(x = competicion)) +
        geom_bar(aes(y = para_box, fill = "Paradas en Boxes"), stat = "identity") +
        geom_point(aes(y = pos_final, color = "Posición Final"), size = 3) +
        scale_color_manual(values = c("red"), labels = c("Posición Final")) +
        scale_fill_manual(values = c("blue"), labels = c("Paradas en Boxes")) +
        labs(title = paste("Paradas en Boxes y Posición Final de", piloto_seleccionado, "en", año_seleccionado),
            x = "Competición", y = "Número paradas en boxes/Posición final") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_x_discrete(limits = competiciones_piloto)
    }
  }

  shinyApp(ui = ui, server = server)
}

ui <- fluidPage(
  titlePanel(title = "Análisis de Datos de Fórmula 1"),
  tabsetPanel(
    # Pestaña para Clasificaciones
    tabPanel("Clasificaciones", clasificaciones_app()),
    
    # Pestaña para Visualización de Correlaciones
    tabPanel("Visualización de Correlaciones", grafo_correlaciones(), visualizacion_correlaciones()),
    
    # Pestaña para Análisis de Reglas de Asociación
    tabPanel("Análisis de Reglas de Asociación", reglas_app()),
    
    # Pestaña para Análisis de Componentes Principales (PCA)
    tabPanel("Análisis de Componentes Principales (PCA)", pca_app()),

    tabPanel("Visualización de Mapas", br(), 
      tabsetPanel(
        tabPanel("Popularidad de Circuitos por Año", mapa_popularidad_circuitos_año()),
        tabPanel("Popularidad de la Carrera según Tweets Publicados por Horas", mapa_visualización_carrera_tweets()),
        tabPanel("Países con más Pilotos", mapa_paises_mas_pilotos()),
        tabPanel("Relación de Tiempos Realizados con Tweets", mapa_tiempos_realizados_tweets()),
        tabPanel("Visualización de Circuitos de F1", mapa_circuitos_f1()),
        tabPanel("Paradas en Boxes y Posición Final", mapa_boxes_posicion_final())
      )
    ),

    # Pestaña para Análisis de Tweets
    tabPanel("Análisis de Tweets", tweets_viz())
  )
)

# Define el servidor
server <- function(input, output, session) {
  # No se necesita ningún servidor específico para la presentación de Shiny
}

# Ejecuta la aplicación de Shiny
shinyApp(ui = ui, server = server)


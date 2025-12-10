#Ultima andando
#–– Librerías necesarias ––#
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(rmarkdown)
library(tibble)
library(purrr)
library(tidyr)
library(shinythemes)

# Feedback global
feedbackGlobal <- reactiveValues(
  Excelente = 0,
  Genial    = 0,
  Aburrida  = 0,
  Corazon   = 0,
  Up        = 0,
  Down      = 0,
  votantes  = character()  # ← aquí guardamos los session$token
)

#––– Módulo: Opción múltiple con gráfico –––#
mcqPlotUI <- function(id, question, choices, graph_height = "250px") {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("plot"), height = graph_height),
    radioButtons(
      inputId = ns("answer"),
      label   = question,
      choices = choices,
      selected = character(0)
    ),
    actionButton(ns("submit"), "Enviar respuesta"),
    uiOutput(ns("feedback")),
    tags$hr()
  )
}

mcqPlotServer <- function(id, question, data, plot_fun, correct, rv) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlotly({
      p <- plot_fun(data) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p)
    })
    
    observeEvent(input$submit, {
      req(input$answer)
      is_correct <- input$answer == correct
      output$feedback <- renderUI({
        if (is_correct) {
          div(style = "color: green;", "✅ ¡Correcto!")
        } else {
          div(
            style = "color: red;",
            "❌ Incorrecto. Respuesta correcta: ", strong(correct)
          )
        }
      })
      key <- session$ns("")
      rv$results[[key]] <- list(
        pregunta = question,
        seleccion = input$answer,
        correcta  = is_correct,
        hora      = Sys.time()
      )
    })
  })
}

#––– Módulo MCQ para identificación de variables jurídicas –––#
mcqVarUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("📝 Quiz: Identificación de tipo de variable"),
    p("Selecciona el tipo correcto para cada ejemplo:"),
    uiOutput(ns("mcq_list")),
    actionButton(ns("submit_vars"), "Enviar respuestas"),
    uiOutput(ns("var_feedback"))
  )
}

mcqVarServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ejemplos <- tibble::tribble(
      ~ejemplo, ~correcta,
      "Número de sentencias por año", "Dependiente",
      "Aprobación de una nueva ley", "Independiente",
      "Nivel socioeconómico de la población", "Contextual",
      "Cantidad de denuncias de corrupción", "Dependiente",
      "Región geográfica donde ocurre el delito", "Contextual",
      "Existencia de sanción penal", "Independiente"
    )
    opciones <- c("Independiente", "Dependiente", "Contextual")
    ns <- session$ns
    output$mcq_list <- renderUI({
      lapply(seq_len(nrow(ejemplos)), function(i) {
        radioButtons(
          inputId = ns(paste0("mcqvar_", i)),
          label = paste(i, ejemplos$ejemplo[i]),
          choices = opciones,
          selected = character(0)
        )
      })
    })
    observeEvent(input$submit_vars, {
      respuestas <- sapply(seq_len(nrow(ejemplos)), function(i) {
        input[[paste0("mcqvar_", i)]]
      })
      corr <- ejemplos$correcta
      aciertos <- respuestas == corr
      feedback <- lapply(seq_along(aciertos), function(i) {
        if (is.na(respuestas[i])) {
          div(style = "color: orange;", paste(i, "Sin respuesta"))
        } else if (aciertos[i]) {
          div(style = "color: green;", paste(i, "✅", ejemplos$ejemplo[i], "- Correcto"))
        } else {
          div(style = "color: red;", paste(i, "❌", ejemplos$ejemplo[i]),
              "Respuesta correcta:", strong(corr[i]))
        }
      })
      score <- sum(aciertos, na.rm = TRUE)
      output$var_feedback <- renderUI({
        tagList(feedback, tags$hr(), h4(paste("Puntaje:", score, "/", nrow(ejemplos))))
      })
      rv$var_quiz_score <- score
    })
  })
}

#––– Módulo interactivo de hipótesis –––#
mcqHypoUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("🧑🔬 Quiz interactivo: Formulación de hipótesis"),
    p("Lee cada enunciado y selecciona la hipótesis correctamente formulada."),
    uiOutput(ns("mcq_list")),
    actionButton(ns("submit_hypo"), "Enviar respuestas"),
    uiOutput(ns("hypo_feedback")),
    tags$hr(),
    h4("Puntuación:"),
    verbatimTextOutput(ns("score"))
  )
}

mcqHypoServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    # Ejercicios de hipótesis y opciones
    ejercicios <- tibble::tribble(
      ~enunciado, ~opciones, ~correcta,
      "Quiero saber si las nuevas leyes de ciberdelitos han impactado en el número de denuncias en Montevideo.",
      c(
        "Si se aprueban nuevas leyes de ciberdelitos, entonces el número de denuncias de delitos informáticos en Montevideo aumentará.",
        "El número de denuncias está relacionado con el acceso a Internet.",
        "Las leyes de ciberdelitos son importantes."
      ),
      "Si se aprueban nuevas leyes de ciberdelitos, entonces el número de denuncias de delitos informáticos en Montevideo aumentará.",
      "Investigo si el nivel socioeconómico de la población influye sobre la cantidad de denuncias de corrupción.",
      c(
        "A mayor nivel socioeconómico, menor cantidad de denuncias de corrupción.",
        "La corrupción es un problema social.",
        "El nivel socioeconómico es una variable relevante."
      ),
      "A mayor nivel socioeconómico, menor cantidad de denuncias de corrupción.",
      "Exploro la relación entre la región geográfica y los delitos ambientales denunciados.",
      c(
        "Si la región es costera, los delitos ambientales denunciados serán más frecuentes.",
        "Los delitos ambientales afectan el ecosistema.",
        "La región geográfica es importante en la investigación."
      ),
      "Si la región es costera, los delitos ambientales denunciados serán más frecuentes."
    )
    ns <- session$ns
    output$mcq_list <- renderUI({
      lapply(seq_len(nrow(ejercicios)), function(i) {
        radioButtons(
          inputId = ns(paste0("mcqh_", i)),
          label = paste(i, ejercicios$enunciado[i]),
          choices = ejercicios$opciones[[i]],
          selected = character(0)
        )
      })
    })
    observeEvent(input$submit_hypo, {
      respuestas <- sapply(seq_len(nrow(ejercicios)), function(i) {
        input[[paste0("mcqh_", i)]]
      })
      corr <- ejercicios$correcta
      aciertos <- respuestas == corr
      feedback <- lapply(seq_along(aciertos), function(i) {
        if (is.na(respuestas[i])) {
          div(style = "color: orange;", paste(i, "Sin respuesta"))
        } else if (aciertos[i]) {
          div(style = "color: green;", paste(i, "✅", ejercicios$enunciado[i], "- Correcto"))
        } else {
          div(style = "color: red;", paste(i, "❌", ejercicios$enunciado[i]),
              "Respuesta correcta:", strong(corr[i]))
        }
      })
      score <- sum(aciertos, na.rm = TRUE)
      output$hypo_feedback <- renderUI({
        tagList(feedback)
      })
      output$score <- renderText({
        paste(score, "/", nrow(ejercicios), "puntos")
      })
      rv$hypo_quiz_score <- score
    })
  })
}

#––– Datos de ejemplo –––#
set.seed(42)
years  <- 2010:2023
topics <- c(
  "Violencia doméstica",
  "Acceso a la justicia",
  "Corrupción/Transparencia",
  "Derechos en cárceles",
  "Medio ambiente",
  "Delitos informáticos"
)

sim_trend <- function(base, slope, noise_sd = 10) {
  base + slope * (years - min(years)) +
    rnorm(length(years), 0, noise_sd)
}

df_list <- list(
  "Violencia doméstica"        = round(pmax(5,  sim_trend(80,  5, 12))),
  "Acceso a la justicia"       = round(pmax(2,  sim_trend(40,  3,  8))),
  "Corrupción/Transparencia"   = round(pmax(1,  sim_trend(20,  4,  6))),
  "Derechos en cárceles"       = round(pmax(3,  sim_trend(25,  2,  8))),
  "Medio ambiente"             = round(pmax(1,  sim_trend(10,  6,  5))),
  "Delitos informáticos"       = round(pmax(0,  sim_trend(2,   8,  6)))
)

df <- tibble(
  año   = rep(years, times = length(topics)),
  tema  = rep(topics, each  = length(years)),
  casos = as.integer(unlist(df_list))
)

#––– UI principal –––#
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-size: 18px; line-height: 1.45; background-color: #f9f9f9; }
      h1,h2,h3,h4 { color:#003366; font-weight:700; margin-top:0.6em; margin-bottom:0.4em; }
      table, th, td, label, .control-label { font-size: 17px; }
      .tab-content, .container-fluid { padding: 20px 24px; }
      iframe { border-radius: 12px; min-height: 450px; }
      .shiny-input-container input, .shiny-input-container textarea { font-size: 17px !important; }
    "))
  ),
  titlePanel("Taller de Investigación - FCS - Mag. José González"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "selected_temas", "Selecciona temas:",
        choices = topics, selected = topics
      ),
      sliderInput(
        "selected_anos", "Rango de años:",
        min   = min(years), max = max(years),
        value = c(min(years), max(years)), step = 1
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Líneas",        plotlyOutput("plot_lines")),
        tabPanel("Área apilada",  plotlyOutput("plot_area")),
        tabPanel("Facetas líneas", plotlyOutput("plot_facets_lines")),
        tabPanel("Facetas área",   plotlyOutput("plot_facets_area")),
        tabPanel("Variables",
                 fluidRow(
                   column(12,
                          h2("📘 Tipos de Variables en la Investigación"),
                          p("En la investigación en Ciencias Sociales, 
                las variables son los elementos que permiten describir, 
                analizar y explicar fenómenos. Aquí aprenderás los tipos principales 
                con ejemplos.")
                   )
                 ),
                 br(),
                 fluidRow(
                   column(4,
                          wellPanel(
                            h3("Variable Independiente (VI)"),
                            p("👉 Es la variable que explica o influye sobre otra. 
                  El investigador la manipula u observa como posible causa."),
                            tags$ul(
                              tags$li("Ejemplo: Regulaciones en materia de ciberdelitos."),
                              tags$li("En Montevideo: implementación de nuevas leyes sobre delitos informáticos.")
                            )
                          )
                   ),
                   column(4,
                          wellPanel(
                            h3("Variable Dependiente (VD)"),
                            p("👉 Es la variable que se ve afectada por la independiente. 
                  Representa el efecto o resultado de la relación."),
                            tags$ul(
                              tags$li("Ejemplo: Número de delitos informáticos registrados."),
                              tags$li("En Montevideo: aumento o disminución de casos reportados en Fiscalía.")
                            )
                          )
                   ),
                   column(4,
                          wellPanel(
                            h3("Variable Contextual"),
                            p("👉 Es la variable que describe el entorno, pero no se manipula. 
                  Sirve para comprender mejor las condiciones de estudio."),
                            tags$ul(
                              tags$li("Ejemplo: Nivel de acceso a Internet en la población."),
                              tags$li("En Montevideo: porcentaje de jóvenes con acceso a dispositivos digitales.")
                            )
                          )
                   )
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          h3("Ejemplo integrado"),
                          p("Si queremos investigar la relación entre la aprobación de nuevas leyes sobre 
                 ciberdelitos (VI) y el número de casos denunciados en Montevideo (VD), 
                 debemos tener en cuenta factores como el acceso a Internet o el nivel de 
                 educación digital (variables contextuales).")
                   )
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          mcqVarUI("mcqvarmod")
                   )
                 )
        ),
       
         #––– NUEVO TAB INDICADORES –––#
        tabPanel("Indicadores",
                 fluidRow(
                   column(12,
                          h2("📊 Indicadores en Investigación"),
                          p("Los indicadores permiten medir fenómenos complejos a través de variables observables. 
                 Son fundamentales para transformar conceptos abstractos (como justicia, equidad o corrupción) 
                 en datos que pueden ser analizados empíricamente."),
                          tags$ul(
                            tags$li("Miden dimensiones específicas de un fenómeno."),
                            tags$li("Permiten comparaciones en el tiempo y entre contextos."),
                            tags$li("Ayudan a evaluar políticas públicas y otros procesos.")
                          ),
                          tags$hr()
                   )
                 ),
                 fluidRow(
                   column(6,
                          wellPanel(
                            h3("Ejercicio 1: Identificación de Indicadores"),
                            p("Selecciona el mejor indicador para medir el acceso a la justicia:"),
                            radioButtons("ind1", NULL,
                                         choices = c(
                                           "Número de abogados por cada 1000 habitantes",
                                           "Color de los juzgados",
                                           "Cantidad de autos oficiales"
                                         ),
                                         selected = character(0)
                            ),
                            actionButton("check_ind1", "Verificar"),
                            uiOutput("feedback_ind1")
                          )
                   ),
                   column(6,
                          wellPanel(
                            h3("Ejercicio 2: Interpretación de Indicadores"),
                            p("El siguiente gráfico muestra la evolución de casos de corrupción denunciados. 
                   Observa y responde:"),
                            plotlyOutput("indic_plot", height = "250px"),
                            radioButtons("ind2", "¿Qué conclusión es correcta?",
                                         choices = c(
                                           "Las denuncias se han mantenido constantes.",
                                           "Existe una tendencia creciente en las denuncias.",
                                           "La corrupción ha desaparecido."
                                         ),
                                         selected = character(0)
                            ),
                            actionButton("check_ind2", "Verificar"),
                            uiOutput("feedback_ind2")
                          )
                   )
                 )
        ),
        
        #––– FIN TAB INDICADORES –––#
 
               tabPanel("Hipótesis",
                 fluidRow(
                   column(12,
                          h2("📚 Formulación de Hipótesis en Investigación"),
                          p("La hipótesis es una proposición que relaciona dos o más variables y que puede ser puesta a prueba en la investigación. 
                En ciencias sociales, las hipótesis permiten anticipar resultados y orientar el análisis de los datos."),
                          tags$ul(
                            tags$li("Debe ser clara, específica y comprobable."),
                            tags$li("Relaciona variables independientes, dependientes y contextuales."),
                            tags$li("Ejemplo: Si se incrementa el acceso a Internet, aumentan los casos de ciberdelitos denunciados.")
                          )
                   )
                 ),
                 br(),
                 h2("🧾 Nota: Delitos informáticos"),
                 tags$ul(
                   tags$li("🧩 Hasta 2024, Uruguay no contaba con una ley integral de ciberdelitos. La Ley N.º 20.327 fue promulgada recién en septiembre de 2024."),
                   tags$li("📚 Antes de dicha ley, los delitos informáticos se procesaban bajo figuras dispersas del Código Penal (falsificación documentaria, violación de correspondencia, daño, estafa, etc.), lo que dificulta su trazabilidad estadística como categoría única."),
                   tags$li("📉 El Poder Judicial no publica estadísticas específicas sobre “delitos informáticos” como categoría separada en sus anuarios ni en el Observatorio de Justicia."),
                   tags$li("🔎 Las cifras usadas son hipoteticas y aproximadas, y reflejan el crecimiento de denuncias por estafas digitales, acceso indebido, suplantación de identidad, y pornografía infantil en medios digitales, según reportes del Ministerio del Interior y estudios como los de Martín Pecoy Taque.")
                 ),
                 br(),
                 h2("🧾 Nota: Medio Ambiente"),
                 tags$ul(
                   tags$li("🔎 Tras revisar fuentes oficiales y académicas, no existen estadísticas públicas desglosadas año por año sobre la cantidad de casos judiciales específicamente relacionados con medio ambiente en Uruguay entre 2010 y 2023. Esto se debe a varios factores:"),
                   tags$li("📚 No existe una categoría judicial explícita “medio ambiente” en los anuarios estadísticos del Poder Judicial. Los asuntos ambientales se tramitan bajo materias como civil, penal, contencioso administrativo o constitucional, según el tipo de daño o conflicto."),
                   tags$li("📚 La Ley General del Ambiente (Ley 17.283) establece sanciones administrativas, pero no generó una tipificación penal clara hasta el proyecto de ley de delitos ambientales presentado en 2017, aún sin aprobación definitiva."),
                   tags$li("📉 La Base de Jurisprudencia Nacional (BJN Pública) permite buscar casos ambientales, pero no ofrece estadísticas agregadas por año.")
                 ),
                 br(),
                 tags$hr(),
                 h4("Recursos complementarios:"),
                 tags$ul(
                   tags$li(
                     tags$a(href = "https://bjn.poderjudicial.gub.uy/BJNPUBLICA/busquedaSelectiva.seam", 
                            target = "_blank", 
                            "Base de Jurisprudencia Nacional")),
                   tags$li(
                     tags$a(href = "https://pmb.parlamento.gub.uy/pmb/opac_css/index.php?lvl=categ_see&id=53736", 
                            target = "_blank", 
                            "Ciberdelitos-parlamento.gub.uy")),
                   
                   
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          mcqHypoUI("mcqhmod")
                   )
                 )
        ),
        
        tabPanel("Desafíos",
                 h4("🧠 Análisis de un tema"),
                 helpText("Selecciona un tema y presiona 'Analizar tema'"),
                 selectInput("quiz_tema", "Tema para analizar:", choices = topics),
                 actionButton("run_quiz", "Analizar tema"),
                 verbatimTextOutput("quiz_result"),
                 tags$hr(),
                 h4("🎮 Quiz de opción múltiple"),
                 mcqPlotUI("mcq1", "1. ¿Qué tema tiene el mayor total acumulado?", topics),
                 mcqPlotUI("mcq2", "2. ¿Qué tema muestra mayor variabilidad?", topics),
                 mcqPlotUI("mcq3", "3. ¿Cuál tuvo el mayor número de casos en el último año disponible ?", topics),
                 tags$hr(),
                 h4("✍️ Interpretación personal"),
                 helpText("Escribe tu interpretación sobre el tema analizado."),
                 textAreaInput("interpretation", "Tu interpretación:", rows = 5),
                 downloadButton("download_report", "Descargar informe en PDF"),
                 tags$hr(),
                 h4("🏆 Puntuación total:"),
                 verbatimTextOutput("score_display"),
                 tags$hr(),
                 h4("🧪 Formulación de hipótesis"),
                 helpText("Define tus variables y redacta la hipótesis:"),
                 selectInput("hypo_dv", "Variable dependiente (VD):", choices = c("Casos registrados" = "casos")),
                 selectInput("hypo_iv", "Variable independiente (VI):", choices = c("Tema" = "tema", "Año" = "año")),
                 checkboxGroupInput("hypo_context", "Variables contextuales:", choices = c("Tiempo" = "año", "Espacio" = "tema")),
                 textAreaInput("hypo_statement", "Hipótesis:", placeholder = "Ej: Si avanza el año, aumentan los casos de Medio ambiente.", rows = 3),
                 actionButton("add_hypo", "Agregar hipótesis"),
                 uiOutput("hypo_feedback"),
                 tags$br(),
                 tableOutput("hypo_table"),
                 downloadButton("download_hypotheses", "Descargar hipótesis en CSV")),
        
        tabPanel("Feedback",
                 fluidRow(
                   column(12, align = "center",
                          h3("¿Cómo calificarías esta clase?"),
                          actionButton("fb_excelente", "Excelente! 🙌", class = "btn btn-success"),
                          actionButton("fb_genial", "Genial! 🔥", class = "btn btn-primary"),
                          actionButton("fb_aburrida", "Aburrida 😴", class = "btn btn-warning"),
                          actionButton("fb_corazon", "❤️", class = "btn btn-danger"),
                          actionButton("fb_up", "👍", class = "btn btn-info"),
                          actionButton("fb_down", "👎", class = "btn btn-secondary")
                   )
                 ),
                 br(),
                 plotlyOutput("feedbackPlot"),
                 tableOutput("feedbackSummary")   # ← nueva línea
                 
        ),
      )
    )
  )
)


#––– Servidor –––#
server <- function(input, output, session) {
  rv <- reactiveValues(
    results    = list(),
    hypotheses = tibble(VD = character(), VI = character(), Contexto = character(), Hipótesis = character()),
    hypo_feedback = "",
    var_quiz_score = 0,
    hypo_quiz_score = 0
  )
 
   #––– SERVIDOR PARA TAB INDICADORES –––#
  observeEvent(input$check_ind1, {
    req(input$ind1)
    output$feedback_ind1 <- renderUI({
      if (input$ind1 == "Número de abogados por cada 1000 habitantes") {
        div(style = "color: green;", "✅ Correcto: mide acceso a justicia.")
      } else {
        div(style = "color: red;", "❌ Incorrecto. El mejor indicador es el número de abogados por cada 1000 habitantes.")
      }
    })
  })
  
  output$indic_plot <- renderPlotly({
    df_corr <- df %>% filter(tema == "Corrupción/Transparencia")
    p <- ggplot(df_corr, aes(x = año, y = casos)) +
      geom_line(color = "steelblue", linewidth = 1.2) +
      geom_point(color = "darkred", size = 2) +
      labs(title = "Denuncias de corrupción", x = "Año", y = "Casos") +
      theme_minimal()
    ggplotly(p)
  })
  
  observeEvent(input$check_ind2, {
    req(input$ind2)
    output$feedback_ind2 <- renderUI({
      if (input$ind2 == "Existe una tendencia creciente en las denuncias.") {
        div(style = "color: green;", "✅ Correcto: se observa una tendencia creciente.")
      } else {
        div(style = "color: red;", "❌ Incorrecto. El gráfico muestra un crecimiento en las denuncias.")
      }
    })
  })
 
  
   #––– FIN SERVIDOR INDICADORES –––#

    # --- INICIO LÓGICA FEEDBACK GLOBAL ---
  yaVoto <- function() {
    session$token %in% feedbackGlobal$votantes
  }
  observeEvent(input$fb_excelente, {
    if (!yaVoto()) {
      feedbackGlobal$Excelente <- feedbackGlobal$Excelente + 1
      feedbackGlobal$votantes <- c(feedbackGlobal$votantes, session$token)
    } else {
      showModal(modalDialog(
        title = "Ya votaste",
        "Solo puedes calificar la clase una vez.",
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(input$fb_genial, {
    if (!yaVoto()) {
      feedbackGlobal$Genial <- feedbackGlobal$Genial + 1
      feedbackGlobal$votantes <- c(feedbackGlobal$votantes, session$token)
    } else {
      showModal(modalDialog(
        title = "Ya votaste",
        "Solo puedes calificar la clase una vez.",
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(input$fb_aburrida, {
    if (!yaVoto()) {
      feedbackGlobal$Aburrida <- feedbackGlobal$Aburrida + 1
      feedbackGlobal$votantes <- c(feedbackGlobal$votantes, session$token)
    } else {
      showModal(modalDialog(
        title = "Ya votaste",
        "Solo puedes calificar la clase una vez.",
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(input$fb_corazon, {
    if (!yaVoto()) {
      feedbackGlobal$Corazon <- feedbackGlobal$Corazon + 1
      feedbackGlobal$votantes <- c(feedbackGlobal$votantes, session$token)
    } else {
      showModal(modalDialog(
        title = "Ya votaste",
        "Solo puedes calificar la clase una vez.",
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(input$fb_up, {
    if (!yaVoto()) {
      feedbackGlobal$Up <- feedbackGlobal$Up + 1
      feedbackGlobal$votantes <- c(feedbackGlobal$votantes, session$token)
    } else {
      showModal(modalDialog(
        title = "Ya votaste",
        "Solo puedes calificar la clase una vez.",
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(input$fb_down, {
    if (!yaVoto()) {
      feedbackGlobal$Down <- feedbackGlobal$Down + 1
      feedbackGlobal$votantes <- c(feedbackGlobal$votantes, session$token)
    } else {
      showModal(modalDialog(
        title = "Ya votaste",
        "Solo puedes calificar la clase una vez.",
        easyClose = TRUE
      ))
    }
  })
  
  # --- GRÁFICO INTERACTIVO CON FRECUENCIA Y PORCENTAJE ---
  output$feedbackPlot <- renderPlotly({
    # 1. Armar el data.frame con votos y porcentajes
    df_fb <- data.frame(
      Emoji = c("🙌", "🔥", "😴", "❤️", "👍", "👎"),
      Votos = c(
        feedbackGlobal$Excelente,
        feedbackGlobal$Genial,
        feedbackGlobal$Aburrida,
        feedbackGlobal$Corazon,
        feedbackGlobal$Up,
        feedbackGlobal$Down
      ),
      stringsAsFactors = FALSE
    )
    df_fb$Porcentaje <- round(df_fb$Votos / sum(df_fb$Votos) * 100, 1)
    
    # 2. Construir el ggplot con etiquetas y texto para tooltips
    p <- ggplot(df_fb, aes(x = Emoji, y = Votos, fill = Emoji,
                           text = paste0(
                             "Votos: ", Votos, "\n",
                             "Porcentaje: ", Porcentaje, " %"))) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = paste0(Votos, " (", Porcentaje, "%)")),
                vjust = -0.5, size = 5) +
      labs(
        title = "Resultados del feedback en tiempo real",
        x = "Reacciones",
        y = "Cantidad de votos"
      ) +
      theme_minimal(base_size = 14)
    
    # 3. Pasar a plotly con tooltip personalizado
    ggplotly(p, tooltip = "text") %>%
      layout(yaxis = list(range = c(0, max(df_fb$Votos) * 1.15)))
  })
  
  # --- TABLA RESUMEN CON FRECUENCIA Y PORCENTAJE ---
  output$feedbackSummary <- renderTable({
    df_fb <- data.frame(
      Emoji      = c("🙌", "🔥", "😴", "❤️", "👍", "👎"),
      Frecuencia = c(
        feedbackGlobal$Excelente,
        feedbackGlobal$Genial,
        feedbackGlobal$Aburrida,
        feedbackGlobal$Corazon,
        feedbackGlobal$Up,
        feedbackGlobal$Down
      ),
      stringsAsFactors = FALSE
    )
    df_fb$Porcentaje <- paste0(
      round(df_fb$Frecuencia / sum(df_fb$Frecuencia) * 100, 1), " %"
    )
    df_fb
  }, align = "c", striped = TRUE, hover = TRUE, spacing = "l")

  filtered_df <- reactive({
    req(input$selected_temas, input$selected_anos)
    df %>%
      filter(
        tema %in% input$selected_temas,
        año  >= input$selected_anos[1],
        año  <= input$selected_anos[2]
      )
  })
  
  output$plot_lines <- renderPlotly({
    data_in <- filtered_df()
    yrs <- seq(input$selected_anos[1], input$selected_anos[2])
    p_lines <- ggplot(data_in, aes(x = año, y = casos, color = tema)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = yrs, limits = c(min(yrs), max(yrs))) +
      scale_y_continuous(labels = comma_format()) +
      labs(
        title = "Evolución anual por tema",
        subtitle = "Datos simulados para fines didácticos",
        x = "Año",
        y = "Casos / registros",
        color = "Tema"
      ) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
    ggplotly(p_lines)
  })
  
  output$plot_area <- renderPlotly({
    data_in <- filtered_df()
    yrs <- seq(input$selected_anos[1], input$selected_anos[2])
    df_area <- data_in %>%
      group_by(año, tema) %>%
      summarize(casos = sum(casos), .groups = "drop") %>%
      pivot_wider(names_from = tema, values_from = casos) %>%
      arrange(año)
    df_area_long <- df_area %>%
      pivot_longer(-año, names_to = "tema", values_to = "casos") %>%
      group_by(año) %>%
      arrange(tema) %>%
      mutate(ymin = lag(cumsum(casos), default = 0), ymax = cumsum(casos)) %>%
      ungroup()
    p_area <- ggplot(df_area_long, aes(x = año, ymin = ymin, ymax = ymax, fill = tema)) +
      geom_ribbon(alpha = 0.9) +
      scale_x_continuous(breaks = yrs, limits = c(min(yrs), max(yrs))) +
      scale_y_continuous(labels = comma_format()) +
      labs(
        title = "Contribución anual por tema (área apilada)",
        subtitle = "Se observa el peso relativo de cada tema en el total",
        x = "Año",
        y = "Total anual (casos/registros)",
        fill = "Tema"
      ) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")
    ggplotly(p_area)
  })
  
  output$plot_facets_lines <- renderPlotly({
    data_in <- filtered_df()
    yrs <- seq(input$selected_anos[1], input$selected_anos[2])
    p_facets_lines <- ggplot(data_in, aes(x = año, y = casos, color = tema)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.5) +
      facet_wrap(~ tema, scales = "free_y", ncol = 2) +
      scale_x_continuous(breaks = yrs, limits = c(min(yrs), max(yrs))) +
      scale_y_continuous(labels = comma_format()) +
      labs(title = "Evolución anual por tema", x = "Año", y = "Casos/registros") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p_facets_lines) %>% layout(margin = list(l = 50, r = 30, b = 50, t = 50))
  })
  
  output$plot_facets_area <- renderPlotly({
    data_in <- filtered_df()
    yrs <- seq(input$selected_anos[1], input$selected_anos[2])
    df_facets <- data_in %>%
      group_by(tema) %>%
      arrange(año) %>%
      mutate(ymin = 0, ymax = casos) %>%
      ungroup()
    p_facets_area <- ggplot(df_facets, aes(x = año, ymin = ymin, ymax = ymax, fill = tema)) +
      geom_ribbon(alpha = 0.8) +
      facet_wrap(~ tema, scales = "free_y", ncol = 2) +
      scale_x_continuous(breaks = yrs, limits = c(min(yrs), max(yrs))) +
      scale_y_continuous(labels = comma_format()) +
      labs(title = "Contribución anual por tema", x = "Año", y = "Total anual (casos/registros)") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p_facets_area) %>% layout(margin = list(l = 50, r = 30, b = 50, t = 50))
  })
  
  observeEvent(input$run_quiz, {
    req(input$quiz_tema)
    dt <- df %>% filter(tema == input$quiz_tema)
    año_max   <- dt %>% slice_max(casos) %>% pull(año)
    total_cas <- sum(dt$casos)
    sd_cas    <- round(sd(dt$casos), 2)
    output$quiz_result <- renderText({
      paste0(
        "Tema: ", input$quiz_tema, "\n",
        "Año con más casos: ", año_max, "\n",
        "Total acumulado: ", total_cas, "\n",
        "Variabilidad (sd): ", sd_cas
      )
    })
  })
  
  question_texts <- reactive({
    data_in <- filtered_df()
    min_year <- min(data_in$año)
    max_year <- max(data_in$año)
    list(
      q1 = paste0("1. ¿Qué tema tiene el mayor total acumulado en el período ", min_year, "–", max_year, "?"),
      q2 = paste0("2. ¿Qué tema muestra mayor variabilidad entre ", min_year, " y ", max_year, "?"),
      q3 = paste0("3. ¿Cuál tuvo el mayor número de casos en ", max_year, "?")
    )
  })
  
  correct_answers <- reactive({
    data_in <- filtered_df()
    list(
      mcq1 = data_in %>% group_by(tema) %>% summarize(total = sum(casos), .groups = "drop") %>% slice_max(total) %>% pull(tema),
      mcq2 = data_in %>% group_by(tema) %>% summarize(var = sd(casos), .groups = "drop") %>% slice_max(var) %>% pull(tema),
      mcq3 = data_in %>% filter(año == max(año)) %>% slice_max(casos) %>% pull(tema)
    )
  })
  
  plot_fun1 <- function(data) {
    min_year <- min(data$año); max_year <- max(data$año)
    tot <- data %>% group_by(tema) %>% summarize(total = sum(casos), .groups = "drop")
    ggplot(tot, aes(reorder(tema, total), total, fill = tema)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = NULL, y = "Total acumulado", title = paste0("Total acumulado por tema (", min_year, "–", max_year, ")")) +
      theme_minimal()
  }
  plot_fun2 <- function(data) {
    min_year <- min(data$año); max_year <- max(data$año)
    ggplot(data, aes(tema, casos, fill = tema)) +
      geom_boxplot(show.legend = FALSE) +
      coord_flip() +
      labs(x = NULL, y = "Distribución anual", title = paste0("Variabilidad de los temas (", min_year, "–", max_year, ")")) +
      theme_minimal()
  }
  plot_fun3 <- function(data) {
    max_year <- max(data$año)
    dmax <- filter(data, año == max_year)
    ggplot(dmax, aes(reorder(tema, casos), casos, fill = tema)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = NULL, y = paste0("Casos en ", max_year), title = paste0("Casos en el último año disponible (", max_year, ")")) +
      theme_minimal()
  }
  observe({
    data_in <- filtered_df()
    ca <- correct_answers()
    qtxt <- question_texts()
    mcqPlotServer("mcq1", qtxt$q1, data_in, plot_fun1, ca$mcq1, rv)
    mcqPlotServer("mcq2", qtxt$q2, data_in, plot_fun2, ca$mcq2, rv)
    mcqPlotServer("mcq3", qtxt$q3, data_in, plot_fun3, ca$mcq3, rv)
  })
  
  mcqVarServer("mcqvarmod", rv)
  mcqHypoServer("mcqhmod", rv)
  
  output$score_display <- renderText({
    res <- rv$results
    if (length(res) == 0) return("0 / 3 puntos")
    puntos <- vapply(res, function(x) if (isTRUE(x$correcta)) 1L else 0L, FUN.VALUE = integer(1))
    paste0(sum(puntos), " / 3 puntos")
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("informe_", gsub(" ", "_", input$quiz_tema), ".pdf")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      rmd <- c(
        "---",
        "title: \"Informe de Análisis de Tema\"",
        "output: pdf_document",
        "params:",
        "  tema: NULL",
        "  año_max: NULL",
        "  total_cas: NULL",
        "  sd_cas: NULL",
        "  interpretation: NULL",
        "---",
        "",
        "# Resumen del tema: `r params$tema`",
        "",
        "- Año con más casos: `r params$año_max`",
        "- Total acumulado: `r params$total_cas`",
        "- Variabilidad (sd): `r params$sd_cas`",
        "",
        "# Evolución",
        "```{r echo=FALSE, warning=FALSE, message=FALSE}",
        "library(ggplot2)",
        "dt <- df %>% filter(tema == params$tema)",
        "ggplot(dt, aes(x = año, y = casos)) +",
        "  geom_line() + geom_point() +",
        "  theme_minimal() +",
        "  labs(x = 'Año', y = 'Casos')",
        "```",
        "",
        "# Interpretación personal",
        "",
        "`r params$interpretation`"
      )
      writeLines(rmd, tempReport)
      dt <- df %>% filter(tema == input$quiz_tema)
      params <- list(
        tema           = input$quiz_tema,
        año_max        = dt %>% slice_max(casos) %>% pull(año),
        total_cas      = sum(dt$casos),
        sd_cas         = round(sd(dt$casos), 2),
        interpretation = input$interpretation
      )
      rmarkdown::render(
        tempReport,
        output_file = file,
        params      = params,
        envir       = new.env(parent = globalenv())
      )
    }
  )
  
  observeEvent(input$add_hypo, {
    req(input$hypo_statement, input$hypo_dv, input$hypo_iv)
    if (input$hypo_dv == input$hypo_iv) {
      rv$hypo_feedback <- "La VD y la VI no pueden ser iguales."
      return()
    }
    if (length(input$hypo_context) < 1) {
      rv$hypo_feedback <- "Seleccione al menos una variable contextual."
      return()
    }
    new_row <- tibble(
      VD        = input$hypo_dv,
      VI        = input$hypo_iv,
      Contexto  = paste(input$hypo_context, collapse = ", "),
      Hipótesis = input$hypo_statement
    )
    dup <- any(pmap_lgl(rv$hypotheses, ~ identical(c(...), as.character(new_row))))
    if (dup) {
      rv$hypo_feedback <- "Esa hipótesis ya existe."
      return()
    }
    rv$hypotheses <- bind_rows(rv$hypotheses, new_row)
    rv$hypo_feedback <- "Hipótesis agregada correctamente."
  })
  
  output$hypo_feedback <- renderText({ rv$hypo_feedback })
  output$hypo_table <- renderTable({
    if (nrow(rv$hypotheses) == 0) return(NULL)
    rv$hypotheses
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  output$download_hypotheses <- downloadHandler(
    filename = function() { paste0("hipotesis_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(rv$hypotheses, file, row.names = FALSE) }
  )
}

shinyApp(ui, server)

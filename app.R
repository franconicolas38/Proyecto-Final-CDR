library(shiny)
library(ggplot2)
library(DT)

df<- read_csv("ObesityDataSet_raw_and_data_sinthetic.csv")
df =
  df |> mutate(NObeyesdad = factor(NObeyesdad,
                                   levels = c("Insufficient_Weight",
                                              "Normal_Weight",
                                              "Overweight_Level_I",
                                              "Overweight_Level_II",
                                              "Obesity_Type_I",
                                              "Obesity_Type_II",
                                              "Obesity_Type_III"),
                                   ordered = TRUE))


columnas_validas <- sapply(df, function(x) is.factor(x) | is.character(x))
variables_categoricas <- names(df)[columnas_validas]
variables_categoricas <- setdiff(variables_categoricas, "NObeyesdad")


ui = fluidPage(
  titlePanel("Explorador de datos"),
  sidebarLayout(
    sidebarPanel(
      selectInput("yvar", "Variable categórica:", choices = variables_categoricas),
      checkboxGroupInput("NObeyesdad", "Categoría",
                         choices = unique(df$NObeyesdad),
                         selected = unique(df$NObeyesdad))
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Gráfico", plotOutput("bar")),
        tabPanel("Resumen", verbatimTextOutput("resumen")),
        tabPanel("Tabla", DTOutput("tabla")),
        tabPanel("Calculadora de IMC",br(),numericInput("peso", "Peso (kg):", value = 70, min = 1, max = 300),
                 numericInput("altura", "Altura (m):", value = 1.70, min = 0.5, max = 2.5, step = 0.01),
                 h3("Resultado:"),
                 textOutput("resultado_imc"),
                 textOutput("categoria_imc"))
      )
    )
  )
)

server = function(input, output){

  datos_filtrados = reactive({
    req(input$NObeyesdad)
    df[df$NObeyesdad %in% input$NObeyesdad, ]
  })

  output$bar = renderPlot({
    ggplot(datos_filtrados(),
           aes(x = .data[[input$yvar]], fill = NObeyesdad )) +
      geom_bar(position = "dodge") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$resumen = renderPrint({
    req(input$yvar)
    datos <- datos_filtrados()
    summary(datos[[input$yvar]])
  })

  output$tabla = renderDT({
    datos_filtrados()
  })

  imc <- reactive({
    req(input$peso, input$altura)
    input$peso / (input$altura^2)
  })

  output$resultado_imc <- renderText({
    paste("Tu IMC es:", round(imc(), 2))
  })

  output$categoria_imc <- renderText({
    valor <- imc()
    categoria <- case_when(
      valor < 18.5 ~ "Insufficient_Weight (Bajo peso)",
      valor < 25.0 ~ "Normal_Weight (Peso normal)",
      valor < 27.0 ~ "Overweight_Level_I (Sobrepeso Nivel 1)",
      valor < 30.0 ~ "Overweight_Level_II (Sobrepeso Nivel 2)",
      valor < 35.0 ~ "Obesity_Type_I (Obesidad Tipo 1)",
      valor < 40.0 ~ "Obesity_Type_II (Obesidad Tipo 2)",
      TRUE ~ "Obesity_Type_III (Obesidad Tipo 3 o mórbida)"
    )
    paste("Categoría:", categoria)
  })
}

shinyApp(ui, server)


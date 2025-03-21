library(shiny)
library(shinyMobile)
Tabela <- read.csv(file="Tab8.csv")
Tabela$UF <- as.factor(Tabela$UF)

ui <- f7Page(
  f7TabLayout(options=list(theme = "md", color="#917bd1"),
              f7Tabs(
                id="tabs",
                 f7Tab(
                   title="Grafico 1",
                   tabName = "Boxplot",
                   HTML('<h1 style="font-size:0.5cm; text-align:center;">
                        Boxplot do valor médio do rendimento domiciliar per capita.
                        </h1> '),
                   f7Select("estado",label="Escolha um estado (ou todo o Brasil) para observar o boxplot.",choices=c(levels(Tabela$UF),"Brasil")),
                   br(),
                   plotOutput("Plot"),outline=T),
                f7Tab(
                  title="Grafico 2",
                  tabName="otra",
                  markdown("teste")
                )
              ),
              navbar = f7Navbar(),
  )
)


server <- function(input, output) {
  output$Plot <- renderPlot({
    estado<-input$estado
    if(estado=="Brasil") dados<-Tabela
      else dados<-subset.data.frame(Tabela,UF==estado)
    boxplot(dados$Media,
          main="Boxplot do valor médio do rendimento mensal total \n domiciliar per capita, por município",
          xlab=estado)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

# ShinyTankPower
# Darren Green
# April 2022

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(includeCSS("www/styles.css")),
  titlePanel(
    "Least conservative sample size for tank mort ANOVA"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select values for the model run and then click 'GO!'."),
      h4("Design treatment proportions"),
      sliderInput("t1","control percentage affected",min=0,max=1,step=0.01,value=0.5),
      sliderInput("t2","treatment percentage affected",min=0,max=1,step=0.01,value=0.5),
      h4("Tank design"),
      sliderInput("N","number of tanks per treatment",min=2,max=25,step=1,value=3),
      sliderInput("n","number of fish per tank",min=3,max=250,step=1,value=30),
      actionButton("submit","GO!"),
      h4("Other parameters"),
      sliderInput("alpha","alpha",min=0.01,max=0.1,step=0.01,value=0.05),
      sliderInput("boot.n","bootstrap replicates",min=100,max=10000,step=100,value=1000)
    ),
    mainPanel(
      h4("Density plot of p values"),
      plotOutput("plot",height="500px"),
      textOutput("power"),
      img(src='ioa_logo.png',style="width: 256px; align: left; margin-right: 2em"),
      img(src='parasite_2.png',style="width: 64px; align: right; margin-left: 2em")
    )
  )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  D <- reactiveValues()
  D$active <- F
  D$err <- F
  D$warn <- ""
  
  observeEvent(input$submit, {
    D$err <- F
    if (!D$err) withProgress(
      message="Calculating",
      detail="...",
      value=0, min=0, max=1,
      {RunModel()}
    )
    if (!D$err) D$active <- T
  })

  bootrep <- function(X, P, n) {
    Y <- sapply(P, function(p) rbinom(1,n,p))
    Yp <- Y/n
    D <- data.frame(Yp=Yp, X=X)
    lm1 <- lm(Yp~X,data=D)
    pval <- summary(lm1)$coefficients[2,4]
  }
  
  RunModel <- function() {
    p <- c(input$t1, input$t2)
    N <- input$N
    n <- input$n
    X <- rep(1:length(p), N)
    P <- rep(p, N)
    alpha <- input$alpha
    boot.n <- input$boot.n
    boot <- NULL
    for (i in 1:boot.n) {
      boot <- cbind(boot, bootrep(X,P,n))
      setProgress(i/boot.n)
    }
    D$power <- sum(boot<alpha)/boot.n
    D$boot <- boot
    D$alpha <- alpha
  }  
  
  DrawGraph <- function() {
    d <- density(log10(D$boot))
    par(mar=c(5,6,4,2)+0.1)
    plot(d, cex.lab=2, cex.axis=1.5, main=NA, xlab="log10(P value)", ylab="Density", xlim=c(-6,0))
    lines(c(log10(D$alpha),log10(D$alpha)), c(0,10), col="red")
  }
  
  output$plot <- renderPlot({
    if (D$active) DrawGraph()
  })
  
  output$power <- renderText({
    if (D$active) paste("Power of experiment (proportion of significant P values) = ",D$power)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

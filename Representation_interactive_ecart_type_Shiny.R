

#Charger les packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, rcompanion, stringr)


#Début
ui <- fluidPage(
  
  # Titre de l'application
  titlePanel("Dispersion d'une distribution normale en fonction  de l'écart type"),
  
  # Sidebar avec les entrées 
  sidebarLayout(
    sidebarPanel(
      sliderInput("Dimension","Nombre de cas:",min = 10, max = 5000, value = 10,step = 10),
      sliderInput("Mean","Moyenne:",min = -10, max = 10, value = 0,step = 1),
      sliderInput("SD","Écart type:",min = 1, max = 50, value = 1,step = 0.5),
      numericInput(inputId = "Viewmin",
                   label = "Minimum (nuage à points et boîte à moustache)",
                   value = -10
      ),
      numericInput(inputId = "Viewmax",
                   label = "Maximum (nuage à points et boîte à moustache)",
                   value = 10
      ),
      numericInput(inputId = "seed",
                   label = "Code de reproductibilité (seed)",
                   value = 1234
      ),width = 2
      
    ),
    
    
    
    # Affichage
    mainPanel("",
              tabsetPanel(
                tabPanel(title = "Formule",
                         h1("Formule"),
                         uiOutput("formula01"),
                         uiOutput("formula02"),
                         uiOutput("formula03"),
                         uiOutput("formula04")),
                tabPanel(title = "Nuage de points et boîte à moustache", 
                         h1("Représentation de la dispersion sur deux types de graphiques"),
                         fluidRow(
                           splitLayout(cellWidths = c("50%", "50%"), verticalLayout(plotOutput("Scatter"), uiOutput("Scatter_explain")),
                                       verticalLayout(plotOutput("Normboxplot"), uiOutput("Box_explain")))
                         
                         )),
                tabPanel(title = "Histogramme",
                         h1("Histogramme avec courbe de distribution normale pour représenter la dispersion"),
                         plotOutput("NormHist"))
              ),width = 10
      )
    ))
  


#Lancer le server
server <- function(input, output) {
 
  mydata <- reactive({
    set.seed(input$seed)
    Base_de_donne<-data.frame(normal=rnorm(input$Dimension,
                                           input$Mean,
                                           input$SD))
  })
  
  
  
  
  #Contenus à afficher:
  
  
  #Formule 1
  output$formula01 <- renderUI({
    Base_de_donne <- mydata()
    withMathJax(paste0("Formule de l'écart type : $$ \\sqrt{\\sigma^2} =
                       \\sqrt{\\frac{1}{N-1} \\sum_{i=1}^{N} (x_i - \\overline{x})^2} =",
                       round(sd(Base_de_donne$normal),2),"$$"))
  })
  
  
  #Formule 2
  output$formula02 <- renderUI({
    Base_de_donne <- mydata()
    valeurs_formules_0<-rep(NA,10)
    valeurs_formules_1<-rep(NA,10)
    valeurs_formules_2<-rep(NA,10)
    
    for (i in c(seq(1,5,1),seq(nrow(Base_de_donne)-4,nrow(Base_de_donne),1))) {
      valeurs_formules_0[i]<-round(Base_de_donne$normal[i],2)
      valeurs_formules_1[i]<-round(Base_de_donne$normal[i]-input$Mean,2)
      valeurs_formules_2[i]<-round((Base_de_donne$normal[i]-input$Mean)^2,2)
      
    }
    valeurs_formules_0<-valeurs_formules_0[c(1:5,seq(length(valeurs_formules_1)-4,length(valeurs_formules_1),1))]
    valeurs_formules_1<-valeurs_formules_1[c(1:5,seq(length(valeurs_formules_1)-4,length(valeurs_formules_1),1))]
    valeurs_formules_2<-valeurs_formules_2[c(1:5,seq(length(valeurs_formules_2)-4,length(valeurs_formules_2),1))]
    
    withMathJax(paste0("Formule de l'écart type en affichant la première valeur :
                       $$ \\sqrt{\\sigma^2} = \\sqrt{\\frac{1}{",
                       input$Dimension,
                       "-1} ((",
                       valeurs_formules_0[1],
                       "-",
                       input$Mean,
                       ")^2+\\dots)} = \\sqrt{\\frac{1}{",
                       input$Dimension,
                       "-1} ((",
                       valeurs_formules_1[1],
                       ")^2+\\dots)}
                       =
                       \\sqrt{\\frac{1}{",
                       input$Dimension,
                       "-1} (",
                       valeurs_formules_2[1],
                       "+\\dots)}= 
                       \\sqrt{\\frac{1 \\times(",
                       valeurs_formules_2[1],
                       "+\\dots)}{",
                       input$Dimension,
                       "-1}}=
                       \\sqrt{\\frac{",
                       valeurs_formules_2[1],
                       "+\\dots}{",
                       input$Dimension-1,
                       "}}
                       $$"
    ))
  })
  
  
  
  #Formule 3
  output$formula03 <- renderUI({
    
    Base_de_donne <- mydata()
    valeurs_formules_0<-rep(NA,10)
    valeurs_formules_1<-rep(NA,10)
    valeurs_formules_2<-rep(NA,10)
    
    for (i in c(seq(1,5,1),seq(nrow(Base_de_donne)-4,nrow(Base_de_donne),1))) {
      valeurs_formules_0[i]<-round(Base_de_donne$normal[i],2)
      valeurs_formules_1[i]<-round(Base_de_donne$normal[i]-input$Mean,2)
      valeurs_formules_2[i]<-round((Base_de_donne$normal[i]-input$Mean)^2,2)
      
    }
    valeurs_formules_0<-valeurs_formules_0[c(1:5,seq(length(valeurs_formules_1)-4,length(valeurs_formules_1),1))]
    valeurs_formules_1<-valeurs_formules_1[c(1:5,seq(length(valeurs_formules_1)-4,length(valeurs_formules_1),1))]
    valeurs_formules_2<-valeurs_formules_2[c(1:5,seq(length(valeurs_formules_2)-4,length(valeurs_formules_2),1))]
    
    valeursnonexpose<-paste(valeurs_formules_1, sep="", collapse=")^2+(")
    valeursexpose<-paste(valeurs_formules_2, sep="", collapse="+")

    withMathJax(paste0("Calcul de l'écart type en affichant les cinq premières et les cinq dernières valeurs :
                       $$ \\sqrt{\\sigma^2} = \\sqrt{\\frac{1}{",
                       input$Dimension,
                       "-1} \\sum_{i=1}^{",
                       input$Dimension,
                       "} (x_i-",
                       input$Mean,
                       ")^2} = \\sqrt{\\frac{(",
                       valeursnonexpose
                       ,")^2+\\dots}{",
                       input$Dimension,
                       "-1}}
                       $$"
    ))
   
  })
  
  
  
  #Formule 4
  output$formula04 <- renderUI({
    
    Base_de_donne <- mydata()
    valeurs_formules_0<-rep(NA,10)
    valeurs_formules_1<-rep(NA,10)
    valeurs_formules_2<-rep(NA,10)
    
    for (i in c(seq(1,5,1),seq(nrow(Base_de_donne)-4,nrow(Base_de_donne),1))) {
      valeurs_formules_0[i]<-round(Base_de_donne$normal[i],2)
      valeurs_formules_1[i]<-round(Base_de_donne$normal[i]-input$Mean,2)
      valeurs_formules_2[i]<-round((Base_de_donne$normal[i]-input$Mean)^2,2)
      
    }
    valeurs_formules_0<-valeurs_formules_0[c(1:5,seq(length(valeurs_formules_1)-4,length(valeurs_formules_1),1))]
    valeurs_formules_1<-valeurs_formules_1[c(1:5,seq(length(valeurs_formules_1)-4,length(valeurs_formules_1),1))]
    valeurs_formules_2<-valeurs_formules_2[c(1:5,seq(length(valeurs_formules_2)-4,length(valeurs_formules_2),1))]
    
    valeursnonexpose<-paste(valeurs_formules_1, sep="", collapse=")^2+(")
    valeursexpose<-paste(valeurs_formules_2, sep="", collapse="+")
    valeursexpose<-paste(valeurs_formules_2, sep="", collapse="+")
    
    calcul<-rep(NA, length(Base_de_donne$normal))
    
    for (i in 1:length(Base_de_donne$normal)) {
      calcul[i]<-(Base_de_donne$normal[i]-mean(Base_de_donne$normal))^2
    }
    
    calcul<-round(sum(calcul),2)
   
    
    withMathJax(paste0("$$ \\sqrt{\\sigma^2} = 
                       \\sqrt{\\frac{",
                       valeursexpose
                       ,"+\\dots}{",
                       input$Dimension,
                       "-1}}=
                        \\sqrt{\\frac{",
                       calcul
                       ,"}{",
                       input$Dimension,
                       "-1}}=
                       \\sqrt{\\frac{",
                       calcul
                       ,"}{",
                       input$Dimension-1,
                       "}}= \\sqrt{",round(calcul/(input$Dimension-1),2),
                       "}=",round(sqrt(calcul/(input$Dimension-1)),2),"$$"
    ))
    
  })
  
  
  
  
  
  
  #Scatter plot
  output$Scatter <- renderPlot({
    Base_de_donne <- mydata()
    plot(jitter(rep(0, length(Base_de_donne$normal))),
         Base_de_donne$normal,
         ylim=c(input$Viewmin, input$Viewmax),
         xaxt='n', xlab="",ylab="Valeurs",
         main=paste("Nuage de points avec moyenne=",input$Mean," et écart type=",input$SD))
    abline(h=input$Mean)
    
  })
  
  
  
  #Boxplot
  output$Normboxplot <- renderPlot({
    Base_de_donne <- mydata()
    boxplot(Base_de_donne$normal,
            main=paste("Boîte à moustache avec moyenne=",input$Mean," et écart type=",input$SD),
            ylab="Valeurs",ylim=c(input$Viewmin, input$Viewmax))
    
  })
  
  
  
  #Histogramme
  output$NormHist <- renderPlot({
    Base_de_donne <- mydata()
    plotNormalHistogram(Base_de_donne$normal,
                        xlab="Valeurs",ylab="Effectifs",
                        main=paste("Histogramme avec moyenne=",input$Mean," et écart type=",input$SD))
    
  })
  
  output$Scatter_explain <- renderUI({
    Base_de_donne <- mydata()
    withMathJax(paste0("Sur l'axe des ordonnées (y), ce nuage à point représente la dispersion des valeurs autour de la moyenne (",input$Mean,") en fonction de l'écart type (",input$SD,").
                        $$ $$
                        La droite horizontale représente la moyenne (",input$Mean,").
                        $$ $$
                       Dans ce cas-ci, l'axe des abscisses (x) ne s'interprête pas. Sur cet axe, les distances entre les points sont aléatoires et ne servent qu'à éviter qu'ils se superposent."
    ))
    
    
  })
  
  
  #Explication boxplot
  output$Box_explain <- renderUI({
    Base_de_donne <- mydata()
      withMathJax(paste0("La boîte représente l'espace interquartile (IQR) qui va de ",
      round(as.numeric(paste(quantile(Base_de_donne$normal)[2])),2)," à ",round(as.numeric(paste(quantile(Base_de_donne$normal)[4])),2),
      ". L'IQR se calcule de la manière suivante :
  $$IQR =Q_{3}-Q_{1} =",round(as.numeric(paste(quantile(Base_de_donne$normal)[4])),2),
                         "-(",round(as.numeric(paste(quantile(Base_de_donne$normal)[2])),2),
                         ")=",round(as.numeric(paste(quantile(Base_de_donne$normal)[4]))-as.numeric(paste(quantile(Base_de_donne$normal)[2])),2),
                         "$$
                         Le trait noir dans la boîte représente la médiane (",round(as.numeric(paste(quantile(Base_de_donne$normal)[3])),2),
                         "). La longueur des « moustaches » vaut 1,5 fois l'IQR, soit:
                         $$1.5 \\times IQR = 1.5 \\times",round(IQR(Base_de_donne$normal),2),
                         "=",round(1.5*IQR(Base_de_donne$normal),2),"
                          $$ La « moustache » inférieure va donc de ",
      round(as.numeric(paste(quantile(Base_de_donne$normal)[2]))-(1.5*IQR(Base_de_donne$normal)),2),
      " (ou la valeur la plus faible de la distribution) à ",
      round(as.numeric(paste(quantile(Base_de_donne$normal)[2])),2),
      " : $$ Q_{1}-(1.5 \\times IQR) = ",
      round(as.numeric(paste(quantile(Base_de_donne$normal)[2])),2),
      "-(1.5 \\times",
      round(IQR(Base_de_donne$normal),2),")=",
      round(as.numeric(paste(quantile(Base_de_donne$normal)[2])),2),"-",
      round(1.5*IQR(Base_de_donne$normal),2),"=",
      round(as.numeric(paste(quantile(Base_de_donne$normal)[2]))-(1.5*IQR(Base_de_donne$normal)),2),"$$ 
      La « moustache » supérieure va de ",
      round(as.numeric(paste(quantile(Base_de_donne$normal)[4])),2),
      " à ",
      round(as.numeric(paste(quantile(Base_de_donne$normal)[4]))+(1.5*IQR(Base_de_donne$normal)),2),
      " (ou la valeur la plus élevée de la distribution):
      $$Q_{3}+(1.5 \\times IQR) = ",
      round(as.numeric(paste(quantile(Base_de_donne$normal)[4])),2),
      "+(1.5 \\times",
      round(IQR(Base_de_donne$normal),2),")=",
      round(as.numeric(paste(quantile(Base_de_donne$normal)[4])),2),"+",
      round(1.5*IQR(Base_de_donne$normal),2),"=",
      round(as.numeric(paste(quantile(Base_de_donne$normal)[4]))+(1.5*IQR(Base_de_donne$normal)),2)," $$
      Les points en dessous (<",
      round(as.numeric(paste(quantile(Base_de_donne$normal)[2]))-(1.5*IQR(Base_de_donne$normal)),2)
      ,") ou au-dessus (>",
      round(as.numeric(paste(quantile(Base_de_donne$normal)[4]))+(1.5*IQR(Base_de_donne$normal)),2)
      ,") des « moustaches » sont considérés comme des valeurs extrêmes."
  ))
  
  })
  }


#Lancer l'application
shinyApp(ui = ui, server = server)

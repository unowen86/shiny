rm(list=ls())
library(shiny)
library(plotly)
library(dplyr)
library(reshape)
library(xtable)
library(rmarkdown)
library(knitr)
library(shinyjs)
library(countrycode)
library(shinythemes)



# # Pacchetti ####
# library(RMySQL)
# all_cons <- dbListConnections(MySQL())
# for(con in all_cons){
#   dbDisconnect(con)
# }
# 
# mydb <- dbConnect(MySQL(), user='pelgantae', password='sw_Ch5cr', dbname='TST_TEST',host="10.100.1.90")
# 
# db_t <- dbReadTable( mydb , "t_provashiny")

load("db_provashiny.Rdata")
db_t<- data

ui <- fluidPage(theme = shinytheme("united"),
  
  # App title ----
  titlePanel("Transparency Exercise"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: 
      selectInput("unit", label = "scegli unit", 
                  choices = list("paese" = "A" ,"banca" = "S"), selected = 1),
      selectInput("cat", label = "scegli categoria", 
                  choices = unique(db_t$DERIVED_TEMPLATE) , selected = 1),
      
      conditionalPanel(
        condition = "input.unit == 'S'",
        selectInput(
          "couv", label="Filtro paese",
          choices=c("nessuno", unique(db_t$Country)) )),
      
      selectInput("per", label = "Scegli Periodo", 
                  choices = list( "201512" , "201606" , "201612" ,"201706"), selected = "201706"),
      
      # conditionalPanel(
      #   condition = "input.cat == 'Capital'",
      #   selectInput("ind", label = "Scegli indicatore", 
      #               choices = list( "Common Equity Tier 1 Capital Ratio" = "0140",
      #                               "Tier 1 Capital Ratio" = "0141",
      #                               "Totale Capital Ratio" = "0142",
      #                               "Total Risk Exposure Amount" = "0138",
      #                               "Tier 1 Capital" = "0133",
      #                               "Tier 2 Capital" = "0134",
      #                               "Own Funds (PATVIG)" = "0101",
      #                               "Common Equity Tier 1 Capital" = "0102"), selected = "0101", multiple=TRUE ) ),
      
      conditionalPanel(
        condition = "input.cat == 'P&L'",
        selectInput("ind", label = "Scegli indicatore", 
                    choices = list( "Risultato di esercizio" = "0335",
                                    "Margine di Interesse" = "0350",
                                    "Margine di Intermediazione" = "0316",
                                    "Risultato Netto della negoziazioine" = "0351",
                                    "Costi Operativi" = "0352",
                                    "Risultato di Gestione" = "0353",
                                    "Rettifiche di Crediti su Margine di Interesse" = "0354",
                                    "Rettifiche di Crediti su Margine di Intermediazione" = "0355",
                                    "Return on Equity (ROE)" = "0356" , 
                                    "Return on Asset (ROA)" = "0357" , 
                                    "Cost Income" = "0358",
                                    "Margine di Interesse su RWA" = "0359", 
                                    "Commissione Nette su RWA" ="0360",
                                    "Margine di intermediazione su RWA" = "0361" ), selected = "0335", multiple=TRUE ) )
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Barplot", plotlyOutput("barplot") , radioButtons("typebp", h3("Tipo grafico"),
                                                                   choices = list("Grouped" = "group", "Stacked" = "stack"),selected = "group") ) ,
        tabPanel("Scatterplot" ,textOutput('textscatter'),plotlyOutput("scatterplot") ),
        tabPanel("Mappa Geografica" ,textOutput('textmap'),plotlyOutput("map") ),
        tabPanel("Table",  tableOutput("tableprev") ),
        tabPanel("Export",  downloadButton("downloadData", "Download csv") ,  downloadButton("report", "Genera report in pdf")
        )
        
      )
    )
  ))



server <- function(input, output) {
  
  ## Colors
  listc <- c( 'rgb(000,082,152)','rgb(237,109,27)' ,'rgb(057,169,052)','rgb(255,237,0)')
  # col = rgb(rbind(c(221,216,207),c(209,201,189),c(000,176,212),c(000,116,173),c(000,082,152),c(134,104,171),c(149,027,129),c(068,051,116),c(131,187,038),
  #                 c(057,169,052),c(058,116,047),c(255,237,000),c(245,156,029),c(237,109,027),c(216,013,050),c(124,003,023)) , max = 255 )
  cols = rgb(rbind(c(000,082,152),c(237,109,27),c(057,169,052),c(255,237,0)) , max = 255 )
  
  ## Create table
  data <- reactive({
    TAB <- select( db_t, "ItemCode","Period","Label","variable", "value", "AS" , "Country" ,"Flag")
    asv <- input$unit
    couv <- input$couv
    if ( input$unit =="A" ){ couv <- "nessuno"}
    
    periodv <- input$per
    labelv <- input$ind
    
    TAB$value <- as.numeric( TAB$value )
    
    if (asv=="A"){ labx <- "paese" } else { labx <- "banca"}
    TF <- filter( TAB, Period==periodv )%>%filter(  AS==asv )%>%filter(  ItemCode %in% labelv )%>% arrange(desc(value)) %>%select(variable,Label,value,ItemCode,Country)%>%
      mutate(ItemCode =  factor(ItemCode, levels = labelv)) %>% arrange(ItemCode)
    if (!couv == "nessuno" ){  TF <- TF %>% filter(  Country==couv ) }
    
    lplot <- unique(TF$Label ) 
    
    TF <- cast(TF,variable~Label, value= "value")
    TF <- TF[, c( "variable", lplot)]
    colnames( TF ) <- c( "variable", paste( "value" ,1:length(lplot),sep="") )
    TF <- arrange(TF,desc(value1))  
    return(list(TF,lplot,labx))
  })
  
  
  ## barplot 
  output$barplot <- renderPlotly({
    
    TF <- data()[[1]]
    lplot <-data()[[2]]
    labx <-data()[[3]]
    xform <- list(categoryorder = "array",
                  categoryarray = TF$variable)
    
    p <- plot_ly(TF, x = ~variable, y =  ~value1, type = 'bar', name = as.character( lplot)[1],marker = list(color = 'rgb(000,082,152)')) %>%
      layout(xaxis = xform,title = as.character(lplot))%>%  layout(xaxis = list(title = labx) , yaxis= list ( title = lplot ),barmode = input$typebp )
    if( length( lplot ) >= 2 ){
      for ( ll in 2:length(lplot) ){
        eval(parse(text= paste( "p <- p %>%",sprintf("add_trace(y = ~value%1.0f , name = as.character( lplot)[%1.0f],marker = list(color = listc[%1.0f]) )", ll , ll, ll))))
      }  
      
    }
    ggplotly(p)
    
  })
  
  
  ## text scatterplot
  text <- reactive( 
    if (!length( input$ind )%in%c(2,3)){
      paste('scatter possibile solo per due/tre variabili')
    } else { paste('')} )
  output$textscatter <- renderText({ text() })
  
  ## scatterplot 
  output$scatterplot <- renderPlotly({
    TF <- data()[[1]]
    lplot <-data()[[2]]
    labx <-data()[[3]]
    
    xform <- list(categoryorder = "array",
                  categoryarray = TF$variable)
    
    if( length( lplot ) ==2 ){
      p <- plot_ly(TF, x = ~value1, y =  ~value2, colors = cols[1], text =~ paste(variable,"<br>",lplot[1],':',value1,"<br>",lplot[2],':',value2) ) %>%  layout(xaxis = list(title = lplot[[1]]) , yaxis= list ( title = lplot[[2]] ) )
    } else {
      if( length( lplot ) ==3 ){
        p <- plot_ly(TF, x = ~value1, y =  ~value2, color=~value3, size = ~value3 ,colors = cols[1:2], text =~ paste(variable,"<br>",lplot[1],':',value1,"<br>",lplot[2],':',value2,"<br>",lplot[3],':',value3) )%>%
          layout(xaxis = list(title = lplot[[1]]) , yaxis= list ( title = lplot[[2]] ), legend = list ( title = lplot[[3]] ) ) 
      } else {
        p <- plotly_empty()}
    }
    ggplotly( p )
    
  })
  
  
  ## text map
  textmap <- reactive( 
    if ( (input$unit=="S")|(length(input$ind)>1) ){
      paste('Mappa possibile solo per aggregazione paese e un solo indicatore')
    } else { paste('')} )
  output$textmap <- renderText({ textmap() })
  
  ## map
  output$map <- renderPlotly({
    if ( (input$unit=="S")|(length(input$ind)>1) ){
      p <- plotly_empty() } else {
        
        TF <- data()[[1]]
        lplot <-data()[[2]]
        labx <-data()[[3]]
        
        # colnames(TF)<- c(labx,lplot)
        TF$variable  <- gsub("UK","GB", TF$variable)
        TF$variable2 <- countrycode(TF$variable, 'iso2c', 'iso3c')
        TF <- na.omit(TF)
        # light grey boundaries
        l <- list(color = toRGB("grey"), width = 0.5)
        
        # specify map projection/options
        g <- list(
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = 'Mercator'),
          scope="europe"
        )
        
        p <- plot_geo(TF) %>%
          add_trace(
            z = ~value1, color = ~value1, colors ='Blues',
            text = ~variable2, locations = ~variable2, marker = list(line = l)
          )%>%
          colorbar(title = '', tickprefix = '') %>%
          layout( title = lplot,
                  geo = g )}
    
    ggplotly(p)
    
  })
  
  
  ## print table
  output$tableprev <- renderTable({
    TF <- data()[[1]]
    lplot <-data()[[2]]
    labx <-data()[[3]]
    
    colnames(TF)<- c(labx,lplot)
    xtable ( TF )
  })
  
  ## download csv
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("export_trasparency_",Sys.Date(), ".csv", sep = "")
    },
    content = function(file){
      TF <- data()[[1]]
      lplot <-data()[[2]]
      labx <-data()[[3]]
      colnames(TF)<- c(labx,lplot)
      xtable ( TF )
      write.csv(TF, file, row.names = FALSE)
    }
  )
  
  ## produce report
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      TF <- data()[[1]]
      lplot <-data()[[2]]
      labx <-data()[[3]]
      per <- input$per
      colnames(TF)<- c(labx,lplot)
      params <- list(TF = TF,lplot = lplot, labx = labx ,per = per)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv() )
      )
    }
  )
  
  
  
  
  
}
shinyApp( ui=ui  , server=server )

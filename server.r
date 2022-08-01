

# Define server logic required to draw a barplot and sunburstplot
shinyServer <- function(input, output) {
  
  
  output$barplot_top10<-renderPlotly({
    
    data<-barchart_top_10_data(level_2a, totGDP, input$yr)
    barplot_broad(input$yr, "Primary industries",data$data, data$data_totals, T )
    
  })
  
  output$sunburstplot2<-renderPlotly({
    
    sunburstplot(input$yr)
    
  })
  
  
  
  output$broad_industry_group<- renderPlotly({
    
    data<-broad_industry_data(level_2a, input$yr_2, input$BroadIndustry)
    barplot_broad(input$yr_2, input$BroadIndustry, data$data, data$groups)
    
    
  })
  
  output$timeseries<-renderPlotly({
    
    if(input$Modeview1 == "Timeseries" && input$mode == "GDP value movement"){
      
      data<- linechart_data(all_industries, input$Industry1, input$Industry2)
      
      linechart(data, input$Industry1, input$Industry2 )
    }
    
    else {
      
      
      data<- linechart_data(all_industries, input$Industry3, input$Industry4, F)
      
      linechart(data, input$Industry3, input$Industry4, F )
    }
    
  })
  
  url <-a("National accounts (industry production and investment) Datainfo+", href="http://datainfoplus.stats.govt.nz/Item/nz.govt.stats/48c58c28-4b3d-4a08-aa91-8af0f878c37d?_ga=2.213553232.519754402.1572300337-1013394136.1569889370")
  
  output$datainfo<-renderUI({
    tagList("  ", url)
  })
  
  output$stats<-renderText({
    print("Stats NZ")
  })
  
  output$footnote1 <- renderText({
    paste0('These figures have not been adjusted for inflation')
    
    
  })
  
  missing_data <- c("Dairy cattle farming", "Horticulture and fruit growing", "Poultry, deer and other livestock farming", "Sheep, beef cattle and grain farming")
  
  output$footnote2 <- renderText({
    if(input$Modeview1 == 'Timeseries' && input$mode == 'GDP value movement' && input$Industry1 %in% missing_data 
       | input$Modeview1 == 'Timeseries' && input$mode == 'GDP value movement' && input$Industry2 %in% missing_data
       | input$Modeview1 == 'Timeseries' && input$mode == 'GDP percentage movement' && input$Industry3 %in% missing_data 
       | input$Modeview1 == 'Timeseries' && input$mode == 'GDP percentage movement' && input$Industry4 %in% missing_data){
      paste0("No line – Data is unavailable")
    }
    
  }
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Annual GDP by industry in current prices.csv")
    },
    content = function(file) {
      
      write.csv(get_download_data(level_2a, level_3a), file, row.names = F)
    }
  )
  
}




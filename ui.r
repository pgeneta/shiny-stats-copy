# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/




ui <- fluidPage(
  
  
    tags$head(tags$style(
    
    
    HTML('
        #sidebar {
            background-color: #FFFFFF;
            border:#FFFFFF;
        }
        body, label, input, button, select { 
          font-family: "Arial";
        }
        #wellPanel {
            background-color: #FFFFFF;
            border-color:#FFFFFF;
        }
        #condtionalPanel {
            background-color: #FFFFFF;
            border-color:#FFFFFF;
        }   
        #footnote1{
            font-size:11px;
                 color:black;
                 display:block;
        }
        #footnote2{
            font-size:11px;
                 color:black; 
                 display:block;
        }
        #stats {font-size:11px;
               color:black;
               display:block; 
        }
        #downloadData{
        margin-right:20px;
        margin-top:25px;
        margin-bottom:25px;
        float:right;
        }
        @media only screen and (max-width:768px){
        #downloadData{
        float:none!important;
        }
        }
         
        ')
  ))
  ,
  
  
  
  
  fluidRow(column(10,titlePanel("Which industries contributed to New Zealand’s GDP?")),
           column(2,downloadButton('downloadData', 'Download'))),
  
  
  
  
  column (3, wellPanel(style = "background-color:#ffffff ;border-color: #FFFFFF;padding-bottom:0px; padding-top:0px; padding-left:5px; padding-right:5px",
                       selectInput("Modeview1", strong("Choose data view: "),
                                   c('Top 10 industries' = "top10ind",
                                     'Broad industry group' ="BroadInd",
                                     'Time series comparison' = "Timeseries")))), 
  
  
  column (3, conditionalPanel(condition = "input.Modeview1 == 'BroadInd'",
                              wellPanel(style = "background-color:#ffffff ;border-color: #FFFFFF;padding-bottom:0px; padding-top:0px; padding-left:5px; padding-right:5px",
                                        selectInput("BroadIndustry", strong("Choose broad industry: "), 
                                                    choices=list("Primary industries", "Goods-producing industries", "Service industries"),
                                                    selected = "Primary industries"))),   
          
          conditionalPanel(condition = "input.Modeview1 == 'Timeseries'",
                           wellPanel(style = "background-color:#ffffff ;border-color: #FFFFFF;padding-bottom:0px; padding-top:0px; padding-left:5px; padding-right:5px", 
                                     selectInput("mode", strong("Choose nominal GDP view: "), 
                                                 choices=list('Value added' = "GDP value movement",'Percentage movement' = "GDP percentage movement"),
                                                 selected = "Value")))),
  
  column(3, conditionalPanel(condition = "input.Modeview1 == 'Timeseries' && input.mode == 'GDP value movement'",
                             wellPanel(style = "background-color:#ffffff ;border-color: #FFFFFF;padding-bottom:0px; padding-top:0px; padding-left:5px; padding-right:5px",
                                       selectInput("Industry1", 
                                                   strong("Choose industry: "),
                                                   choices = sort(industry_names), selected = "Dairy cattle farming"))),
         
         conditionalPanel(condition = "input.Modeview1 == 'Timeseries' && input.mode == 'GDP percentage movement'",
                          wellPanel(style = "background-color:#ffffff ;border-color: #FFFFFF;;padding-bottom:0px; padding-top:0px; padding-left:5px; padding-right:5px",
                                    selectInput("Industry3", strong("Choose industry: "),
                                                choices = sort(industry_names), selected = "Dairy cattle farming")))),
  
  column(3, conditionalPanel(condition = "input.Modeview1 == 'Timeseries' && input.mode == 'GDP value movement'",
                             wellPanel(style = "background-color:#ffffff ;border-color: #FFFFFF;;padding-bottom:0px; padding-top:0px; padding-left:5px; padding-right:5px",
                                       selectInput("Industry2", strong("Compare with: "), 
                                                   choices = sort(industry_names), 
                                                   selected = "Dairy product manufacturing"))), 
         
         conditionalPanel(condition = "input.Modeview1 == 'Timeseries' && input.mode == 'GDP percentage movement'",
                          wellPanel(style = "background-color:#ffffff ;border-color: #FFFFFF;;padding-bottom:0px; padding-top:0px; padding-left:5px; padding-right:5px",
                                    selectInput("Industry4", strong("Compare with: "),
                                                choices = sort(industry_names), 
                                                selected = "Dairy product manufacturing")))
         
  ),
  
  # fluidRow(
  column(12, style = list("padding-bottom:0px; padding-top:0px;"), 
         #chooseSliderSkin("HTML5", color = '#272726'),
         
         conditionalPanel(condition = "input.Modeview1 == 'top10ind'", 
                          sidebarPanel(id="sidebar", width=12, #chooseSliderSkin("HTML5", color = '#272726'),
                                       sliderInput("yr",strong("1972–2019"), min = 1972, max = 2019, value = 2019, sep = "", width= '2000px')),
                          
                          column(7, plotlyOutput(outputId = "barplot_top10",height = "500px")), 
                          column(5, plotlyOutput(outputId = "sunburstplot2",height = "500px"))
                          
         ),
         
         
         
         conditionalPanel(condition = "input.Modeview1 == 'BroadInd'", 
                          sidebarPanel(id="sidebar", width=12, #chooseSliderSkin("HTML5", color = '#272726'),
                                       sliderInput("yr_2",strong("1972–2019"), min = 1972, max = 2019, value = 2019, sep = "", width= '2000px')),
                          
                          column(12, plotlyOutput(outputId = "broad_industry_group",height = "500px"))
                          
         ),
         
         conditionalPanel(condition = "input.Modeview1 == 'Timeseries'", plotlyOutput(outputId = "timeseries",height = "600px")),
         
         
         
         
         column(12,offset = 0,
                uiOutput("datainfo")),
         column(12,offset = 0,
                textOutput("footnote1"),
                textOutput("footnote2")),
         column(1, offset=11,
                textOutput("stats"))
         
  ))










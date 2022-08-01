barplot_broad<- function(year, BroadIndustry, data, groups, is_top_10 = F){
  
  if(is_top_10 == TRUE){
    
    gdp_top10 <-  as.numeric(groups[1,2])
    total_data<-  format(gdp_top10, digits=2, big.mark = ",", scientific = FALSE)
    
    x <-max(data$GDP)
    
    chart <-ggplot(data, aes(x=reorder(Industry, GDP), y=GDP, fill=Main, label=Industry, 
                             text = paste0("Industry: ",Industry,'\n',"GDP: ",GDP, '\n',"Broad Industry Group: ",Main)))+
      geom_bar(stat="identity")+ coord_flip()+
      theme_minimal()+
      xlab(" ")+ylab("Contribution to value-added, $(million)")+
      ggtitle(paste0("Top 10 industries in ", year,"\n", "GDP = $", total_data, "m"))+ 
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=12, family = 'Arial', face = "bold"), panel.grid.major = element_blank(),
            axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
            text = element_text( size = 11, family = "Arial"),
            axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
      scale_y_continuous(limits = c(0,(x+(x*.1))) , expand = c(0,1), labels=comma)+
      scale_fill_manual(values=c("Service industries" = "#085c75", "Goods-producing industries" = "#ae4e51", "Primary industries"="#d2ac2f"))+
      scale_x_discrete(labels=wrap_format(35))
    
    
  }else{
    gdp_by_broad <-  as.numeric(groups[1,3])
    
    total_data<-  format(gdp_by_broad, digits=2, big.mark = ",", scientific = FALSE)
    
    x <-max(data$GDP)
    #plot barchart
    chart <-ggplot(data, aes(x=reorder(Industry, GDP), y=GDP, fill=Main, label=Industry, text = paste0("Industry: ",Industry,'\n',"GDP: ",GDP, '\n',"Broad Industry Group: ",Main)))+
      geom_bar(stat="identity")+ coord_flip()+
      theme_minimal()+
      xlab("")+ylab("Contribution to value-added, $(million)")+
      scale_y_continuous(limits = c(0,(x+(x*.1))) , expand = c(0 ,1), labels=comma)+
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=12, family = 'Arial', face = "bold"), panel.grid.major = element_blank(),
            axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
            text = element_text(size = 11, family = "Arial"),
            axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
      ggtitle(paste0(BroadIndustry," in ", year, ", GDP = $", total_data, "m"))+
      scale_fill_manual(values=c("Service industries" = "#085c75", "Goods-producing industries" = "#ae4e51", "Primary industries"="#d2ac2f"))+
      scale_x_discrete(labels=wrap_format(35))
    
  }
  
  return( ggplotly(chart, tooltip=c("text")) %>% 
            config(displayModeBar = F)%>% 
            layout(margin = list( l=30,b=30,t=50,r=30,pad=5),
                   font = list(size=11, family='Arial')
            ) )
  
}







sunburstplot <- function(yr){
  
  year = yr
  
  groups<- level_2a %>%
    mutate(Period=as.character(Period))%>%
    select(-Industry)%>%
    group_by(Period, Main)%>%
    summarise(GDP=sum(GDP))
  
  Main <- full_join(groups, Main)
  Main <- Main %>% arrange(Period)%>%
    mutate(Main=recode(Main, "Primary industries" = "Primary", "Goods-producing industries" = "Goods-producing"))
  
  Main$Main<-factor(Main$Main, levels = c("Primary","Service industries","Goods-producing","Taxes"))
  main.col=c("#ae4e51","#d2ac2f","#085c75","#35345d")
  
  ## data for total GDP
  totGDP<-tax_data%>%
    filter(Main=="SNEA.SG00NAC00B01")%>%
    mutate(Main='Total GDP')
  
  totalGDP <-Main %>%
    filter(Period==year)
  
  ##Pass GDP to variable
  nzgdp<-format(totGDP[1,2],digits=2, big.mark = ",") ##get GDP value
  
  
  font1 <- list(size=12, family='Arial')
  ptc <- round(totalGDP$GDP/sum(totalGDP$GDP)*100,0)
  ptc <- paste0(ptc,"%")
  
  font1 <- list(family = "Arial", size = 12, color='black')
  ptc <- paste0(round(totalGDP$GDP/sum(totalGDP$GDP)*100,1), "%")
  
  
  
  pie <- totalGDP %>% 
    group_by(Main) %>%
    plot_ly(labels = ~Main, values = ~GDP, textinfo = 'label+text',text= ptc, hoverinfo = 'label+text',
            marker=list(colors= main.col))%>%
    add_pie(hole = 0.6) %>%
    config(displayModeBar = F)%>%
    layout(title = paste0('<b>Share of the economy in <b>', yr), font=(font1),
           showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  return( pie %>% 
            layout(margin = list( l=30,b=30,t=50,r=30,pad=5)
            ) )
  
}






linechart<- function(data, Industry1, Industry2, is_value = T){
  
  
  integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
      breaks <- floor(pretty(x, n, ...))
      names(breaks) <- attr(breaks, "labels")
      breaks
    }
    return(fxn)
  }
  
  if(is_value == T){
    
    txt<-paste0("Year: ", data$Period, '\n', "GDP: ", data$GDP, '\n',"Industry: ", data$Industry)
    title <- paste0("<b>Nominal GDP of <b><i>", Industry1,"</i> vs <i>", Industry2,"</i>")
    ylab <- paste0("$(million)")
  }else{
    txt<-paste0("Year: ", data$Period, '\n', "Movement: ", round(data$Movement,1), '%', '\n',"Industry: ", data$Industry)
    title<-paste0("<b>Nominal GDP percentage movement of <b><i>", Industry1,"</i> vs <i>", Industry2,"</i>")
    ylab <- paste0("Percent")
    
  }
  p<-ggplot(data=data, aes(x=Period, y=GDP, group=Industry, text= txt)) +
    geom_line(aes(color=Industry))+
    geom_point(aes(color=Industry))+
    scale_color_manual(values=c("#085c75","#d2ac2f"))+
    theme_minimal()+
    ylab(ylab)+
    xlab("Year ended March")+
    theme(legend.position="bottom",
          text = element_text( size = 11, family = "Arial"),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"))+ 
    scale_x_discrete(breaks = integer_breaks(), expand = c(0,1))+
    scale_y_continuous(breaks = pretty(data$GDP), 
                       labels = format(abs(pretty(data$GDP)), big.mark = ",",scientific = FALSE))+
    geom_hline(aes(yintercept=0), color="black", linetype="dashed") 
  
  
  
  
  
  return(ggplotly(p, tooltip=c("text"))%>%
           config(displayModeBar= F)%>%
           layout(margin = list( l=0,b=0,t=30,r=0,pad=0), 
                  legend = list(orientation = "h", xanchor = "center", x=0.5, y=-0.15),
                  title = title, font=(list(size=11, family='Arial'))) 
  )
}

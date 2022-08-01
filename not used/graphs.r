##Sunburstgraph for view1

sunburstplot <- function(yr){
  
  year = yr
  
  groups<- level_2a %>%
    mutate(Period=as.character(Period))%>%
    select(-Industry)%>%
    group_by(Period, Main)%>%
    summarise(GDP=sum(GDP))
  
  Main <- full_join(groups, Main)
  Main <- Main %>% arrange(Period)
  
  Main$Main<-factor(Main$Main, levels = c("Primary industries","Service industries","Goods-producing industries","Taxes"))
  main.col=c("#ae4e51","#d2ac2f","#085c75","#35345d")
  
  ## data for total GDP
  totGDP<-tax_data%>%
    filter(Main=="SNEA.SG00NAC00B01")%>%
    mutate(Main='Total GDP')
  
  totalGDP <-Main %>%
    filter(Period==year)
  
  ##Pass GDP to variable
  nzgdp<-format(totGDP[1,2],digits=2, big.mark = ",") ##get GDP value
  
  
  font1 <- list(size=11, family='Arial')
  ptc <- round(totalGDP$GDP/sum(totalGDP$GDP)*100,0)
  ptc <- paste0(ptc,"%")
  
  font1 <- list(famly = "Arial", size = 14, color='white')
  ptc <- paste0(round(totalGDP$GDP/sum(totalGDP$GDP)*100,0), "%")
  
  
  
  pie <- totalGDP %>% 
    group_by(Main) %>%
    plot_ly(labels = ~Main, values = ~GDP, textinfo = 'label+text',text= ptc, hoverinfo = 'label+text',
            marker=list(colors= main.col))%>%
    add_pie(hole = 0.6) %>%
    config(displayModeBar = F)%>%
    layout(title = paste0('<b>Share of the economy in <b>', yr), font=(list(size=11, family='Arial')),
           showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  return(pie)
}

##horizontal bargraph for view1

barplot1 <- function(yr){
  
  year = yr
  
  #Total GDP for title 
  totalGDP <- totGDP%>%
    filter(Period==year)%>%
    mutate(GDP=as.numeric(GDP))
  
  
  gdp<-totalGDP[1,2]
  nzgdp<-format(gdp, digits=2, big.mark = ",", scientific = FALSE)
  totalgdp<- paste0("GDP = $ ", nzgdp,"m")
  
  #Filtering top 10 industries 
  top_10 <- level_2a %>%
    filter(Period==year)%>%
    mutate(GDP=as.numeric(GDP))%>%
    top_n(10, GDP)
  
  x <-max(top_10$GDP)
  
  gdp <-ggplot(top_10, aes(x=reorder(Industry, GDP), y=GDP, fill=Main, label=Industry, 
                           text = paste0("Industry: ",Industry,'\n',"GDP: ",GDP, '\n',"Broad Industry Group: ",Main)))+
    geom_bar(stat="identity")+ coord_flip()+theme_minimal()+
    xlab(" ")+ylab("Contribution to value-added, $(million)")+
    #ggtitle(paste0("<b>Top 10 Industries in <b>", year," ", totalgdp), font=list(size=10))+ 
    theme(legend.position = "none", panel.grid.major = element_blank())+
    scale_fill_manual(values=c("Service industries" = "#085c75", "Goods-producing industries" = "#ae4e51", "Primary industries"="#d2ac2f"))+
    ylim(0,(x+(x*.1)))+
    scale_x_discrete(labels=wrap_format(35))
  
  # gdp <- gdp + guides(fill=guide_legend(title="Broad Industry"))
  ggplotly(gdp) %>% config(displayModeBar = F)
  
  ggplotly(gdp, tooltip="text")%>%
    config(displayModeBar = F)%>% 
    layout(margin = list( l=0,b=40,t=40,r=0,pad=0), 
           title = paste0("<b>Top 10 Industries in <b>", year, '\n', totalgdp), font=(list(size=11, family='Arial')) )
  
}

barplot2 <- function(yr){
  
  year = yr
  
  #Total GDP for title 
  totalGDP <- totGDP%>%
    filter(Period==year)%>%
    mutate(GDP=as.numeric(GDP))
  
  totalGDP<-as.data.frame(totalGDP)
  gdp<-totalGDP[1,2]
  nzgdp<-format(gdp, digits=2, big.mark = ",")
  totalgdp<- paste0(", GDP = $ ", nzgdp," m")
  
  top_all <- level_2a %>%
    filter(Period==year)%>%
    mutate(GDP=as.numeric(GDP))
  
  x <-max(top_all$GDP)
  
  gdp <-ggplot(top_all, aes(x=reorder(Industry, GDP), y=GDP, fill=Main, label=Industry, text = paste0("Industry: ",Industry,'\n',"GDP: ",GDP, '\n',"Broad Industry Group: ",Main)))+
    geom_bar(stat="identity")+ coord_flip()+theme_minimal()+
    xlab(" ")+ylab("Contribution to value-added, $(million)")+
    ggtitle(paste0("All industries in ", year," ",totalgdp))+ 
    theme(legend.position = "none", panel.grid.major = element_blank())+
    scale_fill_manual(values=c("Service industries" = "#085c75", "Goods-producing industries" = "#ae4e51", "Primary industries"="#d2ac2f"))+
    ylim(0,(x+(x*.1))) +
    scale_x_discrete(labels=wrap_format(35))
  
  # gdp <- gdp + guides(fill=guide_legend(title="Broad Industry"))
  
  ggplotly(gdp) %>% config(displayModeBar = F)
  
  ggplotly(gdp, tooltip="text")%>%
    config(displayModeBar = F) %>% layout(margin = list( l=0,b=40,t=30,r=0,pad=0), font=(list(size=11, family='Arial'))) 
  
}


##horizontal bargraph for view2

barplot3 <- function(year, BroadIndustry){
  
  
  
  
  #Total GDP for title 
  groups<- level_2a %>%
    select(Period,GDP,Industry,Main,color)%>%
    mutate(Period=as.character(Period))%>%
    select(-Industry)%>%
    group_by(Period, Main)%>%
    summarise(GDP=sum(GDP)) 
  
  groupsfortitle <- groups%>%
    filter(Period==year, Main==BroadIndustry)%>%
    mutate(GDP=as.numeric(GDP))
  
  groupsfortitle<-as.data.frame(groupsfortitle)
  gdpbybroad <-  groupsfortitle[1,3]
  broad <-format(gdpbybroad, digits=2, big.mark = ",", scientific = FALSE)
  gdpforbroad<- paste0(", GDP = $ ", broad," m")
  
  GDPlevel2 <- level_2a%>%
    filter(Period==year, Main==BroadIndustry) %>%
    mutate(GDP=as.numeric(GDP)) 
  
  #plot barchart
  gdp2 <-ggplot(GDPlevel2, aes(x=reorder(Industry, GDP), y=GDP, fill=Main, label=Industry, text = paste0("Industry: ",Industry,'\n',"GDP: ",GDP, '\n',"Broad Industry Group: ",Main)))+
    geom_bar(stat="identity")+ coord_flip()+theme_minimal()+
    xlab("")+ylab("Contribution to value-added, $(million)")+
    theme(legend.position = "none", panel.grid.major = element_blank())+
    #ggtitle(paste0(BroadIndustry," in ", year," ",gdpforbroad))+
    scale_fill_manual(values=c("Service industries" = "#085c75", "Goods-producing industries" = "#ae4e51", "Primary industries"="#d2ac2f"))+
    scale_x_discrete(labels=wrap_format(35))
  
  # gdp2 <- gdp2 + guides(fill=guide_legend(title="Broad Industry"))
  
  ggplotly(gdp2) %>% config(displayModeBar = F)
  
  ggplotly(gdp2, tooltip=c("text"))%>%
    config(displayModeBar = F)%>% layout(margin = list( l=0,b=40,t=0,r=0,pad=0), font=(list(size=11, family='Arial'))) 
  
}


###comparative linechart for view3
linechart_compare1<- function(Industry1, Industry2){
  
  Industry1=Industry1
  Industry2=Industry2
  
  Industry_compare=c(Industry1, Industry2)
  
  integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
      breaks <- floor(pretty(x, n, ...))
      names(breaks) <- attr(breaks, "labels")
      breaks
    }
    return(fxn)
  }
  
  compare_industry_data <-level_3a %>%
    # filter(Period!=c('1987','1988','1989','1990'))%>%
    select(Industry, Period, GDP)%>%
    mutate(GDP=as.numeric(GDP)) %>%
    filter(Industry %in% Industry_compare)
  # %>%
  # select(Industry, GDP, Period) %>%
  # select(Industry, sort(colnames(.)))
  
  p<-ggplot(data=compare_industry_data, aes(x=Period, y=GDP, group=Industry, text=paste0("Year : ", Period, '\n', "GDP : ", GDP, '\n',"Industry : ", Industry))) +
    geom_line(aes(color=Industry))+geom_point(aes(color=Industry))+
    # geom_smooth(method = "loess")+
    scale_color_manual(values=c("#085c75","#d2ac2f"))+
    theme_minimal()+
    ylab(" ")+
    xlab("Year ended March")+
    theme(legend.position="bottom",
          text = element_text( size = 11, family = "Arial"),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"))+ 
    scale_x_discrete(breaks = integer_breaks())+
    # scale_y_continuous(breaks = pretty(compare_industry_data$GDP), labels=pretty(compare_industry_data$GDP))+
    scale_y_continuous(breaks = pretty(compare_industry_data$GDP), labels = format(abs(pretty(compare_industry_data$GDP)), big.mark = ",",scientific = FALSE))+
    geom_hline(aes(yintercept=0), color="black", linetype="dashed") 
  
  ggplotly(p) %>% config(displayModeBar= F)
  
  ggplotly(p, tooltip=c("text"))%>%
    config(displayModeBar = F)%>% 
    layout(margin = list( l=0,b=0,t=30,r=0,pad=0), 
           legend = list(orientation = "h", xanchor = "center", x=0.5, y=-0.15),
           title = paste0("<b>Nominal GDP value movement of <b><i>", Industry1,"</i> vs <i>", Industry2,"</i>"), font=(list(size=11, family='Arial'))) 
  
}


linechart_compare2 <- function(Industry3, Industry4){
  
  Industry3=Industry3
  Industry4=Industry4
  
  Industry_compare=c(Industry3, Industry4)
  
  integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
      breaks <- floor(pretty(x, n, ...))
      names(breaks) <- attr(breaks, "labels")
      breaks
    }
    return(fxn)
  }
  compare_industry_data <-level_3a %>%
    # filter(Period!=c('1987','1988','1989','1990'))%>%
    select(Industry, Period, GDP)%>%
    filter(Industry %in% Industry_compare)%>%
    # mutate(GDP = replace(GDP, is.na(GDP), ""))%>%
    mutate(lag_val=lag(GDP))%>%
    mutate(Movement=ifelse(Period==min(Period),0,(GDP/lag_val-1)*100))%>%
    mutate(Movement=as.numeric(round(Movement),1))%>%
    select(Industry, Movement, Period)%>%
    filter(Period!=1987)%>%
    select("Industry", sort(colnames(.)))
  
  
  p<-ggplot(data=compare_industry_data, aes(x=Period, y=Movement, group=Industry, text=paste0("Year : ", Period, '\n', "Movement : ", round(Movement,1), '%', '\n',"Industry : ", Industry))) +
    geom_line(aes(color=Industry))+
    geom_point(aes(color=Industry))+
    scale_color_manual(values=c("#085c75","#d2ac2f"))+
    theme_minimal()+
    ylab("")+
    xlab("Year ended March")+
    theme(
      text = element_text( size = 11, family = "Arial"),
      panel.border = element_blank(),
      axis.line = element_line(colour = "black"))+
    scale_x_discrete(breaks = integer_breaks())+
    scale_y_continuous(breaks = pretty(compare_industry_data$Movement), labels=scales::percent(pretty(compare_industry_data$Movement/100))) +
    geom_hline(aes(yintercept=0), color="black", linetype="dashed")
  
  p <- p + theme(legend.position="bottom")
  
  ggplotly(p)%>%config(displayModeBar= F)
  
  ggplotly(p, tooltip=c("text"))%>%
    config(displayModeBar = F) %>%
    layout(margin = list( l=0,b=0,t=30,r=0,pad=0),
           legend = list(orientation = "h", xanchor="center", x=0.5, y=-0.15),
           title = paste0("<b>Nominal GDP percentage movement of <b><i>", Industry3,"</i> vs <i>", Industry4,"</i>"), font=(list(size=11, family='Arial')))
}

# ##sunburst version2
# sunburstplot2 <- function(yr){
#   
#   year = yr
#   
#   level_2a$ids <- paste(level_2a$Main," - ",level_2a$Industry, sep = "")
#   
#   test <- level_2a %>% 
#     select(Period, GDP, parents=Main, color, ids, label=Industry)
#   
#   group <- test %>%
#     group_by(Period, parents,color)%>%
#     summarise(GDP=sum(GDP))%>%
#     select(Period, GDP, ids=parents, color)
#   
#   group$label <- group$ids
#   
#   taxespie2 <- Main %>% select(Period, GDP, ids=Main)
#   taxespie2$label <- taxespie2$ids
#   taxespie2$color <- paste("#35345d")
#   
#   group2 <- full_join(group, taxespie2)
#   
#   group3 <- full_join(test, group2)%>%
#     select(Period,ids, GDP, parents, color, label) %>%
#     mutate(parents = replace(parents, is.na(parents), "")) %>%
#     mutate(label = replace(label, is.na(label), "")) %>%
#     arrange(parents,ids)%>%
#     filter(Period==1990)
#   
#   totGDPtest<-tax_data%>%
#     filter(Main=="SNEA.SG00NAC00B01")%>%
#     mutate(Main='Total GDP')%>%
#     filter(Period==1990)
#   
#   group3$value <- round(group3$GDP/sum(totGDPtest$GDP)*100,0)
#   
#   percenthover <- paste0(group3$value,"%")
#   sunburstg <- plot_ly()
#   
#   sunburstg <- sunburstg %>%
#     add_trace(
#       ids = group3$ids,
#       labels = group3$label,
#       parents = group3$parents,
#       values = group3$value,
#       hovertext = paste(group3$label,percenthover),
#       hoverinfo = "text",
#       type = 'sunburst',
#       branchvalues = "total",
#       maxdepth = 2,
#       domain = list(column = 0)
#     ) %>%
#     layout(title = paste0('Share of the economy in ', yr),
#            showlegend = F,
#            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#            margin = list(l = 0, r = 0, b = 0, t = 0),
#            sunburstcolorway = group3$color) %>%
#     
#     
#     config(displayModeBar = F)
#   
#   sunburstg
# }

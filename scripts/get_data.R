


linechart_data<- function(data, Industry1, Industry2, is_value = T ){
  
  Industry_compare=c(Industry1, Industry2)
  
  
  if (is_value == T){
    data <-data %>%
      select(Industry, Period, GDP)%>%
      mutate(GDP=as.numeric(GDP)) %>%
      filter(Industry %in% Industry_compare)
    
  }else{
    
    
    data <-data %>%
      select(Industry, Period, GDP2= GDP)%>%
      filter(Industry %in% Industry_compare)%>%
      mutate(lag_val=lag(GDP2))%>%
      mutate(Movement=ifelse(Period==min(Period),0,(GDP2/lag_val-1)*100))%>%
      mutate(Movement=as.numeric(round(Movement),1))%>%
      select(Industry, Movement, Period)%>%
      filter(Period!=1987)%>%
      mutate(GDP = Movement)
    
  }
  return(data)
  
}



broad_industry_data<-function(data, year, Broad_Industry ){
  
  groups<- data %>%
    select(Period,GDP,Industry,Main,color)%>%
    mutate(Period=as.character(Period))%>%
    select(-Industry)%>%
    group_by(Period, Main)%>%
    summarise_if(is.numeric, sum) %>%
    ungroup()%>%
    filter(Period==year, Main==Broad_Industry)%>%
    mutate(GDP=as.numeric(GDP))
  
  
  data <- data%>%
    filter(Period==year, Main==Broad_Industry) %>%
    mutate(GDP=as.numeric(GDP)) 
  
  
  return(list(data = data, groups = groups))
  
}

barchart_top_10_data<-function(data, data_totals, year){
  
  year = year
  
  data_totals <- data_totals%>%
    filter(Period==year)
  
  # tot_gdp<-data_totals[1,2]
  
  data <- data %>%
    filter(Period==year)%>%
    mutate(GDP=as.numeric(GDP))%>%
    top_n(10, GDP)
  
  return(list(data= data, data_totals = data_totals))
  
}



get_download_data<-function(data_1, data_2){
  
  
  data<-rbind(data_1, data_2)%>%
    select(Period, GDP, Industry, `Broad Industry Group`= Main)%>%
    distinct()
  
  
  
  return(data)
}



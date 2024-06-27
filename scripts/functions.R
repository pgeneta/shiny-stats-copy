
cleanlevel2 <-function(data){
  GDP <- data
  GDP$Main<-"Service industries"
  GDP$color<-"#085c75"
  
  #Primary industry
  AA1=which(GDP$Industry=="Agriculture")
  AA2=which(GDP$Industry=="Forestry and Logging")
  AA3=which(GDP$Industry=="Fishing, Aquaculture and Agriculture, Forestry and Fishing Support Services")
  BB1=which(GDP$Industry=="Mining")
  
  #Goods producing industry
  CC1=which(GDP$Industry=="Food, Beverage and Tobacco Product Manufacturing")
  CC2=which(GDP$Industry=="Textile, Leather, Clothing and Footwear Manufacturing")
  CC3=which(GDP$Industry=="Wood and Paper Products Manufacturing")
  CC4=which(GDP$Industry=="Printing")
  CC5=which(GDP$Industry=="Petroleum, Chemical, Polymer and Rubber Product Manufacturing")
  CC6=which(GDP$Industry=="Non-Metallic Mineral Product Manufacturing")
  CC7=which(GDP$Industry=="Metal Product Manufacturing")
  CC8=which(GDP$Industry=="Transport Equipment, Machinery and Equipment Manufacturing")
  CC9=which(GDP$Industry=="Furniture and Other Manufacturing")
  DD1=which(GDP$Industry=="Electricity, Gas, Water and Waste Services")
  EE1=which(GDP$Industry=="Construction")
  
  #Assign broad industry group
  GDP$Main[c(AA1,AA2,AA3,BB1)]<- "Primary industries"
  GDP$Main[c(CC1,CC2,CC3,CC4,CC5,CC6,CC7,CC8,CC9,DD1,EE1)]<- "Goods-producing industries"
  #Assign color
  GDP$color[c(AA1,AA2,AA3,BB1)]<-"#d2ac2f"
  GDP$color[c(CC1,CC2,CC3,CC4,CC5,CC6,CC7,CC8,CC9,DD1,EE1)]<-"#ae4e51"
  
  GDP<-GDP%>%
    mutate(Industry=recode(Industry, "Electricity, Gas, Water and Waste Services" = "Electricity, gas, water and waste services" ,
                           "Owner-Occupied Property Operation (National Accounts Only)"  ="Owner-occupied property operation" ,
                           "Forestry and Logging"="Forestry and logging",
                           "Fishing, Aquaculture and Agriculture, Forestry and Fishing Support Services"="Fishing, aquaculture and agriculture, forestry and fishing support services",
                           "Food, Beverage and Tobacco Product Manufacturing"="Food, beverage and tobacco product manufacturing",
                           "Textile, Leather, Clothing and Footwear Manufacturing"="Textile, leather, clothing and footwear manufacturing",
                           "Wood and Paper Products Manufacturing"="Wood and paper products manufacturing",
                           "Petroleum, Chemical, Polymer and Rubber Product Manufacturing"="Petroleum chemical and rubber manufacturing",
                           "Non-Metallic Mineral Product Manufacturing"="Non metallic mineral product manufacturing",
                           "Metal Product Manufacturing"="Metal product manufacturing",
                           "Transport Equipment, Machinery and Equipment Manufacturing"="Transport equipment, machinery and equipment manufacturing",
                           "Furniture and Other Manufacturing"="Furniture and other manufacturing",
                           "Wholesale Trade"="Wholesale trade",
                           "Retail Trade"="Retail trade",
                           "Accommodation and Food Services"="Accommodation and food services",
                           "Transport, Postal and Warehousing"="Transport, postal and warehousing",
                           "Information Media and Telecommunications"="Information media and telecommunications",
                           "Financial and Insurance Services"="Financial and insurance services",
                           "Rental, Hiring and Real Estate Services"="Rental hiring and real estate services",
                           "Professional, Scientific and Technical Services"="Professional scientific and technical services",
                           "Administrative and Support Services"="Administrative and support services",
                           "Local Government Administration"="Local government administration",
                           "Central Government Administration, Defence and Public Safety"="Central government administration defence and public safety",
                           "Education and Training"="Education and training",
                           "Health Care and Social Assistance"="Health care and social assistance",
                           "Arts and Recreation Services"="Arts and recreation services",
                           "Other Services"="Other services","Pulp, paper and converted product manufacturing"="Pulp, paper and converted product manufacturing"))
  
  #Change to sentence case
  data <- GDP %>%
    mutate(Industry=str_to_sentence(Industry))
  return(data)
}

###Creating function for cleaning data 
cleanlevel3 <-function(data){
  GDP <- data
  GDP$Main<-"Service industries"
  GDP$color<-"#085c75"
  
  ###Primary industry
  AA11=which(GDP$Industry=="Horticulture and Fruit Growing")
  AA12=which(GDP$Industry=="Sheep, Beef Cattle and Grain Farming")
  AA13=which(GDP$Industry=="Dairy Cattle Farming")
  AA14=which(GDP$Industry=="Poultry,Deer and Other Livestock Farming")
  AA21=which(GDP$Industry=="Forestry and Logging")
  AA31=which(GDP$Industry=="Fishing and Aquaculture")
  AA32=which(GDP$Industry=="Agriculture, Forestry and Fishing Support Services and Hunting")
  BB11=which(GDP$Industry=="Mining")
  
  ###Goods producing industry
  CC11=which(GDP$Industry=="Meat and Meat Product Manufacturing")
  CC12=which(GDP$Industry=="Seafood Processing")
  CC13=which(GDP$Industry=="Dairy Product Manufacturing")
  CC14=which(GDP$Industry=="Fruit, Oil, Cereal and Other Food Product Manufacturing")
  CC15=which(GDP$Industry=="Beverage and Tobacco Product Manufacturing")
  CC21=which(GDP$Industry=="Textile, Leather, Clothing and Footwear Manufacturing")
  CC31=which(GDP$Industry=="Wood Product Manufacturing")
  CC32=which(GDP$Industry=="Pulp, paper and converted product manufacturing")
  CC41=which(GDP$Industry=="Printing")
  CC51=which(GDP$Industry=="Petroleum and Coal Product Manufacturing")
  CC52=which(GDP$Industry=="Basic Chemical and Chemical Product Manufacturing")
  CC53=which(GDP$Industry=="Polymer Product and Rubber Product Manufacturing")
  CC61=which(GDP$Industry=="Non-Metallic Mineral Product Manufacturing")
  CC71=which(GDP$Industry=="Primary Metal and Metal Product Manufacturing")
  CC72=which(GDP$Industry=="Fabricated Metal Product Manufacturing")
  CC81=which(GDP$Industry=="Transport Equipment Manufacturing")
  CC82=which(GDP$Industry=="Machinery and Other Equipment Manufacturing")
  CC91=which(GDP$Industry=="Furniture and Other Manufacturing")
  DD11=which(GDP$Industry=="Electricity and Gas Supply")
  DD12=which(GDP$Industry=="Water, Sewerage, Drainage and Waste Services")
  EE11=which(GDP$Industry=="Building Construction")
  EE12=which(GDP$Industry=="Heavy and Civil Engineering Construction")
  EE13=which(GDP$Industry=="Construction Services")
  
  ###Assign broad industry group
  GDP$Main[c(AA11,AA12,AA13,AA14,AA21,AA31,AA32,BB11)]<-"Primary industries"
  GDP$Main[c(CC11,CC12,CC13,CC14,CC15,CC21,CC31,CC32,CC41,CC51,CC52,CC53,CC61,CC71,CC72,CC81,CC82,CC91,DD11,DD12,EE11,EE12,EE13)]<-"Goods-producing industries"
  
  ###Assign color
  GDP$color[c(AA11,AA12,AA13,AA14,AA21,AA31,AA32,BB11)]<-"#d2ac2f"
  GDP$color[c(CC11,CC12,CC13,CC14,CC15,CC21,CC31,CC32,CC41,CC51,CC52,CC53,CC61,CC71,CC72,CC81,CC82,CC91,DD11,DD12,EE11,EE12,EE13)]<-"#ae4e51"
  
  ###change to sentence case
  data <- GDP %>%
    mutate(Industry=str_to_sentence(Industry))
  return(data)
}

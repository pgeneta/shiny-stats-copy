# 
# gdp_level_3 <-  as.data.frame(read.csv("AnnualShinyApp/data/2019-level3-industry.csv", stringsAsFactors = F))
# gdp_level_2 <- as.data.frame(read.csv("AnnualShinyApp/data/na-nov2019-gdp-breakdown-csv.csv", stringsAsFactors = F))

# #2019 data
# gdp_level_3 <-as.data.frame( read.csv("data/2019-level3-industry.csv", stringsAsFactors = F))
# gdp_level_2 <-as.data.frame( read.csv("data/na-nov2019-gdp-breakdown-csv.csv", stringsAsFactors = F))

##2020 data
gdp_level_2 <-as.data.frame( readRDS("data/GDP_level_2.rds"))
gdp_level_3 <-as.data.frame( readRDS("data/GDP_level_3.rds"))

# gdp_level_3 <- as.data.frame(read_excel("data/2020level3.xlsx"))
# gdp_level_2 <- as.data.frame(read.csv("data/na-nov2021-gdp-breakdown-csv.csv", stringsAsFactors = F))

exclude <-c("", "Market", "Non-Market","Total Market and Non-Market","Total All Institutonal Sectors","Total All Industries")

###GDP data level2 industry 
level_2 <-gdp_level_2 %>%
  select(everything())%>%
  filter(Series_title_1=="Gross Domestic Product - production measure")%>%
  mutate(Period= as.character(round(Period),0))%>%
  filter(Group =="Series, GDP(P), Nominal, Actual, ANZSIC06 industry groups")%>%
  select(Period, GDP=Data_value, Industry=Series_title_2)%>%
  filter(!Industry %in% exclude)%>%
  arrange(Industry)

###GDP data level 3 industry 
level_3 <-gdp_level_3 %>%
  select(everything())%>%
  filter(Series_title_1=="Gross Domestic Product - production measure")%>%
  mutate(Period=as.character(round(Period),0))%>%
  mutate(Data_value=as.numeric(round(Data_value),0))%>%
  select(Period, GDP=Data_value, Industry=Series_title_2)%>%
  filter(!Industry %in% exclude)


###Taxes data - gst, unallocated tax, duties and total GDP numbers needed for header 
tax_data<-gdp_level_2 %>%
  filter(Series_reference %in% c("SNEA.SG01NAC00D21","SNEA.SG01NAC00D22","SNEA.SG01NAC00D29","SNEA.SG00NAC00B01"))%>%
  mutate(Period= as.character(as.integer(Period)))%>%
  select(Main=Series_reference, GDP=Data_value, Period)

Main <-tax_data%>%
  filter(Main!="SNEA.SG00NAC00B01")%>% ##remove total GDP from dataset
  group_by(Period)%>%
  summarise(GDP=sum(GDP))%>%
  mutate(Main='Taxes')

Main$Main<-factor(Main$Main, levels = c("Primary industries","Service industries","Goods producing industries","Taxes"))
main.col=c("#d2ac2f","#085c75","#ae4e51","#35345d")

###Total GDP 
totGDP<-tax_data%>%
  filter(Main=="SNEA.SG00NAC00B01")%>%
  mutate(Main ='Total GDP')



###Cleaning data for level2 industry 

###create data used for view1,view2 and view3
level_2a <-cleanlevel2(level_2)
level_3a <-cleanlevel3(level_3)%>%
  filter(!Industry %in% c("Owner-occupied property operation (national accounts only)" ,
                          "Professional, scientific and technical services",
                          "Non-metallic mineral product manufacturing",
                          "Central government administration, defence and public safety"))

level_2b<-level_2a%>%filter(Period >= 1987)

# writexl::write_xlsx(all_industries, "Industries.xlsx")

all_industries<-rbind(level_2b, level_3a)

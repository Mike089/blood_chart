## Set Directory
setwd("C:/GitHub/R/blood_chart"

## Libraries
library(tidyverse)
library(scales)
library(ggthemes)
library(lubridate)
library(extrafont)
library(gganimate)
library(firatheme)

#RedColor
red <- "#c5161d"


## Load the data
weekly_covid_deaths <- read.csv("owid-covid-data.csv")


## Date format
weekly_covid_deaths$date <- ymd(weekly_covid_deaths$date)

weekly_covid_deaths <-  weekly_covid_deaths %>%
        mutate(week = week(weekly_covid_deaths$date))

weekly_covid_deaths <-  weekly_covid_deaths %>%
        mutate(month = month(weekly_covid_deaths$date))



#Filter the data
deaths_filtered <- weekly_covid_deaths%>%
        filter(iso_code == "MEX")



daily_deaths <- deaths_filtered %>%
        select(month,week,date,new_deaths,total_deaths)
        

daily_deaths <- daily_deaths %>%
        mutate(months = month.abb[month])



daily_deaths %>%
        filter(new_deaths > 0)%>%
        ggplot(aes(x =date,y =new_deaths))+
        geom_col(fill=red,width = 6)+
        scale_y_reverse(breaks=seq(0,4000,by=500),labels = comma)+
        theme_fira()+
        scale_x_date(date_breaks = "2 months",date_labels = "%b %y",expand = c(0,0))+
        annotate(geom = "text", x = as.Date("2021-01-30", "%Y-%m-%d"), y = 400, label ="Muertes Totales 
295,203",color = "white", size = 10,fontface =2)+
         labs(title = "MUERTES DIARIAS POR COVID-19",
              subtitle = "En Mexico desde el inicio de la pandemia al 05/12/2021",
              caption = "Fuente: OurWorldInData.com | Gráfica: Miguel Hdz - IG: _miguelhg")+
         theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "Verdana"))+
         theme(plot.title = element_text(size=22))+
         theme(axis.title.x = element_blank(),
                 axis.title.y = element_blank())+
         theme(plot.subtitle = element_text(hjust = 0.5),text = element_text(family = "Verdana"))
        





        
        











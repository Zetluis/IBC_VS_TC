#Load the packages. Note: I did not load lubridate or IndexNumber, but I did a function call to those packages, so make sure to have those packages installed.
library(tidyverse)
library(ggthemes)
#### Indice bc ####
indice_bc_raw <- read_csv("Datos IBC.csv") 

#Get a summary of the data and see the col classes
summary(indice_bc_raw)
str(indice_bc_raw)

#Transform the Data according to what I need
indice_bc <- indice_bc_raw %>% 
  slice(n = 1:22) %>%
  mutate(Fecha = lubridate::mdy(Fecha), Variacion = str_remove_all(`% var.`, "[%]")) %>% 
  arrange(Fecha) %>%  
  select(-(3:7)) %>% 
  view()

#Calculate the index for the IBC
new_index_bc <- IndexNumber::index.number.serie(indice_bc$Último, "Ultimo", opt.plot = FALSE, opt.summary = FALSE) 

#Bind indice_bc and the index
indice_bc <- cbind(indice_bc, new_index_bc)

remove(new_index_bc)
#Chech column types  
str(indice_bc)

#Change the col type of variacion to numeric and replace commas with dots.
indice_bc <- indice_bc %>% 
  mutate(Variacion = as.numeric(gsub(",", ".", indice_bc$Variacion))) %>% 
  rename(Variacion_Porcentual_IBC = Variacion, Index_IBC = `Index number`) %>% 
  select(1, 5, 3, 6)

#Chech col types again
str(indice_bc)
#### Tipo de cambio ####
t_cambio_raw <- readxl::read_xlsx("Data meses. Tipo de cambio.xlsx", col_names = c("Fecha", "Valor", "Mes"), col_types = c("text", "numeric", "skip"))

#Chech col type
str(t_cambio_raw)
view(t_cambio_raw)
#Turn Fecha into date format and subset the last day of each month
t_cambio <- t_cambio_raw %>% 
  mutate(Fecha = lubridate::mdy(Fecha)) %>%
  group_by(strftime(Fecha, "%Y-%m")) %>% #Groups by the yearmonths
  filter(Fecha == max(Fecha)) %>% 
  mutate(Valor =  as.numeric(format(Valor, scientific = FALSE))) %>% 
  ungroup() %>% 
  slice(-(1:11)) %>% 
  select(1:2) 

#Check for col types
str(t_cambio)  

#Calculate the index for the exchange rate
Index_TC <- IndexNumber::index.number.serie(t_cambio$Valor, "Valor", opt.plot = FALSE, opt.summary = FALSE)
   
#Bind t_cambio and Index_TC
t_cambio <- cbind(t_cambio, Index_TC)

remove(Index_TC)

#Drop columns 3 and 4 and change col name.
t_cambio <- t_cambio %>% 
  select(-(3:4)) 
  
t_cambio <- t_cambio %>% 
  rename(Index_TC = `Index number`)

####Combine data####
#Filter indice_bc to match t_cambio
indice_bc <- indice_bc %>% 
  filter(Fecha < "2020-09-20") 

#Bind indice_bc and t_cambio, select the columns we need and rename columns
combined_data <- cbind(indice_bc, t_cambio) %>% 
  select(5, 2, 4, 6, 7) %>% 
  rename(Valor_IBC = Ultimo, Valor_TC = Valor)

#Chech col type
str(combined_data)

#### Data Visualization ####

#Create a new DF with the colums, that we need. And making the columns longer in orden to plot.
combined_data_plot <- combined_data %>%
  select(1, 3, 5) %>% 
  pivot_longer(!Fecha, names_to = "Indice", values_to = "Valor_indice") %>% 
  view()

IBC_VS_TC <- combined_data_plot %>% 
  ggplot(aes(Fecha, Valor_indice, group = Indice, color = Indice)) +
  geom_line(size = 1.25) 

IBC_VS_TC + theme_clean() + scale_color_calc() + scale_x_date(breaks = "3 months", date_labels = "%b-%Y") + labs(title = "Crecimiento Dólar Paralelo VS Índice Bursátil de Caracas", x = NULL, y = "Indice. Diciembre 2018 = 100\n", caption = "@luisfrein") + scale_colour_calc(labels = c("Índice Bursátil de Caracas", "Dólar Paralelo")) + theme(legend.position = c(0.15, 0.735)) 

#Checking the distribution of the data
combined_data_plot %>% 
  ggplot(aes(Valor_indice)) +
  geom_histogram()

#Given that the data is skewed to the right, I will make another plot with log transformation.
IBC_VS_TC2 <- combined_data_plot %>% 
  ggplot(aes(Fecha, log(Valor_indice), group = Indice, color = Indice)) +
  geom_line(size = 1.25)

IBC_VS_TC2 + theme_clean() + scale_color_calc() + scale_x_date(breaks = "3 months", date_labels = "%b-%Y") + labs(title = "Crecimiento Dólar Paralelo VS Índice Bursátil de Caracas", x = NULL, y = "Indice Escala Logarítmica\n") + scale_colour_calc(labels = c("Índice Bursátil de Caracas", "Dólar Paralelo")) + theme(legend.position = c(0.15, 0.7))



















 


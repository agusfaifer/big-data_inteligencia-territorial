library(tidyverse)
library(data.table)
library(readr)

#Casos Covid 19 en la provincia de Misiones. Visualización de casos confirmados, edad y cantidad de fallecidos.

# Importación de datos base

base_covid <- data.table::fread("entradas/Covid19Casos.csv",header=TRUE, encoding ="UTF-8")

summary(base_covid)

# Armo tabla de casos confirmados en Misiones.
#Filtrando por provincia de carga y residencia para obtener casos totales dentro del territorio de Misiones.
tabla_misiones <- base_covid %>% 
  filter(clasificacion_resumen == "Confirmado", 
         carga_provincia_nombre %in% ("Misiones"),
         residencia_provincia_nombre %in% ("Misiones"),
         !is.na(fecha_diagnostico),
         edad %in% c(0:110)) 

write.csv(x = tabla_misiones, file = "tabla_covid_misiones.csv", row.names = TRUE ) 
  
summary(tabla_misiones)

#Grafico de casos confirmados y edad media.
grafico_confirmados_misiones <- tabla_misiones %>% 
  ggplot(aes(x = fecha_diagnostico,
             y = edad)) +
  geom_point(alpha = 0.1, 
             size = 1, 
             color = "#00bfc4") +
  scale_y_continuous(breaks = seq(0, 110, 10)) +
  scale_x_date(date_breaks = "month",
               date_labels = '%b %Y') +
  geom_hline(yintercept = mean(tabla_misiones$edad, na.rm = T)) + 
  annotate(geom = "text",
           x = as.Date("2020-06-01"), y = 45, 
          label = paste0("Edad media : ", round(mean(tabla_misiones$edad, na.rm = T))),
           hjust = "left") +
theme_minimal()+
  labs(title = "Total casos de Covid-19 en Misiones",
       x = "Fecha de diagnostico",
       y = "Edad") 

grafico_confirmados_misiones

#Comentario: La edad media de los positivos confirmados es de 41 años.


# Armo tabla casos totales por Departamentos de Misiones
tabla_departamentos <- tabla_misiones %>% 
  rename(departamento = residencia_departamento_nombre) %>% 
  count(departamento)


#Grafico de casos totales por departamento
tabla_departamentos %>% 
  ggplot(aes(x = fct_reorder(departamento, -n), 
             y = n,
             fill = fct_reorder(departamento, -n))) +
  scale_fill_viridis_d() +
  geom_col() +
  geom_text(aes(label = n),
            vjust= -0.5,
            hjust= -0.3) +
  theme_minimal() +
  coord_flip()+
  
  labs(title = "Cantidad de casos por Departamento",
       x = "Departamentos",
       y = "Cantidad",
       fill = "Departamento") 



#Grafico casos totales por sexo
tabla_misiones %>% 
  ggplot(mapping = aes(x = fecha_diagnostico,
                       y = edad,
                       color= sexo)) +
  geom_point(alpha = 0.5,
             size = 1)+ 
  scale_color_manual(values = c("green", "purple", "black"))+
  scale_y_continuous(breaks = seq(0, 110, 10)) +
  scale_x_date(date_breaks = "month",
               date_labels = '%b %Y') +
  theme_minimal() +
  labs(title = "Casos de Covid-19 en Misiones",
       subtitle = "Según sexo",
       x = "Fecha de diagnostico",
       y = "Edad")



#Tabla total de fallecidos en Misiones
tabla_fallecidos_misiones <- tabla_misiones %>% 
  filter(fallecido == "SI", 
         !is.na(fecha_fallecimiento),
         edad %in% c(0:110))
count(tabla_fallecidos_misiones)


#Grafico total de fallecidos dividido por sexo. Analisis de la edad media de los fallecidos.
tabla_fallecidos_misiones %>% 
  ggplot(aes(x = fecha_diagnostico,
             y = edad,
             color= sexo,
             shape= sexo))+
  scale_color_manual(values = c("green", "purple", "black"))+
  geom_point(alpha = 0.5, 
             size = 2) +  
  geom_hline(yintercept = mean(tabla_fallecidos_misiones$edad, na.rm = T)) +
  scale_y_continuous(breaks = seq(0, 110, 10)) +
  scale_x_date(date_breaks = "month",
               date_labels = '%b %Y') +

  theme_minimal() +
  labs(title = "Total casos fallecidos segun sexo",
       x = "Fecha de fallecimiento",
       y = "Edad") +
  annotate(geom = "text",
           x = as.Date("2020-07-01"), y = 70, 
           label = paste0("Edad media : ", round(mean(tabla_fallecidos_misiones$edad, na.rm = T))),
           hjust = "left")

# Comentarios: La edad media de los fallecidos es de 67 años, que es menor que la edad media del total de Argentina que es 71 años.






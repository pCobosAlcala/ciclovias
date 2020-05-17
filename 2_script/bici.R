### Configuración inicial ----
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, readr, scales)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


### Bases y manipulación de base ----

## Importar base 
ciclovias <- read_csv("https://datos.cdmx.gob.mx/explore/dataset/ciclovias/download/?format=csv&timezone=America/Mexico_City&lang=es&use_labels_for_header=true&csv_separator=%2C")

ciclovias
## Manipulación de datos
ciclovias <- ciclovias %>% 
  mutate(LONGITUD = as.numeric(LONGITUD))

# ¿Qué instancias han construido más?
ciclovias %>% 
  group_by(INSTANCIA) %>% 
  summarize(long_inst = sum(LONGITUD)) %>% 
  arrange(-long_inst)

# Agrupo algunas observaciones
base <- ciclovias %>% 
  mutate(INSTANCIA = str_replace_all(INSTANCIA, c("Alcaldia Coyoacan" = "14 alcaldías (sin BJ ni MH)",
                                                  "Alcaldia Alvaro Obregon" = "14 alcaldías (sin BJ ni MH)",
                                                  "Alcaldia Azcapotzalco" = "14 alcaldías (sin BJ ni MH)",
                                                  "Alcaldia Azcaptozalco" = "14 alcaldías (sin BJ ni MH)" ,
                                                  "Alcaldia Benito Juarez" = "Alcaldía Benito Juárez (BJ)" ,
                                                  "Alcaldia Cuauhtemoc" = "14 alcaldías (sin BJ ni MH)",
                                                  "Alcaldia Iztacalco" = "14 alcaldías (sin BJ ni MH)",
                                                  "Alcaldia Iztapalapa" = "14 alcaldías (sin BJ ni MH)",
                                                  "Alcaldia Miguel Hidalgo" = "Alcaldía Miguel Hidalgo (MH)",
                                                  "Autoridad del Centro Historico" = "Otras instancias*",
                                                  "SEDEMA-SOBSE-SEMOVI-ALCALDIA" = "Otras instancias*",
                                                  "SOBSE" = "Otras instancias*",
                                                  "SEDUVI" = "Otras instancias*")))


### Visualización ----

base %>% 
  group_by(AÑO, INSTANCIA) %>% 
  summarize(km = sum(LONGITUD)) %>% 
  ggplot() + 
  geom_col(aes(x = AÑO,
               y = km,
               group = AÑO,
               fill = INSTANCIA), color = "black") +
  scale_x_continuous(breaks = seq(2004, 2020,1)) + 
  scale_y_continuous(breaks = seq(0, 60, 10)) + 
  theme_minimal() + 
  labs(title = "Kilómetros anuales de infraestructura ciclista por instancia implementadora",
       subtitle = "CDMX, 2004-2020",
       x = "Año",
       y = "Kilómetros",
       caption = "*ACH, SOBSE, SEDUVI e implementaciones por varias instancias\n\n@pCobosAlcala - Datos: bit.ly/infraestructuraciclista",
       fill = "Instancia") + 
  theme(plot.caption = element_text(hjust = 0),
        legend.position = c(0.32, 0.6),
        axis.text.x = element_text(vjust = .5,
                                   angle = 90),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Guardado
ggsave("bici.png",
       width = 6.8,
       height = 4.8,
       dpi = 500)  

### Sumas ----
# ¿Cuánto implementó cada instancia por año?
base %>% 
  group_by(AÑO, INSTANCIA) %>% 
  summarize(suma = sum(LONGITUD)) %>% 
  print(n = Inf)

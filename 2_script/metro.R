### Configuración inicial ----
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, readr, scales, readxl, lubridate, janitor)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


### Base de datos ----

tp <- read_csv("https://datos.cdmx.gob.mx/explore/dataset/afluencia-preliminar-en-transporte-publico/download/?format=csv&timezone=America/Mexico_City&lang=es&use_labels_for_header=true&csv_separator=%2C")

glimpse(tp)


### Gráfica Metro ----

## Con respecto a semana anterior
tp %>% 
  filter(ORGANISMO == "STC") %>%
  rename(afluencia = `AFLUENCIA TOTAL\n(Cifras preliminares)`,
         linea = `LINEA/SERVICIO`) %>% 
  select(FECHA, linea, afluencia) %>% 
  arrange(FECHA, linea) %>%
  group_by(year_week = floor_date(FECHA, "1 week"), linea) %>% 
  summarize(sum = sum(afluencia)) %>% 
  ungroup() %>% 
  arrange(year_week, linea) %>% 
  mutate(sum_rez = lag(sum, 12),
         cambio_porcentual = ((sum - sum_rez) / sum_rez)* 100) %>% 
  ggplot() + 
  geom_tile(aes(x = year_week, y = factor(linea, order = T, levels = c("L1","L2","L3","L4","L5","L6","L7","L8","L9","LA","LB","L12")), fill = cambio_porcentual)) + 
  scale_fill_viridis_c()


## Con respecto a semana base
tp %>% 
  filter(ORGANISMO == "STC") %>%
  rename(afluencia = `AFLUENCIA TOTAL\n(Cifras preliminares)`,
         linea = `LINEA/SERVICIO`) %>% 
  select(FECHA, linea, afluencia) %>% 
  arrange(FECHA, linea) %>%
  group_by(year_week = floor_date(FECHA, "1 week"), linea) %>% 
  summarize(sum = sum(afluencia)) %>% 
  ungroup() %>% 
  arrange(year_week, linea) %>% 
  # Usuarixs de semana base
  mutate(sum_rez = case_when(linea == "L1" ~ 4779906,
                             linea == "L2" ~ 5234716,
                             linea == "L3" ~ 4515579,
                             linea == "L4" ~ 618837,
                             linea == "L5" ~ 1035517,
                             linea == "L6" ~ 995800,
                             linea == "L7" ~ 2203692,
                             linea == "L8" ~ 2695265,
                             linea == "L9" ~ 2178366,
                             linea == "LA" ~ 1650995,
                             linea == "LB" ~ 2968426,
                             linea == "L12" ~ 2814269),
         
         cambio_p = ((sum - sum_rez) / sum_rez)* 1,
         cambio_porcentual = ((sum - sum_rez) / sum_rez) * 100) %>% 
  filter(year_week <= as.Date("2020-05-09"),
         year_week >= as.Date("2020-03-08")) %>% 
  ggplot() + 
  geom_tile(aes(x = year_week,
                y = factor(linea,
                           order = T,
                           levels = c("L12","LB","LA","L9",
                                      "L8","L7","L6","L5",
                                      "L4","L3","L2","L1")),
                fill = cambio_p)) + 
  geom_text(aes(x = year_week,
                y = factor(linea,
                           order = T,
                           levels = c("L12","LB","LA","L9",
                                      "L8","L7","L6","L5",
                                      "L4","L3","L2","L1")),
                label = round(cambio_porcentual, 0)),
            size = 3,
            color = "grey0") +
  scale_x_date(breaks = c(seq(as.Date("2020-03-08"),
                              as.Date("2020-05-03"),
                              "1 week")),
               labels = date_format("%b %d"),
              limits = c(as.Date("2020-03-01"),
                              as.Date("2020-05-15"))
               ) + 
  scale_fill_gradient2(labels = percent) +
  labs(title = "Variación porcentual de usuaries semanales en el Metro-CDMX, por línea", subtitle = "Semana base: 1 al 7 de abril (2020)",
       x = "Inicio de semana",
       y = "",
       fill = "Variación\nporcentual",
       caption = "Elaborado por @pCobosAlcala con datos del Gobierno de la Ciudad de México (bit.ly/2zIWC8o)") + 
  theme_minimal() + 
  theme(legend.position = c(.92,.5),
          plot.title = element_text(hjust = 1.5),
        plot.subtitle = element_text(hjust = .085),
          
        axis.title.y = element_blank(),
        axis.text.y = element_text(margin = margin(0, -1.5, 0, 0, "cm"),
                                   hjust = 0),
        panel.grid = element_blank(),
        axis.text.x = element_text(vjust = .5,
                                   angle = 90),
        plot.caption = element_text(hjust = .30))

ggsave("metro.png",
       width = 6,
       height = 4,
       dpi = 800)

# Usuarixs por línea para semana base
tp %>% 
  filter(ORGANISMO == "STC") %>%
  rename(afluencia = `AFLUENCIA TOTAL\n(Cifras preliminares)`,
         linea = `LINEA/SERVICIO`) %>% 
  select(FECHA, linea, afluencia) %>% 
  arrange(FECHA, linea) %>%
  group_by(year_week = floor_date(FECHA, "1 week"), linea) %>% 
  summarize(sum = sum(afluencia)) %>% 
  ungroup() %>% 
  arrange(year_week, linea) %>% 
  mutate(sum_rez = case_when(linea == "L1" ~ 4779906,
                             linea == "L2" ~ 5234716,
                             linea == "L3" ~ 4515579,
                             linea == "L4" ~ 618837,
                             linea == "L5" ~ 1035517,
                             linea == "L6" ~ 995800,
                             linea == "L7" ~ 2203692,
                             linea == "L8" ~ 2695265,
                             linea == "L9" ~ 2178366,
                             linea == "LA" ~ 1650995,
                             linea == "LB" ~ 2968426,
                             linea == "L12" ~ 2814269)) %>% 
  filter(year_week == as.Date("2020-03-01")) %>% 
  arrange(-sum) %>% 
  print(n = Inf)


### Metrobús, Ecobici, RTP y transporte eléctrico ----

## Metrobús
metrobus <- tp %>% 
  filter(ORGANISMO == "Metrobús") %>%
  rename(afluencia = `AFLUENCIA TOTAL\n(Cifras preliminares)`,
         linea = `LINEA/SERVICIO`) %>% 
  select(FECHA, linea, afluencia) %>% 
  arrange(FECHA, linea) %>%
  group_by(year_week = floor_date(FECHA, "1 week"), linea) %>% 
  summarize(sum = sum(afluencia)) %>% 
  ungroup() %>% 
  arrange(year_week, linea) %>% 
  mutate(sum_rez = case_when(linea == "L1" ~ 3085711,
                             linea == "L2" ~ 1239755,
                             linea == "L3" ~ 1130772,
                             linea == "L4" ~ 462313,
                             linea == "L5" ~ 614839,
                             linea == "L6" ~ 1409535,
                             linea == "L7" ~ 783886),
         
         cambio_p = ((sum - sum_rez) / sum_rez)* 1,
         cambio_porcentual = ((sum - sum_rez) / sum_rez) * 100)

# Para visualizar sólo con Metrobús
 metrobus %>%  ggplot() + 
  geom_tile(aes(x = year_week, y = factor(linea,
                                          order = T,
                                          levels = c("L7","L6","L5","L4","L3","L2","L1")),
                fill = cambio_p)) + 
  geom_text(aes(x = year_week, y = factor(linea,
                                          order = T,
                                          levels = c("L7","L6","L5","L4","L3","L2","L1")), label = round(cambio_porcentual, 0)),
            size = 3) +
  scale_fill_gradient2()



## Otros transportes
otros <- tp %>% 
  filter(ORGANISMO == "Ecobici" |
           ORGANISMO == "RTP" |
         ORGANISMO ==  "STE-Tren Ligero" |
           ORGANISMO == "STE-Trolebús") %>% 
  rename(afluencia = `AFLUENCIA TOTAL\n(Cifras preliminares)`,
         linea = ORGANISMO) %>% 
  select(FECHA, linea, afluencia) %>% 
  arrange(FECHA, linea) %>%
  group_by(year_week = floor_date(FECHA, "1 week"), linea) %>%
  summarize(sum = sum(afluencia, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(year_week, linea) %>% 
  mutate(sum_rez = case_when(linea == "Ecobici" ~ 177373,
                             linea == "RTP" ~ 2491215,
                             linea == "STE-Tren Ligero" ~ 596176,
                             linea == "STE-Trolebús" ~ 1047080),
         
         cambio_p = ((sum - sum_rez) / sum_rez)* 1,
         cambio_porcentual = ((sum - sum_rez) / sum_rez) * 100)

# Para visualizar otros transportes
otros %>% 
  ggplot() + 
  geom_tile(aes(x = year_week,
                y = factor(linea,
                           order = T,
                           levels = c("Ecobici", "RTP", "STE-Trolebús", "STE-Tren Ligero")),
                fill = cambio_p)) + 
  geom_text(aes(x = year_week, y = factor(linea,
                                          order = T,
                                          levels = c("Ecobici", "RTP", "STE-Trolebús", "STE-Tren Ligero")),
                label = round(cambio_porcentual, 0)),
            size = 3) +
  scale_fill_gradient2()


# Visualización conjunta
base <- rbind(metrobus, otros)
  
base %>%
  filter(year_week <= as.Date("2020-05-09"),
         year_week >= as.Date("2020-03-08")) %>% 
  ggplot() + 
  geom_tile(aes(x = year_week,
                y = factor(linea,
                           order = T,
                           levels = c("Ecobici","RTP","STE-Tren Ligero","STE-Trolebús",
                                      "L7","L6","L5", "L4", "L3", "L2", "L1")),
                fill = cambio_p)) + 
  geom_text(aes(x = year_week,
                y = factor(linea,
                           order = T,
                           levels = c ("Ecobici","RTP","STE-Tren Ligero","STE-Trolebús",
                                       "L7","L6","L5", "L4", "L3", "L2", "L1")),
                label = round(cambio_porcentual, 0)),
            size = 3) +
  scale_x_date(breaks = c(seq(as.Date("2020-03-08"),
                              as.Date("2020-05-03"),
                              "1 week")),
               labels = date_format("%b %d"),
               limits = c(as.Date("2020-03-01"),
                          as.Date("2020-05-15"))
  ) + 
  scale_fill_gradient2(labels = percent) +
  scale_y_discrete(labels = c("Ecobici","RTP","Tren Ligero", "Trolebús",
                              "L7 Metrobús","L6 Metrobús","L5 Metrobús", "L4 Metrobús", "L3 Metrobús", "L2 Metrobús", "L1 Metrobús")) + 
  labs(title = "Variación porcentual de usuaries semanales en transporte público, CDMX sin Metro", subtitle = "Semana base: 1 al 7 de abril (2020)",
       x = "Inicio de semana",
       y = "",
       fill = "Variación\nporcentual",
       caption = "Elaborado por @pCobosAlcala con datos del Gobierno de la Ciudad de México (bit.ly/2zIWC8o)") + 
  theme_minimal() + 
  theme(legend.position = c(.92,.5),
        plot.title = element_text(hjust = .55),
        plot.subtitle = element_text(hjust = -.02),
        
        axis.title.y = element_blank(),
        axis.text.y = element_text(margin = margin(0, -1.5, 0, 0, "cm"),
                                   hjust = 0),
        panel.grid = element_blank(),
        axis.text.x = element_text(vjust = .5,
                                   angle = 90),
        plot.caption = element_text(hjust = -.08))

ggsave("tp.png",
       width = 7,
       height = 4,
       dpi = 800)

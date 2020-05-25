---
title: "30 días de gráficos - Día 12"
output:
  html_document:
    df_print: paged
    keep_md: true
include_graphics: yes
---
## Día 12: Lollipop plot

## Librerías

En principio usaré ggplot2 para los gráficos, googlesheets4 para levantar los datos y tidyverse para ordenarlos.

La guía la saqué de [R Graph Gallery]("https://www.r-graph-gallery.com/300-basic-lollipop-plot.html")


```r
library(googlesheets4)
library(gargle)
library(tidyverse)
library(ggthemes)
library(scales)
library(extrafont) # la primera vez ejecutar font_import()

loadfonts()
font <- "Leelawadee UI"
```

## Datos

Tomamos los datos de la Encuesta de Sueldos de Sysarmy que publican en su [blog]("https://sysarmy.com/blog/posts/resultados-de-la-encuesta-de-sueldos-2020-1/").


```r
gs4_deauth()
options(scipen = 999) # Cambia la notación científica de los gráficos
```

```r
encuesta_sysarmy <- sheets_read("1_db6zEAMvr-1GQjJb4hV-rSQfJ9w6GmezbqKJ2JJn7I", skip = 9)
```

```
## Warning: `sheets_read()` is deprecated as of googlesheets4 0.2.0.
## Please use `range_read()` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_warnings()` to see where this warning was generated.
```

```
## Reading from "2020.1 - sysarmy - Encuesta de remuneración salarial Argentina"
```

```
## Range "10:5000000"
```

## Preprocesamiento

Selecciono las columnas de género, puesto y sueldo bruto, con sus nombres cambiados para facilitar el trabajo.


```r
analisis_r30 <- encuesta_sysarmy %>%
  select('Me identifico', 'Trabajo de', `Salario mensual BRUTO (en tu moneda local)`)%>%
  rename(Genero = 'Me identifico', 
         Puesto = 'Trabajo de',
         Sueldo_Bruto = `Salario mensual BRUTO (en tu moneda local)`)
```

Luego, filtro los 10 puestos con más observaciones.

```r
top_10_puestos <- analisis_r30 %>%
  select(Puesto, Genero, Sueldo_Bruto) %>%
  filter(Genero != "Otros") %>%
  group_by(Puesto) %>%
  tally(sort = TRUE) %>% 
  top_n(10) %>%
  select(Puesto)
```

```
## Selecting by n
```

```r
top_10_puestos <- as.vector(top_10_puestos$Puesto)
```

# Gráfico 

Voy a hacer un **gráfico de lollipop** de los sueldos promedios de los 10 puestos con más observaciones.


```r
analisis_r30 %>%
  filter(Puesto %in% top_10_puestos) %>%
  group_by(Puesto) %>%
  summarise(Sueldo_Promedio = mean(Sueldo_Bruto)) %>%
  ggplot(aes(x = reorder(Puesto, Sueldo_Promedio), y = Sueldo_Promedio)) +
  geom_point(color = "#BA4A00", size = 4)+
  geom_segment(aes(x=Puesto, xend=Puesto, y=0, yend=Sueldo_Promedio))+
  coord_flip()+
  labs(title = "Sueldo promedio por puesto",
       subtitle = "Fuente: Encuesta de Sueldos Sysarmy 2020.1",
       caption = "#30diasdegraficos #RStats_ES",
       x="", y="")+
  theme(panel.background = element_blank(),
    panel.grid.major.x = element_line(color = "#BDC3C7"),
    text = element_text(family = "Georgia")) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))
```

![](treinta_dias_graficos_12_files/figure-html/unnamed-chunk-5-1.png)<!-- -->



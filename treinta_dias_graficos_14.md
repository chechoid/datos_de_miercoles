---
title: "30 días de gráficos"
output:
  html_document:
    df_print: paged
    keep_md: true
    include_graphics: yes
---

## Día 14: Treemap

Para este ejercicio vuelvo a trabajar con la [Encuesta de Sueldos de Sysarmy](https://sysarmy.com/blog/posts/resultados-de-la-encuesta-de-sueldos-2020-1/) para analizar la **Distribución de Género en el mercado de IT** de los 10 puestos con más observaciones de la Argentina.

### Preprocesamiento

Las librerías que usé son:

```r
library(googlesheets4)
library(gargle)
library(tidyverse)
library(treemap)
library(RColorBrewer)
```


Los datos los cargo con este código:


```r
gs4_deauth()
options(scipen = 999) # Cambia la notación científica de los gráficos
encuesta_sysarmy <- sheets_read("1_db6zEAMvr-1GQjJb4hV-rSQfJ9w6GmezbqKJ2JJn7I", skip = 9)

analisis_r30 <- encuesta_sysarmy %>%
  select('Me identifico', 'Trabajo de', `Salario mensual BRUTO (en tu moneda local)`)%>%
  rename(Genero = 'Me identifico', 
         Puesto = 'Trabajo de',
         Sueldo_Bruto = `Salario mensual BRUTO (en tu moneda local)`)
```

Luego genero un vector con los 10 principales puestos con más observaciones:


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

### Gráfico

Finalmente genero el gráfico, eliminando los registros donde el género es igual a *"Otros"* porque no hay muchas observaciones registradas para todos los puestos.

La guía para hacer este gráfico la saqué de [The R Graph Gallery](https://www.r-graph-gallery.com/236-custom-your-treemap).

```r
analisis_r30 %>%
  filter(Puesto %in% top_10_puestos, Genero != "Otros") %>%
  select(Puesto, Genero, Sueldo_Bruto) %>%
  group_by(Puesto, Genero) %>%
  tally(sort = TRUE) %>%
  treemap(index = c("Puesto", "Genero"),
        vSize = "n",
        type = "index",
        fontsize.labels=c(15,12), 
        fontcolor.labels=c("black","white"),    
        fontface.labels=c(2,1),   
        bg.labels=c("transparent"), 
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ),  
        overlap.labels=0.5, 
        palette = "PuOr", 
        inflate.labels=F,
        title = "Distribución de género en IT por puesto")
```

```
## Warning in if (class(try(col2rgb(bg.labels), silent = TRUE)) == "try-error")
## stop("Invalid bg.labels"): la condición tiene longitud > 1 y sólo el primer
## elemento será usado
```

![](treinta_dias_graficos_14_files/figure-html/unnamed-chunk-4-1.png)<!-- -->



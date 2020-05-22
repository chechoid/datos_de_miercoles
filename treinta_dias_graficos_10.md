---
title: "30 días de gráficos - Día 10"
output:
  html_document:
    df_print: paged
    keep_md: true
include_graphics: yes
---
# 30 días de gráficos - Día 10: Paletas

## Librerías

Para el desafío del día de hoy voy a usar las librerías *ggthemes* y *paletter* de acuerdo al instructivo del [RUG de Milán]("http://www.milanor.net/blog/build-color-palette-from-image-with-paletter/").


```r
library(googlesheets4)
library(gargle)
library(tidyverse)
library(ggthemes)
library(jpeg)
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

Luego, filtro los 10 puestos que tienen más contribuciones para el segundo gráfico.

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

# Gráfico 1 - Color blindness

Para el primer gráfico me inspiré en el gráfico de @violetrzn de su cuenta de [twitter]("https://twitter.com/violetrzn/status/1263383516374499329?s=20"). Voy a tomar los 3 puestos con mayor cantidad de observaciones.


```r
analisis_puestos <- analisis_r30 %>%
  filter(Puesto %in% c("Developer", "SysAdmin / DevOps / SRE", "Technical Leader"),
         between(Sueldo_Bruto, 20000, 1000000))
```


```r
ggplot(analisis_puestos, aes(x = Puesto, y = Sueldo_Bruto, color = Genero)) +
  geom_point(position = "jitter", alpha = 0.5, size = 2) +
  scale_color_colorblind() +
  labs(title = "Sueldos brutos por puesto",
       subtitle = "Fuente: Encuesta de Sueldos de Sysarmy 2020.1",
       caption = "#30diasdegraficos #rstats_es",
       x = "", y = "")+
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))
```

![](treinta_dias_graficos_10_files/figure-html/unnamed-chunk-6-1.png)<!-- -->



## Gráfico 2 - Creando una paleta de colores

Para este segundo gráfico, la idea la tomé del post de @AnguloBrunet en su cuenta de [twitter]("https://twitter.com/AnguloBrunet/status/1263365115660898304?s=20"). Como no me fui a Tailandia, voy a usar una foto de lo más lindo del mundo que es mi hija.


```r
foto <- readJPEG("treinta_dias_graficos_files/beauty.jpg")
```


Preparando la paleta con k-means



```r
foto_dim <- dim(foto)
foto_dim
```

```
## [1] 4160 3120    3
```

```r
foto_rgb <- data.frame(
  x = rep(1:foto_dim[2], each = foto_dim[1]),
  y = rep(foto_dim[1]:1, foto_dim[2]),
  R = as.vector(foto[,,1]), #slicing our array into three
  G = as.vector(foto[,,2]),
  B = as.vector(foto[,,3]))

head(foto_rgb)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["x"],"name":[1],"type":["int"],"align":["right"]},{"label":["y"],"name":[2],"type":["int"],"align":["right"]},{"label":["R"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["G"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["B"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"4160","3":"0.7921569","4":"0.9843137","5":"1.0000000","_rn_":"1"},{"1":"1","2":"4159","3":"0.7843137","4":"0.9764706","5":"1.0000000","_rn_":"2"},{"1":"1","2":"4158","3":"0.7686275","4":"0.9607843","5":"0.9882353","_rn_":"3"},{"1":"1","2":"4157","3":"0.7607843","4":"0.9529412","5":"0.9803922","_rn_":"4"},{"1":"1","2":"4156","3":"0.7607843","4":"0.9529412","5":"0.9803922","_rn_":"5"},{"1":"1","2":"4155","3":"0.7764706","4":"0.9686275","5":"0.9960784","_rn_":"6"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
foto_kmeans <- kmeans(foto_rgb[,c("R","G","B")], centers = 20, iter.max = 30)

str(foto_kmeans)
```

```
## List of 9
##  $ cluster     : int [1:12979200] 1 1 1 1 1 1 1 1 1 1 ...
##  $ centers     : num [1:20, 1:3] 0.702 0.234 0.641 0.861 0.531 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:20] "1" "2" "3" "4" ...
##   .. ..$ : chr [1:3] "R" "G" "B"
##  $ totss       : num 1481947
##  $ withinss    : num [1:20] 2678 3240 2905 8027 7282 ...
##  $ tot.withinss: num 94277
##  $ betweenss   : num 1387671
##  $ size        : int [1:20] 226693 188998 227611 444790 1526296 1219674 99135 180639 740859 1066262 ...
##  $ iter        : int 10
##  $ ifault      : int 0
##  - attr(*, "class")= chr "kmeans"
```

```r
# La paleta de colores de la foto de mi hija
show_col(rgb(foto_kmeans$centers))
```

![](treinta_dias_graficos_10_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


Y ahora el gráfico con la paleta de colores de la foto de mi hija:


```r
analisis_r30 %>%
  filter(Puesto %in% top_10_puestos & Genero != "Otros") %>%
  mutate(Genero = factor(Genero, levels = c("Hombre", "Mujer"))) %>%
  group_by(Puesto, Genero) %>%
  summarise(Sueldo_Promedio = mean(Sueldo_Bruto)) %>%
  ungroup() %>% 
  ggplot(aes(x = reorder(Puesto, Sueldo_Promedio), y = Sueldo_Promedio, fill = Puesto))+
  geom_col(position = "dodge") +
  scale_fill_manual(values = rgb(foto_kmeans$centers))+
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))+
  coord_flip()+
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "#D7DBDD"),
        text = element_text(family = "Leelawadee UI"))+
  labs(title = "Sueldo bruto promedio por puesto y género (en AR$)",
       subtitle = "Fuente: Encuesta de Sueldos de Sysarmy",
      caption = "#30díasdegráficos",
       x = "",
       y = "")
```

![](treinta_dias_graficos_10_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


---
title: "30 días de gráficos"
output:
  html_document:
    df_print: paged
    keep_md: true
include_graphics: yes
---



## Día 13 - Series de tiempo

Para este gráfico usé los datos de la **Encuesta de Indicadores Laborales** publicado en la página de [Estudios y Estadísticas Laborales](http://www.trabajo.gob.ar/estadisticas/) del Ministerio de Trabajo, Empleo, y Seguridad Social de Argentina.

Tomé la Tabla 1.9, del Total de los 12 aglomerados de **Expectativas empresarias y puestos vacantes** y la pasé a Google Sheets.


### Librerías


```r
library(googlesheets4)
library(gargle)
library(lubridate)
library(tidyverse)
library(extrafont)

loadfonts()
```

### Carga de Datos y preprocesamiento

Luego de cargar los datos, transformé al dataframe de *"ancho"* a *"largo"* con pivot_longer, cambié el formato de la variable período a un formato fecha así podía calcular los índices promedios para cada trimestre.


```r
expectativas_laborales <- gs4_get("1HeFbgf0aubb5HBSFJTRzGCpW536O1cji7I6lgiNqvqg") %>%
  read_sheet()

exp_lab <- expectativas_laborales%>%
  rename(Expectativa = Período) %>%
  pivot_longer(-Expectativa, names_to = "Periodo", values_to = "Valor") %>%
  mutate(Periodo = dmy(Periodo),
         Trimestre = quarter(Periodo, with_year = TRUE, fiscal_start = 1),
         Expectativa = factor(Expectativa, levels = c("La dotación aumentará",
                                                      "La dotación disminuirá",
                                                      "La dotación se mantendrá"),
                              labels = c("Aumentará", "Disminuirá", "Sin Cambios"))) %>%
  filter(Trimestre > 2013.04) %>%
  group_by(Trimestre) %>%
  summarise(Exp_Aumento = mean(Valor[Expectativa== "Aumentará"]),
            Exp_Disminuye = mean(Valor[Expectativa== "Disminuirá"]),
            Exp_Igual = mean(Valor[Expectativa == "Sin Cambios"]))
```

Nuevamente hice una transformacion de las nuevas columnas un formato *"largo"*.


```r
exp_empresaria <- exp_lab %>%
  pivot_longer(-Trimestre, names_to = "Expectativa", values_to = "Valor") %>%
  mutate(Expectativa = factor(Expectativa, levels = c("Exp_Aumento",
                                                      "Exp_Disminuye",
                                                      "Exp_Igual"),
                              labels = c("Aumentará", "Disminuirá", "Sin Cambios")))
```

Y luego hice dos gráficos: uno mostrando los tres indicadores, y luego sólo enfocando en las expectativas de aumento y de disminución de contratación, marcando los trimestres que hubo cambio de gobierno nacional.


```r
ggplot(exp_empresaria, aes(x = Trimestre, y = Valor,  color = Expectativa)) +
  geom_line(size = 1)+
  scale_color_manual(values = c("#2980B9", "#E67E22", "#BDC3C7"))+
  geom_smooth(se=FALSE) +
  labs(title = "Promedio de Expectativas Empresarias y Puestos Vacantes por trimestre",
       subtitle = "Fuente: Encuesta de Índicadores Laborales",
       caption = "#30diasdegraficos #RStats_ES",
       x = "Trimestre", y = "Valor (porcentaje)") +
  theme(panel.grid = element_blank(),
      panel.grid.major.y = element_line(color = "#D7DBDD"),
      panel.grid.minor.y = element_line(color = "#D7DBDD"),
      panel.background = element_blank(),
      text = element_text(family = "Lucida Sans Typewriter"))
```

![](treinta_dias_graficos_13_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
ggplot(exp_empresaria, aes(x = Trimestre, y = Valor,  color = Expectativa)) +
  geom_line(size = 1)+
  scale_color_manual(values = c("#2980B9", "#E67E22", "#BDC3C7"))+
  geom_point()+
  geom_smooth() +
  labs(title = "Promedio de Expectativas Empresarias y Puestos Vacantes por trimestre",
       subtitle = "Fuente: Encuesta de Índicadores Laborales",
       caption = "#30diasdegraficos #RStats_ES",
       x = "Trimestre", y = "Valor (porcentaje)") +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "#D7DBDD"),
        panel.grid.minor.y = element_line(color = "#D7DBDD"),
        panel.background = element_blank(),
        text = element_text(family = "Lucida Sans Typewriter")) +
  scale_y_continuous(limits = c(0,15))+
  geom_vline(aes(xintercept = 2015.4), linetype = 2, alpha = 0.3)+
  geom_vline(aes(xintercept = 2019.4), linetype = 2, alpha = 0.3)
```

![](treinta_dias_graficos_13_files/figure-html/unnamed-chunk-6-1.png)<!-- -->



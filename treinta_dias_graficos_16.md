30 días de gráficos
================

<br>

# Día 16: Waffle Charts

Nuevamente voy a jugar con los datos de la [Encuesta de sueldos de
Sysarmy](https://sysarmy.com/blog/posts/resultados-de-la-encuesta-de-sueldos-2020-1/)
para hacer los gráficos para visualizar el gap salarial entre mujeres y
hombres en el mercado IT.

Me costó un huevo hacer este gráfico, así que acepto sugerencias de
mejora. Como reconocido hater de los gráficos de torta, este tipo de
gráficos me parece una gran alternativa. Desde mi punto de vista hace
complejo interpretar el **valor** de los segmentos pero da una gran idea
del impacto de las diferencias.

Hice dos versiones de los mismos gráficos. Me apoyé en el repo del
desarrollador del [paquete
**waffle**](https://github.com/hrbrmstr/waffle) y también fueron muy
útiles el posteo de [Rubén
Bustillo](https://rpubs.com/rubenfbc/waffle_charts) y el repo de
@sporella en
[Github](https://github.com/sporella/nightingale/blob/master/dieciseis.R).

## Librerías

``` r
library(waffle)
library(googlesheets4)
library(gargle)
library(tidyverse)
library(extrafont)
library(scales)
library(kableExtra)
```

## Carga de datos y creación de un subset

``` r
gs4_deauth()
options(scipen = 999) # Cambia la notación científica de los gráficos
encuesta_sysarmy <- sheets_read("1_db6zEAMvr-1GQjJb4hV-rSQfJ9w6GmezbqKJ2JJn7I", skip = 9)

analisis_r30 <- encuesta_sysarmy %>%
  select('Me identifico', 'Trabajo de', `Salario mensual BRUTO (en tu moneda local)`)%>%
  rename(Genero = 'Me identifico', 
         Puesto = 'Trabajo de',
         Sueldo_Bruto = `Salario mensual BRUTO (en tu moneda local)`)

# Ver los 5 puestos principales ocupados por mujeres
puestos_muj <- analisis_r30 %>%
  filter(Genero == "Mujer") %>%
  group_by(Puesto) %>%
  tally(sort = TRUE) %>%
  top_n(5)

kable(puestos_muj, align = "c") %>%
  kable_styling("striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:center;">

Puesto

</th>

<th style="text-align:center;">

n

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:center;">

Developer

</td>

<td style="text-align:center;">

279

</td>

</tr>

<tr>

<td style="text-align:center;">

QA / Tester

</td>

<td style="text-align:center;">

82

</td>

</tr>

<tr>

<td style="text-align:center;">

BI Analyst / Data Analyst

</td>

<td style="text-align:center;">

48

</td>

</tr>

<tr>

<td style="text-align:center;">

Project Manager

</td>

<td style="text-align:center;">

36

</td>

</tr>

<tr>

<td style="text-align:center;">

Consultant

</td>

<td style="text-align:center;">

32

</td>

</tr>

</tbody>

</table>

Ya viendo cuales son los 5 puestos con más observaciones de mujeres,
creo un objeto para empezar la comparación de sueldos:

``` r
comparacion <- analisis_r30 %>%
  filter(Puesto %in% as.vector(puestos_muj$Puesto), 
         Sueldo_Bruto < 1000000,
         Genero != "Otros") 
```

## Transformación 1: ‘Iron’

Para el primer gráfico tuve que manipular los datos hasta poder crear
objetos con los valores del gap salarial de las mujeres respecto de los
hombres, y luego poder graficar esa diferencia.

``` r
gaps <- comparacion %>%
  mutate(Puesto = factor(Puesto),
         Genero = factor(Genero)) %>%
  group_by(Puesto, Genero) %>%
  summarise(Sueldo_Promedio = mean(Sueldo_Bruto)) %>%
  pivot_wider(names_from = Genero, values_from  = Sueldo_Promedio) %>%
  mutate(Gap = Mujer/Hombre*100) %>%
  ungroup()

kable(gaps, align = "c") %>%
  kable_styling("striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:center;">

Puesto

</th>

<th style="text-align:center;">

Hombre

</th>

<th style="text-align:center;">

Mujer

</th>

<th style="text-align:center;">

Gap

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:center;">

BI Analyst / Data Analyst

</td>

<td style="text-align:center;">

76646.68

</td>

<td style="text-align:center;">

62366.70

</td>

<td style="text-align:center;">

81.36909

</td>

</tr>

<tr>

<td style="text-align:center;">

Consultant

</td>

<td style="text-align:center;">

77781.56

</td>

<td style="text-align:center;">

77497.38

</td>

<td style="text-align:center;">

99.63464

</td>

</tr>

<tr>

<td style="text-align:center;">

Developer

</td>

<td style="text-align:center;">

84989.32

</td>

<td style="text-align:center;">

69472.78

</td>

<td style="text-align:center;">

81.74295

</td>

</tr>

<tr>

<td style="text-align:center;">

Project Manager

</td>

<td style="text-align:center;">

106375.78

</td>

<td style="text-align:center;">

89665.75

</td>

<td style="text-align:center;">

84.29151

</td>

</tr>

<tr>

<td style="text-align:center;">

QA / Tester

</td>

<td style="text-align:center;">

75466.38

</td>

<td style="text-align:center;">

67930.53

</td>

<td style="text-align:center;">

90.01430

</td>

</tr>

</tbody>

</table>

Creo un vector de colores y luego objetos con los valores de la
proporción salarial de las mujeres respecto de los hombres. En el único
puesto donde hay paridad es en rol de *Consultor*.

``` r
colores <- c("#88389B","#839192")

# Objetos creados con los resultados de los gaps salariales.
bi <- as.numeric(comma(as.vector(unlist(gaps[1,4])), accuracy = 2))
cons <- as.numeric(comma(as.vector(unlist(gaps[2,4])), accuracy = 2))
devs <- as.numeric(comma(as.vector(unlist(gaps[3,4])), accuracy = 2))
pm <- as.numeric(comma(as.vector(unlist(gaps[4,4])), accuracy = 2))
qa <- as.numeric(comma(as.vector(unlist(gaps[5,4])), accuracy = 2))
```

Y finalmente creo el gráfico usando la función **iron**:

``` r
iron(
waffle(c(Mujer = bi, Hombre = 100-bi) , rows = 5, color=colores, title = "Gap Salarial en IT \n BI / Data Analyst"),
waffle(c(Mujer = cons, Hombre = 100-cons) , rows = 5, color=colores, title = "Consultant"),
waffle(c(Mujer = devs, Hombre = 100-devs) , rows = 5, color=colores, title = "Developer"),
waffle(c(Mujer = pm, Hombre = 100-pm) , rows = 5, color=colores, title = "Project Manager"),
waffle(c(Mujer = qa, Hombre = 100-qa) , rows = 5, color=colores, title = "QA / Tester")
)
```

![](treinta_dias_graficos_16_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
#ggsave("dia16-1.png", plot = last_plot())
```

## Transformación 2: ggplot

Vi en varios repos que usan *ggplot* y *geom\_waffle* para hacer este
tipo de gráficos. Así que tuve que manosear un poco más el data frame
para lograr este objetivo, y *facetear* cada waffle chart por puesto.

Hice un bardo con la manipulación, y el gráfico no salió del todo como
esperaba, pero bueno, aquí va:

### Tranformación de los datos

``` r
gaps2 <- comparacion %>%
  mutate(Puesto = factor(Puesto),
         Genero = factor(Genero)) %>%
  group_by(Puesto, Genero) %>%
  summarise(Sueldo_Promedio = mean(Sueldo_Bruto)) %>%
  pivot_wider(names_from = Genero, values_from  = Sueldo_Promedio) %>%
  mutate(Gap = Mujer/Hombre*100,
         Diff = 100-Gap) %>%
  pivot_longer(-c("Puesto", "Hombre", "Mujer"), names_to="Gaps", values_to = "Diferencia") %>%
  mutate(Gap = fct_recode(Gaps,"Mujer" = "Gap", "Hombre" = "Diff")) %>%
  select(Puesto, Gap, Diferencia)

# Terminé con algo así
kable(gaps2, align = "c") %>%
   kable_styling("striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:center;">

Puesto

</th>

<th style="text-align:center;">

Gap

</th>

<th style="text-align:center;">

Diferencia

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:center;">

BI Analyst / Data Analyst

</td>

<td style="text-align:center;">

Mujer

</td>

<td style="text-align:center;">

81.369091

</td>

</tr>

<tr>

<td style="text-align:center;">

BI Analyst / Data Analyst

</td>

<td style="text-align:center;">

Hombre

</td>

<td style="text-align:center;">

18.630909

</td>

</tr>

<tr>

<td style="text-align:center;">

Consultant

</td>

<td style="text-align:center;">

Mujer

</td>

<td style="text-align:center;">

99.634638

</td>

</tr>

<tr>

<td style="text-align:center;">

Consultant

</td>

<td style="text-align:center;">

Hombre

</td>

<td style="text-align:center;">

0.365362

</td>

</tr>

<tr>

<td style="text-align:center;">

Developer

</td>

<td style="text-align:center;">

Mujer

</td>

<td style="text-align:center;">

81.742951

</td>

</tr>

<tr>

<td style="text-align:center;">

Developer

</td>

<td style="text-align:center;">

Hombre

</td>

<td style="text-align:center;">

18.257049

</td>

</tr>

<tr>

<td style="text-align:center;">

Project Manager

</td>

<td style="text-align:center;">

Mujer

</td>

<td style="text-align:center;">

84.291506

</td>

</tr>

<tr>

<td style="text-align:center;">

Project Manager

</td>

<td style="text-align:center;">

Hombre

</td>

<td style="text-align:center;">

15.708494

</td>

</tr>

<tr>

<td style="text-align:center;">

QA / Tester

</td>

<td style="text-align:center;">

Mujer

</td>

<td style="text-align:center;">

90.014301

</td>

</tr>

<tr>

<td style="text-align:center;">

QA / Tester

</td>

<td style="text-align:center;">

Hombre

</td>

<td style="text-align:center;">

9.985699

</td>

</tr>

</tbody>

</table>

Para el gráfico tuve que revertir el vector de colores.

``` r
colores <- c("#839192", "#88389B")

ggplot(gaps2, aes(fill = Gap, values = Diferencia))+
  geom_waffle(color="white",rows = 20, flip = TRUE, size = 0.2)+
  scale_fill_manual(values = colores)+
  facet_wrap(~Puesto, nrow = 1, strip.position = "bottom")+
  scale_y_continuous(labels = function(x) x * 20, # make this multiplyer the same as n_rows
                     expand = c(0,0))+
  scale_x_discrete() +
  coord_equal()+
  labs(title ="Gap Salarial en el Mercado IT",
       subtitle = "Fuente: Encuesta de Sueldos Sysarmy 2020.1",
       caption = "#30diasdegraficos #RStats_ES")+
  theme(text = element_text(family = "Ubuntu Mono"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom")
```

![](treinta_dias_graficos_16_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
#ggsave("dia16-2.png", plot = last_plot())
```

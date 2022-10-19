#'*PRACTICA 2 - HERRAMIENTAS DE PROGRAMACION R (Karla Rodriguez Vargas)*


library(janitor)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(purrr)

#' **Ejercicio 1** Crear el Dataframe y limpiar los nombres
df_mmm <- read_csv("C:/Users/karla/Documents/CUNEF/2_Herramientas_programacion/2_R/Ficheros/mktmix.csv")
df_mmm <- clean_names(df_mmm)

#' **Ejercicio 2** Identificarnum de columnas y filas, clases e informacaion 
#' que contiene
dim(df_mmm)
class(df_mmm$base_price)
class(df_mmm$discount)

# RESPUESTA - El num filas = 104 y num columnas = 9. La clase de base_price
# y de discount es un numerico. Basado en los datos presentes 
# en el dataframe, lo que se muestra es el numero de ventas de dicho
# producto, y cuales fueron los medios de publicidad los cuales
# pudieran tener un impacto en la cantidad de unidades vendidas.

#' **Ejercicio 3** Cambio de clase para columna newspaper_insert
unique(df_mmm$newspaper_inserts)

df_mmm <- df_mmm %>% 
  mutate(newspaper_inserts = if_else(is.na(df_mmm$newspaper_inserts)==TRUE, 0, 1))

#'**Ejercicio 4** Valores distintos en website_campaign
unique(df_mmm$website_campaign)

df_mmm <- df_mmm %>% 
  mutate(website_campaign = if_else(is.na(website_campaign)==TRUE,"0", website_campaign))

df_mmm <- df_mmm %>% 
  mutate(facebook = if_else(website_campaign == "Facebook", 1,0),
          twitter = if_else(website_campaign == "Twitter", 1,0),
          website_campaign2 = if_else(website_campaign == "Website Campaign",1,0)
          )


# RESPUESTA - En la columna de website existen 3 valores distintos: facebook,
# twitter y website campaing.

#' **Ejercicio 5** cuantas semanas ha habido campana para cada website campaign
sum(df_mmm$facebook)
sum(df_mmm$twitter)
sum(df_mmm$website_campaign2)

#RESPUESTA - Tanto para Facebook como Twitter las campañas duraron 4 semanas, en
# caso de Website Campaign, esta duro un total de 6 semanas

#' **Ejercicio 6** Semanas que se ha realizado una inversion en anun. televesion 
#' < 50
df_mmm %>% 
  select(tv) %>% 
  filter(tv < 50) %>% 
  summarise(n())

# RESPUESTA - Solo 3 semanas , de las 104 semanas en las que se realizo
# publicidad en al tv, tuvieron una inversion inferior a los 50 grp 

#' **Ejercicio 7** Media de inversion en TV
df_mmm %>% 
  select(tv,radio) %>%
  mutate(inversion_radio = if_else(radio == "NaN" | radio == 0, "No Inversion Radio", "Inversion Radio")) %>% 
  group_by(inversion_radio) %>%
  summarise(mean(tv))
 
#RESPUESTA- Cuando mas hubo inversion en tv fue cuando no hubo inversion en 
# radio con una media de 171 de inversion en tv. Cuando hubo inversion en radio
# la media de inversion en tv fue menor con 137 grp. Por tanto la media en los
# momentos que no hubo inversion fue mayor.

#' **Ejercicio 8** Grafico de evolucion en volumen de ventas
ggplot(df_mmm) +
  geom_line(aes(x = c(1:nrow(df_mmm)), y = new_vol_sales), color = "darkblue", size = 1)+
  ggtitle("Grafico temporal de volumen de ventas")

# RESPUESTA 8 - El volumen de las ventas a experimentado grandes variacioones
# a lo largo de las semanas. Siendoo la semana aprx. 20 y 75, donde se efectuaron
# la mayor cantidad de ventas. No obstante ultimamente se puede observar un 
# declive en la cantidad de ventas que se estan realizando en este producto en 
# determinado

#' **Ejercicio 9** Histograma y boxplot

df_mmm <- df_mmm %>% 
  mutate(new_vol_sales = as.numeric(new_vol_sales))

par(mfrow=c(1,2))
hist(x = df_mmm$new_vol_sales)
boxplot(x = df_mmm$new_vol_sales)
par(mfrow=c(1,1))

#RESPUESTA <- La media de la variable esta entre aprox. 19500 o 20000, a partir
# de ambas graficas, visualmente la media se ve muy cerca de la mediana. Por 
# las columnas existentes en el histograma se podria inferir que la media es
# ligeramente menor que la mediana.
 
# RESPUESTA 9 / PART 2 <- Luego de calculas la media y mediana para este conjunto
# de datos, se aprecia que la mediana (19943.5) es ligermente menor que la  
# media (20171). Este resultado es debido a los valores extremos que existen en  
# los datos y que tienen gran impacto en el resultado de la media pero no de la  
# mediana. Por tanto, la inferencia original de que la mediana era mas grande 
# que la media es incorrecta


#' **Ejercicio 10** Construccion de graficos radio, stout, tv

df_media <- df_mmm %>%
  select(tv, radio, stout) %>% 
  pivot_longer(everything()) %>% 
  na.omit(name)

ggplot(df_media)+
  geom_line( aes(x = (1:nrow(df_media)), y = value ,color=name), size = 1) +
  facet_grid(name ~ ., scales = "free_y")

# RESPUESTA - Donde mas inversion se ha realizado del producto es en la radio,
#  y aunque algunas semanas no hubo inversion en radio, fue uno de los medios
#  donde la inversion fue mas constante en relacion al monto que fue invertido.
#  En relacion a las pancartas, esta tubo la menor inversion de los medios 
#  comparados en la grafica pero se puede percibir un incremento a lo largo de las
#  semanas en inversion en este medio. Y por ultimo en relacion con la tv, fue un
#  medio el cual tubo una inversion que iba en aumento al inicio, pero a mediado
#  del lapso evaluado tuvo un declive el cual se puede notar que ha ido recuperando
#  en las ultimas semanas, con grandes variaciones en el monto invertido. Donde 
#  mas inversion se ha realizado es en la radio. 


#' **Ejercicio 11** Construccion grafico volumen de venta vs cant en almacen

ggplot(df_mmm, aes( y= new_vol_sales, x = in_store)) +
  geom_point() +
  geom_smooth()

# RESPUESTA 11 - A medida que la cantidad de productos en stock aumenta, la 
# cantidad de ventas realizadas TAMBIEN aumenta. Esto puede inferirse 
# que la  correlacion entre la cantidad ventas y al cantidad de productos in 
# stock tiene relacion positiva, aunque no directamente proporcional debido
# a que las ventas no aumentan a la misma proporcion de que aumenta la 
# cantidad de productos en almacen 

#' **Ejercicio 12** Construccion grafico en relacion a volumen 
# Volumen de ventas vs in_store (segregado por newspaper_insert)
ggplot(df_mmm) +
  geom_point(aes( y= new_vol_sales, x = in_store, col = as.factor(newspaper_inserts)))  

# Volumen de ventas vs in_store (segregado por tv)
ggplot(df_mmm) +
  geom_point(aes( y= new_vol_sales, x = in_store, col = tv, size = tv))

# RESPUESTA - La grafica de publicidad en el periodico no muestra gran 
# influencia entre las veces que se realizaron anuncios por este medio y una
# creciente cantidad de productos en venta. En relacion a la tv, si se puede
# aprecias una mayor dependencia entre los anuncios realizado en la tv y la
# cantidad de productos que fueron vendidos durante esa semana. Aunque la 
# correlacion no parezca de gran magnitud, se puede inferir que existe cierto
# grado de correlacion entre tv y num de ventas

#' **Ejercicio 13** Comapracion cant de ventas con aplicacion descuento
df_mmm %>%
  mutate(discount_yesno = if_else(discount > 0, TRUE, FALSE )) %>% 
  group_by(discount_yesno) %>% 
  summarise(base_price_av = mean(base_price)) %>% 
  ggplot()+
  geom_col( aes( x = discount_yesno, y = base_price_av, fill = discount_yesno ))

# RESPUESTA - la media del precio de venta cuando hubo descuento y cuanod no hubo
# descuento, rondo cerca del mismo resultado. 

#' **Ejercicio 14** Creacion de funcion de R cuadrado ajustado 
funcion_r <- function(media_use){
  df_auxiliar <- df_mmm %>% 
    select(all_of(media_use),new_vol_sales)
  
  my_model <- lm(df_mmm$new_vol_sales ~., data = df_auxiliar)
  R <- summary(my_model)$adj.r.square
  
  return (R)
  
}

funcion_r(c("radio", "tv"))

# RESPUESTA- la funcion indica un resultado final de 1% lo cual indica que el 
# modelo utilizando lso datos de radio y tv, no es muy bueno para explicar el 
# volumen de ventas realizado.

#' **Ejercicio 15** Comparacion de modelo con mas ajuste a la variable de cant. 
#' ventas

list_media <- list(
      c("base_price", "radio", "tv", "stout"), 
       c("base_price", "in_store", "discount", "radio", "tv", "stout"),
      
       c("in_store", "discount"))

map(list_media, funcion_r)

# RESPUESTA - El modelo que mas se ajusta para poder explicar el volumen de 
# ventas del producto a analiza es tomando en cuenta el: precio base,
# los produccetos en stock, el descuento, la publicidad en la radio, en la tv y
# a traves de pancartas.
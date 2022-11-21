## SEMINARIO DE FUENTES

## REALIZADO POR: PAULA MARTÍNEZ TERRADOS Y JAVIER SÁEZ GARCÍA


## ANTES DE HACER NADA, VAMOS A IMPORTAR LOS DATOS QUE NECESITAREMOS MÁS ADELANTE:

## Para importar los datos de Tasa de paro:
library(readr)
Tasa_de_Paro <- read_delim("input/data/Tasa_de_Paro.csv", 
                           delim = ";", escape_double = FALSE, 
                           col_types = cols(Total = col_double()), 
                           trim_ws = TRUE)

## Para importar los datos de suicidios
library(readr)
Suicidio <- read_delim("input/data/Suicidio.csv", 
                       delim = ";", escape_double = FALSE, 
                       col_types = cols(Total = col_double()), 
                       trim_ws = TRUE)



## Con estos datos ya importados, el objetivo de este seminario será responder a estas preguntas:

## 1. VARIACIÓN DE PARO/SUICIDIO RESPECTO AL SEXO POR AÑOS Y POR COMUNIDAD AUTONOMA

## 2. VARIACION DE SUICIDIOS/PARO RESPECTO A LOS AÑOS

## 3. VER SI EXISTE O NO RELACIÓN ENTRE ELLOS


## EN PRIMER LUGAR, NECESITAREMOS MODIFICAR EL SET DE DATOS DE TASA_DE_PARO PARA QUE NOS MUESTRE SÓLO LA INFORMACIÓN QUE NECESITAMOS

## Filtramos los datos de tasa_de_paro para que nos salgan sólo aquellos datos 
#que nos muestren HOMBRES Y MUJERES, AMBOS SEPARADOS, Y QUE NOS ELIMINE LA LÍNEA DE "AMBOS SEXOS"

library(dplyr)
Tasa_de_Paro <-
  Tasa_de_Paro %>%
  mutate(Sexo = factor(Sexo)) %>%
  filter(Sexo != "Ambos sexos") %>%
  droplevels()

## para ver qué niveles de sexo tiene ahora el set de datos
levels(Tasa_de_Paro$Sexo)
## sólo tendrá Hombres y Mujeres, porque hemos eliminado la categoría "ambos sexos" ya que no es objeto de estudio
## (comprobamos que la categoria "ambos sexos", ha desaparecido)
Tasa_de_Paro %>%
  filter(Sexo != "Ambos sexos") %>%
  levels() ## nos sale NULL porque ha desaparecido


## Asimismo, tendremos que filtrar la tasa de paro para que nos muestre sólo aquellos valores para los que la nacionalidad es "total"
## ya que no es objeto de estudio de nuestro seminario, distinguir el paro entre nacionalidades

Tasa_de_Paro <-
  Tasa_de_Paro %>%
  mutate(Nacionalidad = Tasa_de_Paro$Nacionalidad) %>%
  filter(Nacionalidad == "Total") %>%
  droplevels()

Tasa_de_Paro

## Para la gráfica de  variación del PARO RESPECTO A LOS AÑOS Y EL SEXO, necesitaremos tan sólo el total nacional para hombres y mujeres por año
## Para eso debemos crear un nuevo objeto (Paro_Total_Año), en el que tendremos que filtrar las Comunidades Autónomas por "total nacional"

Paro_Total_Año <-
  Tasa_de_Paro %>%
  mutate(`Comunidades y Ciudades Autónomas` = Tasa_de_Paro$`Comunidades y Ciudades Autónomas`) %>%
  filter(`Comunidades y Ciudades Autónomas` == "Total Nacional") %>%
  droplevels()

str(Paro_Total_Año)
str(Paro_Total_Año$Total)

## PARA EL PRIMER GRÁFICO:

library(ggplot2) 

ggplot(data = Paro_Total_Año, aes(x = Periodo, y = Total)) +
  geom_point(aes(colour = factor(Sexo))) +
  theme_light() +
  geom_smooth(method = "lm", aes(colour = factor(Sexo)))

#con scale_manual
ggplot(data = Paro_Total_Año, aes(x = Periodo, y = Total)) +
  geom_point(aes(colour = factor(Sexo))) +
  scale_size_manual(breaks = c('hombres', 'mujeres'), 
                    labels = c('hombres', 'mujeres'), 
                    values = c(2006:2022)) +
  theme_classic()  # para quitar los cuadrados del fondo de la gráfica

# (ambas son iguales, la única diferencia que la seguda de ellas no tiene regresión lineal(elegir la que más nos convenga al final))


## Por otro lado, PARA LA GRÁFICA DE PARO, RESPECTO A COMUNIDAD AUTONOMA/SEXO Y AÑOS necesitaremos:

## que nos quite de la tabla el Total Nacional, puesto que sólo queremos que nos indique el paro total de hombres y mujeres por cada comunidad autónoma 
## y por cada año

Tasa_de_Paro_CCAA <-
  Tasa_de_Paro %>%
  mutate(`Comunidades y Ciudades Autónomas` = Tasa_de_Paro$`Comunidades y Ciudades Autónomas`) %>%
  filter(`Comunidades y Ciudades Autónomas` != "Total Nacional" ) %>%
  droplevels()



## GRAFICA DE PARO POR COMUNIDAD AUTONOMA
library(ggplot2)


ggplot(data = Tasa_de_Paro_CCAA, aes(x = Periodo, y = Total)) +
  geom_point(aes(colour = factor(Sexo))) +
  facet_grid(Sexo ~ Tasa_de_Paro_CCAA$`Comunidades y Ciudades Autónomas`) +
  theme_light() + #quitamos el gris de fondo
  theme_classic()  #quitar los cuadraditos

# ---------------------------------------------------------------------------

## EN SEGUNDO LUGAR, NECESITAREMOS MODIFICAR EL SET DE DATOS DE SUICIDIO PARA 
## QUE NOS MUESTRE SÓLO LA INFORMACIÓN QUE NECESITAMOS


## Para ello necesitamos ver cuáles son las causas de muerte que hay en el set 
#de datos

library(dplyr)

levels(factor(Suicidio$`Causa de muerte (lista reducida)`))

## y con eso, filtramos para que sólo nos salgan aquellas CAUSAS DE MUERTE que 
#se corresponden con el SUICIDIO y/o lesiones autoinfligidas

Suicidio <-
  Suicidio %>%
  mutate(`Causa de muerte (lista reducida)` = Suicidio$`Causa de muerte (lista reducida)`) %>%
  filter(`Causa de muerte (lista reducida)` == "098 Suicidio y lesiones autoinfligidas" ) %>%
  droplevels()

Suicidio

## Y filtramos por sexo, para que NOS MUESTRE A HOMBRES Y MUJERES, AMBOS POR 
#SEPARADO, y nos elimine aquellos que indiquen 'Total' en sexo

Suicidio <-
  Suicidio %>%
  mutate( Sexo = Suicidio$Sexo) %>%
  filter(Sexo != "Total") %>%
  droplevels() 


Suicidio

## Necesitaremos tambien que nos filtre para cualquier lugar de ocurrencia, es 
#decir, necesitamos el número total de suicidios, no es objetivo del seminario estudiar dónde se produjo

Suicidio <-
  Suicidio %>%
  mutate(`Lugar de ocurrencia`= Suicidio$`Lugar de ocurrencia` ) %>%
  filter(`Lugar de ocurrencia` == "Total") %>%
  droplevels()

Suicidio

##AQUI ELIMINAMOS TODOS LOS NA YA QUE PUEDE DAR A CONFUSIONES

Suicidio <-
  na.omit(Suicidio)

Suicidio


## ASÍ OBTENDREMOS EL SET DE DATOS SUICIDIO, QUE NOS INDICA EL NUMERO DE 
#SUICIDIOS, POR SEXO Y POR COMUNIDAD AUTONOMA QUE SE PRODUJERON ENTRE 2017 Y 
#2020

library(ggplot2) 

#EN ESTE GRÁFICO PODEMOS OBSERVAR EL NºDE SUICIDIOS TOTALES POR CADA AÑO 
## (preguntarle a él, porque no sé cómo se hace)
Suicidio_Total <-
  Suicidio %>%
  group_by(año, Sexo)%>%
  summarise(MT = mean (Total),
            Year = unique(año),
            desviacion = sd(Total))
  
ggplot(data = Suicidio_Total, aes(x = Year, y = MT, fill = Sexo))+
  geom_bar( stat = "identity")+
  geom_errorbar(aes(ymin = MT- desviacion, ymax = MT + desviacion), width=.2,
  position=position_dodge(.9))
  

## (esto no muestra el suicidio total de hombres y mujeres por cada año) (esta no es la gráfica que buscamos)
ggplot(data = Suicidio, aes(x = año, y = Total)) +
  geom_point(aes(colour = factor(Sexo))) +
  theme_light() 



#EN ESTE GRÁFICO QUE VAMOS A REALIZAR PODEMOS OSBERVAR EL Nº DE SUICIDIOS PARA CADA HOMBRE Y MUJER EN CADA COMUNIDAD AUTÓNOMA A LO LARGO DE LOS AÑOS (DE 2017 A 2022)
library(ggplot2)

ggplot(data = Suicidio, aes(x = año, y = Total)) +
  geom_point(aes(colour = factor(Sexo))) +
  facet_grid(Suicidio$`Comunidades y Ciudades Autónomas` ~ Sexo) +
  theme_light() + #quitamos el gris de fondo
  theme_classic()  #quitar los cuadraditos



## ----------------------------------------------------------

## ESTO LO DEBERIAMOS USAR PARA COMPARAR AMBOS
## Filtramos los datos de tasa_de_paro para que nos salgan sólo aquellos AÑOS 
## MAYORES AL 2018 Y MENORES AL 2021 (2019, 2020)


## Para ello sacamos los años de Tasa_de_paro

as.numeric(levels(factor(Tasa_de_Paro$Periodo)))


## Y filtramos los datos para que nos salgan sólo aquellos que nos interesan

Tasa_de_Paro <-
  Tasa_de_Paro %>%
  mutate(Periodo = Tasa_de_Paro$Periodo) %>%
  filter(Periodo > 2016 & Periodo < 2021) %>%
  droplevels()


levels(factor(Tasa_de_Paro$Periodo))

## Y ASÍ OBTENDREMOS EL SET DE DATOS DE TASA_DE_PARO QUE NOS INDICA el paro, 
#por sexo y comunidad autónoma que hubo en los años 2006 y 2020


## A su vez, para que coincida en años con el set de datos de Tasa_de_paro, 
#filtraremos Suicidio para aquellos años que sean mayores que el 2016

## (como sólo estudia hasta 2020, saldrán 2017, 2018 2019 y 2020, al igual que en el 
#anterior set de datos)

Suicidio <-
  Suicidio %>%
  mutate(año = Suicidio$año) %>%
  filter(año > 2016 ) %>%
  droplevels()

#PARA VER SI HAY O NO RELACIÓN HACEMOS UN JOIN
#Pivotes -- años y Cuidades y Comunidades Autónomas

#primero hacemos que las dos columnas se llamen igual
Suicidio <-
  Suicidio %>%
  mutate(`Periodo`= Suicidio$`año`) #no m deja con rename, no se porque

#PROBLEMA, PERIODO ES NUM Y AÑO ES CHR

Suicidio_Paro <-  
  Tasa_de_Paro_CCAA %>% 
  select('Sexo':'Total') %>% 
  full_join(x = ., 
            y = Suicidio_Total %>% 
              select('Comunidades y Ciudades Autónomas', 'año':'Year'),
            by = c("Periodo" = "Year"))

str(Suicidio_Paro)

Suicidio_Paro <-
  na.omit(Suicidio_Paro)

Suicidio_Paro

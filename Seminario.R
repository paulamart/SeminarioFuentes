## SEMINARIO DE FUENTES DE DATOS BIOMEDICAS Y WEB SEMANTICA
## REALIZADO POR: PAULA MARTÍNEZ TERRADOS Y JAVIER SÁEZ GARCÍA


#* Importación de  Datos -------------------------------------------------------------------------
## ANTES DE HACER NADA, VAMOS A IMPORTAR LOS DATOS QUE NECESITAREMOS MÁS ADELANTE:

## Para importar los datos de TASA DE PARO:
library(readr)
Tasa_de_Paro <- read_delim("input/data/Tasa_de_Paro.csv", 
                           delim = ";", escape_double = FALSE, 
                           col_types = cols(Total = col_double()), 
                           trim_ws = TRUE)

## Para importar los datos de SUICIDIOS:
library(readr)
Suicidio <- read_delim("input/data/Suicidio.csv", 
                       delim = ";", escape_double = FALSE, 
                       col_types = cols(Total = col_double()), 
                       trim_ws = TRUE)


# * Cuestiones a Resolver--------------------------------------------------------------------------

## Con los datos ya importados, el objetivo de este seminario será responder a estas preguntas:

## 1. VARIACIÓN DE PARO/SUICIDIO RESPECTO AL SEXO POR AÑOS Y POR COMUNIDAD AUTONOMA

## 2. VARIACION DE SUICIDIOS/PARO RESPECTO A LOS AÑOS

## 3. VER SI EXISTE O NO RELACIÓN ENTRE ELLOS


# * Modificaciones TASA_DE_PARO--------------------------------------------------------------------

# ** Cambio en el nombre de las COMUNIDADES AUTÓNOMAS ------------------------------------------------
## Cambiamos el nombre de las comunidades y ciudades autónomas,principalmente para eliminar los números que las acompañan
## y añadimos los nuevos nombres en una columna nueva  del set de datosque llamaremos CCAA
library(dplyr)
Tasa_de_Paro <- Tasa_de_Paro%>%
  mutate(
    CCAA = case_when( ## cambiamos el nombre con un case_When
      Tasa_de_Paro$`Comunidades y Ciudades Autónomas`== "01 Andalucía" ~ "Andalucia",
      Tasa_de_Paro$`Comunidades y Ciudades Autónomas`== "02 Aragón" ~ "Aragon",
      Tasa_de_Paro$`Comunidades y Ciudades Autónomas`== "03 Asturias. Principado de" ~ "Asturias",
      Tasa_de_Paro$`Comunidades y Ciudades Autónomas`== "04 Balears. Illes" ~ "Baleares",
      Tasa_de_Paro$`Comunidades y Ciudades Autónomas`== "05 Canarias" ~ "Canarias",
      Tasa_de_Paro$`Comunidades y Ciudades Autónomas`== "06 Cantabria" ~ "Cantabria",
      Tasa_de_Paro$`Comunidades y Ciudades Autónomas`== "07 Castilla y León" ~ "CyL",
      Tasa_de_Paro$`Comunidades y Ciudades Autónomas`== "08 Castilla - La Mancha" ~ "Castilla la Mancha",
      Tasa_de_Paro$`Comunidades y Ciudades Autónomas`== "09 Cataluña" ~ "Cataluña",
      Tasa_de_Paro$`Comunidades y Ciudades Autónomas`== "10 Comunitat Valenciana" ~ "C.Valencia",
      Tasa_de_Paro$`Comunidades y Ciudades Autónomas`== "11 Extremadura" ~ "Extremadura",
      Tasa_de_Paro$`Comunidades y Ciudades Autónomas`== "12 Galicia" ~ "Galicia",
      Tasa_de_Paro$`Comunidades y Ciudades Autónomas`== "13 Madrid. Comunidad de" ~ "Madrid",
      Tasa_de_Paro$`Comunidades y Ciudades Autónomas`== "14 Murcia. Región de" ~ "Murcia",
      Tasa_de_Paro$`Comunidades y Ciudades Autónomas`== "15 Navarra. Comunidad Foral de" ~ "Navarra",
      Tasa_de_Paro$`Comunidades y Ciudades Autónomas`== "16 País Vasco" ~ "País Vasco",
      Tasa_de_Paro$`Comunidades y Ciudades Autónomas`== "17 Rioja. La" ~ "La Rioja",
      Tasa_de_Paro$`Comunidades y Ciudades Autónomas`== "18 Ceuta" ~ "Ceuta",
      Tasa_de_Paro$`Comunidades y Ciudades Autónomas`== "19 Melilla" ~ "Melilla",
    )) ## y definimos que los nombres "antiguos", van a tener un "nuevo" nombre

str(Tasa_de_Paro)


## ** Muestra de ambos sexos por separado---------------------------------------------------------

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


##** Muestra de Nacionalidad Total --------------------------------------------------------------------------------------------------

## Asimismo, tendremos que filtrar la tasa de paro para que nos muestre sólo aquellos valores para 
## los que la nacionalidad es "total", ya que no es objetivo de estudio de nuestro seminario, distinguir entre nacionalidades

Tasa_de_Paro <-
  Tasa_de_Paro %>%
  mutate(Nacionalidad = Tasa_de_Paro$Nacionalidad) %>%
  filter(Nacionalidad == "Total") %>%
  droplevels()

## ** PARO TOTAL por SEXO/AÑOS ----------------------------------------------------------------------------------------------------

## Para la gráfica de  variación del PARO RESPECTO A LOS AÑOS Y EL SEXO, necesitaremos tan sólo el total nacional 
## para hombres y mujeres por año.
## Para eso debemos crear un nuevo objeto (Paro_Total_Año), en el que tendremos que filtrar las Comunidades Autónomas por "total nacional"
## nos mostrará el paro total nacional por cada año

Paro_Total_Año <-
  Tasa_de_Paro %>%
  mutate(`Comunidades y Ciudades Autónomas` = Tasa_de_Paro$`Comunidades y Ciudades Autónomas`) %>%
  filter(`Comunidades y Ciudades Autónomas` == "Total Nacional") %>%
  droplevels()

str(Paro_Total_Año)
str(Paro_Total_Año$Total)

## *** Gráfica PARO TOTAL SEXO/AÑOS -----------------------------------------------------------------------------
## PARA EL PRIMER GRÁFICO:

library(ggplot2) 
Plot_Paro_Sexo_Años <- ## necesitamos guardarlo con un nombre determinado
  ggplot(data = Paro_Total_Año, aes(x = Periodo, y = Total)) +
  geom_point(aes(colour = factor(Sexo))) +
  theme_light() +
  geom_smooth(method = "lm", aes(colour = factor(Sexo))) +
  labs(
    x= "Años" ,
    y ="Tasa de Paro" , 
    title= "Paro respecto años/sexo"
  )+
  theme_classic()

Plot_Paro_Sexo_Años



## ** PARO por COMUNIDAD AUTÓNOMA ---------------------------------------------------------------------------------------

## Por otro lado, PARA LA GRÁFICA DE PARO, RESPECTO A COMUNIDAD AUTONOMA/SEXO Y AÑOS necesitaremos que nos quite 
## "Total Nacional", puesto que sólo queremos que nos indique el paro total de hombres y mujeres por cada 
## comunidad autónoma y por cada año

Tasa_de_Paro_CCAA <-
  Tasa_de_Paro %>%
  mutate(`CCAA` = Tasa_de_Paro$`CCAA`) %>%
  filter(`CCAA` != is.na(CCAA) ) %>%
  droplevels()



## *** Gráfica PARO POR COMUNIDAD AUTONOMA ------------------------------------------------------------------------------ 
library(ggplot2)

Plot_Paro_CCAA <- ## necesitamos guardarlo con un nombre determinado
  ggplot(data = Tasa_de_Paro_CCAA, aes(x = Periodo, y = Total)) +
  geom_point(aes(colour = factor(Sexo))) +
  facet_grid(. ~ Tasa_de_Paro_CCAA$`CCAA`) +
  geom_smooth(method = "lm", aes(colour = factor(Sexo))) +
  labs(
    x= "Años" ,
    y ="Tasa de paro" , 
    title= "Paro por Comunidad Autónoma"
  )+
  theme_light() + #quitamos el gris de fondo
  theme_classic() + #quitamos los cuadraditos
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

Plot_Paro_CCAA


# * Modificaciones SUICIDIO ------------------------------------------------------------------------------------------

## Para ello necesitamos ver cuáles son las causas de muerte que hay en el set 
#de datos

library(dplyr)
levels(factor(Suicidio$`Causa de muerte (lista reducida)`))


## ** Cambio del nombre de las COMUNIDADES AUTÓNOMAS -------------------------------------------------------------------------
## Modificamos el nombre de las comunidades autónomas, y los añadimos a una nueva columna, que llamaremos CCAA

Suicidio <- Suicidio%>%
  mutate(
    CCAA = case_when( ## cambiamos el nombre con case_when
      Suicidio$`Comunidades y Ciudades Autónomas`== "Andalucía" ~ "Andalucia",
      Suicidio$`Comunidades y Ciudades Autónomas`== "Aragón" ~ "Aragon",
      Suicidio$`Comunidades y Ciudades Autónomas`== "Asturias, Principado de" ~ "Asturias",
      Suicidio$`Comunidades y Ciudades Autónomas`== "Balears, Illes" ~ "Baleares",
      Suicidio$`Comunidades y Ciudades Autónomas`== "Canarias" ~ "Canarias",
      Suicidio$`Comunidades y Ciudades Autónomas`== "Cantabria" ~ "Cantabria",
      Suicidio$`Comunidades y Ciudades Autónomas`== "Castilla y León" ~ "CyL",
      Suicidio$`Comunidades y Ciudades Autónomas`== "Castilla-La Mancha" ~ "Castilla la Mancha",
      Suicidio$`Comunidades y Ciudades Autónomas`== "Cataluña" ~ "Cataluña",
      Suicidio$`Comunidades y Ciudades Autónomas`== "Comunitat Valenciana" ~ "C.Valencia",
      Suicidio$`Comunidades y Ciudades Autónomas`== "Extremadura" ~ "Extremadura",
      Suicidio$`Comunidades y Ciudades Autónomas`== "Galicia" ~ "Galicia",
      Suicidio$`Comunidades y Ciudades Autónomas`== "Madrid, Comunidad de" ~ "Madrid",
      Suicidio$`Comunidades y Ciudades Autónomas`== "Murcia, Región de" ~ "Murcia",
      Suicidio$`Comunidades y Ciudades Autónomas`== "Navarra, Comunidad Foral de" ~ "Navarra",
      Suicidio$`Comunidades y Ciudades Autónomas`== "País Vasco" ~ "País Vasco",
      Suicidio$`Comunidades y Ciudades Autónomas`== "Rioja, La" ~ "La Rioja",
      Suicidio$`Comunidades y Ciudades Autónomas`== "Ceuta" ~ "Ceuta",
      Suicidio$`Comunidades y Ciudades Autónomas`== "Melilla" ~ "Melilla",
    )) ## y le damos un "nuevo" nombre a cada comunidad


## ** Muestra de causa de muerte por Suicidio --------------------------------------------------------------------------------

## Filtramos para que sólo nos salgan aquellas CAUSAS DE MUERTE que 
## se corresponden con el SUICIDIO y/o lesiones autoinfligidas

Suicidio <-
  Suicidio %>%
  mutate(`Causa de muerte (lista reducida)` = Suicidio$`Causa de muerte (lista reducida)`) %>%
  filter(`Causa de muerte (lista reducida)` == "098 Suicidio y lesiones autoinfligidas" ) %>%
  droplevels()


## ** Muestra de ambos sexos por separado -----------------------------------------------------------------------------
## Y filtramos por sexo, para que NOS MUESTRE A HOMBRES Y MUJERES, AMBOS POR 
#SEPARADO, y nos elimine aquellos que indiquen 'Total' en sexo

Suicidio <-
  Suicidio %>%
  mutate( Sexo = Suicidio$Sexo) %>%
  filter(Sexo != "Total") %>%
  droplevels() 



## ** Muestra de Lugar de ocurrencia Total -----------------------------------------------------------------------------
## Necesitaremos tambien que nos filtre para cualquier lugar de ocurrencia, es 
#decir, necesitamos el número total de suicidios, no es objetivo del seminario estudiar dónde se produjo

Suicidio <-
  Suicidio %>%
  mutate(`Lugar de ocurrencia`= Suicidio$`Lugar de ocurrencia` ) %>%
  filter(`Lugar de ocurrencia` == "Total") %>%
  droplevels()

#str(factor(Suicidio$CCAA))

## ** Eliminación de datos NA ---------------------------------------------------------------------------------------
## AQUI ELIMINAMOS TODOS LOS DATOS QUE DEN COMO RESULTADO "NA", YA QUE PUEDE DAR A CONFUSIONES posteriormente

Suicidio <-
  na.omit(Suicidio)


## ** SUICIDIO TOTAL POR SEXO ------------------------------------------------------------------

## Creamos un nuevo objeto (Suicidio_Total), en el que incluimos la media del total de suicidios que se han 
## producido en cada año, y la desviación, respecto de la media, del numero total de suicidios
Suicidio_Total <-
  Suicidio %>%
  group_by(año, Sexo)%>%
  summarise(MT = mean (Total),
            Year = unique(año),
            desviacion = sd(Total))

### *** Gráfica SUICIDIO TOTAL POR CADA SEXO/AÑO --------------------------------------------------------------------

library(ggplot2)
Plot_Suicidio_Total_Sexo_Año <-  
  ggplot(data = Suicidio_Total, aes(x = Year, y = MT, fill = Sexo))+
  geom_bar( stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = MT- desviacion, ymax = MT + desviacion), width=.2+
  position = position_dodge(.9))+
  labs(
    x= "Años" ,
    y ="Número de suicidios" , 
    title= "Suicidio total por año/sexo")   

Plot_Suicidio_Total_Sexo_Año


### *** Gráfica SUICIDIO POR COMUNIDAD AUTÓNOMA -----------------------------------------------------------------------

# EN ESTE GRÁFICO QUE VAMOS A REALIZAR PODEMOS OSBERVAR EL Nº DE SUICIDIOS PARA CADA HOMBRE Y MUJER EN CADA 
# COMUNIDAD AUTÓNOMA A LO LARGO DE LOS AÑOS (DE 2017 A 2022)

library(ggplot2)
Plot_Suicidio_CCAA <-
  ggplot(data = Suicidio, aes(x = año, y = Total)) +
  geom_point(aes(colour = factor(Sexo))) +
  facet_grid(. ~ Suicidio$`CCAA`)+
  geom_smooth(method = "lm", aes(colour = factor(Sexo))) +
  labs(
    x= "Años" ,
    y ="Número de suicidios" , 
    title= "Suicidio por Comunidad Autónoma"
  ) + 
  theme_light() + #quitamos el gris de fondo
  theme_classic() + #quitar los cuadraditos
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
Plot_Suicidio_CCAA


### * RELACIÓN ENTRE PARO Y SUICIDIO ------------------------------------------------------------------------------------

## Filtramos los datos de tasa_de_paro para que nos salgan sólo aquellos AÑOS 
## MAYORES AL 2018 Y MENORES AL 2021 (2019, 2020)

## Para ello sacamos los años de Tasa_de_paro
as.numeric(levels(factor(Tasa_de_Paro$Periodo)))

## Y filtramos los datos para que nos salgan sólo aquellos que nos interesan
Paro_JOIN <-
  Tasa_de_Paro %>%
  mutate(Periodo = Tasa_de_Paro$Periodo) %>%
  filter(Periodo > 2016 & Periodo < 2021) %>%
  droplevels()

levels(factor(Paro_JOIN$Periodo))

## filtramos Paro_JOIN para que nos quite aquellas filas de la columna CCAA que tienen "NA"
Paro_JOIN <-
  Paro_JOIN %>%
  mutate(CCAA = Paro_JOIN$CCAA) %>%
  filter(CCAA != is.na(CCAA)) %>%
  droplevels()


## A su vez, para que coincida en años con el set de datos de Tasa_de_paro, 
# filtraremos Suicidio para aquellos años que sean mayores que el 2016

## (como sólo estudia hasta 2020, saldrán 2017, 2018 2019 y 2020, al igual que en el 
#anterior set de datos)

Suicidio_JOIN <-
  Suicidio %>%
  mutate(año = Suicidio$año) %>%
  filter(año > 2016 ) %>%
  droplevels()

## ** JOIN entre PARO Y SUICIDIO ----------------------------------------------------------------------------------------

#PREPARAMOS LAS TABLAS PARA EL JOIN
# PARA VER SI HAY O NO RELACIÓN HACEMOS UN JOIN
# Pivotes -- años y Cuidades y Comunidades Autónomas

# primero hacemos que las dos columnas se llamen igual
Suicidio_JOIN <-
  Suicidio %>%
  rename(`Periodo`=`año`, `TotalSuicidio`=`Total`) #año pasa a llamarse periodo en el set de datos de suicidio_JOIN


#PROBLEMA, PERIODO ES NUM Y AÑO ES CHR

str(Paro_JOIN)
str(Suicidio_JOIN)


Paro_JOIN
Suicidio_JOIN


#LO UNIMOS

Suicidio_Paro <-  
Paro_JOIN %>% 
  select(c("Sexo","CCAA", "Periodo":"Total")) %>%
  full_join(x = ., 
        y = Suicidio_JOIN %>% 
          select(c("CCAA", "Sexo", "Periodo":"TotalSuicidio")),
        by = c("Periodo", "CCAA", "Sexo") ) 

str(Suicidio_Paro)



# Relación visual entre el número de suicidios y la tasa de paro
library(ggplot2)
Plot_JOIN <-
  ggplot(data = Suicidio_Paro, mapping = aes(x = Total, y = TotalSuicidio)) +
  geom_point(na.rm = TRUE)+
  geom_smooth()+
  labs(
    x= "Tasa de Paro" ,
    y ="Número de Suicidios" , 
    title= "Correlación entre paro y suicidio"
  ) +
  theme_classic()

Plot_JOIN


#* Importación mediante Json-------------------------

#** Datos de Tasa de Paro ----------------------------
library(tidyjson)
library(rjson)
library(tidyverse)

#importamos el set de datos mediante JSON
ParoJSON <- fromJSON(file = "input/data/Paro.json")

#Distribuimos los objetos JSON en forma de nuevas columnas (es decir, colocamos los predicados como columnas)
ParoJSON %>% 
  spread_all() %>%
  str()

# Estudiamos el tipo de datos que es cada variable
ParoJSON %>% 
  gather_object %>% 
  json_types %>% 
  count(name, type)

## Y estudiamos las variables que tienen el tipo array

# Seleccionamos aquellas columnas que nos interesen de la variable DATA
Paro1_JSON <-
ParoJSON %>%
  enter_object(Data) %>%
  gather_array %>%
  spread_all %>%
  select(Anyo:Valor, document.id)

# Y hacemos lo mismo con la variable MEGADATA
Paro2_JSON<-
ParoJSON %>%
  enter_object(MetaData) %>%
  gather_array %>%
  spread_all%>%
  select(Nombre, document.id) 
  #filter(Nombre = c("Ambos sexos","Hombres", "Mujeres"))
  


#Luego Hacemos EL JOIN entre las columnas que nos interesan de DATA y MEGADATA para conseguir el set de datos adecuado para trabajar

JOIN_PARO_JSON <-
Paro1_JSON %>% 
  select(c("Anyo", "Valor", "document.id")) %>%
  full_join(x = ., 
            y = Paro2_JSON %>% 
              select(c("Nombre", "document.id")),
            by = c("document.id") ) 


#FILTRAMOS POR AÑOS
JOIN_PARO_JSON <-
  JOIN_PARO_JSON %>%
  filter(Anyo>2016)


# e intentamos FILTRAMOS POR AMBOS SEXO
JOIN_PARO_JSON <-
  JOIN_PARO_JSON %>%
  
  #filter(Nombre == "Hombres") %>%
  #filter(Nombre == "Mujeres")
  
  #filter(Nombre == "Ambos sexos") %>%
  #filter(Nombre == "Extranjera: Unión Europea") %>%
  #filter(Nombre == "Tasa de paro de la población")

#** Datos de Suicidio --------------------------------

library(tidyjson)
library(rjson)

# Importamos los datos desde JSON
SuicidioJSON <- fromJSON(file = "input/data/suicidios.json")

#Distribuimos los objetos JSON en forma de nuevas columnas (es decir, colocamos los predicados como columnas)
SuicidioJSON %>%
  spread_all() %>%
  str()

# Estudiamos el tipo de datos que es cada variable
library(dplyr)
SuicidioJSON %>% 
  gather_object %>% 
  json_types %>% 
  count(name, type)

## Y estudiamos las variables que tienen el tipo array

# Seleccionamos aquellas columnas que nos interesen de la variable DATA
library(tidyverse)
Suicidio1_JSON <-
  SuicidioJSON %>%
    enter_object(Data) %>%
    gather_array %>%
    spread_all %>%
    select("Valor","document.id")

# Y hacemos lo mismo con la variable MEGADATA
Suicidio2_JSON <-
  SuicidioJSON %>%
    enter_object(MetaData) %>%
    gather_array %>%
    spread_all%>%
    select("Nombre", "document.id")



#Luego Hacemos EL JOIN entre las columnas que nos interesan de DATA y MEGADATA para conseguir el set de datos adecuado para trabajar

JOIN_SUICIDIO_JSON <-
  Suicidio1_JSON %>% 
  select(c("Valor", "document.id")) %>%
  full_join(x = ., 
            y = Suicidio2_JSON %>% 
              select(c("Nombre", "document.id")),
            by = c("document.id") ) 


#e intentamos filtrar POR SEXO
JOIN_SUICIDIO_JSON <-
  JOIN_SUICIDIO_JSON %>%
  filter()
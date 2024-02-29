# *********
# Conexión con la BD
# *********

# Instalar y cargar la libreria RSQLite
install.packages("RSQLite")
library(RSQLite)

# Conectar a la BD
BD <- dbConnect(RSQLite::SQLite(), "BD_ADRES.db")

# Listar las tablas disponibles en la BD
dbListTables(BD)


# *********
# Librerias requeridas
# *********

# manipulación de datos espaciales
library(raster) 
library(sf)
library(ggspatial)

# manipulación de datos
library(ggplot2)
library(ggrepel)
library(tidyverse)


# *********
# Consultas a la BD
# *********


# Consulta 1: Número total de Prestadores por departamento
# ----

C1 <- c("SELECT Cod_Dep, depa_nombre As Departamento, count(codigo_habilitacion) As Total_Prestadores
          FROM Prestadores
          Group By Cod_Dep
          Order by Total_Prestadores DESC;")

# Se ejecuta la consulta 1 (C1) en la BD y el resultado se almacena en la variable RptaC1
RptaC1 <- dbGetQuery(BD,C1)
RptaC1

# Mostrar el resultado en un mapa de calor
# Cargar una cobertura shp de departamentos de Colombia
dptos <- st_read("Departamentos.shp")

# Visualizar la cobertura
plot(dptos)

# Unir el resultado de la consulta C1 con la tabla de atributos del shp usando merge
dptos2 <- merge(dptos, RptaC1, by.x=c('Cod_Dpto'), by.y=c('Cod_Dep'))

# Generar mapa de calor según el núméro de prestadores por departamento
ggplot(data=dptos2) +
  geom_sf(aes(fill = Total_Prestadores)) +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Prestadores de servicio por departamento") +
  scale_fill_gradientn(colours = rev(grDevices::hcl.colors(15,"Purple-Orange")), 
                       breaks = c(500,3500,6500,9500,12500,15000), name = "Total Prestadores") +
  geom_sf_text(aes(label = Departamento), colour = "black", size=3) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         height = unit(1.5, "cm"), width = unit(1.5, "cm"), 
                         pad_x = unit(2, "cm"), pad_y = unit(2, "cm"))  +
  theme_bw()


# ----
# Consulta 2: Número total prestadores por Municipio
# ----

C2 <- c("SELECT  depa_nombre As Departamento, muni_nombre As Municipio, count(Cod_Muni) As Total_Prestadores
        FROM Prestadores
        Group By Cod_Muni
        Order by Total_Prestadores DESC;")

# Se ejecuta la consulta 2 (C2) en la BD y el resultado se almacena en la variable RptaC2
RptaC2 <- dbGetQuery(BD,C2)
RptaC2

# Diagrama de barras los 10 municipios con más prestadores 
RptaC2 %>% head(n = 10) %>% 
ggplot(aes(x = reorder(Municipio,Total_Prestadores), y = Total_Prestadores)) + 
  geom_bar(stat = "identity", color="lightskyblue", fill="lightskyblue") +
  labs(title = "Los 10 municipios con mayor número de prestadores",
       x ="Municipio",
       y = "Núm Prestadores")

# Tabla resumen de número de prestadores por municipio
x <-RptaC2 %>%  group_by(Total_Prestadores) %>% 
  summarise(Num_Municipios = n())


# ----
# Consulta 3: Total por tipo de prestador
# ----

C3 <- c("SELECT clpr_nombre As Tipo_Prestador, COUNT(clpr_codigo) As Total
          FROM Prestadores
          Group By clpr_nombre;")

# Se ejecuta la consulta 3 (C3) en la BD y el resultado se almacena en la variable RptaC3
RptaC3 <- dbGetQuery(BD,C3)
RptaC3

# Calcular el porcentaje de cada tipo
RptaC3$Porcentaje <- round((RptaC3$Total*100)/60946,2)

# Diagrama de pastel que muestra el porcentaje de participacion de cada tipo de prestador
ggplot(RptaC3,aes(x="",y=Porcentaje, fill=Tipo_Prestador))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=Porcentaje),
            position=position_stack(vjust=0.5),color="white",size=5) +
  coord_polar(theta = "y") +
  scale_fill_manual(values=c("salmon","steelblue","orange","gray")) +
  theme_void() +
  guides(fill = guide_legend(title = "Tipo de Prestador")) +
  labs(title="Participación según Tipo de Prestador")


# ----
# Consulta 4: Total según caracter del prestador
#     NOTA: este atributo no está disponible para todos los registros
# ----

C4 <- c("SELECT caracter As Caracter, COUNT(caracter) As Total
          FROM Prestadores
          WHERE caracter IN ('DEPARTAMENTAL','NACIONAL','MUNICIPAL','DISTRITAL','INDIGENA')
          Group By caracter;")

# Se ejecuta la consulta 4 (C4) en la BD y el resultado se almacena en la variable RptaC4
RptaC4 <- dbGetQuery(BD,C4)
RptaC4

# Diagrama de barras los 10 municipios con más prestadores 
ggplot(RptaC4, aes(x = reorder(Caracter,Total), y = Total)) + 
  geom_bar(stat = "identity", color="seagreen4", fill="seagreen4") +
  labs(title = "Prestadores según su caracter",
       x ="Caracter",
       y = "Núm Prestadores")


# ----
# Consulta 5: Número total de radicaciones de Prestadores por año
# ----

C5 <- c("SELECT DISTINCT(substring(fecha_radicacion,1,4)) As Año, count(substring(fecha_radicacion,1,4)) As Total
          FROM Prestadores
          Group By Año
          Order by Año DESC;")

# Se ejecuta la consulta 5 (C5) en la BD y el resultado se almacena en la variable RptaC5
RptaC5 <- dbGetQuery(BD,C5)
RptaC5

# Diagrama de líneas que muestra la evolución del número de radicaciones por año
ggplot(RptaC5, aes(x=Año, y=Total)) +
  geom_point(shape=21, fill="white", colour="blue") +
  geom_line(group=1, color="orange")+
  labs(title = "Número total de radicaciones de Prestadores por año",
       y = "Total", x= "Año") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # gira las etiquetas 90 grados


# ----
# Consulta 6: Número total de vencimientos de Prestadores por año
# ----

C6 <- c("SELECT DISTINCT(substring(fecha_vencimiento,1,4)) As Año, count(substring(fecha_vencimiento,1,4)) As Total
          FROM Prestadores
          Group By Año
          Order by Año;")

# Se ejecuta la consulta 6 (C6) en la BD y el resultado se almacena en la variable RptaC6
RptaC6 <- dbGetQuery(BD,C6)
RptaC6

# Diagrama de líneas 
ggplot(RptaC6, aes(x=Año, y=Total)) +
  geom_point(shape=21, fill="white", colour="red") +
  geom_line(group=1, color="royalblue")+
  labs(title = "Número total de vencimientos de Prestadores por año",
       y = "Total", x= "Año") 


# ----
# Consulta 7: Total de Prestadores según su naturaleza y tipo de persona
# ----

C7 <- c("SELECT DISTINCT clase_persona As Persona, naju_nombre As Tipo, count(naju_nombre) As Total
          FROM Prestadores
          Group By Persona, Tipo;")

# Se ejecuta la consulta 7 (C7) en la BD y el resultado se almacena en la variable RptaC7
RptaC7 <- dbGetQuery(BD,C7)
RptaC7


# ----
# Consulta 8: Número total de prestadores por Region
# ----

C8 <- c("SELECT Municipios.Region As Region, count(Prestadores.Cod_Muni) As Total_Prestadores
          FROM Municipios INNER JOIN Prestadores
          ON Municipios.Depmun = Prestadores.Cod_Muni
          Group By Region
          Order by Total_Prestadores;")

# Se ejecuta la consulta 8 (C8) en la BD y el resultado se almacena en la variable RptaC8
RptaC8 <- dbGetQuery(BD,C8)
RptaC8

# Diagrama de barras los 10 municipios con más prestadores 
ggplot(RptaC8, aes(x = reorder(Region,Total_Prestadores), y = Total_Prestadores)) + 
  geom_bar(stat = "identity", color="red", fill="salmon2") +
  labs(title = "Número total de prestadores por Región",
       x ="Región",
       y = "Núm Prestadores")


# ----
# Consulta 9: Indice prestadores según poblacion por departamento
#   NOTA:No hay prestadores en todos los municipios
# ----

C9 <- c("SELECT Prestadores.Cod_Dep, Prestadores.depa_nombre as Departamento, COUNT(DISTINCT Prestadores.codigo_habilitacion) as Total_Prestadores, SUM(DISTINCT Municipios.Poblacion) As Poblacion, 
          ((SUM(DISTINCT Municipios.Poblacion))/(COUNT(DISTINCT Prestadores.codigo_habilitacion))) as 'Indice_Cobertura'
          FROM Municipios INNER JOIN Prestadores
          ON Municipios.Depmun = Prestadores.Cod_Muni
          Group By Departamento
          Order by Departamento;")

# Se ejecuta la consulta 9 (C9) en la BD y el resultado se almacena en la variable RptaC9
RptaC9 <- dbGetQuery(BD,C9)
RptaC9

# Unir el resultado de la consulta C9 con la tabla de atributos del shp usando merge
dptos3 <- merge(dptos, RptaC9, by.x=c('Cod_Dpto'), by.y=c('Cod_Dep'))

# Generar mapa de calor del índice prestadores según poblacion por departamento
ggplot(data=dptos3) +
  geom_sf(aes(fill = Indice_Cobertura)) +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Índice Prestadores según poblacion por departamento") +
  scale_fill_gradientn(colours = rev(grDevices::hcl.colors(12,"Green-Yellow")), 
                       name = "Índice") +
  geom_sf_text(aes(label = Departamento), colour = "black", size=3) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         height = unit(1.5, "cm"), width = unit(1.5, "cm"), 
                         pad_x = unit(2, "cm"), pad_y = unit(2, "cm"))  +
  theme_bw()


# *********
# Desconectar y cerrar la BD
# *********

dbDisconnect(BD)

#Manejo de Datos
library(readxl)
library(dplyr)
library(tidyr)

#Graficos
library(ggplot2)

#Mapas
library(maps)
library(rworldmap)
library(viridis)

#Importar los datos
BDCAI20082018 <- read_excel("D:/EJS/BDCAI20082018.xlsx")

#Cantidadde de casos
BDCAI20082018 %>%
  summarise(n(),na.rm=TRUE)

#Cantidad de casos por género
BDCAI20082018 %>%
  group_by(Genero) %>%
  summarise(cantidad=n())

BDCAI20082018 %>%
  distinct(IdPaper) %>%
  summarise(cantidadPapers=n())

#Cantidad de autores únicos
BDCAI20082018 %>%
  filter(Rol=='Autor') %>%
  group_by(Nombre) %>%
  summarise(cantidad=n())

#Cantidad de autores únicos por género
BDCAI20082018 %>%
  filter(Rol=='Autor') %>%
  group_by(Genero) %>%
  distinct(Nombre) %>%
  summarise(cantidad=n())

# Cantidad por Rol, genero y año 
participacion <- BDCAI20082018 %>%
  filter(Genero != 'Sin Definir') %>%
  group_by(Año, Genero, Rol) %>%
  summarise(cantidad=n()) %>%
  spread(key = Rol, value = cantidad) %>%
  mutate(Total=sum(c(Autor, Chair,`Comité Científico`,`Comité Organizador`,Moderadores,`Disertantes Invitados` ), na.rm=TRUE)) %>%
  select (Año, Genero, Chair,`Comité Científico`,`Comité Organizador`,`Disertantes Invitados`, Moderadores, Autor, Total)

#Stacked bar
BDCAI20082018 %>%
  filter(Genero != 'Sin Definir') %>%
  group_by(Año, Genero, Rol) %>%
  summarise(cantidad=n()) %>%
ggplot(aes(fill=Rol, y=cantidad, x=Genero)) +
  geom_bar(stat="identity", position = 'stack') + facet_grid(~ Año)
  xlab("Años de las ediciones del CAI") + ylab("Cantidad de participantes")

#Total de cada rol
total <- participacion %>% 
  ungroup() %>%
  select (Chair,`Comité Científico`,`Comité Organizador`,`Disertantes Invitados`, Moderadores, Autor, Total)%>%
  summarise_all(sum,na.rm=TRUE)


#Porcentajes
porcentaje <- participacion %>% 
  ungroup() %>%
  select (Genero, Chair,`Comité Científico`,`Comité Organizador`,`Disertantes Invitados`, Moderadores, Autor, Total)%>%
  group_by(Genero) %>%
  summarise_all(sum,na.rm=TRUE) %>%
  mutate(Genero, 
         chair_pct = Chair / sum(Chair),
         cc_pct = `Comité Científico` / sum(`Comité Científico`),
         co_pct= `Comité Organizador`/ sum(`Comité Organizador`),
         di_pct=`Disertantes Invitados`/ sum(`Disertantes Invitados`),
         md_pct= Moderadores/ sum(Moderadores), 
         autor_pct= Autor/ sum(Autor),
         total_pct= Total/ sum(Total))
  
#Porcentajes por año
porcentajeAnio <- participacion %>% 
  ungroup() %>%
  select (Año, Genero, Chair,`Comité Científico`,`Comité Organizador`,`Disertantes Invitados`, Moderadores, Autor, Total)%>%
  group_by(Año, Genero) %>%
  summarise_all(sum,na.rm=TRUE) %>%
  mutate(Genero, 
         chair_pct = Chair / sum(Chair),
         cc_pct = `Comité Científico` / sum(`Comité Científico`),
         co_pct= `Comité Organizador`/ sum(`Comité Organizador`),
         di_pct=`Disertantes Invitados`/ sum(`Disertantes Invitados`),
         md_pct= Moderadores/ sum(Moderadores), 
         autor_pct= Autor/ sum(Autor),
         total_pct= Total/ sum(Total))


#Stacked bar x año, genero y rol
datosCantidades <- porcentajeAnio %>%
  select(1:8) %>% 
  gather("Rol", "cantidad", 3:8)

datosPorcentajes <- porcentajeAnio %>%
  select(Año, Genero, Chair=chair_pct, `Comité Científico`=cc_pct, `Comité Organizador`=co_pct, `Disertantes Invitados`=di_pct, Moderadores=md_pct, Autor=autor_pct) %>% 
  gather("Rol", "porcentaje", 3:8)%>%
  mutate(porcentaje=ifelse(is.nan(porcentaje*100), 0, porcentaje*100)) %>%
  mutate(etiqueta=paste(as.character(round(porcentaje, digits = 0)),"%", sep = ""))

datosalgrafico <- inner_join(datosCantidades, datosPorcentajes, by = c("Año", "Genero", "Rol"))

datosalgrafico %>%
  ggplot(aes(fill=Rol, y=cantidad, x=Genero)) +
  geom_col(position = 'stack', width = .7) + 
  geom_text(data=subset(datosalgrafico,porcentaje != 0), aes(label=etiqueta), position = position_stack(vjust = 0.5), size = 3)+
  xlab("Años de las ediciones del CAI") + ylab("Cantidad de participantes") +
  theme(legend.position="bottom") +
  facet_grid(~ Año)



# Stacked bar  grafico de participantes por género
BDCAI20082018 %>%
  group_by(Año, Genero) %>%
  summarise(cantidad=n()) %>%
  ggplot(aes(fill=Genero, y=cantidad, x=Año)) +
  geom_bar( stat="identity") + scale_x_continuous(breaks = c(2008:2018), 
                                               labels = factor(2008:2018), 
                                               limits = c(2008,2018)) +
  xlab("Años de las ediciones del CAI") + ylab("Cantidad de participantes")


#Cantidad de autores por tipo de paper, año y género
partTipoTrabajo <- BDCAI20082018 %>%
  filter(Rol == 'Autor' && Genero != 'Sin Definir') %>%
  group_by(Año, Genero, TipoTrabajo) %>%
  summarise(cantidadAutores=n()) %>%
  spread(key = TipoTrabajo, value = cantidadAutores) %>%
  mutate(Total=sum(c(`Full Paper`, `Comunicaciones Orales`, `Short Paper`, `Extended Abstract`, `Demo`), na.rm=TRUE)) %>%
  select (Año, Genero, `Full Paper`, `Comunicaciones Orales`, `Short Paper`, `Extended Abstract`, `Demo`, Total) %>%
  ungroup() %>%
  group_by(Año, Genero) %>%
  summarise_all(sum,na.rm=TRUE) %>%
  mutate(Genero, 
         fp_pct = `Full Paper` / sum(`Full Paper`),
         co_pct = `Comunicaciones Orales` / sum(`Comunicaciones Orales`),
         sp_pct= `Short Paper`/ sum(`Short Paper`),
         ea_pct=`Extended Abstract`/ sum(`Extended Abstract`),
         dm_pct= Demo/ sum(Demo), 
         total_pct= Total/ sum(Total)) %>%
  select (Año, Total, total_pct, Genero, `Full Paper`, fp_pct, `Comunicaciones Orales`, co_pct, `Short Paper`, sp_pct, `Extended Abstract`, ea_pct, `Demo`, dm_pct)

write.csv(partTipoTrabajo, "Tabla3.csv")


#Por tipo de presentación
partTipoPresentacion <- BDCAI20082018 %>%
  filter(Genero != 'Sin Definir' && Rol != 'Comité Organizador' && Rol != 'Comité Científico' && Rol != 'Chair') %>%
  group_by(Año, Genero, TipoPresentacion) %>%
  summarise(cantidad=n()) %>%
  spread(key = TipoPresentacion, value = cantidad)%>%
  mutate(Total=sum(c(`Exposición Oral`, `Poster`), na.rm=TRUE)) %>%
  select (Año, Genero, `Exposición Oral`, `Poster`, Total) %>%
  ungroup() %>%
  group_by(Año, Genero) %>%
  summarise_all(sum,na.rm=TRUE) %>%
  mutate(Genero, 
         eo_pct = `Exposición Oral` / sum(`Exposición Oral`),
         p_pct = Poster / sum(Poster),
         total_pct= Total/ sum(Total)) %>%
  select (Año, Total, total_pct, Genero, `Exposición Oral`,eo_pct, Poster, p_pct)

write.csv(partTipoPresentacion, "Tabla4.csv")

# Stacked bar  grafico de tipoPresentación por género
BDCAI20082018 %>%
  filter (TipoPresentacion != 'No presenta' & Genero != 'Sin Definir') %>%
  group_by(Genero, TipoPresentacion) %>%
  summarise(cantidad=n()) %>%
  ggplot(aes(fill=TipoPresentacion, y=cantidad, x=Genero)) +
  geom_bar( stat="identity") +
  xlab("Años de las ediciones del CAI") + ylab("Cantidad de participantes")


# Instituciones

Instituciones <- BDCAI20082018 %>%
  group_by(Institución, Genero) %>%
  summarise(cantidad=n())
  
#Instituciones de autores
Instituciones <- BDCAI20082018 %>%
  group_by(Institución) %>%
  filter(Rol == 'Autor') %>%
  summarise(cantidad=n())

#Cantidad de Paper por institucion
InstitucionesPapers <- BDCAI20082018 %>%
  group_by(Institución) %>%
  filter(Rol == 'Autor' & !is.na(IdPaper)) %>%
  distinct(IdPaper) %>%
  summarise(cantidad=n())

#Cantidad de papers x año
Papers <- BDCAI20082018 %>%
  group_by(Año) %>%
  filter(!is.na(IdPaper)) %>%
  distinct(IdPaper) %>%
  summarise(cantidad=n())

#Instituciones de acuerdo al género
Institucioes <- BDCAI20082018 %>%
  filter(Genero != 'Sin Definir') %>%
  group_by(Institución, Genero) %>%
  summarise(cantidad=n())%>%
  spread(key = Genero, value = cantidad)%>%
  mutate(Total=sum(c(Hombre, Mujer), na.rm=TRUE)) %>%
  arrange(desc(Total)) %>%
  ungroup() %>%
  top_n(15) %>%
  gather(key = 'Genero', value = 'Cantidad', Hombre:Mujer)%>%
  select(Institución, Genero, Cantidad)

write.csv(Institucioes, "Tabla5.csv")

#Gráfico por Instituciones y género Top 15
Institucioes %>%
  mutate(name = fct_reorder(Institución, Cantidad)) %>%
  ggplot( aes(fill=Genero, x=name, y=Cantidad)) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("") + ylab("Cantidad de participantes")

#Instituciones de acuerdo al género de los autores
InstitucionAutor <- BDCAI20082018 %>%
  filter(Genero != 'Sin Definir' & Rol == 'Autor') %>%
  group_by(Institución, Genero) %>%
  summarise(cantidad=n())%>%
  spread(key = Genero, value = cantidad)%>%
  mutate(Total=sum(c(Hombre, Mujer), na.rm=TRUE)) %>%
  arrange(desc(Total)) 


#Instituciones de acuerdo al género y el Rol
Instituciones <- BDCAI20082018 %>%
  filter(Genero != 'Sin Definir') %>%
  group_by(Institución, Rol, Genero) %>%
  summarise(cantidad=n())%>%
  spread(key = Genero, value = cantidad)%>%
  mutate(Total=sum(c(Hombre, Mujer), na.rm=TRUE)) %>%
  arrange(desc(Total)) %>%
  select(Institución, Rol, Hombre, Mujer, Total)


#Participantes por provincia
pctProvincia <- BDCAI20082018 %>%
  filter(Genero != 'Sin Definir' & País=='Argentina' & !is.na(Provincia)) %>%
  group_by(Provincia, Genero, Rol) %>%
  summarise(cantidad=n()) %>%
  spread(key = Rol, value = cantidad) %>%
  mutate(Total=sum(c(Autor, Chair,`Comité Científico`,`Comité Organizador`,Moderadores,`Disertantes Invitados` ), na.rm=TRUE)) %>%
  select (Provincia, Genero, Chair,`Comité Científico`,`Comité Organizador`,`Disertantes Invitados`, Moderadores, Autor, Total)

#Distrbución Geográfica

#Mapa de Argentina
Ar <- map_data("world") %>% filter(region=="Argentina")

#Ciudades de Argentina
ciudades=world.cities %>% filter(country.etc=="Argentina")

#Obtengo todos los nombres de las ciudades en el set de datos
ciudad <- BDCAI20082018 %>% 
  filter(País=='Argentina')%>%
  distinct(Ciudad) %>%
  rename(name=Ciudad)

#Uno las localidades del set de datos que coinciden con el set del mapa
localidades <- ciudad %>%
  inner_join(ciudades)


# Esta parte del código la uso solamente para obtener los datos de lat y long 
# de las ciudades que no están en el listado de ciudades
#Debería ver la forma de obtener el geocoding desde R

#write.csv(anti_join(ciudad, localidades), "localidades.csv")

# library(readxl)

# DatosLocalidades <- read_excel("D:/EJS/DatosLocalidades.xlsx", 
#                                sheet = "Hoja2")


#Genero el listado con los códigos georeferenciados
localidades <- DatosLocalidades %>%
  rename(lat=Latitude, long=Longitude, country.etc=pais) %>%
  bind_rows(localidades) %>%
  select(name, lat, long)

#Genero la cantidad de participantes en cada ciudad
LocalidadXGenero <- BDCAI20082018 %>%
  filter(Genero != 'Sin Definir') %>%
  group_by(Ciudad, Genero) %>%
  summarise(cantidad=n())%>%
  spread(key = Genero, value = cantidad) %>%
  rename(name=Ciudad)

#Junto los datos que quiero mapear con la latitud y la longitud
localidades <- LocalidadXGenero %>%
  inner_join(localidades)

#Hago el mapa de los Hombres
localidades%>%
  ggplot() +
  geom_polygon(data = Ar, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( aes(x=long, y=lat, size=Hombre, color=Hombre), alpha=0.9) +
  scale_size_continuous(breaks = c(40, 80, 120, 160),
                       limits = c(1, 165)) +
  scale_color_continuous(high = "blue", 
                         low = "green", 
                         limits = c(1, 165))+
  guides(colour = guide_legend("Hombre"), size = guide_legend("Hombre"))

#Hago el mapa de las mujeres
localidades%>%
  #arrange(Mujer) %>% # This reorder your data frame
  ggplot() +
  geom_polygon(data = Ar, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( aes(x=long, y=lat, size=Mujer, color=Mujer), alpha=0.9) +
  scale_size_continuous(breaks = c(40, 80, 120, 160),
                        limits = c(1, 165))+
  scale_color_continuous(high = "blue", 
                         low = "green", 
                         limits = c(1, 165))+
  guides(colour = guide_legend("Mujer"), size = guide_legend("Mujer"))



#Tabla de participantes por provincia
ProvinciaXGenero <- BDCAI20082018 %>%
  filter(Genero != 'Sin Definir' & `País` == 'Argentina') %>%
  group_by(Provincia, Genero) %>%
  summarise(cantidad=n())%>%
  spread(key = Genero, value = cantidad) %>%
  rename(name=Ciudad)

#Tabla de participantes por provincia x rol y x genero
ProvinciaXGeneroXRol <- BDCAI20082018 %>%
  filter(Genero != 'Sin Definir' & `País` == 'Argentina') %>%
  group_by(Provincia, Rol, Genero) %>%
  summarise(cantidad=n())%>%
  spread(key = Genero, value = cantidad) 

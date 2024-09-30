install.packages("readr")
library(readr)
install.packages("tidyverse")
library(ggplot2)
library(tidyverse)
install.packages(("sandwich"))
library(sandwich)
install.packages("sf")
library(sf)
install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library(stringr)
install.packages("janitor")
library(janitor)
install.packages("sf", method = "libcurl")

denue <- read_excel("C:/Users/zelid_bnxwud7/Desktop/OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO/ITAM/Noveno semestre/Datatón/denue.xlsx",)
censo <- read_csv("C:/Users/zelid_bnxwud7/Desktop/Dataton/pedro/Dataton/Censos/conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv",)
View(censo)
censo <- censo%>%
  janitor::clean_names()

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(readr, dplyr, magrittr, ggplot2, stringr, sf, RColorBrewer)
install.packages("C:/Users/zelid_bnxwud7/Desktop/sf_1.0-17.zip", repos = NULL, type = "win.binary")


denue <- denue%>%
  mutate(nombre_act = str_to_lower(nombre_act))%>%
  filter(str_detect(nombre_act, "farm"))
Sys.setenv(OGR_GEOMETRY_ACCEPT_UNCLOSED_RING = "NO")
secciones = st_read("C:/Users/zelid_bnxwud7/Desktop/Dataton/pedro/Dataton/Mapas/SECCION.shp")
View(censo)
censo <- censo %>%
  mutate(across(-c(nom_ent,nom_mun, nom_loc, longitud, latitud), as.integer))

View(secciones)
secciones_full = left_join(secciones, censo, 
                           by = c("SECCION" = "loc",
                                  "ENTIDAD" = "entidad",
                                  "MUNICIPIO" = "mun")) |>
  mutate(area_seccion_m2 = st_area(geometry),
         area_seccion_km2 = area_seccion_m2/1000,
         pob60plus_per_km2 = p_60ymas/area_seccion_km2,
         pob12menos_per_km2=(p_60ymas-p_12ymas)/area_seccion_km2,
         pea_per_km2=pea/area_seccion_km2,
         pobde0a4_per_km2=p_0a4/area_seccion_km2,
         pcon_disc_per_km2=pcon_disc/area_seccion_km2,
         pcon_limi_per_km2=pcon_limi/area_seccion_km2,
         psinaf_serv_sal_per_km2=psinder/area_seccion_km2)
View(secciones_full)
# Make the farmacias locations be coded in the same crs as the shapefile
farmacias = st_as_sf(denue, coords = c("longitud", "latitud"), crs = 4326) |>
  st_transform(st_crs(secciones_full))

names(farmacias)
View(denue)
ggplot()+
  geom_sf(data=secciones, color = "grey",fill = "white", size = 0.5)

ggplot()+
  geom_sf(data = secciones_full, fill = "white", color = "grey")+
  geom_sf(data = farmacias, color = "#750014", alpha = 0.2, size = 0.5)+
  theme_minimal()

View(farmacias)
names(farmacias)
farmacias <- farmacias %>%
  mutate(id_loc = paste(cve_ent, cve_mun, cve_loc, sep="-"))
farmacias <- farmacias %>%
  select(-cve_ent, -cve_mun, -cve_loc)
censo <- censo %>%
  mutate(id_loc= paste(entidad, mun, loc, sep="-"))

secciones_full <- secciones_full %>%
  mutate(id_loc=paste(ENTIDAD, MUNICIPIO, SECCION, sep="-"))

farmacias_censo <- left_join(farmacias, censo, by="id_loc")
View(farmacias_censo)
View(censo)


write.csv(farmacias_censo, "C:/Users/zelid_bnxwud7/Desktop/farmacias_censo.csv", row.names = FALSE)

#mapa 
loc_rec <- read_csv("C:/Users/zelid_bnxwud7/Desktop/OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO/ITAM/Noveno semestre/Datatón/localidades_recomendadas.csv",)
View(loc_rec)
View(secciones)

secciones_rec = merge(secciones, loc_rec, 
                           by = "id_loc") 
View(secciones_rec)
rec = st_as_sf(secciones_rec, coords = c("LONGITUD", "LATITUD"), crs = 4326) |>
  st_transform(st_crs(secciones_full))
View(rec)             
localidades <- st_as_sf(loc_rec, coords = c("LONGITUD_DEC", "LATITUD_DEC"), crs = 4326)
View(localidades)
secciones <- secciones %>%
  mutate(id_loc= paste(ENTIDAD, MUNICIPIO, SECCION, sep="-"))
loc_rec <- loc_rec%>%
  mutate(id_loc=paste(ENTIDAD, MUN, LOC, sep="-"))

print(loc_rec$id_loc)
codigos_loc <- c("9-7-1", "21-114-1", "14-120-1", "15-33-1", "15-58-1", 
                 "14-39-1", "24-28-1", "9-5-1", "9-15-1", "9-14-1", 
                 "11-20-1", "15-31-1", "9-12-1", "1-1-1", "9-3-1", 
                 "9-17-1", "16-53-1", "9-16-1", "7-101-1", "9-2-1", 
                 "9-6-1", "15-121-1", "15-13-1", "15-57-1", "23-5-1", 
                 "11-17-1", "9-10-1", "10-5-1", "31-50-1", "24-35-1", 
                 "14-98-1", "18-17-1", "14-101-1", "15-104-1", "17-7-1", 
                 "15-122-1", "11-7-1", "9-13-1", "22-14-1", "12-1-1", 
                 "5-35-1", "9-11-1", "21-156-1", "2-4-1", "15-20-1", 
                 "20-67-1", "13-48-1", "5-30-1", "12-29-1", "15-60-1", 
                 "4-3-1", "15-39-1", "30-193-1", "30-87-1", "3-8-54", 
                 "27-4-1", "23-8-1", "17-11-1", "15-106-1", "8-19-1", 
                 "15-37-71", "15-109-3", "3-3-1", "21-119-13", "9-4-1", 
                 "23-4-1", "10-7-1", "25-12-1", "15-109-25", "30-131-1", 
                 "12-35-1", "32-10-1", "17-6-1", "21-15-1", "28-22-1", 
                 "26-30-1", "30-28-37", "9-8-1", "16-102-1", "7-89-1", 
                 "15-25-1", "17-18-1", "16-108-1", "6-10-1", "6-2-1", 
                 "15-29-1", "11-27-1", "7-19-1", "11-37-1", "19-39-1", 
                 "5-18-1", "29-25-1", "16-69-1", "14-23-1", "14-63-1", 
                 "14-67-1", "32-17-1", "15-81-19", "30-118-1", "16-76-1", 
                 "22-6-1", "13-77-1", "14-8-1", "21-140-1", "7-78-1", 
                 "15-70-1", "19-19-1", "16-112-1", "16-6-1", "22-14-58", 
                 "20-39-1", "14-73-1", "28-32-1", "23-1-1", "14-53-1", 
                 "18-8-1", "20-385-1", "15-76-1", "11-41-1", "32-56-1", 
                 "4-2-1", "15-37-9", "16-52-1", "14-15-1", "15-2-15", 
                 "14-97-25", "21-41-1", "30-44-1", "7-17-1", "9-12-27", 
                 "15-109-68", "7-65-1", "7-108-1", "20-184-1", "30-39-1", 
                 "16-66-1", "9-12-26", "13-28-1", "21-85-1", "17-29-1", 
                 "16-34-1", "26-29-1", "7-61-1", "5-27-1", "20-79-1", 
                 "6-9-1", "14-70-13", "15-54-49", "10-12-1", "26-43-1", 
                 "12-38-1", "21-119-1", "15-99-1", "14-124-1", "1-5-1", 
                 "14-18-1", "21-41-8", "14-120-231", "6-7-1", "7-12-1", 
                 "11-11-1", "21-154-1", "16-55-1", "14-93-1", "21-174-1", 
                 "20-390-1", "7-27-1", "2-1-1", "14-70-1", "21-19-1", 
                 "28-27-1", "14-70-14", "20-515-1", "17-8-5", "29-5-1", 
                 "14-98-14", "31-59-1", "17-8-1", "14-6-1", "15-106-75", 
                 "16-75-1", "16-71-1", "15-39-12", "11-21-1", "12-66-1", 
                 "11-2-1", "16-43-1", "30-189-1", "15-70-5", "3-8-1", 
                 "21-132-1", "20-318-9", "25-6-1", "15-24-1", "30-138-1", 
                 "29-6-1", "14-94-1", "11-44-1", "12-55-1", "14-97-424")

secciones_filtradas <- secciones %>%
  filter(id_loc %in% codigos_loc)
View(secciones_rec)

convert_dms_to_dd <- function(dms) {
  # Inicializar un vector para los resultados
  dd_values <- numeric(length(dms))
  
  for (i in seq_along(dms)) {
    # Eliminar caracteres no deseados
    current_dms <- gsub("[\" ]", "", dms[i])  # Eliminar comillas y espacios
    
    # Extraer la dirección (último carácter)
    direction <- substr(current_dms, nchar(current_dms), nchar(current_dms))
    current_dms <- substr(current_dms, 1, nchar(current_dms) - 1)  # Eliminar dirección
    
    # Separar en partes (grados, minutos, segundos)
    dms_parts <- unlist(strsplit(current_dms, "[°'']"))  # Separar por grados y minutos
    dms_parts <- dms_parts[nzchar(dms_parts)]  # Eliminar partes vacías
    
    # Verificar si hay suficientes partes
    if (length(dms_parts) < 3) {
      cat("Error en el formato de DMS:", dms[i], "\n")  # Imprimir el valor problemático
      dd_values[i] <- NA  # Asignar NA si el formato es incorrecto
      next  # Continuar con la siguiente iteración
    }
    
    # Convertir a grados decimales
    degrees <- as.numeric(dms_parts[1])
    minutes <- as.numeric(dms_parts[2]) / 60
    seconds <- as.numeric(dms_parts[3]) / 3600
    
    # Calcular el valor en grados decimales
    dd <- degrees + minutes + seconds
    
    # Manejar la dirección: ajustar el signo si es Oeste (W) o Sur (S)
    if (direction %in% c("W", "S")) {
      dd <- -dd
    }
    
    dd_values[i] <- dd  # Guardar el resultado
  }
  
  return(dd_values)
}

# Aplicar la transformación
loc_rec <- loc_rec %>%
  mutate(
    LONGITUD_DEC = convert_dms_to_dd(LONGITUD),
    LATITUD_DEC = convert_dms_to_dd(LATITUD)
  )





View(loc_rec)
ggplot()+
  geom_sf(data = secciones_full, fill = "white", color = "grey")+
  geom_sf(data=localidades, color="red", alpha=0.2, size=0.5)
  theme_minimal()
  
  library(ggplot2)
  
  ggplot() +
    geom_sf(data = secciones_full, fill = "white", color = "grey") +
    geom_sf(data = localidades, color = "#B03060", alpha = 0.8, size = 1, fill = NA) +  # Aumentar el tamaño y opacidad
    theme_minimal() +
    theme(  
      plot.background = element_rect(fill = "#FFF0F5")  # Cambiar el fondo del plot
    ) +
    labs(title = "Recomendaciones para la Apertura de Farmacias", 
         subtitle = "Análisis basado en el èxito de farmacias existentes",
         caption = "Fuente: Elaboración propia a partir de datos del INEGI e INE")
  install.packages("RColorBrewer")
  library(RColorBrewer)
  
  # Ver paletas disponibles
  display.brewer.all()
  
  ggplot() +
    geom_sf(data = secciones_full, fill = brewer.pal(9, "Blues")[3], color = "grey") +
    geom_sf(data = localidades, color = brewer.pal(9, "Reds")[3], alpha = 0.8, size = 1.5) +
    theme_minimal()+
    labs(title = "Mapa de Localidades", 
         subtitle = "Recomendaciones de nuevas farmacias ",
         caption = "Fuente: Elaboración propia a partir de datos del INEGI e INE")

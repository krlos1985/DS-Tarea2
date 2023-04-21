# PREGUNTA 1.1
#Instalar las librerías: HTTR y XML
install.packages("httr")
install.packages("XML")

#Cargar las librerías: HTTR y XML
library(httr)
library(XML)

#Definir la URL
url <- "https://www.mediawiki.org/wiki/MediaWiki"

#Descargar la página
page <- GET(url)

#Status
status_code(page)

#Convertir el contenido HTML de la página a formato XML
parsed_page <- htmlParse(content(page, as = "text"))

# PREGUNTA 1.2
#Título de la página
title <- xpathSApply(parsed_page, "//title", xmlValue)
title

#Ficheros de estilo
stylesheets <- xpathSApply(parsed_page, "//link[@rel='stylesheet']/@href")
stylesheets

#Nombre del autor
author <- xpathSApply(parsed_page, "//meta[@name='author']/@content")
author

#Descripción de la página
description <- xpathSApply(parsed_page, "//meta[@name='description']/@content")
description

#tipo de codificación
encoding <- xpathSApply(parsed_page, "//meta[@charset]/@charset")
encoding

#palabras clave
keywords <- xpathSApply(parsed_page, "//meta[@name='keywords']/@content")
keywords

# PREGUNTA 1.3
#Texto del enlace
links_text <- xpathSApply(parsed_page, "//a", xmlValue)
print(links_text)

#URL
links_url <- xpathSApply(parsed_page, "//a", xmlGetAttr, 'href')
print(links_url)

# PREGUNTA 1.4 & 1.5
#Numero de veces que aparece que aparece cada enlace
link_counts <- table(links_url)

#Crear data.frame
tabla <- data.frame(links_text = character(),
                    links_url = character(),
                    repeticiones = numeric(),
                    scraps = character(),
                    stringsAsFactors = FALSE)

frecuencia <- table(links_url)

for (i in 1:length(links_text)) {
  Sys.sleep(4)
  print("round done")
  tabla[i, "links_text"] <- links_text[i]
  tabla[i, "links_url"] <- links_url[i]
  tabla[i, "repeticiones"] <- frecuencia[links_url[i]]
  
  #si inicia con /wiki/
  validation_wiki <- startsWith(links_url[i], "/wiki/")
  if(validation_wiki) {
    tabla[i, "links_url"] <- paste0("https://www.mediawiki.org", links_url[i])
  }
  
  #si inicia con /https/
  validation_wiki <- startsWith(links_url[i], "https:")
  if(validation_wiki) {
    tabla[i, "links_url"] <- paste0("", links_url[i])
  }
  
  #si inicia con //
  validation_wiki <- startsWith(links_url[i], "//")
  if(validation_wiki) {
    tabla[i, "links_url"] <- paste0("https:", links_url[i])
  }
  
  #si inicia con /w/
  validation_wiki <- startsWith(links_url[i], "/w/")
  if(validation_wiki) {
    tabla[i, "links_url"] <- paste0("https://www.mediawiki.org", links_url[i])
  }
  
  #si inicia con /#/
  validation_wiki <- startsWith(links_url[i], "#")
  if(validation_wiki) {
    tabla[i, "links_url"] <- paste0("https://www.mediawiki.org/wiki/MediaWiki", links_url[i])
  }
  
  print(paste0("TEST> " , links_url[i]))
  
  #obtencion de STATUS CODE
  code <- status_code(HEAD(tabla[i, "links_url"]))
  tabla[i, "scraps"] <- code
  print(code)
}

head(tabla)

#PREGUNTA 2.1
library(ggplot2)

# Añadimos una columna para indicar si la URL es absoluta o relativa
tabla$tipo_url <- ifelse(grepl("^https?", tabla$links_url), "Absoluta", "Relativa")

# Creamos el histograma usando ggplot2
ggplot(tabla, aes(x=repeticiones)) + 
  geom_histogram(aes(fill=tipo_url), 
                 binwidth = 1, 
                 position = "dodge") +
  scale_fill_manual(values=c("#FF6666", "#66CCFF")) +
  labs(x = "Frecuencia de aparición", y = "Número de enlaces") +
  theme_minimal()

#PREGUNTA 2.2
# Añadir columna de enlaces internos/externos
tabla$interno <- ifelse(grepl("https://www.mediawiki.org", tabla$links_url), "Interno", "Externo")

# Calcular suma de enlaces internos y externos
suma_interno <- sum(tabla$repeticiones[tabla$interno == "Interno"], na.rm = TRUE)
suma_externo <- sum(tabla$repeticiones[tabla$interno == "Externo"], na.rm = TRUE)

# Generar gráfico de barras
barplot(c(suma_interno, suma_externo), names.arg = c("Enlaces internos", "Enlaces externos"),
        xlab = "Tipo de enlace", ylab = "Frecuencia", col = c("blue", "red"))

#PREGUNTA 2.3

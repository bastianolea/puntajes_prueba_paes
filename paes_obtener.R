# descargar base de datos paes
download.file(url = "https://datosabiertos.mineduc.cl/wp-content/uploads/2024/05/PAES-2024-Inscritos-Puntajes.rar",
              destfile = "datos/originales/PAES-2024-Inscritos-Puntajes.rar")

# descomprimir archivo rar
untar("datos/originales/PAES-2024-Inscritos-Puntajes.rar",
      exdir = "datos/originales")

# eliminar archivo comprimido
file.remove("datos/originales/PAES-2024-Inscritos-Puntajes.rar")


# utilidades ----

mes_a_numeric <- function(x) {
  recode(x, 
         "ene" = 1,
         "feb" = 2,
         "mar" = 3,
         "abr" = 4,
         "may" = 5,
         "jun" = 6,
         "jul" = 7,
         "ago" = 8,
         "sep" = 9,
         "oct" = 10,
         "nov" = 11,
         "dic" = 12)
}



trimestre_a_numeric <- function(x) {
  recode(tolower(x), 
         "i" = 1,
         "ii" = 2,
         "iii" = 3,
         "iv" = 4)
}

trimestre_a_mes <- function(x) {
  recode(as.character(x),
         "1" = 3*1,
         "2" = 3*2,
         "3" = 3*3, 
         "4" = 3*4)
}


cifra_comas_a_numeric <- function(x) {
  x <- str_remove_all(x, "\\.")
  x <- str_replace_all(x, ",", "\\.")
  x <- as.numeric(x)
}

extraer_año <- function(x) {
  x <- as.numeric(str_extract(x, "\\d{4}"))
}



# funciones banco central ----
scrapear_tabla_bc <- function(url, convertir = TRUE) {
  stopifnot(nchar(url) > 1)
  message(url)
  # browser()
  
  tryCatch({
    dato_0 <- session(url) |> 
      read_html() |> 
      html_table(convert = convertir)
    
    dato_1 <- dato_0[[1]] |> 
      janitor::clean_names() |> 
      select(-1)
    
  }, error = function(error) {
    warning(error)
    return(NULL)
  })
  
  stopifnot(length(dato_1) > 1)
  stopifnot(nrow(dato_1) >= 1)
  
  return(dato_1)
}


limpiar_tabla_bc <- function(dato_1, frecuencia = "mensual",
                             chequear_missings_valor = TRUE) {
  dato_1a <- dato_1 |> 
    tidyr::pivot_longer(cols = 2:length(dato_1), names_to = "fecha", values_to = "valor") |> 
    mutate(año = extraer_año(fecha))
  
  if (frecuencia == "mensual") {
    dato_1b <- dato_1a |> 
      mutate(mes = str_extract(fecha, "^\\w+(?=_)"),
             mes = mes_a_numeric(mes)) |> 
      mutate(fecha = paste(año, mes, "1", sep = "-"))
    
  } else if (frecuencia == "trimestral") {
    dato_1b <- dato_1a |> 
      mutate(trimestre = str_extract(fecha, "^\\w+(?=_)"),
             trimestre = trimestre_a_numeric(trimestre)) |> 
      mutate(mes = trimestre_a_mes(trimestre)) |> 
      mutate(fecha = paste(año, mes, "1", sep = "-"))
  }
  # browser()
  dato_2 <- dato_1b |> 
    mutate(valor = cifra_comas_a_numeric(valor))
  
  stopifnot(any(is.na(dato_2$año)) == FALSE)
  if (chequear_missings_valor == TRUE) stopifnot(any(is.na(dato_2$valor)) == FALSE)
  
  return(dato_2)  
}



# obtención de datos ----
# funciones que obtienen datos oficiales directo de la web del Banco Central, y los retorna como dataframe en formato tidy

obtener_pib <- function() {
  message("obtienendo PIB desde web del Banco Central...")
  # https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_CCNN/MN_CCNN76/CCNN2018_P0_V2/637801082315858005?cbFechaInicio=2010&cbFechaTermino=2023&cbFrecuencia=QUARTERLY&cbCalculo=NONE&cbFechaBase=
  # Cuentas nacionales > Producto Interno Bruto (PIB), gasto e ingreso > Referencia 2018 > Producto interno bruto > PIB total
  
  dato_1 <- scrapear_tabla_bc("https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_CCNN/MN_CCNN76/CCNN2018_P0_V2/637801082315858005", 
                              convertir = FALSE)
  
  message("limpiando datos...")
  dato_2 <- dato_1 |> 
    limpiar_tabla_bc(frecuencia = "trimestral")
  
  stopifnot(length(dato_2) >= 3)
  stopifnot(nrow(dato_2) > 12)
  
  return(dato_2)
}


obtener_pib_regional <- function() {
  message("obtienendo PIB regional desde web del Banco Central...")
  # https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_ESTADIST_REGIONAL/MN_REGIONAL1/CCNN2018_PIB_REGIONAL_T
  
  dato_1 <- scrapear_tabla_bc("https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_ESTADIST_REGIONAL/MN_REGIONAL1/CCNN2018_PIB_REGIONAL_T")
  
  message("limpiando datos...")
  dato_2 <- dato_1 |> 
    limpiar_tabla_bc(frecuencia = "trimestral")
  
  stopifnot(length(dato_2) >= 3)
  stopifnot(nrow(dato_2) > 12)
  
  return(dato_2)
}


obtener_imacec <- function() {
  message("obtienendo IMACEC desde web del Banco Central...")
  # https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_ESTADIST_MACRO/MN_EST_MACRO_IV/PEM_ACTyDDA_IMACEC_2_2018/637807927445790326
  # Principales Estadísticas Macro > Actividad y demanda > Indicadores de coyuntura > Imacec
  
  dato_1 <- scrapear_tabla_bc("https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_ESTADIST_MACRO/MN_EST_MACRO_IV/PEM_ACTyDDA_IMACEC_2_2018/637807927445790326")
  
  message("limpiando datos...")
  dato_2 <- dato_1 |> 
    limpiar_tabla_bc()
  
  stopifnot(length(dato_2) >= 3)
  stopifnot(nrow(dato_2) > 12)
  
  return(dato_2)
}

obtener_ipc <- function() {
  message("obtienendo IPC desde web del Banco Central...")
  # https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_PRECIOS/MN_CAP_PRECIOS/IPC_EMP_2023/638415285164039007
  # Precios > IPC General y medidas subyacentes, INE > Índice empalmado - 2023
  
  dato_1 <- scrapear_tabla_bc("https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_PRECIOS/MN_CAP_PRECIOS/IPC_EMP_2023/638415285164039007")
  
  message("limpiando datos...")
  dato_2 <- dato_1 |> 
    limpiar_tabla_bc()
  
  stopifnot(length(dato_2) >= 3)
  stopifnot(nrow(dato_2) > 12)
  
  return(dato_2)
}

obtener_ipsa <- function() {
  message("obtienendo IPSA desde web del Banco Central...")
  # https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_ESTADIST_MACRO/MN_EST_MACRO_IV/PEM_INDBUR/PEM_INDBUR
  # Principales Estadísticas Macro > Tasa de interés y estadísticas monetarias > Indicadores bursátiles
  
  dato_1 <- scrapear_tabla_bc("https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_ESTADIST_MACRO/MN_EST_MACRO_IV/PEM_INDBUR/PEM_INDBUR")
  
  message("limpiando datos...")
  dato_2 <- dato_1 |> 
    limpiar_tabla_bc()
  
  stopifnot(length(dato_2) >= 3)
  stopifnot(nrow(dato_2) > 12)
  
  return(dato_2)
}


obtener_desempleo <- function() {
  message("obtienendo desempleo desde web del Banco Central...")
  # https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_ESTADIST_MACRO/MN_EST_MACRO_IV/PEM_ML_FT_M/PEM_ML_FT_M
  # Principales Estadísticas Macro > Mercado Laboral > Desempleo
  
  dato_1 <- scrapear_tabla_bc("https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_ESTADIST_MACRO/MN_EST_MACRO_IV/PEM_ML_FT_M/PEM_ML_FT_M")
  
  message("limpiando datos...")
  dato_2 <- dato_1 |> 
    limpiar_tabla_bc()
  
  stopifnot(length(dato_2) >= 3)
  stopifnot(nrow(dato_2) > 12)
  
  return(dato_2)
}

obtener_uf <- function() {
  message("obtienendo UF desde web del Banco Central...")
  # https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_PRECIOS/MN_CAP_PRECIOS/UF_IVP_UTM/UF_IVP_UTM
  # Precios > Unidad de Fomento (UF) - Índice de Valor Promedio (IVP) - Unidad Tributaria Mensual (UTM) > UF-IVP-UTM mensual
  
  dato_1 <- scrapear_tabla_bc("https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_PRECIOS/MN_CAP_PRECIOS/UF_IVP_UTM/UF_IVP_UTM")
  
  message("limpiando datos...")
  dato_2 <- dato_1 |> 
    limpiar_tabla_bc(chequear_missings_valor = FALSE) |> 
    filter(!is.na(valor))
  
  stopifnot(length(dato_2) >= 3)
  stopifnot(nrow(dato_2) > 12)
  
  return(dato_2)
}


obtener_desocupados <- function() {
  message("obtienendo desocupados (INE, promedios móviles trimestrales, miles de personas) desde web del Banco Central...")
  
  # dato_1 <- scrapear_tabla_bc("https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_EMP_REM_DEM/MN_EMP_REM_DEM13/ED_FTM2/a6?cbFechaInicio=2010&cbFechaTermino=2024&cbFrecuencia=MONTHLY&cbCalculo=PCT&cbFechaBase=")
  dato_1 <- scrapear_tabla_bc("https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_EMP_REM_DEM/MN_EMP_REM_DEM13/ED_FTM2/a6")
  
  message("limpiando datos...")
  dato_2 <- dato_1 |> 
    limpiar_tabla_bc(chequear_missings_valor = FALSE) |> 
    filter(serie == "Desocupados")
  
  stopifnot(length(dato_2) >= 3)
  stopifnot(nrow(dato_2) > 12)
  
  return(dato_2)
}


obtener_remuneraciones <- function() {
  message("obtienendo índice de remuneraciones INE (base 2023=100) desde web del Banco Central...")
  
  # dato_1 <- scrapear_tabla_bc("https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_EMP_REM_DEM/MN_EMP_REM_DEM13/ED_VAR_REM_M_2023/638532110333037937")
  dato_1 <- scrapear_tabla_bc("https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_EMP_REM_DEM/MN_EMP_REM_DEM13/ED_IND_REM_M_2023/638532110891789445")
  
  message("limpiando datos...")
  dato_2 <- dato_1 |> 
    limpiar_tabla_bc(chequear_missings_valor = FALSE)
  
  stopifnot(length(dato_2) >= 3)
  stopifnot(nrow(dato_2) > 12)
  
  return(dato_2)
}








# guardar datos solo si tienen cambios con respecto a los ya guardados
guardar_solo_con_cambios <- function(dato_nuevo, ruta = "app/datos/pib.rds") {
  # browser()
  tryCatch({
    # revisiones mínimas
    stopifnot(length(dato_nuevo) >= 3)
    stopifnot(nrow(dato_nuevo) >= 1)
    
    #guardar si no existe
    if (file.exists(ruta) == FALSE) {
      message("dato no existía, guardando...")
      # agregarle la fecha
      dato_nuevo <- dato_nuevo |> 
        mutate(fecha_scraping = Sys.Date())
      
      # guardar
      saveRDS(dato_nuevo, ruta)
      
    } else {
      
      # si dato existe, cargar dato anterior
      dato_anterior <- readRDS(ruta) |> select(-any_of("fecha_scraping"))
      
      # comparar dato nuevo con dato anterior
      if (length(all.equal(dato_nuevo, dato_anterior)) > 1) {
        message("dato ", ruta, " con diferencias: guardando...")
        
        # agregarle la fecha
        dato_nuevo <- dato_nuevo |> 
          mutate(fecha_scraping = Sys.Date())
        
        # guardar
        # saveRDS(dato_nuevo, ruta)
        write.csv2(dato_nuevo, ruta)
        
      } else {
        message("dato ", ruta, " sin diferencias, omitiendo")
      }
    }
  }, error = function(error) {
    warning(error)
  })
}





obtener_canasta <- function() {
  message("obteniendo canasta básica de alimentos desde GitHub (bastianolea/canasta_basica_chile)")
  url_canasta = "https://raw.githubusercontent.com/bastianolea/canasta_basica_chile/main/datos_procesados/canasta_basica_alimentos_2018-2024.csv" 
  
  read.csv2(url_canasta) |> tibble()
}


obtener_sueldo_minimo <- function() {
  message("obteniendo sueldo mínimo desde... wikipedia :(")
  
  url_sueldo_minimo <- "https://es.wikipedia.org/wiki/Anexo:Salario_mínimo_en_Chile#Sueldo_Mínimo_Nominal"
  
  sitio_sueldo_minimo <- session(url_sueldo_minimo) |> 
    read_html()
  
  tablas_sueldo_minimo <- sitio_sueldo_minimo |> 
    html_table()
  
  tabla_sueldo_minimo <- tablas_sueldo_minimo[[3]] |> 
    janitor::clean_names()
  
  sueldo_minimo_1 <- tabla_sueldo_minimo |> 
    select(fecha, monto_bruto, salario_real)
  
  sueldo_minimo_2 <- sueldo_minimo_1 |> 
    mutate(monto_bruto = str_remove(monto_bruto, "pesos") |> str_trim(),
           monto_bruto = str_replace(monto_bruto, ",", "\\."),
           monto_bruto = str_remove_all(monto_bruto, "\\."),
           monto_bruto = str_extract(monto_bruto, "\\d+"),
           monto_bruto = as.numeric(monto_bruto)) |> 
    mutate(salario_real = str_replace(salario_real, ",", "\\."),
           salario_real = str_remove_all(salario_real, "\\."),
           salario_real = as.numeric(salario_real))
  
  sueldo_minimo_3 <- sueldo_minimo_2 |> 
    mutate(año = str_extract(fecha, "\\d{4}")) |> 
    filter(año >= 2000,
           monto_bruto >= 100000,
           !is.na(monto_bruto))
  
  sueldo_minimo_4 <- sueldo_minimo_3 |> 
    mutate(mes = str_extract(fecha, "\\w{3,}")) |> 
    mutate(mes2 = recode(mes,
                         "enero" = 1,
                         "febrero" = 2,
                         "marzo" = 3,
                         "abril" = 4,
                         "mayo" = 5,
                         "junio" = 6,
                         "julio" = 7,
                         "agosto" = 8,
                         "septiembre" = 9,
                         "octubre" = 10,
                         "noviembre" = 11,
                         "diciembre" = 12)) |> 
    mutate(fecha = paste(año, mes2, 1, sep = "-"))
  
  return(sueldo_minimo_4)
}

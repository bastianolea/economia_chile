
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
         "sept" = 9,
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
    
    Sys.sleep(1)
    
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
  
  # revisiones
  stopifnot(any(is.na(dato_2$año)) == FALSE)
  
  if (chequear_missings_valor == TRUE) {
    # stopifnot(any(is.na(dato_2$valor)) == FALSE)
    if (any(is.na(dato_2$valor))) {
      warning("limpiar_tabla_bc(): datos perdidos en columna valor")
    }
  }
  
  dato_3 <- dato_2 |>
    filter(!is.na(valor))
  
  return(dato_3)  
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
      write.csv2(dato_nuevo, ruta)
      
    } else {
      
      # si dato existe, cargar dato anterior
      dato_anterior <- read.csv2(ruta) |> select(-any_of("fecha_scraping"))
      
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


descargar_inversion_extranjera <- function() {
  message("descargando inversión extranjera (Excel) desde web del Banco Central...")
  
  # descargar archivo
  url_inversion_extranjera = "https://si3.bcentral.cl/estadisticas/Principal1/excel/SE/BDP/Excel/Mensual/Cuenta_Financiera_categoria.xlsx"
  
  download.file(url_inversion_extranjera, 
                destfile = "fuentes/bc_inv_extranjera/Cuenta_Financiera_categoria.xlsx")
}


obtener_inversion_extranjera <- function(descargar = TRUE) {
  
  # descargar archivo excel
  if (descargar) {
    descargar_inversion_extranjera()
  }
  
  # cargar local    
  message("cargando excel...")
  invext <- read_excel("fuentes/bc_inv_extranjera/Cuenta_Financiera_categoria.xlsx",
                       .name_repair = "unique_quiet")
  
  message("limpiando...")
  
  # limpieza
  invext_0 <- invext |> 
    clean_names() |> 
    rename(x1 = 1) |> 
    rename_with(~str_replace(.x, "x", "col_"), everything())
  
  # fechas
  # hay que encontrar las filas que tienen los años y meses, que en el excel son las filas 5 y 6
  # pero como no podemos confiar en que siempre van a ser las filas 5 y 6, hay que detectar los años
  # independiente de la fila en al que puedan estar, y asumir que la fila de debajo del año será el mes
  
  # para encontrar la fila donde están los años, simplemente unimos todas las columnas en una sola para
  # poder buscar las filas donde haya un numero de 4 dígitos, y que sea entre 2000 y 2024. La primera fila
  # que cumpla con ese criterio será la fila de los años
  filas_con_años <- invext_0 |> 
    # unir todas las columnas en una sola
    unite(col = "todo", everything()) |> 
    # crear un índice con el numero de cada fila
    mutate(id = 1:n()) |> 
    # crear columna con un valor en cada fila que tenga un numero de 4 dígitos
    mutate(año = str_extract(todo, "\\d{4}")) |> 
    # confirmar que el numero encontrado sea un año
    mutate(año_confirmar = año >= 2000 & año <= format(Sys.Date(), "%Y"))
  
  # de las filas con años, dejar la primera coincidencia (porque los años están 
  # arriba de la tabla, obviamente) y obtener qué numero de fila era
  fila_año <- filas_con_años |> 
    filter(año_confirmar == TRUE) |> 
    slice(1) |> 
    pull(id)
  
  message("los años están en la fila ", fila_año)
  
  # años_invext <- t(invext_1)[,fila_año] |> unname()
  # meses_invext <- t(invext_1)[,fila_año+1] |> unname()
  
  # obtener la fila como un vector
  años_invext <- invext_0 |> 
    slice(fila_año) |> 
    unlist() |> 
    unname()
  
  meses_invext <- invext_0 |> 
    slice(fila_año+1) |> 
    unlist() |> 
    unname()
  
  stopifnot(length(meses_invext) > 12)
  stopifnot(length(años_invext) > 5)
  
  # listoco picoroco
  
  # cifras
  # como la tabla tiene variables en las filas, y las columnas son fechas,
  # hay que crear nombres de columnas, luego hacer que los textos de las primeras columnas
  # se rellenen hacia abajo para ir filtrando las variables (filas) de las primeras
  # columnas, dado que esas variables son como una especie de índice en cascada 
  
  # rellenar hacia abajo las primeras tres columnas
  invext_1 <- invext_0 |> 
    fill(col_1, col_2, col_3, 
         .direction = "down")
  
  invext_2 <- invext_1 |> 
    filter(str_detect(col_1, "^A.*")) |> 
    filter(str_detect(col_2, "(I|i)nversión.*(d|D)irecta"))
  
  invext_3 <- invext_2 |> 
    filter(str_detect(col_3, "(p|P)asivos"))
  
  cifras_invext <- invext_3 |> 
    slice(1) |> 
    unlist() |> 
    unname()
  
  stopifnot(length(cifras_invext) > 100)
  
  # unir columnas
  message("uniendo...")
  # unimos las columnas para obtenerun dataframe tidy
  invext_4 <- tibble("año" = años_invext,
                     "mes" = meses_invext,
                     "valor" = cifras_invext) 
  
  # dejamos solo datos 
  invext_5 <- invext_4 |> 
    fill(año, .direction = "down") |> 
    filter(nchar(año) == 4)
  
  # contemplar
  # invext_5 |> print(n=Inf)
  
  # limpiar datos
  invext_6 <- invext_5 |> 
    # convertir a formatos correctos
    mutate(año = as.numeric(año),
           mes_t = tolower(mes),
           valor = as.numeric(valor)) |> 
    # meses en número
    mutate(mes = mes_a_numeric(mes_t)) |> 
    # crear fecha
    # mutate(fecha = as.Date(paste(año, mes, 1), tryFormats = c("%Y %m %d"))) |> 
    mutate(fecha = paste(año, mes, 1, sep = "-")) |> 
    mutate(serie = "Inversión extranjera directa (pasivos)")
  
  stopifnot(nrow(invext_6) > 100)
  
  return(invext_6 |> select(serie, fecha,  año, mes, valor))
}


cargar_precio_cobre_anterior <- function() {
  message("cargando Excel del precio del cobre del año anterior...")
  
  indicador_cobre_0 <- read_excel("fuentes/bc_precio_cobre/Indicador.xls", 
                                  .name_repair = "unique_quiet")
  
  indicador_cobre_1 <- indicador_cobre_0 |> 
    row_to_names(3) |> 
    clean_names() |> 
    mutate(fecha = janitor::excel_numeric_to_date(as.numeric(dia))) |> 
    fill(valor, .direction = "downup") |> 
    mutate(fecha = as.character(fecha)) |> 
    mutate(año = str_extract(fecha, "\\d{4}"),
           mes = str_extract(fecha, "-\\d+-") |> str_remove_all("-"),
           dia = str_extract(fecha, "\\d+$")) |> 
    mutate(año = as.numeric(año),
           mes = as.numeric(mes),
           dia = as.numeric(dia)) |> 
    mutate(valor = as.numeric(valor))
  
  indicador_cobre_2 <- indicador_cobre_1 |> 
    filter(!is.na(valor), 
           !is.na(año),
           !is.na(mes),
           !is.na(dia))
  
  return(indicador_cobre_2)
}


obtener_precio_cobre <- function() {
  message("obteniendo precio del cobre desde web del Banco Central...")
  
  url_precio_cobre = "https://si3.bcentral.cl/Indicadoressiete/secure/Serie.aspx?gcode=LIBRA_COBRE&param=cgBnAE8AOQBlAGcAIwBiAFUALQBsAEcAYgBOAEkASQBCAEcAegBFAFkAeABkADgASAA2AG8AdgB2AFMAUgBYADIAQwBzAEEAMQBJAG8ATwBzAEgATABGAE4AagB1AFcAYgB2AFAAZwBhADIAbABWAHcAXwBXAGgATAAkAFIAVAB1AEIAbAB3AFoAdQBRAFgAZwA5AHgAdgAwACQATwBZADcAMwAuAGIARwBFAFIASwAuAHQA"
  
  año_e = 2024 #solo va a funcionar por 2024
  sitio_cobre <- session(url_precio_cobre) |> 
    read_html()
  
  tablas <- sitio_cobre |> 
    html_table()
  
  tabla_cobre <- tablas[[2]] |> 
    pivot_longer(cols = 2:length(tablas[[2]]), 
                 names_to = "mes", values_to = "valor") |> 
    rename(dia = 1)
  
  stopifnot(nrow(tabla_cobre) > 12)
  
  tabla_cobre_2 <- tabla_cobre |> 
    mutate(valor = cifra_comas_a_numeric(valor)) |> 
    mutate(con_dato = ifelse(!is.na(valor), TRUE, FALSE))
  
  tabla_cobre_3 <- tabla_cobre_2 |> 
    mutate(mes_t = mes,
           mes = str_extract(mes, "...") |> tolower() |> mes_a_numeric()) |> 
    mutate(año = año_e) |> 
    # mutate(fecha = as.Date(paste(2024, mes, dia), tryFormats = c("%Y %m %d")))
    mutate(fecha = paste(año, mes, dia, sep = "-"))
  
  tabla_cobre_4 <- tabla_cobre_3 |> 
    arrange(mes, dia) |> 
    fill(valor, .direction = "downup") |> 
    filter(as.Date(fecha) <= Sys.Date())
  
  # cargar valor del año pasado
  cobre_anterior <- cargar_precio_cobre_anterior()
  
  # anexar valor del año pasado
  tabla_cobre_5 <- bind_rows(tabla_cobre_4, cobre_anterior) |> 
    arrange(año, mes, dia) |> 
    mutate(serie = "Precio del cobre")
  
  stopifnot(nrow(tabla_cobre_5) > 12)
  
  return(tabla_cobre_5 |> select(serie, fecha, año, mes, valor, con_dato))
}
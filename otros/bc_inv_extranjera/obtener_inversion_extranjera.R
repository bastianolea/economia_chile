library(dplyr)
library(readxl)
library(janitor)
library(tidyr)
library(stringr)

source("funciones.R")

# obtener ----
# descargar archivo
url_inversion_extranjera = "https://si3.bcentral.cl/estadisticas/Principal1/excel/SE/BDP/Excel/Mensual/Cuenta_Financiera_categoria.xlsx"

download.file(url_inversion_extranjera, 
              destfile = "otros/bc_inv_extranjera/Cuenta_Financiera_categoria.xlsx")

# cargar ----
# cargar local              
invext <- read_excel("otros/bc_inv_extranjera/Cuenta_Financiera_categoria.xlsx",
                     .name_repair = "unique_quiet")

# limpieza ----

invext_0 <- invext |> 
  clean_names() |> 
  rename(x1 = 1) |> 
  rename_with(~str_replace(.x, "x", "col_"), everything())


# fechas ----
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
años_invext <- invext_1 |> 
  slice(fila_año) |> 
  unlist() |> 
  unname()

meses_invext <- invext_1 |> 
  slice(fila_año+1) |> 
  unlist() |> 
  unname()

# listoco picoroco



# cifras ----

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


# unir columnas ----
# unimos las columnas para obtenerun dataframe tidy
invext_4 <- tibble("año" = años_invext,
                   "mes" = meses_invext,
                   "valor" = cifras_invext) 

# dejamos solo datos 
invext_5 <- invext_4 |> 
  fill(año, .direction = "down") |> 
  filter(nchar(año) == 4)

# contemplar
invext_5 |> print(n=Inf)

# limpiar datos
invext_6 <- invext_5 |> 
  # convertir a formatos correctos
  mutate(año = as.numeric(año),
         mes_t = tolower(mes),
         valor = as.numeric(valor)) |> 
  # meses en número
  mutate(mes = mes_a_numeric(mes_t)) |> 
  # crear fecha
  mutate(fecha = as.Date(paste(año, mes, 1), tryFormats = c("%Y %m %d"))) |> 
  mutate(serie = "Inversión extranjera directa (pasivos)")

# guardar
write.csv2(invext_6 |> 
             select(serie, fecha, valor),
           "app/datos/inversion_extranjera.csv")

pib <- obtener_pib()

imacec <- obtener_imacec()

ipc <- obtener_ipc()

ipsa <- obtener_ipsa()

desempleo <- obtener_desempleo()

uf <- obtener_uf()



# pib |> 
#   ggplot(aes(as_date(fecha), valor,
#              color = serie)) +
#   geom_line()

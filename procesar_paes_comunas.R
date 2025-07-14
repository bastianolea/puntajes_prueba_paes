library(dplyr)
library(tidyr)

paes <- readr::read_csv2("datos/originales/A_INSCRITOS_PUNTAJES_PAES_2024_PUB_MRUN.csv") |> 
  janitor::clean_names()

paes |> glimpse()

# limpiar
paes_2 <- paes |> 
  select(año = anyo_proceso, 
         cod_sexo, 
         fecha_nacimiento, 
         codigo_comuna = codigo_comuna_egreso,
         promedio_notas,
         ends_with("actual"),
         -matches("_inv_"),
         -rindio_proceso_actual
         ) |> 
  filter(!is.na(codigo_comuna)) |> 
  mutate(across(where(is.numeric), ~na_if(.x, 0)))


# anexar comunas
cut_comuna <- readr::read_csv2("https://github.com/bastianolea/cut_comunas/raw/master/datos/cut_comuna.csv") |> 
  select(codigo_region, nombre_region, codigo_comuna, nombre_comuna) |> 
  mutate(codigo_comuna = as.numeric(codigo_comuna))

paes_3 <- paes_2 |> 
  left_join(cut_comuna, by = "codigo_comuna") |> 
  relocate(nombre_comuna, codigo_comuna, nombre_region, codigo_region, .before = 1)

# guardar base completa
readr::write_csv2(paes_3, "datos/puntajes_paes_2024.csv")
arrow::write_parquet(paes_3, "datos/puntajes_paes_2024.parquet")


# obtener promedios comunales
paes_4 <- paes_3 |> 
  group_by(nombre_comuna, codigo_comuna, nombre_region, codigo_region) |> 
  summarize(across(c(promedio_notas, ends_with("actual")), ~mean(.x, na.rm = T))) |> 
  group_by(nombre_comuna, codigo_comuna) |> 
  mutate(n_comuna = n()) |> 
  rename_with(~stringr::str_remove(.x, "_actual")) |> 
  rename_with(~stringr::str_remove(.x, "_reg")) |> 
  rename(paes_complectora = clec,
         paes_matematica1 = mate1,
         paes_matematica2 = mate2,
         paes_histciesoc = hcsoc,
         paes_ciencias = cien)

# guardar base por comunas
readr::write_csv2(paes_4, "datos/puntajes_paes_comuna_2024.csv")
arrow::write_parquet(paes_4, "datos/puntajes_paes_comuna_2024.parquet")

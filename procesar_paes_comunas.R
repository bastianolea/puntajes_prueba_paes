library(dplyr)
library(tidyr)

paes <- readr::read_csv2("datos_originales/PAES-2024-Inscritos-Puntajes/A_INSCRITOS_PUNTAJES_PAES_2024_PUB_MRUN.csv") |> 
  janitor::clean_names()

paes |> glimpse()

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
  mutate(across(where(is.numeric), ~na_if(.x, 0)))

# obtener promedios comunales
paes_3 <- paes_2 |> 
  group_by(codigo_comuna) |> 
  summarize(across(c(promedio_notas, ends_with("actual")), ~mean(.x, na.rm = T))) |> 
  rename_with(~stringr::str_remove(.x, "_actual")) |> 
  rename_with(~stringr::str_remove(.x, "_reg")) |> 
  rename(paes_complectora = clec,
         paes_matematica1 = mate1,
         paes_matematica2 = mate2,
         paes_histciesoc = hcsoc,
         paes_ciencias = cien)

# anexar comunas
cut_comuna <- read.csv2("datos_originales/comunas_chile_cut.csv")

paes_4 <- paes_3 |> 
  rename(cut_comuna = codigo_comuna) |> 
  left_join(cut_comuna, by = "cut_comuna") |> 
  relocate(comuna, cut_comuna, region, cut_region, .before = 1)

# guardar
readr::write_csv2(paes_4, "datos_procesados/puntajes_paes_comuna_2024.csv")

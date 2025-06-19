# ------------------------------------------------------------------------------
# Script: UCR_proder.R
# Purpose: Clean and construct variables from the PRODER 2019 dataset
# Author: L. Guillermo Woo-Mora
# Output: data/derived/proder.rds
# Description:
#   - Reads raw PRODER dataset and generates key demographic and socioeconomic variables
#   - Constructs standardized education measures for respondents and parents
#   - Calculates absolute intergenerational mobility (education)
#   - Standardizes and inverts ITA-based skin tone metrics from colorimeter readings
#   - Creates consistent skin tone measures: interviewer-rated, self-rated, and objective
#   - Adds contextual information from childhood: neighborhood, parental occupation, location
#   - Filters out extreme latitude/longitude outliers to remove data noise
# ------------------------------------------------------------------------------

proder <- 
  haven::read_dta("data/source/proder/baseproder2019.dta") |> 
  mutate(
    mujer = if_else(p0201 == 2, 1, 0),
    edad = p0202,
    # Years of schooling
    edc = case_when(
      p0204_nivel == 1 ~ 0,
      p0204_nivel == 2 ~ 0,
      p0204_nivel == 3 ~ p0204_grado,  # Primaria: 1 a 6 años
      p0204_nivel == 4 ~ 6 + p0204_grado,  # Secundaria: 7 a 9 años
      p0204_nivel == 5 ~ 9 + p0204_grado,  # Preparatoria o bachillerato: 10 a 12 años
      p0204_nivel == 6 ~ 9 + p0204_grado,  # Normal básica: 10 a 12 años
      p0204_nivel == 7 ~ 6 + p0204_grado,  # Técnicos con primaria terminada: 7 a 9 años
      p0204_nivel == 8 ~ 9 + p0204_grado,  # Técnicos con secundaria terminada: 10 a 12 años
      p0204_nivel == 9 ~ 12 + p0204_grado,  # Técnicos con preparatoria terminada: 13 a 15 años
      p0204_nivel == 10 ~ 16,  # Normal de licenciatura
      p0204_nivel == 11 ~ 16 + p0204_grado,  # Licenciatura o profesional: 17 años
      p0204_nivel == 12 ~ 18 + p0204_grado,  # Maestría: 19 años
      p0204_nivel == 13 ~ 22  # Doctorado
    ),
    # Years of schooling: Father
    edc_padre = case_when(
      p0505_nivel == 1 ~ 0,
      p0505_nivel == 2 ~ 0,
      p0505_nivel == 3 ~ p0505_grado,  # Primaria: 1 a 6 años
      p0505_nivel == 4 ~ 6 + p0505_grado,  # Secundaria: 7 a 9 años
      p0505_nivel == 5 ~ 9 + p0505_grado,  # Preparatoria o bachillerato: 10 a 12 años
      p0505_nivel == 6 ~ 9 + p0505_grado,  # Normal básica: 10 a 12 años
      p0505_nivel == 7 ~ 6 + p0505_grado,  # Técnicos con primaria terminada: 7 a 9 años
      p0505_nivel == 8 ~ 9 + p0505_grado,  # Técnicos con secundaria terminada: 10 a 12 años
      p0505_nivel == 9 ~ 12 + p0505_grado,  # Técnicos con preparatoria terminada: 13 a 15 años
      p0505_nivel == 10 ~ 16,  # Normal de licenciatura
      p0505_nivel == 11 ~ 16 + p0505_grado,  # Licenciatura o profesional: 17 años
      p0505_nivel == 12 ~ 18 + p0505_grado,  # Maestría: 19 años
      p0505_nivel == 13 ~ 22  # Doctorado
    ),
    # Years of schooling: Mother
    edc_madre = case_when(
      p0605_nivel == 1 ~ 0,
      p0605_nivel == 2 ~ 0,
      p0605_nivel == 3 ~ p0605_grado,  # Primaria: 1 a 6 años
      p0605_nivel == 4 ~ 6 + p0605_grado,  # Secundaria: 7 a 9 años
      p0605_nivel == 5 ~ 9 + p0605_grado,  # Preparatoria o bachillerato: 10 a 12 años
      p0605_nivel == 6 ~ 9 + p0605_grado,  # Normal básica: 10 a 12 años
      p0605_nivel == 7 ~ 6 + p0605_grado,  # Técnicos con primaria terminada: 7 a 9 años
      p0605_nivel == 8 ~ 9 + p0605_grado,  # Técnicos con secundaria terminada: 10 a 12 años
      p0605_nivel == 9 ~ 12 + p0605_grado,  # Técnicos con preparatoria terminada: 13 a 15 años
      p0605_nivel == 10 ~ 16,  # Normal de licenciatura
      p0605_nivel == 11 ~ 16 + p0605_grado,  # Licenciatura o profesional: 17 años
      p0605_nivel == 12 ~ 18 + p0605_grado,  # Maestría: 19 años
      p0605_nivel == 13 ~ 22  # Doctorado
    ),
    edc_padres = round((edc_padre + edc_madre)/2, 0),
    # Absolute mobility
    abs_ed_im = if_else(edc > edc_padres, 1, 0),
    # Skin tone measures
    color_int = as.numeric(p0106),
    color_int = max(color_int) + min(color_int) - color_int,
    color_resp = as.numeric(p0307),
    color_resp = max(color_resp) + min(color_resp) - color_resp,
    # Colorimeter
    colorimetro = as.numeric(p160103),
    color_hex_dor = p160101_codigohex,
    color_hex_mun = p160102_codigohex,
    ita_dor = atan((l_labd - 50) / b_labd) * (180 / pi),
    ita_mun = atan((l_labm - 50) / b_labm) * (180 / pi),
    ita_dor_invsd = -ita_dor,
    ita_dor_invsd = (ita_dor_invsd - mean(ita_dor_invsd, na.rm = T)) / sd(ita_dor_invsd, na.rm = T),  
    ita_mun_invsd = -ita_mun,
    ita_mun_invsd = (ita_mun_invsd - mean(ita_mun_invsd, na.rm = T)) / sd(ita_mun_invsd, na.rm = T),
    ita_invsd = round((ita_dor_invsd + ita_mun_invsd)/2, 2),
    # Neighborhood by fourteen
    barrio14yo = p0403,
    # Houshold head occupation by fourteen
    father_ocup14yo = p0508cod,
    mother_ocup14yo = p0608cod,
    # Logitude and latitud to double
    lon = as.double(longitud),
    lat = as.double(latitud),
    # Zip code by municipality
    cp_mun = str_c(mun, "-", cp)
  ) |> 
  filter(lon >= quantile(lon, .01, na.rm = T)) |> 
  filter(lat <= quantile(lat, .99, na.rm = T))

# Save data
write_rds(proder, "data/derived/proder.rds")

#
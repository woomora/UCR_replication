# ----------------------------------------------------------------------------
# --- LAPOP Data Cleaning and Harmonization Script ----
# Author: Guillermo Woo-Mora
# Paper: Unveiling the Cosmic Race: 
#        Skin Tone and Intergenerational Economic Disparities in Latin America and the Caribbean
# Description: This script compiles and cleans AmericasBarometer LAPOP datasets
#              from 2012, 2014, and 2016/2017 waves, producing a harmonized dataset
#              with standardized variables across countries and years.
# ----------------------------------------------------------------------------

# Fix select function override
select <- dplyr::select

# Read special functions
source("scripts/ucr_functions.R")

# ----------------------------------------------------------------------------
# --- Load Purchasing Power Parity (PPP) exchange rates from the World Bank ----
# ----------------------------------------------------------------------------
ppp <- read_csv("data/source/PPP World Bank/API_PA.NUS.PPP_DS2_en_csv_v2_1927762.csv", skip = 3) |> 
  clean_names() |> 
  rename(country = country_name, countrycode = country_code) |> 
  select(-country, -indicator_name, -indicator_code) |> 
  pivot_longer(!c(countrycode), names_to = "year", values_to = "pppexrate") |> 
  mutate(year = as.numeric(str_remove(year, "x"))) |> 
  filter(year >= 2010)

# ----------------------------------------------------------------------------
# --- Define Harmonized Variable Names Across LAPOP Waves ----
# ----------------------------------------------------------------------------
# Purpose: Create a consistent list of variable names to retain and harmonize
#          across all survey waves and country datasets in LAPOP AmericasBarometer.
# ----------------------------------------------------------------------------

# Vector of harmonized variable names
# 61 variables from raw data
variables_harmonized <- c(
  
  # --- Identifiers and Weights ---
  "idnum", "pais", "year", "estratopri", "weight1500",
  
  # --- Geographic Location ---
  "prov", "municipio", "cluster", "ur", "tamano",
  
  # --- Language and Demographics ---
  "q1", "q2", "ed", "edre", "q11", "q11n",
  "q12", "q12c", "q3c", "colorr", "etid", "leng1", "leng4",
  
  # --- Household Assets ---
  # 11 variables into one
  "r1", "r3", "r4", "r4a", "r5", "r6", "r7", "r8", "r12", "r14", "r15",
  
  # --- Occupation and Parental Background ---
  # 4 variables into 3
  "ocup1a", "ocup4a", "ocupoit", "ed2",
  
  # --- Income Variables ---
  # 12 variables into 2
  "q10new", "q10new_12", "q10new_14", "q10new_16", "q10new_18", "q10inc",
  "q10g", "q10g_12", "q10g_14", "q10g_16", "q10a", "q10e",
  
  # --- Interviewer Information ---
  "colori", "sexi", "intid",
  
  # --- National Pride and trust ---
  "b43", "mil5", "it1",
  
  # --- Discrimination Experiences ---
  "dis2", "dis3", "dis5"
)

#
# ----------------------------------------------------------------------------
# --- Merge all country-level LAPOP .sav files from selected waves into one dataset ----
# ----------------------------------------------------------------------------

# Initialize empty tibble to store appended data
lapop_raw <- tibble()

# List all .sav files (Stata/SPSS) recursively from the specified directory
list_of_files <- list.files(
  path = "data/source/lapop/merge_by_country",  # Folder containing country-level datasets
  recursive = TRUE,
  pattern = "\\.sav$",                          # Match only .sav files
  full.names = TRUE                             # Return full file paths
)

# Loop through each file and process relevant variables and waves
for (f in list_of_files) {
  
  # Extract 3-letter ISO country code from file path (positions 36–38 in the path string)
  countrycode <- str_sub(f, start = 36, end = 38)
  print(countrycode)  # Print progress for debugging
  
  # Read and filter raw data
  foo <- haven::read_sav(f) |>
    filter(year >= 2012, year <= 2017) |>             # Keep only 2012–2017 survey waves
    haven::as_factor(levels = c("default"), ordered = FALSE) |>  # Convert labelled vars to factors
    select(any_of(variables_harmonized)) |>           # Keep only harmonized variables
    mutate(
      pais = stringi::stri_trans_general(pais, "Latin-ASCII"),  # Remove accents from 'pais'
      countrycode = countrycode                                 # Add ISO3 country code as new variable
    )
  
  # Append to main tibble
  lapop_raw <- bind_rows(lapop_raw, foo)
}

# Ensure 'year' is numeric (in case it's been read as a labelled factor)
lapop_raw <- lapop_raw |> mutate(year = as.numeric(as.character(year)))

#
# ----------------------------------------------------------------------------
# --- LAPOP Data Cleaning ----
# ----------------------------------------------------------------------------
# Purpose: Standardize, harmonize, and clean LAPOP AmericasBarometer data
# ----------------------------------------------------------------------------

# --- Step 1: Rename raw variables to harmonized names ---
lapop <- lapop_raw |> 
  rename(
    sex = q1,
    age = q2,
    hh_kids = q12,
    hh_total = q12c,
    religion = q3c,
    etnia = etid,
    lengua = leng1,
    lengua_pad = leng4,
    assets_car = r5,
    assets_tv = r1,
    assets_ref = r3,
    assets_phone = r4,
    assets_cellphone = r4a,
    assets_washmch = r6,
    assets_microw = r7,
    assets_motorcyc = r8,
    assets_plumbing = r12,
    assets_bath = r14,
    assets_computer = r15,
    salary_status = ocup1a,
    occupa_status = ocup4a,
    mothers_ed = ed2,
    remittances = q10a,
    national_identity_proud = b43,
    national_identity_anthem = mil5,
    trust = it1,
    dis_gov = dis2,
    dis_school = dis3,
    dis_public = dis5
  )

# --- Step 2: Normalize factor levels and clean non-response values ---
lapop <- lapop |> 
  mutate_if(
    is.factor,
    ~ case_when(
      . %in% c(
        "NR", "DK", "N/A", "N/A Country", "No Response", "Don't Know", 
        "Not Applicable", "Not asked in this country or year", "Not asked in this year",
        "Could not be classified"
      )  ~ NA_character_,
      TRUE ~ as.factor(.)
    )
  )

# --- Step 3: Generate key identifiers, clean location names, derive regions ---
lapop <- lapop |> 
  mutate(
    idobs = 1:nrow(lapop_raw),
    sex = if_else(sex %in% c("Mujer", "Female"), "Female", "Male"),
    sexi = if_else(sexi %in% c("Mujer", "Female"), "Female", "Male"),
    # wt = as.numeric(wt),
    weight1500 = as.double(weight1500),
    country_mun = str_c(countrycode, "_", municipio) |> 
      str_replace_all(c("\\(" = "", "\\)" = "", "\\+" = "", "\\&" = "", "\\." = "", "," = "", "/" = "")) |> 
      str_replace_all(c(" " = "_", "-"  = "_")),
    country_prov = str_c(countrycode, "_", prov) |> 
      str_replace_all(c("\\(" = "", "\\)" = "", "\\+" = "", "\\&" = "", "\\." = "", "," = "", "/" = "")) |> 
      str_replace_all(c(" " = "_", "-"  = "_")),
    country_region = str_c(countrycode, "_", estratopri) |> 
      str_replace_all(c("\\(" = "", "\\)" = "", "\\+" = "", "\\&" = "", "\\." = "", "," = "", "/" = "")) |> 
      str_replace_all(c(" " = "_", "-"  = "_")),
    cluster_id = str_c(country_mun, "-", year, "-", cluster),
    region = case_when(
      countrycode %in% c("ARG", "BOL", "BRA", "CHL", "COL", "ECU", "GUY", "PER", "PRY", "SUR", "URY", "VEN") ~ "South America",
      countrycode %in% c("BLZ", "CRI", "GTM", "HND", "MEX", "NIC", "PAN", "SLV") ~ "Central_America", 
      TRUE ~ "Caribbean"
    )
  )

# --- Step 4: Recode household and demographic variables ---
lapop <- lapop |> 
  mutate(
    tamano = case_when(
      str_detect(tamano, "Capital") ~ "National Capital (Metropolitan Area)",
      str_detect(tamano, "Rural") ~ "Rural Area",
      str_detect(tamano, "medi") ~ "Medium City",
      tamano %in% c("Ciudad grande", "Large City") ~ "Large City",
      tamano %in% c("Ciudad pequeña", "Small City") ~ "Small City",
      TRUE ~ NA_character_
    ),
    tamano = fct_relevel(
      tamano, "Rural Area", "Small City", "Medium City", "Large City", "National Capital (Metropolitan Area)"
    ),
    age = as.numeric(age),
    year_born = year - age,
    cohort = case_when(
      year_born <= 1949 ~ "Before 1950s",
      year_born %in% 1950:1959 ~ "1950s",
      year_born %in% 1960:1969 ~ "1960s",
      year_born %in% 1970:1979 ~ "1970s",
      year_born %in% 1980:1989 ~ "1980s",
      year_born >= 1990 ~ "1990s",
      TRUE ~ NA_character_
    ),
    cohort = fct_relevel(
      cohort, "Before 1950s", "1950s", "1960s", "1970s", "1980s", "1990s"
    )
  )

# --- Step 5: Harmonize marital status and household size ---
lapop <- lapop |> 
  mutate(
    marital_status = q11,
    marital_status = if_else(is.na(marital_status), q11n, marital_status),
    marital_status = stringi::stri_trans_general(marital_status, "Latin-ASCII"),
    marital_status = case_when(
      marital_status %in% c("Soltero", "Solteiro", "Single", "Soltero [Pasar a Q12C]", "Soltero(a)",
                            "single", "Soltero  [Pasar a Q12C]", "Soltero (Omenda'yva)") ~ "Single",
      marital_status %in% c("Casado", "Casado (Omendava)", "Casado [Siga]", "Casado(a)", 
                            "married", "Married") ~ "Married",
      str_detect(marital_status, "(?i)Divorc") | str_detect(marital_status, "(?i)Separ") ~ "Divorced",
      str_detect(marital_status, "(?i)Viu") | str_detect(marital_status, "(?i)Widowed") ~ "Widowed",
      str_detect(marital_status, "(?i)Union") | str_detect(marital_status, "(?i)Common") ~ "Common law marriage",
      marital_status %in% c("Living together but not married", "Amigado(a) (casado na practica mas nao no papel)",
                            "Conviviente") ~ "Common law marriage",
      TRUE ~ NA_character_
    ),
    hh_kids = case_when(
      hh_kids %in% c("None", "Ninguno") ~ 0,
      hh_kids == "20+" ~ 20,
      hh_kids == "25+" ~ 26,
      TRUE ~ as.numeric(as.character(hh_kids))
    ),
    hh_total = case_when(
      hh_total %in% c("None", "Ninguno") ~ 0,
      hh_total == "20+" ~ 20,
      hh_total == "25+" ~ 26,
      TRUE ~ as.numeric(as.character(hh_total))
    ),
    hh_total = case_when(
      hh_total == 0 ~ 1,
      is.na(hh_total) & marital_status %in% c("Divorced", "Single", "Widowed") ~ 1,
      is.na(hh_total) & marital_status %in% c("Married", "Common law marriage") ~ 2,
      TRUE ~ hh_total
    )
  )

# --- Step 6: Recode and clean religion variable ---
lapop <- lapop |> 
  mutate(
    religion = stringi::stri_trans_general(religion, "Latin-ASCII"),
    religion = case_when(
      str_detect(religion, "(?i)Catolico|Catholic") ~ "Catholic",
      str_detect(religion, "(?i)Agnostic|Atheist|None|Ninguna") ~ "Agnostic",
      str_detect(religion, "(?i)Mormon|Mormones") ~ "Mormon",
      str_detect(religion, "(?i)Judio|Jewish") ~ "Jewish",
      str_detect(religion, "(?i)Jehova") ~ "Jehovah's Witness",
      str_detect(religion, "(?i)Protestant.*no") ~ "Protestant non-Evangelical",
      str_detect(religion, "(?i)Tradi") ~ "Traditional or Native Religion",
      str_detect(religion, "(?i)stian.*no") ~ "Non-Christian Eastern Religion",
      religion == "Evangelica y Pentecostal" ~ "Evangelical and Pentecostal",
      religion == "Espirita Kardecista" ~ "Other",
      str_detect(religion, "(?i)Other|Otro") ~ "Other",
      TRUE ~ religion
    )
  )

# --- Step 7: Clean and harmonize ethnicity variable ---
lapop <- lapop |> 
  mutate(
    etnia = stringi::stri_trans_general(etnia, "Latin-ASCII"),
    etnia = str_remove_all(etnia, '"'),
    etnia = case_when(
      etnia %in% c("White", "Mestizo", "Indigenous", "Black", "Other") ~ etnia,
      etnia %in% c("Mulatto", "Morena", "Mixed", "Criollo", "Hispanic or Latino", "Mixed Race", "Moreno", "Mestiza", "Mulata", "Mestiza/India", "Ladina") ~ "Mestizo",
      etnia %in% c("Amarela", "Indian", "Chinese", "Chino", "Asian", "Oriental", "Yellow", "Asian or Asian American", "Asiatico") ~ "Other",
      etnia %in% c("Hindu", "Hindustani (East Indians)", "East Indian", "Hindustani (East Indians)") ~ "Other",
      etnia %in% c("Otra", "Jewish") ~ "Other",
      etnia %in% c("Garifuna", "Maroons", "Zamba", "Afro-Surinamese/Creole", "Black or Afro-Guyanese", "Garifuna or black", "Negra", "Negra/dominicano negro") ~ "Black",
      etnia %in% c("Maya Ketchi", "Maya Mopan", "Maya Yucatec", "Quechua", "Aymara", "Amazonian", "Native American", "Indo-guyanés", "Amerindian", "De la Amazonia", "Indian (Indigenous)", "Indigena", "Indigena - originaria", "Indo-Guyanese") ~ "Indigenous",
      etnia %in% c("Syrian/Lebanese", "Mennonite", "Menonitas", "Javanese", "Middle Eastern", "Jews", "Creole") ~ "Other",
      etnia %in% c("Portuguese", "Español", "Hispanic", "Spanish", "Blanca", "Caucasian (White)", "Portugues") ~ "White",
      TRUE ~ NA_character_
    ),
    etnia = fct_relevel(etnia, "White", "Mestizo", "Indigenous", "Black", "Other")
  )

# --- Step 8: Clean and top-code skin tone variable ---
lapop <- lapop |> 
  mutate(
    colorr = stringi::stri_trans_general(colorr, "Latin-ASCII"),
    colorr = str_to_lower(colorr),
    colorr = case_when(
      colorr %in% c("very light", "mas claro") ~ 1,
      colorr %in% c("very dark", "mas oscuro") ~ 11,
      TRUE ~ as.numeric(as.character(colorr))
    ),
    colorr_topc = if_else(colorr >= 9, 9, colorr, missing = NA_real_)
  )

# --- Step 9: Prepare education variables and ISCED levels ---
lapop <- lapop |> 
  mutate(
    edc = case_when(
      ed %in% c("None", "Ninguno") ~ 0,
      ed == "18+" ~ 18,
      TRUE ~ as.numeric(as.character(ed))
    ),
    ed_isced = case_when(
      edc < 6 ~ "Less than primary",
      edc == 6 ~ "Primary",
      edc >= 7 & edc <= 9 ~ "Lower secondary",
      edc >= 10 & edc <= 12 ~ "Upper secondary",
      edc >= 13 ~ "Tertiary",
      TRUE ~ NA_character_
    ),
    ed_isced = fct_relevel(ed_isced, "Less than primary", "Primary", "Lower secondary", "Upper secondary", "Tertiary"),
    ed_isced_tertiary = if_else(edc >= 13, 1, 0),
    grad_student = case_when(
      occupa_status == "Studying" & edc >= 13 ~ 1,
      TRUE ~ 0
    ),
    gdim_sample = case_when(
      age > 21 | occupa_status != "Studying" ~ 1,
      grad_student == 1 ~ 1,
      TRUE ~ 0
    )
  )

# --- Step 10: Process mother's education variables (detailed and ISCED) ---
lapop <- lapop |> 
  mutate(
    mothers_ed = case_when(
      mothers_ed == "Ninguno" ~ "None",
      mothers_ed == "Primaria incompleta" ~ "Primary incomplete",
      mothers_ed == "Primaria completa" ~ "Primary complete",
      mothers_ed %in% c("Secundaria o bachillerato incompleto", "Secundaria incompleta") ~ "Secondary incomplete",
      mothers_ed %in% c("Secundaria o bachillerato completo", "Secundaria completa") ~ "Secondary complete",
      mothers_ed == "Técnica/Tecnológica incompleta" ~ "Technical school/Associate degree incomplete",
      mothers_ed == "Técnica/Tecnológica completa" ~ "Technical school/Associate degree complete",
      mothers_ed == "Universitaria incompleta" ~ "University incomplete",
      mothers_ed == "Universitaria completa" ~ "University complete",
      TRUE ~ mothers_ed
    ),
    mothers_ed = fct_relevel(
      mothers_ed,
      "None",
      "Primary incomplete",
      "Primary complete",
      "Secondary incomplete",
      "Secondary complete",
      "Technical school/Associate degree incomplete",
      "Technical school/Associate degree complete",
      "University incomplete",
      "University complete"
    ),
    mothers_edc = case_when(
      mothers_ed == "None" ~ 0,
      mothers_ed == "Primary incomplete" ~ 3,
      mothers_ed == "Primary complete" ~ 6,
      mothers_ed == "Secondary incomplete" ~ 7.5,
      mothers_ed == "Secondary complete" ~ 9,
      mothers_ed == "Technical school/Associate degree incomplete" ~ 10.5,
      mothers_ed == "Technical school/Associate degree complete" ~ 12,
      mothers_ed == "University incomplete" ~ 14,
      mothers_ed == "University complete" ~ 16,
      TRUE ~ NA_real_
    ),
    mothers_ed_isced = case_when(
      mothers_ed %in% c("None", "Primary incomplete") ~ "Less than primary",
      mothers_ed %in% c("Primary complete") ~ "Primary",
      mothers_ed %in% c("Secondary incomplete") ~ "Lower secondary",
      mothers_ed %in% c("Secondary complete",  "Technical school/Associate degree incomplete", "University incomplete") ~ "Upper secondary",
      mothers_ed %in% c("Technical school/Associate degree complete", "University complete") ~ "Tertiary",
      TRUE ~ NA_character_
    ),
    mothers_ed_isced = fct_relevel(
      mothers_ed_isced, 
      "Less than primary",
      "Primary",
      "Lower secondary",
      "Upper secondary",
      "Tertiary"
    )
  ) |> 
  # Educational mobility: by country and cohort
  group_by(countrycode, cohort) |> 
  mutate(
    mothers_ed_pct = weighted_ntile(mothers_edc, weights = weight1500, 100),
    ed_pct = weighted_ntile(edc,  weights = weight1500, 100),
  ) |> 
  ungroup()

# --- Step 11: Absolute Intergenerational Mobility (Education) ---
lapop <- lapop |> 
  mutate(
    abs_ed_im = case_when(
      mothers_ed_isced != "Tertiary" & as.numeric(ed_isced) > as.numeric(mothers_ed_isced) ~ 1,
      mothers_ed_isced != "Tertiary" & as.numeric(ed_isced) <= as.numeric(mothers_ed_isced) ~ 0,
      mothers_ed_isced == "Tertiary" & as.numeric(ed_isced) == as.numeric(mothers_ed_isced) ~ 1,
      mothers_ed_isced == "Tertiary" & as.numeric(ed_isced) <= as.numeric(mothers_ed_isced) ~ 0,
      TRUE ~ NA_real_
    )
  )

# --- Step 12: Interviewer Skin Tone ---
lapop <- lapop |> 
  mutate(
    colori = stringi::stri_trans_general(colori, "Latin-ASCII"),
    colori = str_to_lower(colori),
    colori = case_when(
      colori %in% c("very light", "mas claro") ~ 1,
      colori %in% c("very dark", "mas oscuro") ~ 11,
      TRUE ~ as.numeric(as.character(colori))
    )
  )

# --- Step 13: National Identity Variables ---
lapop <- lapop |> 
  mutate(
    national_identity_proud = str_remove_all(national_identity_proud, "\\("),
    national_identity_proud = str_remove_all(national_identity_proud, "\\)"),
    national_identity_proud = case_when(
      str_detect(national_identity_proud, "Nada") ~ "1",
      str_detect(national_identity_proud, "(?i)not at") ~ "1",
      str_detect(national_identity_proud, "Mucho") ~ "7",
      str_detect(national_identity_proud, "(?i)lot") ~ "7",
      TRUE ~ national_identity_proud
    ),
    national_identity_proud = as.numeric(national_identity_proud),
    
    national_identity_anthem = stringi::stri_trans_general(national_identity_anthem, "Latin-ASCII"),
    national_identity_anthem = case_when(
      str_detect(national_identity_anthem, "(?i)Algo orgul") ~ "0",
      str_detect(national_identity_anthem, "(?i)Somewhat proud") ~ "0",
      str_detect(national_identity_anthem, "(?i)Do you not care") ~ "0",
      str_detect(national_identity_anthem, "(?i)O no le importa?") ~ "0",
      str_detect(national_identity_anthem, "(?i)or You do not care?") ~ "0",
      str_detect(national_identity_anthem, "(?i)Nao se importa?") ~ "0",
      str_detect(national_identity_anthem, "(?i)Nada orgul") ~ "0",
      str_detect(national_identity_anthem, "(?i)Not at all proud") ~ "0",
      TRUE ~ "1"
    ),
    national_identity_anthem = as.numeric(national_identity_anthem)
  )

# --- Step 14: Interpersonal Trust Variable ---
lapop <- lapop |> 
  mutate(
    trust = case_when(
      str_detect(trust, "Algo confiable") ~ "Somewhat trustworthy",
      str_detect(trust, "Muy confiable") ~ "Very trustworthy",
      str_detect(trust, "Nada confiable") ~ "Untrustworthy",
      str_detect(trust, "Poco confiable") ~ "Not very trustworthy",
      TRUE ~ trust
    ),
    trust = case_when(
      trust == "Very trustworthy" ~ 4,
      trust == "Somewhat trustworthy" ~ 3,
      trust == "Not very trustworthy" ~ 2,
      trust == "Untrustworthy" ~ 1,
      TRUE ~ NA_real_
    )
  )

# --- Step 15: Asset Ownership Indicators ---
lapop <- lapop |> 
  mutate_at(
    vars(
      assets_tv, assets_ref, assets_phone, assets_cellphone, assets_washmch,
      assets_microw, assets_motorcyc, assets_plumbing, assets_bath, assets_computer,
      remittances, dis_gov, dis_school, dis_public
    ),
    ~ case_when(
      . %in% c("Yes", "Sí", "Si") ~ 1,
      . == "No" ~ 0,
      TRUE ~ NA_real_
    )
  ) |> 
  mutate(
    assets_car = case_when(
      assets_car %in% c("No") ~ 0,
      assets_car %in% c("Uno", "One") ~ 1,
      assets_car %in% c("Dos", "Two") ~ 2,
      assets_car %in% c("Tres o más", "Three or more") ~ 3,
    )
  )

# --- Step 17: Harmonize household income variables across years ---
lapop <- lapop |> 
  mutate(
    hhincome = case_when(
      year == 2012 ~ q10new_12,
      year == 2014 ~ q10new_14,
      year >= 2016 ~ q10new_16
    ),
    hhincome = stringi::stri_trans_general(hhincome, "Latin-ASCII"),
    hhincome = str_remove_all(hhincome, "\\$|\\,|\\."),
    hhincome = str_replace_all(hhincome, " – | - | and | y | a | to ", "-"),
    hhincome = str_replace(hhincome, "\\[..\\]", ""),
    hhincome = case_when(
      str_detect(hhincome, "De ") ~ str_c("-", str_remove_all(hhincome, "De ")),
      str_detect(hhincome, "Mas de|More than|Above") ~ str_c(str_remove_all(hhincome, "Mas de|More than|Above"), "-"),
      str_detect(hhincome, "Menos de|Less than|Below") ~ str_c("-", str_remove_all(hhincome, "Menos de|Less than|Below")),
      TRUE ~ hhincome
    ),
    hhincome = str_remove_all(hhincome, "(?i)Entre|De|Between|From"),
    hhincome = str_remove_all(hhincome, "(?i)pesos|soles|quetzales|cordobas|gourdes|gours|Bs|L|BZ|SRD|R"),
    hhincome = case_when(
      str_detect(hhincome, "(?i)No income|Ningun ingreso|None") ~ "0-",
      TRUE ~ hhincome
    ),
    hhincome = case_when(
      str_count(hhincome, "-") == 0 ~ str_c(hhincome, "-"),
      str_count(hhincome, "-") == 2 ~ str_remove(hhincome, "-"),
      TRUE ~ hhincome
    ),
    hhincome = str_remove_all(hhincome, " "),
    hhincome_lb = str_split(hhincome, "-", 2, simplify = TRUE)[,1],
    hhincome_ub = str_split(hhincome, "-", 2, simplify = TRUE)[,2],
    hhincome = case_when(
      !is.na(hhincome_lb) & !is.na(hhincome_ub) ~ (as.numeric(hhincome_lb) + as.numeric(hhincome_ub)) / 2,
      hhincome_lb == "" & !is.na(hhincome_ub) ~ as.numeric(hhincome_ub),
      is.na(hhincome_lb) ~ NA_real_
    )
  ) |> 
  select(-hhincome_lb, -hhincome_ub)

# --- Step 18: Harmonize personal income variables across years ---
lapop <- lapop |> 
  mutate(
    income = case_when(
      year == 2012 ~ q10g_12,
      year == 2014 ~ q10g_14,
      year >= 2016 ~ q10g_16
    ),
    income = stringi::stri_trans_general(income, "Latin-ASCII"),
    income = str_remove_all(income, "\\$|\\,|\\."),
    income = str_replace_all(income, " – | - | and | y | a | to ", "-"),
    income = str_replace(income, "\\[..\\]", ""),
    income = case_when(
      str_detect(income, "De ") ~ str_c("-", str_remove_all(income, "De ")),
      str_detect(income, "Mas de|More than|Above") ~ str_c(str_remove_all(income, "Mas de|More than|Above"), "-"),
      str_detect(income, "Menos de|Less than|Below") ~ str_c("-", str_remove_all(income, "Menos de|Less than|Below")),
      TRUE ~ income
    ),
    income = str_remove_all(income, "(?i)Entre|De|Between|From"),
    income = str_remove_all(income, "(?i)pesos|soles|quetzales|cordobas|gourdes|gours|Bs|L|BZ|SRD|R"),
    income = case_when(
      str_detect(income, "(?i)No income|Ningun ingreso|None") ~ "0-",
      TRUE ~ income
    ),
    income = case_when(
      str_count(income, "-") == 0 ~ str_c(income, "-"),
      str_count(income, "-") == 2 ~ str_remove(income, "-"),
      TRUE ~ income
    ),
    income = str_remove_all(income, " "),
    income_lb = str_split(income, "-", 2, simplify = TRUE)[,1],
    income_ub = str_split(income, "-", 2, simplify = TRUE)[,2],
    income = case_when(
      !is.na(income_lb) & !is.na(income_ub) ~ (as.numeric(income_lb) + as.numeric(income_ub)) / 2,
      income_lb == "" & !is.na(income_ub) ~ as.numeric(income_ub),
      is.na(income_lb) ~ NA_real_
    )
  ) |> 
  select(-income_lb, -income_ub)

# --- Step 19: Convert incomes to PPP-adjusted values and derive per capita income ---
lapop <- lapop |> 
  left_join(ppp) |> 
  mutate(
    income = income / pppexrate,
    hhincome = hhincome / pppexrate,
    hhincomepc = hhincome / hh_total,
    hhincomepc_mx0 = calibrate_ext_margin(hhincomepc),
    income_mx0 = calibrate_ext_margin(income)
  )

# ----------------------------------------------------------------------------
# --- Extra code tasks ----
# ----------------------------------------------------------------------------
# --- Identify Most Common Language per Country (Weighted by Survey Weights) ----
# ----------------------------------------------------------------------------

# --- Step 1: Compute most common language by country using survey weights ---
most_common_language <- lapop |>
  filter(!is.na(lengua)) |> 
  group_by(countrycode, lengua) |> 
  summarise(
    weighted_n = sum(weight1500, na.rm = TRUE), .groups = "drop"
  ) |> 
  group_by(countrycode) |> 
  slice_max(order_by = weighted_n, n = 1, with_ties = FALSE) |> 
  select(countrycode, lengua) |> 
  rename(lengua_most_common_label = lengua)

# --- Step 2: Flag respondents speaking the most common language ---
lapop <- lapop |> 
  left_join(most_common_language, by = "countrycode") |> 
  mutate(
    lengua_most_common = if_else(lengua == lengua_most_common_label, 1, 0)
  ) |> 
  select(-lengua_most_common_label)

# Clean up temporary object
rm(most_common_language)

#

# ----------------------------------------------------------------------------
# --- Construct Household Wealth Index via PCA on Durable Assets ----
# ----------------------------------------------------------------------------
# Note: PCA is performed separately by country to avoid cross-country comparability bias.
# Assets included: TV, Refrigerator, Washing Machine, Microwave, Landline Phone, Cellphone,
# Number of Cars, Motorcycle, Drinking Water, Bathroom, Computer
# ----------------------------------------------------------------------------

# Initialize output object
hh_asset_index <- tibble()

# Loop over each country
for (p in unique(lapop$countrycode)) {
  
  message("Processing country: ", p)
  
  # Subset country-level data
  foo <- lapop |> filter(countrycode == p)
  
  if (nrow(foo) > 0) {
    
    # Select required variables and drop incomplete cases
    foo1 <- foo |> 
      select(
        idobs, hhincomepc_mx0,
        assets_car, assets_tv, assets_ref, assets_phone, assets_cellphone,
        assets_washmch, assets_microw, assets_motorcyc, assets_plumbing,
        assets_bath, assets_computer
      ) |> 
      drop_na() |> 
      as.data.frame()
    
    if (nrow(foo1) > 0) {
      
      # Run PCA on asset variables (scaled)
      pca1 <- prcomp(foo1[ , -c(1:2)], center = TRUE, scale. = TRUE)
      pc_scores <- as_tibble(predict(pca1))
      
      # Flip sign of PC1 if negatively correlated with income
      foo1 <- foo1 |> 
        mutate(
          ind_riq = pc_scores$PC1,
          cor = cor(ind_riq, hhincomepc_mx0),
          ind_riq = if_else(cor > 0, ind_riq, -ind_riq)
        ) |> 
        select(idobs, ind_riq, hhincomepc_mx0) |> 
        as_tibble()
      
      # Append to index
      hh_asset_index <- bind_rows(hh_asset_index, foo1)
    }
  }
}

# Clean up intermediate objects
rm(foo, foo1, pca1, pc_scores)

# Merge asset-based wealth index back into main dataset
lapop <- lapop |> left_join(hh_asset_index |> select(-hhincomepc_mx0), by = "idobs")

#

# ----------------------------------------------------------------------------
# --- Standardize Province Names and Recategorize Regions ----
# ----------------------------------------------------------------------------
# This step ensures consistent province names ("prov") and regional groupings ("country_region")
# across all countries and survey waves. Manual recoding is critical to ensure geographic
# comparability, especially where raw data includes codes or inconsistent labels.
# ----------------------------------------------------------------------------

lapop <- 
  lapop %>% 
  mutate(
    # Ensure province is character type
    prov = as.character(prov),
    
    # Mexico
    prov = case_when(
      countrycode == "MEX" & prov == "Estado de México" ~ "México",
      TRUE ~ prov
    ),
    
    # Argentina 
    prov = case_when(
      countrycode == "ARG" & prov == "1701" ~ "AMBA",
      countrycode == "ARG" & prov == "1702" ~ "Córdoba",
      countrycode == "ARG" & prov == "1703" ~ "Santa Fe",
      countrycode == "ARG" & prov == "1704" ~ "Entre Ríos",
      countrycode == "ARG" & prov == "1706" ~ "Corrientes",
      countrycode == "ARG" & prov == "1707" ~ "Chaco",
      countrycode == "ARG" & prov == "1709" ~ "Salta",
      countrycode == "ARG" & prov == "1710" ~ "Santiago del Estero",
      countrycode == "ARG" & prov == "1711" ~ "Tucumán",
      countrycode == "ARG" & prov == "1714" ~ "Mendoza",
      countrycode == "ARG" & prov == "1715" ~ "San Juan",
      countrycode == "ARG" & prov == "1719" ~ "Neuquén",
      countrycode == "ARG" & prov == "1720" ~ "Río Negro",
      countrycode == "ARG" & prov == "1722" ~ "Prov. de Buenos Aires",
      countrycode == "ARG" & prov == "1723" ~ "La Pampa",
      TRUE ~ prov
    ),
    
    country_region = case_when(
      countrycode == "ARG" & prov == "Ciudad de Buenos Aires" ~ "ARG_AMBA_Capital_Federal_y_GBA",
      countrycode == "ARG" & prov == "Buenos Aires" ~ "ARG_Prov_de_Buenos_Aires",
      
      countrycode == "ARG" & prov == "San Luis" ~ "ARG_Cuyo",
      
      countrycode == "ARG" & prov == "Chubut" ~ "ARG_Patagonia",
      countrycode == "ARG" & prov == "Santa Cruz" ~ "ARG_Patagonia",
      countrycode == "ARG" & prov == "Tierra del Fuego" ~ "ARG_Patagonia",
      
      countrycode == "ARG" & prov == "Catamarca" ~ "ARG_NOA",
      countrycode == "ARG" & prov == "Jujuy" ~ "ARG_NOA",
      countrycode == "ARG" & prov == "La Rioja" ~ "ARG_NOA",
      
      countrycode == "ARG" & prov == "Misiones" ~ "ARG_NEA",
      countrycode == "ARG" & prov == "Formosa" ~ "ARG_NEA",
      TRUE ~  country_region 
    ),
    
    country_region = case_when(
      countrycode == "ARG" & country_region == "ARG_Capital_Federal" ~ "ARG_AMBA_Capital_Federal_y_GBA",
      countrycode == "ARG" & country_region == "ARG_GBA" ~ "ARG_AMBA_Capital_Federal_y_GBA",
      TRUE ~ country_region
    ),
    
    
    # Bolivia 
    prov = case_when(
      countrycode == "BOL" & prov == "Beni" ~ "El Beni",
      TRUE ~ prov
    ),
    
    # Brazil
    
    # Chile,
    prov = case_when(
      countrycode == "CHL" & prov == "1301" ~ "Tarapaca",
      countrycode == "CHL" & prov == "1302" ~ "Antofagasta",
      countrycode == "CHL" & prov == "1303" ~ "Atacama",
      countrycode == "CHL" & prov == "1304" ~ "Coquimbo",
      countrycode == "CHL" & prov == "1305" ~ "Valparaiso",
      
      countrycode == "CHL" & prov == "1306" ~ "Libertador General Bernardo O'Higgins",
      countrycode == "CHL" & prov == "1307" ~ "Maule",
      countrycode == "CHL" & prov == "1308" ~ "Bio-Bio (Nuble)",
      countrycode == "CHL" & prov == "1309" ~ "Araucania",
      
      countrycode == "CHL" & prov == "1310" ~ "Los Lagos",
      countrycode == "CHL" & prov == "1311" ~ "Aisen del General Carlos Ibanez del Campo",
      countrycode == "CHL" & prov == "1312" ~ "Magallanes y Antartica Chilena",
      
      countrycode == "CHL" & prov == "1313" ~ "Region Metropolitana de Santiago",
      countrycode == "CHL" & prov == "1314" ~ "Los Rios",
      countrycode == "CHL" & prov == "1315" ~ "Arica y Parinacota",
      TRUE ~ prov,
    ),
    country_region = case_when(
      countrycode == "CHL" & str_detect(country_region, "Central") == T ~ "CHL_Central",
      countrycode == "CHL" & str_detect(country_region, "North") == T ~ "CHL_North",
      countrycode == "CHL" & str_detect(country_region, "South") == T ~ "CHL_South",
      TRUE ~ country_region
    ),
    country_region = if_else(
      countrycode == "CHL" & prov == "Region Metropolitana de Santiago", 
      "CHL_Metropolitan", country_region),
    
    # Colombia
    prov = case_when(
      countrycode == "COL" & prov == "Bogotá, D.C." ~ "Bogota",
      TRUE ~ prov
    ),
    
    country_region = case_when(
      countrycode == "COL" & prov == "La Guajira" ~ "COL_Atlántica",
      countrycode == "COL" & prov == "Norte de Santander" ~ "COL_Oriental",
      countrycode == "COL" & prov == "Choco" ~ "COL_Pacífica",
      countrycode == "COL" & prov == "Valle del Cauca" ~ "COL_Pacífica",
      countrycode == "COL" & prov == "Quindio" ~ "COL_Central",
      
      TRUE ~ country_region,
    ),
    
    country_region = if_else(
      countrycode == "COL" & prov == "Bogota",
      "COL_Bogotá",
      country_region
    ),
    
    # Costa Rica
    
    # Domnican Republic
    prov = case_when(
      countrycode == "DOM" & prov == "San Pedro de M." ~ "San Pedro de Macoris",
      countrycode == "DOM" & prov == "San Juan de la M." ~ "San Juan",
      countrycode == "DOM" & prov == "Ma. T. Sánchez" ~ "Maria Trinidad Sanchez",
      TRUE ~ prov
    ),
    
    country_region = case_when(
      countrycode == "DOM" & prov %in% c("Santo Domingo", "Distrito Nacional") ~ "DOM_Metropolitana",
      countrycode == "DOM" & prov %in% c("Ma. T. Sánchez", "San Pedro de M.", 
                                         "El Seybo", "La Romana", "La Altagracia") ~ "DOM_Este",
      countrycode == "DOM" & prov %in% c("Pedernales", "Independencia", 
                                         "Bahoruco", "Barahona",
                                         "Azua", "San Juan", "La Estrelleta", 
                                         "San Jose de Ocoa", "San Cristobal", "Peravia") ~ "DOM_Sur",
      countrycode == "DOM" & !(country_region %in% c("DOM_Sur", "DOM_Este", "DOM_Metropolitana")) ~ "DOM_Norte",
      TRUE ~ country_region
    ),
    
    # Dominica
    
    # Ecuador
    country_region = case_when(
      countrycode == "ECU" & prov == "Orellana" ~ "ECU_Oriente",
      countrycode == "ECU" & prov == "Pastaza" ~ "ECU_Oriente",
      
      countrycode == "ECU" & prov == "Santo Domingo de los Tsachilas" ~ "ECU_Sierra",
      countrycode == "ECU" & prov == "Carchi" ~ "ECU_Sierra",
      
      TRUE ~ country_region
    ),
    
    # El Salvador
    country_region = case_when(
      countrycode == "SLV" & prov %in% c("Chalatenango", "La Libertad", "Cuscatlan", "San Salvador") ~ "SLV_Región_2_Central_1",
      countrycode == "SLV" & prov == "AMSS" ~ "SLV_Región_5_AMSS",
      TRUE ~ country_region),
    
    country_region = case_when(
      countrycode == "SLV" & country_region == "SLV_Región_1_Occidental" ~ "SLV_Occidental",
      countrycode == "SLV" & country_region == "SLV_Región_2_Central_1" ~ "SLV_Central1",
      countrycode == "SLV" & country_region == "SLV_Región_3_Central_2" ~ "SLV_Central2",
      countrycode == "SLV" & country_region == "SLV_Región_4_Oriental" ~ "SLV_Oriental",
      countrycode == "SLV" & country_region == "SLV_Región_5_AMSS" ~ "SLV_AMSS",
      TRUE ~ country_region,
    ),
    
    # Guatemala
    prov = case_when(
      countrycode == "GTM" & prov == "201" ~ "Guatemala",
      countrycode == "GTM" & prov == "202" ~ "El Progreso",
      countrycode == "GTM" & prov == "203" ~ "Sacatepequez",
      countrycode == "GTM" & prov == "204" ~ "Chimaltenango",
      countrycode == "GTM" & prov == "205" ~ "Escuintla",
      countrycode == "GTM" & prov == "206" ~ "Santa Rosa",
      countrycode == "GTM" & prov == "207" ~ "Solola",
      countrycode == "GTM" & prov == "208" ~ "Totonicapan",
      
      countrycode == "GTM" & prov == "209" & country_region == "GTM_Sur" ~ "Quezaltenango Sur",
      countrycode == "GTM" & prov == "209" & country_region == "GTM_Noroccidente" ~ "Quezaltenango",
      
      countrycode == "GTM" & prov == "210" ~ "Suchitepequez",
      countrycode == "GTM" & prov == "211" ~ "Retalhuleu",
      
      countrycode == "GTM" & prov == "212" & country_region == "GTM_Sur" ~ "San Marcos Sur",
      countrycode == "GTM" & prov == "212" & country_region == "GTM_Noroccidente" ~ "San Marcos",
      
      countrycode == "GTM" & prov == "213" ~ "Huehuetenango",
      countrycode == "GTM" & prov == "214" ~ "Quiche",
      countrycode == "GTM" & prov == "215" ~ "Baja Verapaz",
      countrycode == "GTM" & prov == "216" ~ "Alta Verapaz",
      countrycode == "GTM" & prov == "217" ~ "Peten",
      countrycode == "GTM" & prov == "218" ~ "Izabal",
      countrycode == "GTM" & prov == "219" ~ "Chiquimula",
      countrycode == "GTM" & prov == "220" ~ "Jalapa",
      countrycode == "GTM" & prov == "221" ~ "Jutiapa",
      TRUE ~ prov
    ),
    
    country_region = if_else(
      countrycode == "GTM" & prov == "Jalapa", "GTM_Sur", country_region
    ),
    
    # Haiti
    country_region = case_when(
      countrycode == "HIT" & country_region == "HTI_2201" ~ "HTI_Metropolitan_Area",
      countrycode == "HIT" & country_region == "HTI_2202" ~ "HTI_Northern",
      countrycode == "HIT" & country_region == "HTI_2203" ~ "HTI_Central",
      countrycode == "HIT" & country_region == "HTI_2204" ~ "HTI_Rest_of_West",
      countrycode == "HIT" & country_region == "HTI_2205" ~ "HTI_Southern",
      TRUE ~ country_region),
    
    # Haiti
    country_region = case_when(
      countrycode == "HTI" & country_region == "HTI_2201" ~ "HTI_Metropolitan_Area",
      countrycode == "HTI" & country_region == "HTI_2202" ~ "HTI_Northern",
      countrycode == "HTI" & country_region == "HTI_2203" ~ "HTI_Central",
      countrycode == "HTI" & country_region == "HTI_2204" ~ "HTI_Rest_of_West",
      countrycode == "HTI" & country_region == "HTI_2205" ~ "HTI_Southern",
      TRUE ~ country_region),
    
    # Honduras
    prov = case_when(
      countrycode == "HND" & prov == "401" ~ "Francisco Morazan",
      countrycode == "HND" & prov == "402" ~ "Comayagua",
      countrycode == "HND" & prov == "403" ~ "La Paz",
      countrycode == "HND" & prov == "404" ~ "Cortes",
      countrycode == "HND" & prov == "405" ~ "Atlantida",
      countrycode == "HND" & prov == "406" ~ "Colon",
      countrycode == "HND" & prov == "407" ~ "Yoro",
      countrycode == "HND" & prov == "408" ~ "Islas de la Bahia",
      countrycode == "HND" & prov == "409" ~ "Copan",
      countrycode == "HND" & prov == "410" ~ "Intibuca",
      countrycode == "HND" & prov == "411" ~ "Lempira",
      countrycode == "HND" & prov == "412" ~ "Ocotepeque",
      countrycode == "HND" & prov == "413" ~ "Santa Barbara",
      countrycode == "HND" & prov == "418" ~ "El Paraiso",
      countrycode == "HND" & prov == "419" ~ "Olancho",
      countrycode == "HND" & prov == "420" ~ "Gracias a Dios",
      countrycode == "HND" & prov == "421" ~ "Choluteca",
      countrycode == "HND" & prov == "422" ~ "Valle",
      TRUE ~ prov
    ),
    
    # Jamaica
    country_region = if_else(
      countrycode == "JAM" & prov %in% c("Kingston", "Saint Andrew"), "JAM_KMA", country_region
    ),
    country_region = if_else(
      countrycode == "JAM" & prov %in% c("Saint Catherine"), "JAM_MIDDLESEX", country_region
    ),
    
    # Nicaragua
    prov = case_when(
      countrycode == "NIC" & prov == "RAAN" ~ "Atlantico Norte",
      countrycode == "NIC" & prov == "RAAS" ~ "Atlantico Sur",
      countrycode == "NIC" & country_region == "NIC_Pacifico_Sur" & prov == "Managua" ~ "Managua Sur",
      TRUE ~ prov
    ),
    
    # Panama
    prov = case_when(
      countrycode == "PAN" & prov == "Comarca Ngöbe Buglé" ~ "Ngobe Bugle",
      countrycode == "PAN" & prov == "Panamá" & country_region == "PAN_Metropolitana" ~ "Panama Metro",
      TRUE ~ prov
    ),
    
    country_region = case_when(
      countrycode == "PAN" & prov %in% c("Chiriqui", "Bocas del Toro", "Ngobe Bugle") ~ "PAN_Occidental",
      countrycode == "PAN" & prov %in% c("Veraguas", "Herrera", "Los Santos", "Cocle") ~ "PAN_Central",
      countrycode == "PAN" & prov == "Panama Metro" ~ "PAN_Metropolitana",
      countrycode == "PAN" & !(country_region %in% c("PAN_Occidental", "PAN_Central", "PAN_Metropolitana")) ~ "PAN_Oriental",
      TRUE ~ country_region
    ),
    
    # Paraguay
    prov = case_when(
      countrycode == "PRY" & prov == "Pdte Hayes" ~ "Presidente Hayes",
      TRUE ~ prov
    ),
    
    country_region = case_when(
      countrycode == "PRY" & prov == "Alto Paraguay" ~ "PRY_Zona_Norte",
      countrycode == "PRY" & prov == "Canindeyu" ~ "PRY_Zona_Este",
      TRUE ~ country_region
    ),
    
    # Peru
    prov = case_when(
      countrycode == "PER" & prov == "1101" ~ "Amazonas",
      countrycode == "PER" & prov == "1102" ~ "Ancash",
      countrycode == "PER" & prov == "1103" ~ "Apurimac",
      countrycode == "PER" & prov == "1104" ~ "Arequipa",
      countrycode == "PER" & prov == "1105" ~ "Ayacucho",
      countrycode == "PER" & prov == "1106" ~ "Cajamarca",
      countrycode == "PER" & prov == "1107" ~ "Callao",
      countrycode == "PER" & prov == "1108" ~ "Cusco",
      countrycode == "PER" & prov == "1109" ~ "Huancavelica",
      countrycode == "PER" & prov == "1110" ~ "Huanuco",
      countrycode == "PER" & prov == "1111" ~ "Ica",
      countrycode == "PER" & prov == "1112" ~ "Junin",
      countrycode == "PER" & prov == "1113" ~ "La Libertad",
      countrycode == "PER" & prov == "1114" ~ "Lambayeque",
      countrycode == "PER" & prov == "1115" ~ "Lima",
      countrycode == "PER" & prov == "1116" ~ "Loreto",
      countrycode == "PER" & prov == "1118" ~ "Madre de Dios",
      countrycode == "PER" & prov == "1118" ~ "Moquegua",
      countrycode == "PER" & prov == "1119" ~ "Pasco",
      countrycode == "PER" & prov == "1120" ~ "Piura",
      countrycode == "PER" & prov == "1121" ~ "Puno",
      countrycode == "PER" & prov == "1122" ~ "San Martin",
      countrycode == "PER" & prov == "1123" ~ "Tacna",
      countrycode == "PER" & prov == "1124" ~ "Tumbes",
      countrycode == "PER" & prov == "1125" ~ "Ucayali",
      TRUE ~ as.character(prov)
    ),
    country_region = case_when(
      countrycode == "PER" & prov %in% c("Amazonas", "Loreto", "San Martin", "Ucayali", "Madre de Dios") ~ "PER_Selva",
      countrycode == "PER" & prov %in% c("Ica", "Arequipa", "Moquegua", "Tacna") ~ "PER_Costa_Sur",
      countrycode == "PER" & prov %in% c("Puno", "Cusco", "Apurimac", "Ayacucho", "Huancavelica") ~ "PER_Sierra_Sur",
      countrycode == "PER" & prov %in% c("Junin", "Pasco", "Huanuco", "Ancash", "Ica Sierra Centro", "Lima Sierra Centro", "La Libertad Sierra Centro") ~ "PER_Sierra_Centro",
      countrycode == "PER" & prov == "Callao" | prov == "Lima" ~ "PER_Lima_Metropolitana",
      countrycode == "PER" & prov %in% c("Tumbes", "Piura", "Lambayeque", "Ancash Costa Norte", "La Libertad", "Lima Costa Norte", "Cajamarca Costa Norte") ~ "PER_Costa_Norte",
      countrycode == "PER" & prov %in% c("Piura Sierra Norte", "Cajamarca", "La Libertad Sierra Norte") ~ "PER_Sierra_Norte",
      TRUE ~ country_region
    ),
    
    # Uruguay
    prov = case_when(
      countrycode == "URY" & prov == "1401" ~ "Montevideo",
      countrycode == "URY" & prov == "1402" ~ "Artigas",
      countrycode == "URY" & prov == "1403" ~ "Canelones",
      countrycode == "URY" & prov == "1404" ~ "Cerro Largo",
      countrycode == "URY" & prov == "1405" ~ "Colonia",
      countrycode == "URY" & prov == "1406" ~ "Durazno",
      countrycode == "URY" & prov == "1407" ~ "Florida",
      countrycode == "URY" & prov == "1408" ~ "Lavalleja",
      countrycode == "URY" & prov == "1409" ~ "Maldonado",
      countrycode == "URY" & prov == "1410" ~ "Paysandu",
      countrycode == "URY" & prov == "1411" ~ "Rio Negro",
      countrycode == "URY" & prov == "1412" ~ "Rivera",
      countrycode == "URY" & prov == "1413" ~ "Rocha",
      countrycode == "URY" & prov == "1414" ~ "Salto",
      countrycode == "URY" & prov == "1415" ~ "San Jose",
      countrycode == "URY" & prov == "1416" ~ "Soriano",
      countrycode == "URY" & prov == "1417" ~ "Tacuarembo",
      countrycode == "URY" & prov == "1418" ~ "Treinta y Tres",
      TRUE ~ as.character(prov)
    ),
    
    # Haiti
    country_region = case_when(
      countrycode == "HTI" & prov %in% c("Grand'Anse", "Sud", "Nippes", "Sud-Est") ~ "HTI_Southern", 
      countrycode == "HTI" & prov %in% c("Nord-Ouest", "Nord", "Nord-Est") ~ "HTI_Northern", 
      countrycode == "HTI" & prov == "Ouest" ~ "HTI_Rest_of_the_West",
      countrycode == "HTI" & prov == "Metro Area" ~ "HTI_Metro_Area",
      countrycode == "HTI" & prov %in% c("L'Artibonite", "Centre") ~ "HTI_Central", 
      TRUE ~ country_region
    ),
    
    # Venezuela
    country_region = case_when(
      countrycode == "VEN" & prov %in% c("Amazonas", "Bolivar", "Delta Amacuro") ~ "VEN_Guayana",
      countrycode == "VEN" & prov %in% c("Anzoategui", "Monagas", "Sucre", "Nueva Esparta") ~ "VEN_Oriental",
      countrycode == "VEN" & prov %in% c("Zulia") ~ "VEN_Zuliana",
      countrycode == "VEN" & prov %in% c("Tachira", "Merida", "Trujillo") ~ "VEN_Andes",
      countrycode == "VEN" & prov %in% c("Lara", "Yaracuy", "Falcon") ~ "VEN_Centro_Occidental",
      countrycode == "VEN" & prov %in% c("Carabobo", "Aragua") ~ "VEN_Centro",
      countrycode == "VEN" & prov %in% c("Vargas", "Distrito Capital", "Miranda") ~ "VEN_Capital",
      countrycode == "VEN" & !(country_region %in% c("VEN_Guayana", "VEN_Oriental", "VEN_Zuliana", "VEN_Andes", "VEN_Centro_Occidental", "VEN_Centro", "VEN_Capital")) ~ "VEN_Llanos",
      TRUE ~ country_region
    ),
    
    # Bahamas 
    country_region = if_else(countrycode == "BHS", "BHS", country_region),
    
    # Belize 
    country_region = if_else(countrycode == "BLZ", "BLZ", country_region),
    
    # Barbados
    country_region = if_else(countrycode == "BRB", "BRB", country_region),
    
    # Dominica
    country_region = if_else(countrycode == "DMA", "DMA", country_region),
    
    # Grenada
    country_region = if_else(countrycode == "GRD", "GRD", country_region),
    
    # Guyana
    country_region = if_else(countrycode == "GUY", "GUY", country_region),
    
    # Suriname
    country_region = if_else(countrycode == "SUR", "SUR", country_region),
    
    # St. Kitts & Nevis
    country_region = if_else(countrycode == "KNA", "KNA", country_region),
    
    # St. Lucia
    country_region = if_else(countrycode == "LCA", "LCA", country_region),
    
    # St. Vincent
    country_region = if_else(countrycode == "VCT", "VCT", country_region),
    
    # Trinidad & Tobago
    country_region = if_else(countrycode == "TTO", "TTO", country_region),
    
  ) 

#
# ----------------------------------------------------------------------------
# --- Save final (derived) data set ----
# ----------------------------------------------------------------------------

cols_to_remove <- c(
  "q11", "q11n",
  "q1", "q2", "q12", "q12c", "q3c", "etid", "leng1", "leng4",
  "r5", "r1", "r3", "r4", "r4a", "r6", "r7", "r8", "r12", "r14", "r15",
  "ocup1a", "ocup4a", "ed2", "q10a", "b43", "mil5", "it1", "dis2", "dis3", "dis5",
  "edre"
)

lapop <- 
lapop |> 
  select(
    # Remove raw variables
    -any_of(cols_to_remove),
    # Remove income variables
    -starts_with("q10"),
    # Remove assets to construct HH asset index
    -starts_with("assets"),
    # Remove 'repeated' variables
    -c(estratopri, prov, municipio, cluster, ed),
    # Remove extra variables
    -c(pppexrate, colorr_topc)
  ) |> 
  glimpse()

lapop |> 
  write_rds("data/derived/UCR_lapop_merge.rds")

#
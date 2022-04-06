library(readxl)
library(magrittr)
library(data.table)
library(tidyverse)

# Risk File ---------------------------------------------------------------

id <- list.files("J:/Mwangi Files/AWS Downloads/",
                  recursive = TRUE, full.names = TRUE,
                  pattern = "dce_attr.+03-.+.zip$"
) %>%
  .[!str_detect(., "(~)")]  %>% # ~ omit open files
  
  map_df(
    readr::read_csv,
    col_names = TRUE,
    col_types = cols(.default = "c"), na = c("", "NA", "n/a")
  ) %>% 
  janitor::clean_names() %>% 
  .[!duplicated(.["empi"]),]


miss_codes <- list.files("J:/Mwangi Files/AWS Downloads/",
                            pattern = "Standard_Risk.+23-3.+",
                            full.names = TRUE) %>% 
  .[!str_detect(., "(~)")] %>%
  map_df(readr::read_csv,
         col_names = TRUE,
         col_types = cols(.default = "c"),
         na = c("", "NA", "n/a")) %>% 
  janitor::clean_names() %>% 
  filter(!is.na(pcp) & pcp_npi != "-1" & grepl("Central|North I|Water", chapter)) %>% 
  mutate(Missed_ICD10_Code = str_remove_all(missed_code, "\\.")) %>% 
  .[!duplicated(.[c("empi","missed_code")]),] %>% 
  select(-c(missed_code,missed_risk_factor)) %>% 
  filter(!is.na(Missed_ICD10_Code))



temp_map <- tempfile(pattern = "map2021_", tmpdir = tempdir())

mapping <- list.files("J:/Mwangi Files/AWS Downloads/",
                         pattern = "2021 Midyear.+ICD-10-CM.+\\.zip$",
                         full.names = TRUE) %>% 
unzip(
  zipfile = .,
  files = "2021 Midyear_Final ICD-10-CM Mappings.xlsx",
  exdir = temp_map
) %>%
  
  .[!str_detect(., "(~)")] %>%
  map_df(readxl::read_excel,
         sheet = "FY20-FY21 ICD10 Payment Codes", range = "A4:E11063", col_names = TRUE,
         col_types = "text", na = c("", "NA", "n/a")) %>% 
  rename_with(~{(str_replace_all(.,"\\r\\n", "_") %>% #
      str_remove_all(".+Category_+"))}) %>%
  filter(!is.na(V24)) %>% 
  mutate(V24 = as.numeric(V24))
  

miss_codes_desc <- miss_codes %>% 
  left_join(select(id,id,empi, fn, ln), by = c("empi")) %>%
  .[!duplicated(.[c("empi","Missed_ICD10_Code")]),] %>% 
  left_join(mapping[c("Diagnosis_Code", "Description", "V24")], 
            by = c("Missed_ICD10_Code" = "Diagnosis_Code")) %>% 
  mutate(V24 = as.integer(V24)) %>%
  rename_with(.fn = tolower, .cols = everything()) %>% 
  arrange(empi,v24) %>% 
  unite("HCC",c(v24,description,missed_icd10_code),sep = ": ",remove = TRUE) %>% 
  group_by(chapter,id,empi,fn,ln,pcp_npi,pcp) %>% 
  mutate(rownum = paste0("V",row_number())) %>% 
  pivot_wider(names_from = rownum, values_from = HCC, values_fill = NULL) %>% 
  relocate(chapter,.before = empi) %>% 
  relocate(id, .after = empi) %>% 
  mutate(across(.cols = starts_with("^V\\d+?"),~str_remove_all(.x, "(NA:?)+?")))
  

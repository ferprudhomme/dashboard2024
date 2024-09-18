###########################################
# excel                                   #
#                                         #
# @(2024-09-05)                           #
# @(Gadex EP)                             #
#                                         #
#-----------------------------------------#
#                                         #
# Descripción:                            #
# IEnova Pipelines, S. de R.L. de C.V.    #
# Datos Hueco-Samalayuca                  #
# Original unit: m3                       #
###########################################
#########
process_file <- function(inputdate, file_prefix, data_path) {
  # Construct the filename based on the prefix and input date
  file_name <- format(inputdate, paste0(file_prefix, "_%Y_%m_%d_transvol_en.xlsx"))
  
  # Read and process the data
  data <- read_excel(file.path(data_path, file_name),
                     sheet = "Transported Volumes Repor", skip = 11, col_names = TRUE) %>%
    mutate(
      Date = as.Date(`Gas Day`, format = "%m/%d/%Y"),
      recibos = as.numeric(gsub(",", "", `Allocated Qty at the Receipt`)),
      entregas = as.numeric(gsub(",", "", `Allocated Qty at the Delivery`)),
      efectiva = ifelse(Date == inputdate - 1, 1, 0)  # Flag rows where Date is 1 day before inputdate
    ) %>%
    select(Date, `Contract Type`, `Receipt Point`, `Delivery Point`, recibos, entregas, efectiva) %>%
    filter(efectiva == 1)  # Keep only rows where efectiva is 1
  
  return(data)
}

process_filult <- function(inputdate, file_prefix, data_path) {
  # Construct the filename based on the prefix and input date
  file_name <- format(inputdate, paste0(file_prefix, "_%Y_%m_%d_transvol_en.xlsx"))
  
  # Read and process the data
  data <- read_excel(file.path(data_path, file_name),
                     sheet = "Transported Volumes Repor", skip = 11, col_names = TRUE) %>%
    mutate(
      Date = as.Date(`Gas Day`, format = "%m/%d/%Y"),
      recibos = as.numeric(gsub(",", "", `Allocated Qty at the Receipt`)),
      entregas = as.numeric(gsub(",", "", `Allocated Qty at the Delivery`)),
      efectiva = 1  # Flag rows where Date is 7 days before `inputdate`
    ) %>%
    select(Date, `Contract Type`, `Receipt Point`, `Delivery Point`, recibos, entregas, efectiva) %>%
    filter(efectiva == 1)  # Keep only rows where efectiva is 1
  
  return(data)
}

# 1. Relevant dates              ----

t_sempra01 <- gen_dat("2024-02-02", "2024-08-22")
ph_sempra01 <- file.path(getwd(), "01_data/Sempra 01")
pre_sempra01 <- "iep_sam"

t_sempra01u <- gen_dat("2024-02-01", "2024-02-01")


# Process files for a list of dates (e.g., August 22, 2024, and others if needed)
proc_sempra01 <- do.call(rbind, lapply(t_sempra01, function(date) process_file(date, pre_sempra01, ph_sempra01)))
proc_sempra01u <- do.call(rbind, lapply(t_sempra01u, function(date) process_filult(date, pre_sempra01, ph_sempra01)))

DBsempra01 <- rbind(proc_sempra01,proc_sempra01u) 

DBR_se01 <- DBsempra01 %>%
  group_by(Date, `Receipt Point`) %>%
  summarize(volume = sum(recibos) * fctmf * 0.000001) %>%
  rename(node = `Receipt Point`) %>%
  drop_na()
#######

DBD_se01 <- DBsempra01 %>%
  group_by(Date, `Delivery Point`) %>%
  summarize(volume = sum(entregas) * fctmf * 0.000001) %>%
  rename(node = `Delivery Point`) %>%
  drop_na()
#######

ggplot(DBR_se01, aes(x = Date)) +
  geom_area(aes(y = volume, group = node, fill= node)) 

ggplot(DBD_se01, aes(x = Date)) +
  geom_area(aes(y = volume, group = node, fill= node))

# 4. data export ----
# Get effective daily data
# Create a named vector where names are old values and values are new ones
rep_node_sem01 <- c("EMRyC San Isidro Samalayuca - IEnova Pipelines" = "SIS",
                 "Hueco-Kinder Morgan" = "KM_Hueco",
                 "Chihuahua III" = "CHI_III",
                 "Gloria a Dios-Chihuahua" ="GAD_CHI",
                 "Gloria a Dios-Juárez" = "GAD_JUA",
                 "Samalayuca - Calentadores" = "SMY_BRN",
                 "Samalayuca I" = "SMY_I",
                 "Samalayuca II" = "SMY_II")

DBR_se01 <- DBR_se01 %>%
  mutate(node = recode(node, !!!rep_node_sem01)) %>%
  select(Date,node,volume) 

DBD_se01 <- DBD_se01 %>%
  mutate(node = recode(node, !!!rep_node_sem01)) %>%
  select(Date,node,volume) 

# Use mutate() with recode() to apply the mapping
DB_se01 <- rbind(DBR_se01,DBD_se01) %>%
  select(Date,node,volume) %>%
  spread(key = node, value = volume)
  
getwd()
write.csv(DBR_se01, file = "TRA_016_97_REC.csv", row.names = TRUE)
write.csv(DBD_se01, file = "TRA_016_97_DEL.csv", row.names = TRUE)
write.csv(DB_se01, file = "TRA_016_97_CONF.csv", row.names = TRUE)
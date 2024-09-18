##########################################
# excel                                  #
#                                         #
# @(2024-09-05)                           #
# @(Gadex EP)                             #
#                                         #
#-----------------------------------------#
#                                         #
# Descripción:                            #
# Datos Ojinaga - El Encino               #
# Original unit: MM BTU's                 #
###########################################
# 
#########
# 1. Relevant dates              ----

dat_04 <- gen_dat("2024-08-01", "2024-08-21")
dat_04u <- gen_dat("2024-08-22", "2024-08-22")

#########
# 2. File paths              ----
data.path <- file.path(getwd(), "01_data/Sempra 04")

#########
# 3. Functions to process the file for a given date ----
pr_se4 <- function(inputdate) {
  file_se41 <- format(inputdate, "gro_main_%Y_%m_%d_transvol_en.xlsx")
  file_se42 <- format(inputdate, "gro_spur_%Y_%m_%d_transvol_en.xlsx")
  file_se43 <- format(inputdate, "gro_yum-l_%Y_%m_%d_transvol_en.xlsx")
  
  # Read and process the data
  dat1 <- read_excel(file.path(data.path, file_se41),
                     sheet = "Transported Volumes Repor", skip = 11, col_names = TRUE) %>%
    mutate(
      gas_day = as.Date(`Gas Day`, format = "%m/%d/%Y"),
      recibos = as.numeric(gsub(",", "", `Allocated Qty at the Receipt`)),
      entregas = as.numeric(gsub(",", "", `Allocated Qty at the Delivery`)),
      efectiva = ifelse(gas_day == inputdate - 7, 1, 0)  # Assign 1 if `gas_day` is 7 days before `inputdate`
    ) %>%
    select(gas_day, `Contract Type`, `Receipt Point`, `Delivery Point`, recibos, entregas, efectiva) %>%
    filter(efectiva == 1)  # Filter rows where efectiva is 1 (7 days before `inputdate`)
  
  # Read and process the data
  dat2 <- read_excel(file.path(data.path, file_se42),
                     sheet = "Transported Volumes Repor", skip = 11, col_names = TRUE) %>%
    mutate(
      gas_day = as.Date(`Gas Day`, format = "%m/%d/%Y"),
      recibos = as.numeric(gsub(",", "", `Allocated Qty at the Receipt`)),
      entregas = as.numeric(gsub(",", "", `Allocated Qty at the Delivery`)),
      efectiva = ifelse(gas_day == inputdate - 7, 1, 0)  # Assign 1 if `gas_day` is 7 days before `inputdate`
    ) %>%
    select(gas_day, `Contract Type`, `Receipt Point`, `Delivery Point`, recibos, entregas, efectiva) %>%
    filter(efectiva == 1)  # Filter rows where efectiva is 1 (7 days before `inputdate`)
  
  # Read and process the data
  dat3 <- read_excel(file.path(data.path, file_se43),
                     sheet = "Transported Volumes Repor", skip = 11, col_names = TRUE) %>%
    mutate(
      gas_day = as.Date(`Gas Day`, format = "%m/%d/%Y"),
      recibos = as.numeric(gsub(",", "", `Allocated Qty at the Receipt`)),
      entregas = as.numeric(gsub(",", "", `Allocated Qty at the Delivery`)),
      efectiva = ifelse(gas_day == inputdate - 7, 1, 0)  # Assign 1 if `gas_day` is 7 days before `inputdate`
    ) %>%
    select(gas_day, `Contract Type`, `Receipt Point`, `Delivery Point`, recibos, entregas, efectiva) %>%
    filter(efectiva == 1)  # Filter rows where efectiva is 1 (7 days before `inputdate`)
  
  data <- rbind(dat1,dat2,dat3)
  
  return(data)
}

# Function to process the file for a given date
pr_se4_u <- function(inputdate) {
  file_41 <- format(inputdate, "gro_main_%Y_%m_%d_transvol_en.xlsx")
  file_42 <- format(inputdate, "gro_spur_%Y_%m_%d_transvol_en.xlsx")
  file_43 <- format(inputdate, "gro_yum-l_%Y_%m_%d_transvol_en.xlsx")
  
  # Read and process the data
  da41 <- read_excel(file.path(data.path, file_41),
                     sheet = "Transported Volumes Repor", skip = 11, col_names = TRUE) %>%
    mutate(
      gas_day = as.Date(`Gas Day`, format = "%m/%d/%Y"),
      recibos = as.numeric(gsub(",", "", `Allocated Qty at the Receipt`)),
      entregas = as.numeric(gsub(",", "", `Allocated Qty at the Delivery`)),
      efectiva = ifelse(gas_day >= inputdate - 7, 1, 0)  # Assign 1 if `gas_day` is 7 days before `inputdate`
    ) %>%
    select(gas_day, `Contract Type`, `Receipt Point`, `Delivery Point`, recibos, entregas, efectiva) %>%
    filter(efectiva == 1)  # Filter rows where efectiva is 1 (7 days before `inputdate`)
  
  # Read and process the data
  da42 <- read_excel(file.path(data.path, file_42),
                     sheet = "Transported Volumes Repor", skip = 11, col_names = TRUE) %>%
    mutate(
      gas_day = as.Date(`Gas Day`, format = "%m/%d/%Y"),
      recibos = as.numeric(gsub(",", "", `Allocated Qty at the Receipt`)),
      entregas = as.numeric(gsub(",", "", `Allocated Qty at the Delivery`)),
      efectiva = ifelse(gas_day >= inputdate - 7, 1, 0)  # Assign 1 if `gas_day` is 7 days before `inputdate`
    ) %>%
    select(gas_day, `Contract Type`, `Receipt Point`, `Delivery Point`, recibos, entregas, efectiva) %>%
    filter(efectiva == 1)  # Filter rows where efectiva is 1 (7 days before `inputdate`)
  
  # Read and process the data
  da43 <- read_excel(file.path(data.path, file_43),
                     sheet = "Transported Volumes Repor", skip = 11, col_names = TRUE) %>%
    mutate(
      gas_day = as.Date(`Gas Day`, format = "%m/%d/%Y"),
      recibos = as.numeric(gsub(",", "", `Allocated Qty at the Receipt`)),
      entregas = as.numeric(gsub(",", "", `Allocated Qty at the Delivery`)),
      efectiva = ifelse(gas_day >= inputdate - 7, 1, 0)  # Assign 1 if `gas_day` is 7 days before `inputdate`
    ) %>%
    select(gas_day, `Contract Type`, `Receipt Point`, `Delivery Point`, recibos, entregas, efectiva) %>%
    filter(efectiva == 1)  # Filter rows where efectiva is 1 (7 days before `inputdate`)
  
  data <- rbind(da41,da42,da43)
  
  return(data)
}

# Process files for August 22, 2024
tra_se4 <- do.call(rbind,lapply(dat_04, pr_se4))
tra_se4u <- do.call(rbind,lapply(dat_04u, pr_se4_u))

tra_se4 <- rbind(tra_se4,tra_se4u) 

iny_se4 <- tra_se4 %>%
  group_by(gas_day, `Receipt Point`) %>%
  summarize(inyeccion = sum(recibos) * fctmf * 0.000001) 
#######

ext_se4 <- tra_se4 %>%
  group_by(gas_day, `Delivery Point`) %>%
  summarize(extraccion = sum(entregas) * fctmf * 0.000001)
#######

ggplot(iny_se4, aes(x = gas_day)) +
  geom_area(aes(y = inyeccion, group = `Receipt Point`, fill= `Receipt Point`)) 

ggplot(ext_se4, aes(x = gas_day)) +
  geom_area(aes(y = extraccion, group = `Delivery Point`, fill= `Delivery Point`)) 

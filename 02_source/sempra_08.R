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
dat_8a <- gen_dat("2024-01-01", "2024-05-02")
dat_8b <- gen_dat("2024-06-01", "2024-08-21")

dat_08 <- c(dat_8a,dat_8b)

dat_08u <- gen_dat("2024-08-22", "2024-08-22")

#########
# 2. File paths              ----
data.path <- file.path(getwd(), "01_data/Sempra 08")

#########
# 3. Functions to process the file for a given date ----
pr_se8 <- function(inputdate) {
  file_se81 <- format(inputdate, "gps_emp_%Y_%m_%d_transvol_en.xlsx")
  file_se82 <- format(inputdate, "gps_gueo_%Y_%m_%d_transvol_en.xlsx")
  file_se83 <- format(inputdate, "gps_sagu_%Y_%m_%d_transvol_en.xlsx")
  
  # Read and process the data
  dat1 <- read_excel(file.path(data.path, file_se81),
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
  dat2 <- read_excel(file.path(data.path, file_se82),
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
  dat3 <- read_excel(file.path(data.path, file_se83),
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
pr_se8_u <- function(inputdate) {
  file_81 <- format(inputdate, "gps_emp_%Y_%m_%d_transvol_en.xlsx")
  file_82 <- format(inputdate, "gps_gueo_%Y_%m_%d_transvol_en.xlsx")
  file_83 <- format(inputdate, "gps_sagu_%Y_%m_%d_transvol_en.xlsx")
  
  # Read and process the data
  da81 <- read_excel(file.path(data.path, file_81),
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
  da82 <- read_excel(file.path(data.path, file_82),
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
  da83 <- read_excel(file.path(data.path, file_83),
                     sheet = "Transported Volumes Repor", skip = 11, col_names = TRUE) %>%
    mutate(
      gas_day = as.Date(`Gas Day`, format = "%m/%d/%Y"),
      recibos = as.numeric(gsub(",", "", `Allocated Qty at the Receipt`)),
      entregas = as.numeric(gsub(",", "", `Allocated Qty at the Delivery`)),
      efectiva = ifelse(gas_day >= inputdate - 7, 1, 0)  # Assign 1 if `gas_day` is 7 days before `inputdate`
    ) %>%
    select(gas_day, `Contract Type`, `Receipt Point`, `Delivery Point`, recibos, entregas, efectiva) %>%
    filter(efectiva == 1)  # Filter rows where efectiva is 1 (7 days before `inputdate`)
  
  data <- rbind(da81,da82,da83)
  
  return(data)
}

# Process files for August 22, 2024
tra_se8 <- do.call(rbind,lapply(dat_08, pr_se8))
tra_se8u <- do.call(rbind,lapply(dat_08u, pr_se8_u))

tra_se8 <- rbind(tra_se8,tra_se8u) 

iny_se8 <- tra_se8 %>%
  group_by(gas_day, `Receipt Point`) %>%
  summarize(inyeccion = sum(recibos) * fctmf * 0.000001) 
#######

ext_se8 <- tra_se8 %>%
  group_by(gas_day, `Delivery Point`) %>%
  summarize(extraccion = sum(entregas) * fctmf * 0.000001)
#######

ggplot(iny_se8, aes(x = gas_day)) +
  geom_area(aes(y = inyeccion, group = `Receipt Point`, fill= `Receipt Point`)) 

ggplot(ext_se8, aes(x = gas_day)) +
  geom_area(aes(y = extraccion, group = `Delivery Point`, fill= `Delivery Point`)) 

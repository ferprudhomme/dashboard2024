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

dat_05 <- gen_dat("2023-12-19", "2024-08-21")
dat_05u <- gen_dat("2024-08-22", "2024-08-22")

#########
# 2. File paths              ----
data.path <- file.path(getwd(), "01_data/Sempra 05")

#########
# 3. Functions to process the file for a given date ----
pr_se5 <- function(inputdate) {
  file_se5 <- format(inputdate, "tgn_tgn-ros_%Y_%m_%d_transvol_en.xlsx")
  
  # Read and process the data
  data <- read_excel(file.path(data.path, file_se5),
                     sheet = "Transported Volumes Repor", skip = 11, col_names = TRUE) %>%
    mutate(
      gas_day = as.Date(`Gas Day`, format = "%m/%d/%Y"),
      recibos = as.numeric(gsub(",", "", `Allocated Qty at the Receipt`)),
      entregas = as.numeric(gsub(",", "", `Allocated Qty at the Delivery`)),
      efectiva = ifelse(gas_day == inputdate - 7, 1, 0)  # Assign 1 if `gas_day` is 7 days before `inputdate`
    ) %>%
    select(gas_day, `Contract Type`, `Receipt Point`, `Delivery Point`, recibos, entregas, efectiva) %>%
    filter(efectiva == 1)  # Filter rows where efectiva is 1 (7 days before `inputdate`)
  
  return(data)
}

# Function to process the file for a given date
pr_se5_u <- function(inputdate) {
  file_se5 <- format(inputdate, "tgn_tgn-ros_%Y_%m_%d_transvol_en.xlsx")
  
  # Read and process the data
  data <- read_excel(file.path(data.path, file_se5),
                     sheet = "Transported Volumes Repor", skip = 11, col_names = TRUE) %>%
    mutate(
      gas_day = as.Date(`Gas Day`, format = "%m/%d/%Y"),
      recibos = as.numeric(gsub(",", "", `Allocated Qty at the Receipt`)),
      entregas = as.numeric(gsub(",", "", `Allocated Qty at the Delivery`)),
      efectiva = ifelse(gas_day >= inputdate - 7, 1, 0)  # Assign 1 if `gas_day` is 7 days before `inputdate`
    ) %>%
    select(gas_day, `Contract Type`, `Receipt Point`, `Delivery Point`, recibos, entregas, efectiva) %>%
    filter(efectiva == 1)  # Filter rows where efectiva is 1 (7 days before `inputdate`)
  
  return(data)
}

# Process files for August 22, 2024
tra_se5 <- do.call(rbind,lapply(dat_05, pr_se5))
tra_se5u <- do.call(rbind,lapply(dat_05u, pr_se5_u))

tra_se5 <- rbind(tra_se5,tra_se5u) 

iny_se5 <- tra_se5 %>%
  group_by(gas_day, `Receipt Point`) %>%
  summarize(inyeccion = sum(recibos) * fctmf * 0.000001) 
#######

ext_se5 <- tra_se5 %>%
  group_by(gas_day, `Delivery Point`) %>%
  summarize(extraccion = sum(entregas) * fctmf * 0.000001)
#######

ggplot(iny_se5, aes(x = gas_day)) +
  geom_area(aes(y = inyeccion, group = `Receipt Point`, fill= `Receipt Point`)) 

ggplot(ext_se5, aes(x = gas_day)) +
  geom_area(aes(y = extraccion, group = `Delivery Point`, fill= `Delivery Point`)) 

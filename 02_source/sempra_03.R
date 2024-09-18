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

dat_03 <- gen_dat("2024-02-01", "2024-08-21")
dat_03u <- gen_dat("2024-08-22", "2024-08-22")

#########
# 2. File paths              ----
data.path <- file.path(getwd(), "01_data/Sempra 03")

#########
# 3. Functions to process the file for a given date ----
pr_se3 <- function(inputdate) {
  file_se3 <- format(inputdate, "goe_ojg-enc_%Y_%m_%d_transvol_en.xlsx")
  
  # Read and process the data
  data <- read_excel(file.path(data.path, file_se3),
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
pr_se3_u <- function(inputdate) {
  file_se3 <- format(inputdate, "goe_ojg-enc_%Y_%m_%d_transvol_en.xlsx")
  
  # Read and process the data
  data <- read_excel(file.path(data.path, file_se3),
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
tra_se3 <- do.call(rbind,lapply(dat_03, pr_se3))
tra_se3u <- do.call(rbind,lapply(dat_03u, pr_se3_u))

tra_se3 <- rbind(tra_se3,tra_se3u) 

iny_se3 <- tra_se3 %>%
  group_by(gas_day, `Receipt Point`) %>%
  summarize(inyeccion = sum(recibos) * fctmf * 0.000001) 
#######

ext_se3 <- tra_se3 %>%
  group_by(gas_day, `Delivery Point`) %>%
  summarize(extraccion = sum(entregas) * fctmf * 0.000001)
#######

ggplot(iny_se3, aes(x = gas_day)) +
  geom_area(aes(y = inyeccion, group = `Receipt Point`, fill= `Receipt Point`)) 

ggplot(ext_se3, aes(x = gas_day)) +
  geom_area(aes(y = extraccion, group = `Delivery Point`, fill= `Delivery Point`)) 

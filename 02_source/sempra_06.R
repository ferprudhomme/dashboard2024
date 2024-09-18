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

dat_06 <- gen_dat("2024-01-01", "2024-08-21")
dat_06u <- gen_dat("2024-08-22", "2024-08-22")

#########
# 2. File paths              ----
data.path <- file.path(getwd(), "01_data/Sempra 06")

#########
# 3. Functions to process the file for a given date ----
pr_se6 <- function(inputdate) {
  file_se6 <- format(inputdate, "gap_gap-ori_%Y_%m_%d_transvol_en.xlsx")
  
  # Read and process the data
  data <- read_excel(file.path(data.path, file_se6),
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
pr_se6_u <- function(inputdate) {
  file_se6 <- format(inputdate, "gap_gap-ori_%Y_%m_%d_transvol_en.xlsx")
  
  # Read and process the data
  data <- read_excel(file.path(data.path, file_se6),
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
tra_se6 <- do.call(rbind,lapply(dat_06, pr_se6))
tra_se6u <- do.call(rbind,lapply(dat_06u, pr_se6_u))

tra_se6 <- rbind(tra_se6,tra_se6u) 

iny_se6 <- tra_se6 %>%
  group_by(gas_day, `Receipt Point`) %>%
  summarize(inyeccion = sum(recibos) * fctmf * 0.000001) 
#######

ext_se6 <- tra_se6 %>%
  group_by(gas_day, `Delivery Point`) %>%
  summarize(extraccion = sum(entregas) * fctmf * 0.000001)
#######

ggplot(iny_se6, aes(x = gas_day)) +
  geom_area(aes(y = inyeccion, group = `Receipt Point`, fill= `Receipt Point`)) 

ggplot(ext_se6, aes(x = gas_day)) +
  geom_area(aes(y = extraccion, group = `Delivery Point`, fill= `Delivery Point`)) 

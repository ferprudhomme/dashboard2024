##########################################
sempra_01.R                               #
#                                         #
# @(2024-08-26)                           #
# @(Gadex EP)                             #
#                                         #
#-----------------------------------------#
#                                         #
# Descripción:                            #
# Datos Rosarito                          #
#                                         #
###########################################
# 1. Librerias          ----

# 1. Load data ----

# 1.1 Load NGB.csv
se01 <- read_csv(file.path(data.path, 'Sempra 01 Rosarito/northbaja_deliver_ogilby_00.csv'),skip = 3, col_names = TRUE)
se01_loc <- read_csv(file.path(data.path, 'Sempra 01 Rosarito/northbaja_location.csv'),skip = 5, col_names = TRUE)

se01_loc_01 <- filter(se01_loc, `Up/Dn Name` == "Gasoducto Rosarito, S. de R.L. de")

##########################################
# rbn                                    #
#                                         #
# @(2024-08-26)                           #
# @(Gadex EP)                             #
#                                         #
#-----------------------------------------#
#                                         #
# Descripción:                            #
# Datos Rosarito                          #
# Original unit: MM BTU's                 #
###########################################

# 1. Load data functions ----

# RBN lectura "D3:Z976" ----
#

rbn01 <- read_excel(file.path(data.path, "RBN Permian/data_from_pdf.xlsx"),
                    sheet = "time_series", range = "D3:Z976", col_names = TRUE)
rbn01_01 <- rbn01 %>%
  gather(`Waha`, `Comanche Trail`, `Roadrunner`,`Trans-Pecos`,`EPGN`,
           `Samalayuca`, `Agua Prieta`, `Mexicana de Cobre`,`WillMex`,`Monument 90`,
           `Sierrita`,`Baja`,`South`, `KM Mier`, `KM Border`, `Energy Transfer`,
           `NET`,`TGP`,`TETCO`, `Nueva Era`, `Valley Crossing`, `Total`,
           key = "interna", value = "volume")

# 2. data analysis ----

# Plotting DC over time
ggplot(rbn01_01, aes(x = Date, y = volume, color = interna)) +
  geom_point(size = 0.2) +  # Change point size
  labs(title = "Imports thru Comanche Trails over Time",
       x = "Effective Gas DateTime",
       y = "Volume (BCF per day)") +
  scale_color_manual(values = palGADEXComp) +
  theme_minimal()  # Optional: for a cleaner look

# 3. time series ----

# create a time series for multiple columns, you can do the following:
rbn01_waha_ts <- zoo(rbn01[, c("Comanche Trail", "Roadrunner","Trans-Pecos")], order.by = rbn01$Date)

# Monthly aggregation using different operators

rbn01_waha_m_ave <- apply.monthly(rbn01_waha_ts, FUN = mean)
rbn01_waha_m_max <- apply.monthly(rbn01_waha_ts, FUN = max)

# Yearly aggregation using different operators

rbn01_waha_y_ave <- apply.yearly(rbn01_waha_ts, FUN = mean)
rbn01_waha_y_max <- apply.yearly(rbn01_waha_ts, FUN = max)

# 4. Plot time series ----

gdx_dygraph(rbn01_waha_ts, "Natural Gas Imports by Ogilby", palGADEXComp)
gdx_dygraph(rbn01_waha_m_ave, "Natural Gas Imports by Ogilby", palGADEXComp)

autoplot(rbn01_waha_ts) +
  labs(title = "Time Series Plot", x = "Date", y = "Value") +
  theme_minimal()  # Optional: Change to a minimal theme



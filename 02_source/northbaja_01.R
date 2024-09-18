##########################################
# northbaja_01.R                          #
#                                         #
# @(2024-09-11)                           #
# @(Gadex EP)                             #
#                                         #
#-----------------------------------------#
#                                         #
# Descripción:                            #
# EBB North Baja data                     #
# Original unit: MM BTU's
#
#
###########################################
# 1. Librerias          ----

# 1. Load data ----
# original from 10/01/2014 to 08/29/2024
nb00 <- read_csv(file.path(data.path, 'northbaja_01/northbaja_deliver_00.csv'),skip = 3, col_names = TRUE)
nb01 <- read_csv(file.path(data.path, 'northbaja_01/northbaja_deliver_01.csv'),skip = 3, col_names = TRUE)
nb_del <- rbind(nb00,nb01)

# consolida
nb00_heat <- read_csv(file.path(data.path, 'northbaja_01/northbaja_heat_00.csv'),skip = 0, col_names = TRUE)
nb01_heat <- read_csv(file.path(data.path, 'northbaja_01/northbaja_heat_01.csv'),skip = 0, col_names = TRUE)
nb_heat <- rbind(nb00_heat, nb01_heat)

# 2. filter data
nb00_loc <- read_csv(file.path(data.path, 'northbaja_01/northbaja_location.csv'),skip = 5, col_names = TRUE)
nb00_loc <- filter(nb00_loc, `Up/Dn Name` == "Gasoducto Rosarito, S. de R.L. de") %>%
                pull(Loc)

# Define cycle order for correct factor ordering
cycle_order <- c("TIM", "EVE", "ID1", "ID2")  # order

nb_del <- nb_del %>% 
  filter(Loc %in% nb00_loc, `Loc Purp Desc` == "Delivery Location") %>%
  mutate(Date = mdy_hms(`Eff.Gas DateTime`)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Cycle = factor(Cycle, levels = cycle_order, ordered = TRUE)) %>% # Ensure 'Cycle' is a factor with the correct order
  group_by(Date) %>%                            # Group by date
  slice_max(order_by = Cycle, with_ties = FALSE) %>%    # Filter to keep only the last cycle
  ungroup() %>%
  select(Date, Cycle, DC, OPC, TSQ) %>%
  arrange(Date)  # Ensure the data is sorted by date

# 3. data plot ----
# Get effective daily data
# plot daily data
ggplot(nb_del, aes(x = Date, y = TSQ, color = Cycle)) +
  geom_point(size = 0.5) +  # Change point size
  geom_line(aes(y = DC), linetype = "solid", size = 0.5, color = "darkred") +  # Add a line with another column value
  geom_line(aes(y = OPC), linetype = "solid", size = 0.5, color = "steelblue") +  # Add a line with another column value
  labs(title = "Transported energy",
       x = "Effective Gas Day",
       y = "Energy and Capacity (MM BTU)") +
  scale_color_manual(values = palGADEX) +
  GadexTheme() # Optional: for a cleaner look

#######

nb_heat <- nb_heat %>%
        select(`Gas Day`, `Gross Heating Value (BTU/CF)`) %>%
        mutate(Date = mdy(`Gas Day`)) %>%
        filter(complete.cases(.)) %>%
        rename(heat_val = `Gross Heating Value (BTU/CF)`) %>%
        select(Date, heat_val)

# Assuming you want to average the column `gross_heating_value`
nb_heat_ave <- mean(nb_heat$heat_val, na.rm = TRUE)


# 4. Conversion from BTU to FT

nb_del <- nb_del %>%
  full_join(nb_heat, by = "Date") %>%
  mutate(volume = if_else(!is.na(TSQ) & !is.na(heat_val),
                                  TSQ / heat_val,         # Multiply when both have values
                                  TSQ / nb_heat_ave)) %>%  # Use average of var1 when var1 is NA
  mutate(vol_dc = DC / nb_heat_ave, vol_opc = OPC / nb_heat_ave) %>%
  rename(NRBJ01 = TSQ, dc = DC , opc = OPC)

# 4. data export ----
# Get effective daily data
nb_del <- nb_del %>%
  select(Date, NRBJ01, dc)
  
getwd()
write.csv(nb_del, file = "NB_CONF.csv", row.names = TRUE)

# 5. time series ----
# Convert to xts object for daily data
nb_xts <- nb_del %>%
  arrange(Date) %>%
  select(NRBJ01) %>%
  xts::xts(order.by = nb_del$Date)

# 6. Plot time series ----

gdx_dygraph(nb_xts, "Natural Gas Imports by Ogilby", palGADEX)
gdx_hchart(nb_xts, "Natural Gas Imports by Ogilby", palGADEX)

# Monthly aggregation using different operators

nb_m_ave <- apply.monthly(nb_xts, FUN = mean)
nb_m_max <- apply.monthly(nb_xts, FUN = max)

# Combining different aggregated series into a single xts object
nb_m <- merge(
  nb_m_ave, 
  nb_m_max,
  all = TRUE, suffixes = c("_ave", "_max")
)

gdx_dygraph(nb_m, "Natural Gas Imports by Ogilby", palGADEX)
gdx_hchart(nb_m, "Natural Gas Imports by Ogilby", palGADEX)


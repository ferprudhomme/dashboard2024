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

t_sempra02 <- gen_dat("2024-02-02", "2024-08-22")
ph_sempra02 <- file.path(getwd(), "01_data/Sempra 02")
pre_sempra02 <- "goe_ojg-enc"

t_sempra02u <- gen_dat("2024-02-01", "2024-02-01")


# Process files for a list of dates (e.g., August 22, 2024, and others if needed)
proc_sempra02 <- do.call(rbind, lapply(t_sempra02, function(date) process_file(date, pre_sempra02, ph_sempra02)))
proc_sempra02u <- do.call(rbind, lapply(t_sempra02u, function(date) process_filult(date, pre_sempra02, ph_sempra02)))

DBsempra02 <- rbind(proc_sempra02,proc_sempra02u) 

DBR_se02 <- DBsempra02 %>%
  group_by(Date, `Receipt Point`) %>%
  summarize(volume = sum(recibos) * fctmf * 0.000001) %>%
  rename(node = `Receipt Point`)
#######

DBD_se02 <- DBsempra02 %>%
  group_by(Date, `Delivery Point`) %>%
  summarize(volume = sum(entregas) * fctmf * 0.000001) %>%
  rename(node = `Delivery Point`)
#######

ggplot(DBR_se02, aes(x = Date)) +
  geom_area(aes(y = volume, group = node, fill= node)) 

ggplot(DBD_se02, aes(x = Date)) +
  geom_area(aes(y = volume, group = node, fill= node))

# 4. data export ----
# Get effective daily data
# Create a named vector where names are old values and values are new ones
rep_node_sem02 <- c("El Encino-Corredor Chihuahua (Recibo)" = "TARA_R",
                    "El Encino-La Laguna (Recibo)" = "EELL_R",
                    "EM Presidio" = "PRESI",
                    "MakeUp-GOE" ="MKUP",
                    "El Encino-Corredor Chihuahua (Entrega)" = "TARA_D",
                    "El Encino-La Laguna (Entrega)" = "EELL_D",
                    "El Encino-Topolobampo (Entrega)" = "EETOP_D",
                    "Payback GOE" = "PYBK")

# Use mutate() with recode() to apply the mapping
DB_se02 <- rbind(DBR_se02,DBD_se02)

DBR_se02 <- DBR_se02 %>%
  mutate(node = recode(node, !!!replace_node)) %>%
  select(Date,node,volume) 

DBD_se02 <- DBD_se02 %>%
  mutate(node = recode(node, !!!replace_node)) %>%
  select(Date,node,volume) 


DB_se02 <- DB_se02 %>%
  mutate(node = recode(node, !!!replace_node)) %>%
  select(Date,node,volume) %>%
  spread(key = node, value = volume)

getwd()
write.csv(DBR_se02, file = "TRA_049_2016_REC.csv", row.names = TRUE)
write.csv(DBD_se02, file = "TRA_049_2016_DEL.csv", row.names = TRUE)
write.csv(DB_se02, file = "TRA_049_2016_CONF.csv", row.names = TRUE)

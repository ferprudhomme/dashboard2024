##########################################
# control.R                                #
#                                         #
# @(2024-08-26)                           #
# @(Gadex EP)                            #
#                                         #
#-----------------------------------------#
#                                         #
# Descripción:                            #
# global.R simplifica y posibilita        #
# llamar distintos scripts a emplear      #
# en el ui.R y server.R para              #
# shinyapp.                               #
#                                         #
###########################################
# 1. Librerias          ----
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(xts)
library(dygraphs)
library(highcharter)
library(pdftools)
library(stringr)
library(readxl)
library(tidyverse)
library(zoo)
library(ggfortify)
library(tseries)
library(urca)
library(tseries)
library(networkD3)

# 2. Rutas              ----
data.path <- file.path(getwd(), "01_data/")

getwd()

# 3. Diseño de graficos ----
GadexTheme <- function(){
  theme_classic(base_family = "Open Sans") + 
    theme(text = element_text(color = '#cdcdcd'),
          axis.line = element_line(colour = "#cdcdcd"), 
          axis.text.x = element_text(color = '#cdcdcd', angle = 90, hjust = 1, vjust = .5),
          axis.text.y = element_text(color = '#cdcdcd'),
          axis.title.x = element_blank(),
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent", color = NA) # get rid of legend panel bg
    )
  
}


# Define the dygraph custom function
gdx_dygraph <- function(data, title, colores) {
  dygraph(data, main = title) %>%
    dyOptions(colors = colores, gridLineColor = "lightgray") %>%
    dyOptions(strokeWidth = 0.5, drawPoints = TRUE, pointSize = 1, gridLineWidth = 0.5)
}

gdx_hchart <- function(data, title, colores) {
  hchart(data, type = "line") %>%
  hc_title(text = title) %>%
  hc_colors(colores) %>% # Custom colors for series
  hc_plotOptions(line = list(lineWidth = 1))  # Set line width to 3 (you can adjust this value)
}


# 0 Sequence generation function -----

gen_dat <- function(start_date, end_date) {
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  return(dates)
}


# 4. Constantes         ----
# 1 MMpc equiv u Gj
u <- 1042.05986
fctmf <- 35.3147

'northbaja_01.R' %>%
  paste('02_source', ., sep = '/') %>%
  source(., echo = FALSE, encoding = 'utf-8')

'sempra_01.R' %>%
  paste('02_source', ., sep = '/') %>%
  source(., echo = FALSE, encoding = 'utf-8')


'sempra_01.R' %>%
    paste('02_source', ., sep = '/') %>%
    source(., echo = FALSE, encoding = 'utf-8')

'rbn_01.R' %>%
  paste('02_source', ., sep = '/') %>%
  source(., echo = FALSE, encoding = 'utf-8')

'sempra_02.R' %>%
  paste('02_source', ., sep = '/') %>%
  source(., echo = FALSE, encoding = 'utf-8')

'sempra_03.R' %>%
  paste('02_source', ., sep = '/') %>%
  source(., echo = FALSE, encoding = 'utf-8')

'sempra_04.R' %>%
  paste('02_source', ., sep = '/') %>%
  source(., echo = FALSE, encoding = 'utf-8')

'sempra_05.R' %>%
  paste('02_source', ., sep = '/') %>%
  source(., echo = FALSE, encoding = 'utf-8')

'sempra_06.R' %>%
  paste('02_source', ., sep = '/') %>%
  source(., echo = FALSE, encoding = 'utf-8')

'sempra_07.R' %>%
  paste('02_source', ., sep = '/') %>%
  source(., echo = FALSE, encoding = 'utf-8')

'sempra_08.R' %>%
  paste('02_source', ., sep = '/') %>%
  source(., echo = FALSE, encoding = 'utf-8')
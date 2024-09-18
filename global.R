###########################################
# global.R                                #
#                                         #
# @(2020-12-03)                           #
# @(Eduardo Prud'homme)                   #
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
require(rgdal)
require(readxl)
require(jsonlite)
require(tidyverse)
require(dplyr)
require(mxmaps)
require(leaflet)
require(leaflet.extras)
require(htmlwidgets)
require(shiny)
require(shinydashboard)
require(shinydashboard)
library(shinyjs)
require(sp)
require(DT)
require(sodium)
require(r2d3)
require(extrafont)
require(ggplot2)
require(ggthemes)
require(scales)
require(plotly)
require(networkD3)
require(knitr)
require(kableExtra)
require(dashboardthemes)
require(shinyalert)
library(dplyr)
library(readr)
library(reshape2)
library(shinycssloaders)
library(dygraphs)
library(xts)


# 2. Rutas              ----
data.path <- file.path(getwd(), "data/")
src.path  <- file.path(getwd(), "src/")

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

GadexThemePlotly <- function() {
  list(
    font = list(family = "Open Sans", color = '#cdcdcd'),  # Base font and color
    xaxis = list(tickfont = list(color = '#cdcdcd'),
                 tickangle = 90,  # Rotate x-axis text
                 showline = TRUE,  # Show axis line
                 linecolor = '#cdcdcd',
                 title = "",  # Remove axis title
                 zeroline = FALSE
                ),
    yaxis = list(tickfont = list(color = '#cdcdcd'),
                showline = TRUE,  # Show axis line
                linecolor = '#cdcdcd',
                zeroline = FALSE
                ),
    plot_bgcolor = 'rgba(0,0,0,0)',  # Transparent background for the plot area
    paper_bgcolor = 'rgba(0,0,0,0)',  # Transparent background for the entire figure
    legend = list( bgcolor = 'rgba(0,0,0,0)'),
    showlegend = TRUE  # Show legend
  )
}

# Paleta Gadex 
# 5 colores
palGADEX <- c("#18A478","#05647D","lightsteelblue3","palegreen","skyblue4")

# Paleta Gadex Completa
# 10 colores
palGADEXComp <- c('lightgreen', 'cadetblue', 'navy', '#59B353',
                  'deepskyblue', 'darkseagreen', '#00A5B8', 'steelblue1', 
                  '#066D8B', '#1F5999', '#333366', 'dodgerblue',
                  '#929292')

# Paleta Color Blind con Negro
# 8 colores
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# 4. Constantes         ----
# 1 MMpc equiv u Gj
u <- 1042.05986

# 5. Runs Scripts       -----
formatoComas <- function(n) {
  if (inherits(n, "Date")) {
    # If 'n' is a Date, format it as a date
    return(format(n, "%Y-%m-%d"))
  } else {
    # If 'n' is a number, format it with commas
    return(format(round(n, 2), nsmall = 0, big.mark = ","))
  }
}

'NGB.R' %>%
  paste('src', ., sep = '/') %>%
  source(., echo = FALSE, encoding = 'utf-8')

'FCPR.R' %>%
  paste('src', ., sep = '/') %>%
  source(., echo = FALSE, encoding = 'utf-8')

'TV.R' %>%
  paste('src', ., sep = '/') %>%
  source(., echo = FALSE, encoding = 'utf-8')

'TVS.R' %>%
  paste('src', ., sep = '/') %>%
  source(., echo = FALSE, encoding = 'utf-8')

'CO.R' %>%
  paste('src', ., sep = '/') %>%
  source(., echo = FALSE, encoding = 'utf-8')

'Prices.R' %>%
  paste('src', ., sep = '/') %>%
  source(., echo = FALSE, encoding = 'utf-8')

'PR.R' %>%
  paste('src', ., sep = '/') %>%
  source(., echo = FALSE, encoding = 'utf-8')

'Gadex.R' %>%
  paste('src', ., sep = '/') %>%
  source(., echo = FALSE, encoding = 'utf-8')

# 7. log-in             ----
credentials = data.frame(
  username_id =     c('EP',
                      'eduardo.prudhomme@gadex.mx',
                      'edgar.deleon@gadex.mx',
                      'laura.escobar@gadex.mx',
                      'andrea.castillo@gadex.mx',
                      'GadexTrial',
                      'james.alston@naturalgasintel.com',
                      'phaedra.friend@naturalgasintel.com',
                      'christopher.lenton@naturalgasintel.com',
                      'NGI',
                      'eaj@vitol.com',
                      'thania_delgado@kindermorgan.com',
                      'rguevara@zumma.com.mx',
                      'vrodriguez@zumma.com.mx',
                      # 'acovarrubias@ienova.com.mx',
                      # 'MacquarieTrial1',
                      # 'MacquarieTrial2',
                      'BPTrial1',
                      'BPTrial2',
                      'norberto.catalan@shell.com',
                      'alejandro.reyes@shell.com',
                      'diana.nicolas@shell.com',
                      'santiago.villarreal@shell.com',
                      'ignacio.guzman@shell.com',
                      'nicole.david@shell.com',
                      'montserrat.garay@shell.com',
                      'daniel_paniagua@tcenergy.com',
                      'Clark.Landrum@nexteraenergy.com',
                      'adoehner@finestra.com.mx',
                      'Invenergy',
                      'Invitado',
                      'Completo'
                      ),
  passod   = sapply(c('Nisha',
                      '9kC8cCUKG&$kbF7D', 
                      'x.meh2qQMc%YX7YZ',
                      'BSx-H_2sYeB6N*Bu',
                      'aEs&RyDE+YM2FPrR', 
                      'GadexTrial',
                      'wB^PjvYZbdv?7#Fs',
                      'DCQa$SqQbe5h5t#!',
                      '24UVvFRNBE25n!mg',
                      '14UVv2ReHE24n1Lv',
                      'qktxGRjCkELZVn8E', 
                      'wS9KrmqGHrXp8GWy',
                      'wS9KrmqGHrXp8GWy',
                      'rY8t5366Dq7prYcd',
                      # 'MYxDG2tf76rkVCu7',
                      # '3szDmCeNDh9K3SWp',
                      # 'pp6JUADK9b2N2L4Z',
                      'B943MXvPXHq9D2R9',
                      'cBaDh7B4eAarHMAV',
                      'Nt3Ya6JDE7wYTrR5',
                      'HrnQaT8zU7v7ADuh',
                      '4ttP7u3NJU7e9SHP',
                      'b7quE89Kw9mQmTjS',
                      'qAwUQ2a8X8Vfg5SP',
                      'G5Z6ts6254TfxRpE',
                      'GZR6EdkPNuv5bpsv',
                      'M7ZNk62LUVL5Lx4x',
                      'M32k62LUNL1Lx4y',
                      'b1KuE29kw9mQmTjS',
                      'b2KuE29kw7mLnHjS',
                      'LBXfZYdTNNk6saYL',
                      'cdcxBpsFfdKaT9sn'
                      ),
                    password_store),
  permission  = c(
    rep('all', 5), 'view', # Gadex
    rep('all', 4),         # NGI
    'view',                # Vitol
    'view',                # Kinder Morgan
    rep('view', 2),        # Zumma
    # 'view',              # IEnova
    # rep('view', 2),      # Macquaie
    rep('view', 2),        # BP
    rep('view', 7),        # Shell
    'view',                # TCenergia
    'view',                # NextEraEnergy
    'view',                # Finestra
    'view',                 # Invenergy 
    'view',
    'all'
    ),
  stringsAsFactors = F
)


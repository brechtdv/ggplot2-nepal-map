## Nepal District Map -- Human data
### Last update: 06/09/2015

## attach libraries
library(bd)
library(XLConnect)

## load mapping functions
source("nepal-map-utils.R")

## read data
wb <- loadWorkbook("data/Nepal_Rabies_Data.xlsx")
data <- readWorksheet(wb, sheet = "dog-bites")

## change district names to match SHP file
data$District[data$District == "Dhanusha"] <- "Dhanusa"
data$District[data$District == "Kavre"] <- "Kavrepalanchok"
data$District[data$District == "Sankhuwasava"] <- "Sankhuwasabha"
data$District[data$District == "Chitwan"] <- "Chitawan"
data$District[data$District == "Ramechap"] <- "Ramechhap"
data$District[data$District == "Sindhupalchowk"] <- "Sindhupalchok"

## obtain median per district
data$median <- apply(t(data[, 5:14]), 2, median)
range(data$median)

## define color palette
col <- c("white", muted("red"))

## define xy size
np_xy <- 8 * c(1000, 550) / 1000

## make plot
win.graph(np_xy[1], np_xy[2])
cairo_pdf("Fig2.pdf", np_xy[1], np_xy[2])
tiff("Fig2.tiff", np_xy[1], np_xy[2], units = "in", res = 300)
nepal_map(data, "District", "median",
          "Dog bites, annual\n(2004-2013)", col,
          legend.limits = c(0, max(data$median)),
          legend.breaks = c(0, 250, 500, 750, 1000))
graphics.off()

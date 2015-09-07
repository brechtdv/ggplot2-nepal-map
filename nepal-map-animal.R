### Nepal District Map -- Animal outbreak data
### Last update: 06/09/2015

## attach libraries
library(bd)
library(XLConnect)

## set dropbox working directory
setwd(dropbox("- Rabies/__maps"))
source("nepal-map-utils.R")

## read data
wb <- loadWorkbook("data/Nepal_Rabies_Data.xlsx")
data <- readWorksheet(wb, sheet = "animal-outbreaks")

## change district names to match SHP file
data$District[data$District == "Dhanusha"] <- "Dhanusa"
data$District[data$District == "Kavre"] <- "Kavrepalanchok"
data$District[data$District == "Sankhuwasava"] <- "Sankhuwasabha"
data$District[data$District == "Chitwan"] <- "Chitawan"
data$District[data$District == "Ramechap"] <- "Ramechhap"
data$District[data$District == "Sindhupalchowk"] <- "Sindhupalchok"

## obtain median per district
data$sum <- apply(t(data[, 5:14]), 2, sum)
range(data$sum)
data[order(data$sum), c(1, 3, 15)]

## define color palette
col <- c("white", muted("red"))

## define xy size
np_xy <- 8 * c(1000, 550) / 1000

## make plot
win.graph(np_xy[1], np_xy[2])
cairo_pdf("Fig1.pdf", np_xy[1], np_xy[2])
tiff("Fig1.tiff", np_xy[1], np_xy[2], units = "in", res = 300)
nepal_map(data, "District", "sum",
          "Rabies outbreaks\n(2005-2014)", col,
          legend.limits = c(0, max(data$sum)))
graphics.off()
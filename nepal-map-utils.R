library(rgeos)
library(maptools)
library(ggplot2)
library(scales)
library(plyr)
library(reshape2)

# MAP setup
np_dist <- readShapeSpatial("baselayers/NPL_adm3.shp")
np_distf <- fortify(np_dist, region="NAME_3")

KTM <- c("Kathmandu", "Bhaktapur", "Lalitpur")

choropleth <- function(data, x, y, exp_x, exp_y, border, bg, ktm = FALSE) {
  # force upper case for better matching
  data[, x] <- as.character(toupper(data[, x]))
  np_distf$id <- as.character(toupper(np_distf$id))
  # basic contract, data$x and np_distf$id has to include the same things
  stopifnot(all(data[, x] %in% levels(factor(np_distf$id))))

  ggplot() + 
    geom_map(data = data, aes_string(map_id = x, fill = y),
             map = np_distf, colour = border) + 
    expand_limits(x = exp_x, y = exp_y) + 
    scale_fill_gradient(low = "white", high = muted("blue")) +
    districtoverlay(ktm) +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = bg))
}

# district overlay
districts <- read.csv("baselayers/districts.csv")
districtoverlay <-
function(ktm = FALSE) {
  if (ktm) {
    districts <- districts[districts$X %in% KTM, ]

  } else {
    districts <- districts[!(districts$X %in% KTM), ]
  }

  geom_text(data = districts,
            aes(x = clong, y = clat, label = abbreviate(id, 3)),
            size = 2.5)
}

# np choropleth
npchoropleth <-
function (data, x, y, border = "grey", bg = "white") {
  exp_x <- np_distf$long
  exp_y <- np_distf$lat
  choropleth(data, x, y, exp_x, exp_y, border, bg, ktm = FALSE)
}

# ktm choropleth
ktmchoropleth <-
function (data, x, y, border = "grey", bg = "white") {
  exp_x <- np_distf[np_distf$id %in% KTM, "long"]
  exp_y <- np_distf[np_distf$id %in% KTM, "lat"]

  choropleth(data, x, y, exp_x, exp_y, border, bg, ktm = TRUE) +
    scale_fill_gradient(low = col[1], high = col[2]) +
    theme(legend.position = "none",
          panel.background = element_rect(colour = "#333333"),
          plot.background = element_rect(fill = "transparent", colour = NA),
          plot.title = element_text(size = rel(.75))) +
    labs(title = "Kathmandu Valley") +
    geom_map(data = data[!(data$District %in% KTM), ], map = np_distf,
             aes_string(map_id = "District"),
             fill = "grey95", colour = "grey") +
    theme(panel.background = element_rect(fill = "#333333"))
}

ktm_rect <-
function(border = "grey") {
  exp_x <- np_distf[np_distf$id %in% KTM, "long"]
  exp_y <- np_distf[np_distf$id %in% KTM, "lat"]

  ggplot(data = data[data$District %in% KTM, ]) + 
    geom_map(aes(map_id = District), map = np_distf,
             fill = "transparent", colour = NA) +
    expand_limits(x = exp_x, y = exp_y) +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "transparent",
                                          colour = border),
          plot.background = element_rect(fill = "transparent",
                                         colour = NA),
          plot.title = element_text(colour = "transparent",
                                    size = rel(.75))) +
    labs(title = "Kathmandu Valley")
}

nepal_map <-
function(data, x, y, legend, col = c("white", muted("red")),
         ktm.border = "grey50", legend.limits = NULL, legend.breaks = waiver()) {

  ## main plot
  main <-
    npchoropleth(data, x, y, bg = "white") +
      scale_fill_gradient(name = legend, low = col[1], high = col[2],
                          limits = legend.limits, breaks = legend.breaks) +
      theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
      geom_map(data = data[data[, x] %in% KTM, ],
               aes_string(map_id = x), fill = "grey90",
               map = np_distf, colour = "grey")
  print(main)

  ## ktm valley inset
  print(ktmchoropleth(data, x, y, bg = "white"),
        vp = grid::viewport(0, 0, .2, .5, just = c(0, 0)))
  print(ktm_rect(ktm.border),
        vp = grid::viewport(0, 0, .2, .5, just = c(0, 0)))
}
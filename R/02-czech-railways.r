############################################################
#                 Map railways using OpenStreetMap data in R
#                 Milos Popovic
#                 2023/04/09
#############################################################
# libraries we need
libs <- c(
    "tidyverse", "sf",
    "giscoR", "httr",
    "XML"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# MAKE WORKING DIRECTORY
#-----------------------
# Czechia dir
czechia_dir <- "czechia_osm"
dir.create(czechia_dir)
out_dir_czechia <- main_path |>
    paste0("/", czechia_dir)
setwd(out_dir_czechia)
getwd()

# METHOD 1: SINGLE OSM FILE
#-----------------------------------
### download
url1 <- "https://download.geofabrik.de/europe/czech-republic-latest-free.shp.zip"
download.file(url1, destfile = basename(url1), mode = "wb")
list.files()
unzip("czech-republic-latest-free.shp.zip")
list.files(pattern = "*.shp")

### unzip
zip_file <- list.files()
zip_name <- grep("railways", unzip(zip_file, list = T)$Name,
    ignore.case = T, value = T
)
unzip(zip_file, files = zip_name, exdir = out_dir_czechia, overwrite = T)
list.files()

### load shapefile
rail_cz <- sf::st_read("gis_osm_railways_free_1.shp")
unique(rail_cz$fclass)

# load national map
get_czechia_sf <- function() {
    czechia_sf <- giscoR::gisco_get_countries(
        year = "2016", epsg = "4326",
        resolution = "3", country = "CZ"
    )

    return(czechia_sf)
}

czechia_sf <- get_czechia_sf()

ggplot() +
    geom_sf(
        data = czechia_sf,
        fill = "transparent",
        color = "#0FAAB8", size = 1.15
    ) +
    geom_sf(
        data = subset(rail_cz, fclass %in% c("rail", "narrow_gauge")),
        color = "#B82178", size = 1.25
    ) +
    theme_void()

# MAP
#-------
p <- ggplot() +
    geom_sf(
        data = czechia_sf,
        fill = "transparent", color = "#07CFF7", 
        size = .1
    ) +
    geom_sf(
        data = subset(
            rail_cz, fclass %in% c("rail", "narrow_gauge")
        ),
        color = "#FFB115", size = .15
    ) +
    labs(
        x = "",
        y = "",
        title = "Czech railways",
        subtitle = "",
        caption = 
        "©2023 Milos Popovic (https://milospopovic.net) Data: ©OpenStreetMap contributors"
    ) +
    theme_minimal() +
    theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_line(color = "#010D1F", size = 0),
        panel.grid.minor = element_blank(),
        plot.title = element_text(
            face = "bold", size = 24, color = "grey90", hjust = .5
        ),
        plot.caption = element_text(
            size = 10, color = "grey90",
            hjust = .5, vjust = 0
        ),
        plot.margin = unit(
            c(t = 0, r = 0, b = 0, l = 0), "lines"
        ),
        plot.background = element_rect(fill = "#010D1F", color = NA),
        panel.background = element_rect(fill = "#010D1F", color = NA),
        legend.background = element_rect(fill = "#010D1F", color = NA),
        panel.border = element_blank()
    )

ggsave(
    filename = "czechia_railways.png",
    width = 7.5, height = 7, dpi = 600, device = "png", p
)

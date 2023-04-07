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

# MAKE WORKING DIRECTORIES
#-------------------------
# Spain dir
main_path <- "" #set your own working directory
setwd(main_path)
spain_dir <- "spain_osm"
dir.create(spain_dir)
out_dir_spain <- main_path |>
    paste0("/", spain_dir)
out_dir_spain
setwd(out_dir_spain)

# METHOD 2: DOWNLOAD MULTIPLE OSM FILES
#-------------------------------
# Spain URL
url <- paste0("https://download.geofabrik.de/europe/spain.html")
get_osm_links <- function() {
    # make http request
    res <- httr::GET(url)
    # parse data to html format
    parse <- XML::htmlParse(res)
    # scrape all the href tags
    links <- XML::xpathSApply(parse, path = "//a", XML::xmlGetAttr, "href")
    # make all links and store in a list
    for (l in links) {
        all_links <- paste0(url, links)
    }

    return(all_links)
}

all_links <- get_osm_links()

spain_links <- all_links[grepl("latest-free.shp.zip", all_links)] |>
    stringr::str_remove(".htmlspain")

spain_links

# DOWNLOAD SPAIN DATA
#------------------------
for (link in spain_links) {
    download.file(link, destfile = basename(link), mode = "wb")
}

list.files()

# UNZIP SPAIN DATA
#---------------------
# unzipping and renaming
setwd("..")
spain_clone_dir <- "spain_clone_osm"
dir.create(spain_clone_dir)
spain_clone_dir_out <- main_path |>
    paste0("/", spain_clone_dir)

zip_files <- list.files()

for (z in 1:length(zip_files)) {
    zip_names <- grep("railways", unzip(zip_files[z], list = T)$Name,
        ignore.case = T, value = T
    )
    unzip(
        zip_files[z],
        files = zip_names,
        exdir = spain_clone_dir_out, overwrite = F
    )
    x <- sample(1:length(zip_files), 1, replace = T)
    file_old <- c(list.files(spain_clone_dir_out)) # existing file names
    file_new <- c(paste0(x, "_", file_old))
    file.rename(
        paste0(spain_clone_dir_out, "/", file_old),
        paste0(spain_clone_dir_out, "/", file_new)
    )
    rm(file_old)
    rm(file_new) # Delete vectors from the environment
}

setwd(spain_clone_dir_out)
list.files()

# LOAD ALL REGION-LEVEL RAILWAYS
#-------------------------------
get_railways <- function() {
    railway_files <- list.files(
        path = spain_clone_dir_out,
        pattern = "*.shp", full.names = T
    )
    railway_list <- lapply(railway_files, sf::st_read)
    spain_railway_sf <- do.call(rbind, railway_list)
    return(spain_railway_sf)
}

spain_railway_sf <- get_railways()

# 4. SHP OF SPAIN
#-----------------
# load national map
get_spain_sf <- function() {
    spain_sf <- giscoR::gisco_get_countries(
        year = "2016", epsg = "4326",
        resolution = "3", country = "ES"
    )

    return(spain_sf)
}

spain_sf <- get_spain_sf()

ggplot() +
    geom_sf(
        data = spain_sf,
        fill = "transparent", color = "#0FAAB8", size = 1.15
    ) +
    geom_sf(
        data = subset(spain_railway_sf, fclass %in% c("rail", "narrow_gauge")),
        color = "#B82178", size = 1.25
    ) +
    theme_void()

# MAP
#-------
p <- ggplot() +
    geom_sf(
        data = spain_sf,
        fill = "transparent", color = "#07CFF7", 
        size = .1
    ) +
    geom_sf(
        data = subset(
            spain_railway_sf, fclass %in% c("rail", "narrow_gauge")
        ),
        color = "#FFB115", size = .15
    ) +
    labs(
        x = "",
        y = "",
        title = "Spanish railways",
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
    filename = "spain_railways.png",
    width = 7, height = 7.5, dpi = 600, device = "png", p
)

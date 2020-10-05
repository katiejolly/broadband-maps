library(shiny)
library(sf)
library(tidyverse)
library(showtext)
library(ggrepel)
library(janitor)
library(ggtext)
library(glue)
library(cowplot)
library(shadowtext)
library(tippy)
library(ggspatial)
library(raster)
library(ragg)
library(lwgeom)
library(Cairo)
library(DBI)
library(formattable)

font_add_google(name = "Lato", regular.wt = 300)
font_add_google(name = "Karla")

showtext_auto()

# load("shiny_data.RData")

con <- dbConnect(RSQLite::SQLite(), "ofg_local.db")

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n") %>%
    paste0("\n")
}

transform_utm <- function(x){
  # Checks
  if(!"sf" %in% class(x)){
    rlang::abort(message = "It doesn't look like your data is an sf object.")
  }
  
  longitude <- x %>%
    st_transform(4326) %>% # needs to be in decimal degrees
    st_union() %>% # combine all geometries
    st_centroid() %>% # centroid
    st_coordinates() %>% # just the centroid coordinates
    as_tibble() %>% # matrix to tibble
    pull(X) # just the longitude
  
  latitude <- x %>%
    st_transform(4326) %>% # needs to be in decimal degrees
    st_union() %>% # combine all geometries
    st_centroid() %>% # centroid
    st_coordinates() %>% # just the centroid coordinates
    as_tibble() %>% # matrix to tibble
    pull(Y) # just the latitude
  
  utm_zone <- paste0(if_else(latitude >= 0, "269", "327"), ceiling((longitude + 180) / 6) %>% as.character()) %>% as.numeric()
  message(paste0("The centroid of your area is in UTM zone "), as.character(utm_zone))
  
  x_transformed <- st_transform(x, crs = utm_zone)
  return(x_transformed)
}

map_colors <- rev(c("#23a6ab", "#8fcfb1", "#fce22d", "#fcb901"))
  
  # c('#de77ae','#f1b6da','#b8e186','#a2c671')

label_size <- 7

legend_text_size <- 24

title_size <- 70

caption_size <- 20

subtitle_size <- 30

county_names <- tbl(con, "counties") %>%
  filter(wkt != "wkt") %>%
  distinct(county_full_name) %>%
  arrange(county_full_name) %>%
  dplyr::select(county_full_name) %>%
  rename(`County Name` = county_full_name) %>%
  collect()
  
urban_names <- tbl(con, "urban") %>%
  filter(wkt != "wkt") %>%
  distinct(name) %>%
  arrange(name) %>%
  dplyr::select(name) %>%
  rename(`City Name` = name) %>%
  collect()

states <- tbl(con, "states") %>%
  filter(wkt != "wkt") %>%
  collect() %>%
  st_as_sf(wkt = "wkt", crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

cities <- tbl(con, "cities") %>%
  filter(wkt != "wkt") %>%
  collect() %>%
  st_as_sf(wkt = "wkt", crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

# legend <- image_read_svg("ggplot_legend.svg", height = 100)

plot_styling <- list(
  scale_fill_manual(
  values = map_colors,
  guide = guide_legend(
    title = "Fixed Internet Download Speed",
    direction = "horizontal",
    keyheight = unit(4, units = "mm"),
    keywidth = unit(50 / length(labels), units = "mm"),
    title.position = 'top',
    # I shift the labels around, the should be placed 
    # exactly at the right end of each legend key
    title.hjust = 1,
    label.hjust = 1,
    nrow = 1,
    byrow = T,
    order = 1,
    label.theme = element_text(size = legend_text_size),
    label.position = "bottom"
  ),
  labels = c("0 to 10 Mbps", "10 to 25 (slow)", "25 to 100 (broadband)", "100+ (high-speed)"),
  drop = FALSE
),
  scale_x_continuous(expand = expansion(mult = c(.25, .25))),
  scale_y_continuous(expand = expansion(mult = c(.15, .15))),
  theme_void(),
  theme(
    text = element_text(family = "Lato", size = legend_text_size),
    legend.position = "top",
    legend.text = element_text(family = "Lato", lineheight = 0.6),
    plot.title = element_textbox_simple(size = title_size, margin=margin(30,0,30,0), color = "gray30"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(margin=margin(0,0,30,0), size = caption_size, lineheight = 0.4, family = "Lato",  face= "italic"),
    plot.subtitle = element_text(family = "Karla", size = subtitle_size),
    plot.margin=unit(c(1, 1, 1, 1),"cm")
  ),
  annotation_scale(plot_unit = "m", line_width = 0.8, bar_cols = c("gray40", "white"), text_family = "Lato", location = "br", style = "ticks", text_cex = 2, unit_category = "imperial")
)

vals <- reactiveValues()

# Define UI
ui <- function(request) {
  navbarPage(
  "Local Fixed Internet Maps",
  tabPanel(
    "About",
    fluidPage(
      includeCSS("www/style.css"),
      verticalLayout(
        wellPanel(
          p("This is a tool to help you learn about broadband internet in different communities across the United States. The goal is to provide people with easy-to-read maps and direct you to data sources if you want something more detailed."),
          h5("How to use this tool"),
          p("First, decide whether you want to you want maps of urban places or counties. In this case, I am using the definition of urban place from the U.S. Census Bureau-- this means broadly most communities with at least 2,500 people. Keep in mind that some towns are included in the urban areas of others. For example, most Seattle suburbs are included in the Seattle urban area and do not have their own maps. In this quick demo I will make a map of a county."),
          h6("Choose 'random' or a specific place name"),
          img(src = "choose-county.png", height="30%", width="30%"),
          p("If you know which place you're looking for, just type into the search bar until it comes up. If you are just curious about internet performance in different communities, selecting 'Random' is a fun option to learn about some new ones Once you've chosen, decide whether or not you want a Carto basemap (this is nice for providing some context to the otherwise minimal maps) and then draw the map! This step can take a little bit to load, especially with a basemap."),
          h6("Download the map and/or data"),
          p("Once you have a map you can save it and download the data that was used to draw it. I recommend saving by right-clicking > 'Save Image As.' If you download the data you will get a csv that has the tile geometry as WKT. This is readable by all of the major GIS programs as well as programming languages like R and Python."),
          h5("How to read the maps"),
          HTML("The FCC currently recommends at least 25 Mbps download/3 Mbps upload for a broadband internet connection, although many experts consider that to be the <a href='https://www.govtech.com/network/Does-the-Federal-Broadband-Definition-Reflect-Real-World-Need.html'>minimum, rather than a goal</a>. Most people need better internet than that especially as more things are happening online like school, health, and socializing. These maps are designed to highlight places that are struggling to keep up with these demands, rather than the places with the highest speeds. The legend categories reflect this design decision with smaller categories towards the end of the spectrum. Everything above 100 Mbps is grouped into one category."),
          br(),
          img(src = "guide.png", height="60%", width="60%"),
          br(),
          h5("Where to learn more about broadband internet"),
          tags$div(tags$ul(
            tags$li(tags$a(href = 'https://www.digitalinclusion.org/', "National Digital Inclusion Alliance")),
            tags$li(tags$a(href = 'https://www.speedtest.net', "Ookla (Speedtest)")),
            tags$li(tags$a(hreg = "https://www.pewresearch.org/topics/digital-divide/", "Pew Research Center: Digital Divide")))),
          h5("Where to let me know about issues, comments, or questions"),
          tags$a(href = "https://github.com/katiejolly/broadband-maps/issues", "github.com/katiejolly/broadband-maps/issues"),
          h5("How to cite this tool"),
          HTML("<p>Jolly, K. (2020 October). <i>Local Broadband Maps</i>. Retrieved from katiejolly.io/broadband-maps.</p>"),
          h5("Just want to get all the data?"),
          tags$a(href = "https://github.com/teamookla/ookla-open-data", "Ookla Open Data")
        )
      )
    )),
  tabPanel(
    "Urbanized Places",
    fluidPage(
      includeCSS("www/style.css"),
      verticalLayout(
        wellPanel(
          tags$form(
            class = "form-horizontal",
            tags$div(
              class = "form-group",
              tags$label(class = "col-sm-2 control-label", `for` = "place_name", toupper("urbanized place")),
              column(width = 4, selectizeInput(
                inputId = "place_name",
                label = NULL,
                choices = c("Random", urban_names),
                multiple = FALSE,
                options = list(
                  placeholder = "Search or select 'Random'",
                  onInitialize = I('function() { this.setValue(""); }'),
                  maxOptions = 3595
                )
              )),

              tags$label(class = "col-sm-2 control-label", `for` = "backround_city", toupper("basemap")),
              column(width = 2, checkboxInput(inputId = "background_city", label = NULL, value = FALSE)),
              column(width = 2, actionButton("go", "Draw map"))
            )
          ),
          br(),
          
          # tippy("What should I be looking for in the map?", tooltip = "The FCC defines broadband as a connection with a minimum download speed of 25 Mbps and minimum upload speed of 3 Mbps, but that should be thought of as a baseline and not a goal. Connections with at least 100 Mbps download are better for multiple users and modern uses like telehealth and remote work. Internet connections below the broadband threshold may experience interruptions or struggle to manage high use.", width = "900px"),
          
          
          br(),

          plotOutput("city_map", width = 900, height = 900),

          br(),

          downloadButton("downloadCity", label = "Download city map"),
          downloadButton("downloadCityData", label = "Download city data"),
          
          br(),
          
          br(),
          tippy("What is an urbanized place?", tooltip = "For the 2010 Census, an urban area will comprise a densely settled core of census tracts and/or census blocks that meet minimum population density requirements, along with adjacent territory containing non-residential urban land uses as well as territory with low population density included to link outlying densely settled territory with the densely settled core. Urbanized areas (UAs) have 50,000+ people and urban clusters (UCs) have 2,500 to 50,000 people"),
          
          br(),
          
          HTML("Reference maps for urban areas and clusters with 2010 census boundaries are available <a href='https://www.census.gov/geographies/reference-maps/2010/geo/2010-census-urban-areas.html'>here</a>"),
          
          br(),

          br()
        )
      )
    )
  ),
  tabPanel(
    "Counties",
    fluidPage(
      includeCSS("www/style.css"),
      verticalLayout(
        wellPanel(
          tags$form(
            class = "form-horizontal",
            tags$div(
              class = "form-group",
              tags$label(class = "col-sm-2 control-label", `for` = "county_name", toupper("county")),
              column(width = 4, selectizeInput(
                inputId = "county_name",
                label = NULL,
                choices = c("Random", county_names),
                multiple = FALSE,
                options = list(
                  placeholder = "Search or select 'Random'",
                  onInitialize = I('function() { this.setValue(""); }'),
                  maxOptions = 3218
                )
              )),
              tags$label(class = "col-sm-2 control-label", `for` = "backround_county", toupper("basemap")),
              column(width = 2, checkboxInput(inputId = "background_county", label = NULL, value = FALSE)),
              column(width = 2, actionButton("goCounty", "Draw map"))
            )
          ),
          br(),

          plotOutput("county_map", width = 900, height = 900),

          br(),

          downloadButton("downloadCounty", label = "Download county map"),
          downloadButton("downloadCountyData", label = "Download county data"),
          
          br(),
          br()
        )
      )
    )
  )
)
}

# Define server logic
server <- function(input, output) {


  city <- eventReactive(input$go, {
    if(input$place_name != "Random") {
      x <- tbl(con, "urban") %>%
        filter(wkt != "wkt") %>%
        filter(name == !!input$place_name) %>%
        collect() %>%
        st_as_sf(wkt = "wkt", crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    } else {
      x <- dbGetQuery(con, "SELECT * FROM urban ORDER BY RANDOM() LIMIT 1") %>%
        filter(wkt != "wkt") %>%
        st_as_sf(wkt = "wkt", crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    }
    
    x <- x %>%
      transform_utm()
  })
  
  state_city <- eventReactive(input$go ,{
    st_join(states %>% st_transform(st_crs(city())), city(), left = FALSE)
  })


  county <- eventReactive(input$goCounty, {
    if(input$county_name != "Random") {
      x <- tbl(con, "counties") %>%
        filter(wkt != "wkt") %>%
        filter(county_full_name == !!input$county_name) %>%
        collect() %>%
        st_as_sf(wkt = "wkt", crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    } else {
      x <- dbGetQuery(con, "SELECT * FROM counties ORDER BY RANDOM() LIMIT 1") %>%
        filter(wkt != "wkt") %>%
        st_as_sf(wkt = "wkt", crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    }
    x <- x %>% transform_utm()
  })
  
  county_city <- eventReactive(input$goCounty ,{
    st_join(states %>% st_transform(st_crs(county())), county(), left = FALSE)
  })

  tiles_city <- eventReactive(input$go, {
    tbl(con, "urban_area_tiles") %>%
      filter(tile != "tile") %>%
      filter(name %in% !!city()$name) %>%
      collect() %>%
      mutate(avg_d_mbps_cat = cut(avg_d_kbps / 1000, c(0, 10, 25, 100, max(max(avg_d_kbps / 1000), 101)),
                                  ordered_result = TRUE)) %>%
      st_as_sf(wkt = "tile", crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") %>%
      st_transform(st_crs(city()))
  })

  tiles_county <- eventReactive(input$goCounty, {
    tbl(con, "county_tiles") %>%
      filter(tile != "tile") %>%
      filter(name %in% !!county()$county_full_name) %>%
      collect() %>%
      mutate(avg_d_mbps_cat = cut(avg_d_kbps / 1000, c(0, 10, 25, 100, max(max(avg_d_kbps / 1000), 101)), 
                                  ordered_result = TRUE)) %>%
      st_as_sf(wkt = "tile", crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") %>%
      st_transform(st_crs(county()))
  })


  title_city <- eventReactive(input$go, {
    label <- city() %>%
      pull(long_name)
    
    label <- paste0("Fixed internet download speed in " , label)
  })

  title_county <- eventReactive(input$goCounty, {
    label <- county() %>%
      pull(county_full_name)

    paste0(label)
  })
  
  subtitle_city <- eventReactive(input$go, {
    label <-  glue("The average download speed is {round(weighted.mean(tiles_city()$avg_d_kbps/1000, w = tiles_city()$tests), 2)} Mbps based on {comma(sum(tiles_city()$tests), digits = 0)} tests in {comma(nrow(tiles_city()), digits = 0)} tiles")
    
    paste0(label, "\n\n")
  })
  
  subtitle_county <- eventReactive(input$goCounty, {
    label <-  glue("The average download speed is {round(weighted.mean(tiles_county()$avg_d_kbps/1000, w = tiles_county()$tests), 2)} Mbps based on {comma(sum(tiles_county()$tests), digits = 0)} tests in {comma(nrow(tiles_county()), digits = 0)} tiles")
    
    paste0(label, "\n\n")
  })


  label_cities <- eventReactive(input$go, {
    label_cities <- st_intersection(cities %>% st_transform(st_crs(city())), city())


    if (nrow(label_cities) == 0) {
      st_centroid(city()) %>%
        cbind(st_coordinates(.)) %>%
        st_set_geometry(NULL) %>%
        rename(city = name) %>%
        mutate(city = str_remove(city, ",.*$"))
    } else {
      label_cities %>%
        top_n(n = 4, wt = population) %>%
        cbind(st_coordinates(.)) %>%
        st_set_geometry(NULL)
    }
  })


  label_cities_county <- eventReactive(input$goCounty, {
    label_cities <- st_intersection(cities %>% st_transform(st_crs(county())), county())


    if (nrow(label_cities) == 0) {
      st_centroid(county()) %>%
        cbind(st_coordinates(.)) %>%
        st_set_geometry(NULL) %>%
        rename(city = county_full_name)
    } else {
      label_cities %>%
        top_n(n = 4, wt = population) %>%
        cbind(st_coordinates(.)) %>%
        st_set_geometry(NULL)
    }
  })


  # hwy_crop <- eventReactive(input$go, {
  #   st_crop(hwy %>% st_transform(st_crs(city())), st_bbox(city()))  %>% 
  #     filter(st_geometry_type(.) %in% c("LINESTRING", "MULTILINESTRING")) %>%
  #     st_cast("LINESTRING")
  # })
  # 
  # hwy_crop_county <- eventReactive(input$goCounty, {
  #   st_crop(hwy %>% st_transform(st_crs(county())), st_bbox(county())) %>% 
  #     filter(st_geometry_type(.) %in% c("LINESTRING", "MULTILINESTRING")) %>%
  #     st_cast("LINESTRING")
  # })
  
  plot_start_county <- eventReactive(input$goCounty, {
    if(input$background_county == TRUE) {
      
      p_county <- ggplot(county()) +
        geom_sf(aes(color = "0 to 10\nMbps"), lwd = 0, fill = NA) +
        scale_color_manual(name = NULL,
                           values = c(map_colors[1]), 
                           guide = guide_legend(label.hjust = 1,
                                                label.position = "bottom",
                                                order = 1, 
                                                keywidth = 2, 
                                                keyheight = unit(4, units = "mm"),
                                                direction = "horizontal",
                                                override.aes = 
                                                  list(fill = map_colors[1], 
                                                       color = "gray15", 
                                                       lwd = 0.04))) + 
        ggnewscale::new_scale_color() +
        geom_sf(aes(color = "10 to 25"), lwd = 0, fill = NA) +
        scale_color_manual(name = NULL,
                           values = c(map_colors[2]), 
                           guide = guide_legend(label.hjust = 1,
                                                label.position = "bottom",
                                                order = 2, 
                                                keywidth = 3, 
                                                keyheight = unit(4, units = "mm"),
                                                direction = "horizontal",
                                                override.aes = 
                                                  list(fill = map_colors[2], 
                                                       color = "gray15", 
                                                       lwd = 0.04))) +
        ggnewscale::new_scale_color() +
        geom_sf(aes(color = "25 to 100"), lwd = 0, fill = NA) +
        scale_color_manual(name = NULL,
                           values = c(map_colors[3]), 
                           guide = guide_legend(label.hjust = 1,
                                                label.position = "bottom",
                                                order = 3, 
                                                keywidth = 15, 
                                                keyheight = unit(4, units = "mm"),
                                                direction = "horizontal",
                                                override.aes = 
                                                  list(fill = map_colors[3], 
                                                       color = "gray15", 
                                                       lwd = 0.04))) +
        ggnewscale::new_scale_color() +
        geom_sf(aes(color = "100 +"), lwd = 0, fill = NA) +
        scale_color_manual(name = NULL,
                           values = c(map_colors[4]),
                           guide = guide_legend(label.hjust = 1,
                                                label.position = "bottom",
                                                order = 4,
                                                keywidth = 10,
                                                keyheight = unit(4, units = "mm"),
                                                direction = "horizontal",
                                                override.aes =
                                                  list(fill = map_colors[4],
                                                       color = "gray15",
                                                       lwd = 0.04))) +
        annotation_map_tile(type = "cartolight", zoomin = 0, alpha = 0.8, forcedownload = TRUE, quiet = FALSE, cachedir = NULL) +
        geom_sf(fill = NA, color = "gray20", lwd = 0.3, linetype = 2)
      
    } else {
      p_county <- ggplot(county()) +
        geom_sf(aes(color = "0 to 10\nMbps"), lwd = 0, fill = NA) +
        scale_color_manual(name = NULL,
                           values = c(map_colors[1]), 
                           guide = guide_legend(label.hjust = 1,
                                                label.position = "bottom",
                                                order = 1, 
                                                keywidth = 2, 
                                                keyheight = unit(4, units = "mm"),
                                                direction = "horizontal",
                                                override.aes = 
                                                  list(fill = map_colors[1], 
                                                       color = "gray15", 
                                                       lwd = 0.04))) + 
        ggnewscale::new_scale_color() +
        geom_sf(aes(color = "10 to 25"), lwd = 0, fill = NA) +
        scale_color_manual(name = NULL,
                           values = c(map_colors[2]), 
                           guide = guide_legend(label.hjust = 1,
                                                label.position = "bottom",
                                                order = 2, 
                                                keywidth = 3, 
                                                keyheight = unit(4, units = "mm"),
                                                direction = "horizontal",
                                                override.aes = 
                                                  list(fill = map_colors[2], 
                                                       color = "gray15", 
                                                       lwd = 0.04))) +
        ggnewscale::new_scale_color() +
        geom_sf(aes(color = "25 to 100"), lwd = 0, fill = NA) +
        scale_color_manual(name = NULL,
                           values = c(map_colors[3]), 
                           guide = guide_legend(label.hjust = 1,
                                                label.position = "bottom",
                                                order = 3, 
                                                keywidth = 15, 
                                                keyheight = unit(4, units = "mm"),
                                                direction = "horizontal",
                                                override.aes = 
                                                  list(fill = map_colors[3], 
                                                       color = "gray15", 
                                                       lwd = 0.04))) +
        ggnewscale::new_scale_color() +
        geom_sf(aes(color = "100 +"), lwd = 0, fill = NA) +
        scale_color_manual(name = NULL,
                           values = c(map_colors[4]),
                           guide = guide_legend(label.hjust = 1,
                                                label.position = "bottom",
                                                order = 4,
                                                keywidth = 10,
                                                keyheight = unit(4, units = "mm"),
                                                direction = "horizontal",
                                                override.aes =
                                                  list(fill = map_colors[4],
                                                       color = "gray15",
                                                       lwd = 0.04))) +
        geom_sf(fill = "gray99", color = "gray20", lwd = 0.1)
    }
  })
  
  
  plot_start_city <- eventReactive(input$go, {
    if(input$background_city == TRUE) {
      p_city <- ggplot(city()) +
        geom_sf(aes(color = "0 to 10\nMbps"), lwd = 0, fill = NA) +
        scale_color_manual(name = NULL,
                           values = c(map_colors[1]), 
                           guide = guide_legend(label.hjust = 1,
                                                label.position = "bottom",
                                                order = 1, 
                                                keywidth = 2, 
                                                keyheight = unit(4, units = "mm"),
                                                direction = "horizontal",
                                                override.aes = 
                                                  list(fill = map_colors[1], 
                                                       color = "gray15", 
                                                       lwd = 0.04))) + 
        ggnewscale::new_scale_color() +
        geom_sf(aes(color = "10 to 25"), lwd = 0, fill = NA) +
        scale_color_manual(name = NULL,
                           values = c(map_colors[2]), 
                           guide = guide_legend(label.hjust = 1,
                                                label.position = "bottom",
                                                order = 2, 
                                                keywidth = 3, 
                                                keyheight = unit(4, units = "mm"),
                                                direction = "horizontal",
                                                override.aes = 
                                                  list(fill = map_colors[2], 
                                                       color = "gray15", 
                                                       lwd = 0.04))) +
        ggnewscale::new_scale_color() +
        geom_sf(aes(color = "25 to 100"), lwd = 0, fill = NA) +
        scale_color_manual(name = NULL,
                           values = c(map_colors[3]), 
                           guide = guide_legend(label.hjust = 1,
                                                label.position = "bottom",
                                                order = 3, 
                                                keywidth = 15, 
                                                keyheight = unit(4, units = "mm"),
                                                direction = "horizontal",
                                                override.aes = 
                                                  list(fill = map_colors[3], 
                                                       color = "gray15", 
                                                       lwd = 0.04))) +
        ggnewscale::new_scale_color() +
        geom_sf(aes(color = "100 +"), lwd = 0, fill = NA) +
        scale_color_manual(name = NULL,
                           values = c(map_colors[4]), 
                           guide = guide_legend(label.hjust = 1,
                                                label.position = "bottom",
                                                order = 4, 
                                                keywidth = 10, 
                                                keyheight = unit(4, units = "mm"),
                                                direction = "horizontal",
                                                override.aes = 
                                                  list(fill = map_colors[4], 
                                                       color = "gray15", 
                                                       lwd = 0.04))) +
        annotation_map_tile(type = "cartolight", 
                            zoomin = 0, 
                            alpha = 0.8, 
                            forcedownload = TRUE, quiet = FALSE) +
        geom_sf(fill = NA, color = "gray20", lwd = 0.3, linetype = 2) 
    } else {
      p_city <- ggplot(city()) +
        geom_sf(aes(color = "0 to 10\nMbps"), lwd = 0, fill = NA) +
        scale_color_manual(name = NULL,
                           values = c(map_colors[1]), 
                           guide = guide_legend(label.hjust = 1,
                                                label.position = "bottom",
                                                order = 1, 
                                                keywidth = 2, 
                                                keyheight = unit(4, units = "mm"),
                                                direction = "horizontal",
                                                override.aes = 
                                                  list(fill = map_colors[1], 
                                                       color = "gray15", 
                                                       lwd = 0.04))) + 
        ggnewscale::new_scale_color() +
        geom_sf(aes(color = "10 to 25"), lwd = 0, fill = NA) +
        scale_color_manual(name = NULL,
                           values = c(map_colors[2]), 
                           guide = guide_legend(label.hjust = 1,
                                                label.position = "bottom",
                                                order = 2, 
                                                keywidth = 3, 
                                                keyheight = unit(4, units = "mm"),
                                                direction = "horizontal",
                                                override.aes = 
                                                  list(fill = map_colors[2], 
                                                       color = "gray15", 
                                                       lwd = 0.04))) +
        ggnewscale::new_scale_color() +
        geom_sf(aes(color = "25 to 100"), lwd = 0, fill = NA) +
        scale_color_manual(name = NULL,
                           values = c(map_colors[3]), 
                           guide = guide_legend(label.hjust = 1,
                                                label.position = "bottom",
                                                order = 3, 
                                                keywidth = 15, 
                                                keyheight = unit(4, units = "mm"),
                                                direction = "horizontal",
                                                override.aes = 
                                                  list(fill = map_colors[3], 
                                                       color = "gray15", 
                                                       lwd = 0.04))) +
        ggnewscale::new_scale_color() +
        geom_sf(aes(color = "100 +"), lwd = 0, fill = NA) +
        scale_color_manual(name = NULL,
                           values = c(map_colors[4]),
                           guide = guide_legend(label.hjust = 1,
                                                label.position = "bottom",
                                                order = 4,
                                                keywidth = 10,
                                                keyheight = unit(4, units = "mm"),
                                                direction = "horizontal",
                                                override.aes =
                                                  list(fill = map_colors[4],
                                                       color = "gray15",
                                                       lwd = 0.04))) +
        geom_sf(fill = "gray99", color = "gray30", lwd = 0.1, show.legend = FALSE) 
    }
  })


  output$city_map <- renderPlot({
    
    withProgress(message = "Drawing map...", {
      
    p_city <- plot_start_city() +
      geom_sf(data = tiles_city(), aes(fill = avg_d_mbps_cat), color = "gray15", lwd = 0.04, alpha = 0.8, show.legend = FALSE) +
      geom_text_repel(data = label_cities(), aes(label = city, x = X, y = Y), size = label_size, color = "black", family = "Lato", fontface = "bold") +
      plot_styling +
      labs(caption = paste("", "Data: Ookla, US Census Bureau, Carto, OpenStreetMap contributors | Q2 2020 | By Katie Jolly", "\nSpeeds based on mean in tile", sep = "\n"), subtitle = wrapper(title_city(), width = 60)) +
      coord_sf(crs = st_crs(city()))

    incProgress(amount = 0.8)
    
    box <- st_minimum_bounding_circle(city()) %>% st_buffer(dist = 13000)
    # + c(-10000, -10000, 10000, 10000)
    # box <- box %>% st_as_sfc()

    basemap <- ggplot() +
      geom_sf(data = state_city(), color = "gray40", fill = "white", alpha = 0.2, lwd = .2) +
      geom_sf(data = city(), fill = "gray60", color = "gray60") +
      geom_sf(data = box, color = "black", fill = NA, lwd = 0.3) +
      # labs(title = "Locator map", size = title_size) +
      theme_void() +
      theme(text = element_text(family = "Lato", color = "gray30"),
            plot.title = element_text(face = "bold"))
    
    city_weighted_mean <- round(weighted.mean(tiles_city()$avg_d_kbps/1000, w = tiles_city()$tests), 2)
    
    vline_color <- case_when(city_weighted_mean < 10 ~ map_colors[1],
                             city_weighted_mean < 25 ~ map_colors[2],
                             city_weighted_mean < 100 ~ map_colors[3],
                             city_weighted_mean >= 100 ~ map_colors[4])
    
    dist_subtitle <- glue("Mean: {city_weighted_mean} Mbps")
    
    distribution <- ggplot(tiles_city()) +
      geom_histogram(aes(x = avg_d_kbps / 1000), color = "white", fill = "gray50") +
      geom_vline(xintercept = city_weighted_mean, linetype = 2, color = vline_color) +
      theme_half_open() +
      labs(x = dist_subtitle, y = "") +
      theme(text = element_text("Lato", size = 20),
            axis.text.y = element_blank(),
            axis.line.y=element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_text(size = 20))

    p_city <- ggdraw() +
      draw_plot(p_city) +
      draw_plot(basemap, x = 0, y = 0.2,  width = 0.15, height = 0.15) +
      draw_plot(distribution, x = 0, y = 0.4, width = 0.15, height = 0.15)


    vals$p_city <- p_city

    (p_city)
  })
  }
)

  output$county_map <- renderPlot({
    
    withProgress(message = "Drawing map...", {

    p_county <- plot_start_county() +
      geom_sf(data = tiles_county(),  aes(fill = avg_d_mbps_cat), color = "gray15", lwd = 0.04, alpha = 0.8, show.legend = FALSE) +
      geom_text_repel(data = label_cities_county(), 
                aes(label = city, x = X, y = Y), 
                size = label_size, 
                color = "black", 
                family = "Lato", fontface = "bold") +
      plot_styling +
      labs(caption = paste("","Data: Ookla, US Census Bureau, Carto, OpenStreetMap contributors | Q2 2020 | By Katie Jolly", "\nSpeeds based on mean in tile", sep = "\n"), subtitle = paste0("Fixed internet download speed in ", title_county(), "\n")) +
      coord_sf(crs = st_crs(county()))

    
    incProgress(amount = 0.7)
    
    box <- st_minimum_bounding_circle(county()) %>% st_buffer(dist = 13000)
    # + c(-10000, -10000, 10000, 10000)
    # box <- box %>% st_as_sfc()

    #ebdda9 old outline color
    basemap <- ggplot() +
      geom_sf(data = county_city(), color = "gray40", fill = "white", alpha = 0.2, lwd = .2) +
      geom_sf(data = county(), fill = "gray60", color = "gray60") +
      geom_sf(data = box, color = "black", fill = NA, lwd = 0.3) +
      # labs(title = "Locator map", size = title_size) +
      theme_void() +
      theme(text = element_text(family = "Lato", color = "gray30"),
            plot.title = element_text(face = "bold"))
    
    county_weighted_mean <- round(weighted.mean(tiles_county()$avg_d_kbps/1000, w = tiles_county()$tests), 2)
    
    vline_color_county <- case_when(county_weighted_mean < 10 ~ map_colors[1],
                             county_weighted_mean < 25 ~ map_colors[2],
                             county_weighted_mean < 100 ~ map_colors[3],
                             county_weighted_mean >= 100 ~ map_colors[4])
    
    dist_subtitle_county <- glue("Mean: {county_weighted_mean} Mbps")
    
    distribution <- ggplot(tiles_county()) +
      geom_histogram(aes(x = avg_d_kbps / 1000), color = "white", fill = "gray50") +
      geom_vline(xintercept = county_weighted_mean, linetype = 2, color = vline_color_county) +
      theme_half_open() +
      labs(x = dist_subtitle_county, y = "") +
      theme(text = element_text("Lato", size = 20, color = "gray20"),
            axis.text.y = element_blank(),
            axis.line.y=element_blank(),
            axis.ticks = element_blank())
    incProgress(amount = 0.8)
    
    p_county <- ggdraw() +
      draw_plot(p_county) +
      draw_plot(basemap, x = 0, y = 0.1, width = 0.15, height = 0.15) +
      draw_plot(distribution, x = 0, y = 0.3, width = 0.15, height = 0.15)
    vals$p_county <- p_county
    
    incProgress(0.9)
    
    p_county
  })
    

})

  output$downloadCity <- downloadHandler(
    filename = function() {
      paste(str_replace_all(str_replace_all(str_squish(str_remove_all(as.character(city()$name), " ")), "--", "-"), "[^[:alnum:]]", "_"), ".png", sep = "")
    },

    content = function(file) {

      ggsave(file, vals$p_city, dpi = 300, width = 8, height = 8, type = "cairo")

    }
  )

  output$downloadCounty <- downloadHandler(
    filename = function() {
      paste(str_replace_all(str_replace_all(str_squish(str_remove_all(as.character(county()$county_full_name), " ")), "--", "-"), "[^[:alnum:]]", "_"), ".png", sep = "")
    },

    content = function(file) {

      ggsave(file, vals$p_county, dpi = 300, width = 7, height = 7, type = "cairo")

    }
  )
  
  output$downloadCityData <- downloadHandler(
    filename = function() {
      paste(str_replace_all(str_replace_all(str_squish(str_remove_all(as.character(city()$name), " ")), "--", "-"), "[^[:alnum:]]", "_"), "_data", ".csv", sep = "")
    },
    content = function(file) {
      write_csv(tiles_city() %>% st_transform(4326) %>% mutate(wkt = st_as_text(geometry)) %>% st_set_geometry(NULL) %>% dplyr::select(avg_d_kbps, avg_u_kbps, avg_lat_ms, tests, devices, place_geoid = geoid, place_name = name, wkt), file)
    }
  )
  
  output$downloadCountyData <- downloadHandler(
    filename = function() {
      paste(str_replace_all(str_replace_all(str_squish(str_remove_all(as.character(county()$county_full_name), " ")), "--", "-"), "[^[:alnum:]]", "_"), "_data", ".csv", sep = "")
    },
    content = function(file) {
      write_csv(tiles_county() %>% st_transform(4326) %>% mutate(wkt = st_as_text(geometry)) %>% st_set_geometry(NULL) %>% dplyr::select(avg_d_kbps, avg_u_kbps, avg_lat_ms, tests, devices, county_geoid = geoid, county_name = long_name, wkt), file)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
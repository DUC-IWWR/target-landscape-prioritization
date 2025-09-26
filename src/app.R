library(shiny.semantic)
library(ggplot2)
library(ggpubr)
library(tidyterra)
library(terra)
library(shiny)
theme_set(theme_pubclean())

bwte <- terra::rast("../data/raw/rasters/bwte.tif")
canv <- terra::rast("../data/raw/rasters/canv.tif")
gadw <- terra::rast("../data/raw/rasters/gadw.tif")
mall <- terra::rast("../data/raw/rasters/mall.tif")
nopi <- terra::rast("../data/raw/rasters/nopi.tif")
nsho <- terra::rast("../data/raw/rasters/nsho.tif")
redh <- terra::rast("../data/raw/rasters/redh.tif")

tl <- terra::vect("../data/raw/tl-2025/TL_0.74.shp")

species_list <- c("BWTE","GADW", "MALL", "NOPI",
            "NSHO", "CANV", "REDH")

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

ui <- semanticPage(
  
  sidebar_layout(
    sidebar_panel(
      h3("Species"),
      shiny.semantic::multiple_checkbox(input_id = "species",
                                      label = "",
                                     choices = species_list,
                                     choices_value = tolower(species_list),
                                     default_text = "Select",
                                     value = NULL),
      width = 2
    ),
    main_panel(
      plotOutput("plot"),
      width = 10
    )
  )
  

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$plot <-  renderPlot({
    density_plot <- NULL

    if (length(input$species) == 1 ) {
      density_plot <- eval(parse(text = input$species))
    } else if (length(input$species) > 1) {
      density_plot <- eval(parse(text = input$species[1]))

      for (i in 2:length(input$species)) {
        density_plot <- sum(density_plot, eval(parse(text = input$species[i])))
      }
    }

    tl_pops <- terra::extract(density_plot, tl)
    tl_pops$ID <- as.integer(tl_pops$ID)
    tl_sums <- aggregate(tl_pops[,2] ~ tl_pops[,1], FUN = "mean")
    names(tl_sums) <- c("id", "density")
    tl_sums$density01 <- range01(tl_sums$density)
    #tl_sums$prop <- tl_sums$density / sum(tl_sums$density)
    to_plot <- merge(tl, tl_sums, by = "id")

    map <- ggplot2::ggplot() +
      tidyterra::geom_spatvector(data = to_plot, aes(fill = (density01))) +
      ggplot2::labs(fill = "Density Index") +
      NULL

    hist <- ggplot2::ggplot(data = to_plot) +
      ggplot2::geom_histogram(aes(density01)) +
      xlab("Density Index") +
      NULL

    ggpubr::ggarrange(map, hist, nrow = 1)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

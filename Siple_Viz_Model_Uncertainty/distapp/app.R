#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#
# Libraries ---------------------------------------------------------------
library(shiny)
library(ggplot2)
library(dplyr)
library(palmerpenguins)
library(ggradar)

# Accessory code for half-violin plots ------------------------------------
# We are using this code snippet, but there are other options for raincloud plots.
source("https://raw.githubusercontent.com/datavizpyr/data/master/half_flat_violinplot.R")


# Palettes and theme ------------------------------------------------------
penpal <- ghibli::ghibli_palette("SpiritedMedium", direction = -1)
pentheme <- hrbrthemes::theme_ipsum_rc(base_size = 18) +
  theme(
    axis.title.x = element_text(size = rel(1.5)),
    axis.title.y = element_text(size = rel(1.5)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

################### APP CONTENTS BEGIN ####################################
# Define user interface ---------------------------------------------------
ui <- fluidPage(

  # Application title
  titlePanel("Palmer penguins data"),

  # Sidebar with a selectizeInput for islands
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "island",
        label = "Islands to show:",
        choices = unique(penguins$island),
        multiple = TRUE,
        selected = "Torgersen"
      ),
      textOutput("numSelectedDisp")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Raincloud", plotOutput("raincloudPlot")),
        tabPanel("Violin", plotOutput("violinPlot")),
        tabPanel("Kite", plotOutput("kitePlot"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$raincloudPlot <- renderPlot({
    p1 <- penguins %>%
      filter(!is.na(sex)) %>%
      filter(island %in% input$island) %>%
      ggplot(aes(x = sex, y = body_mass_g, fill = species)) +
      geom_flat_violin(
        position = position_nudge(x = .1, y = 0),
        adjust = 1.5, trim = FALSE, alpha = .5, colour = NA
      ) +
      geom_point(aes(x = sex, y = body_mass_g, colour = species),
        position = position_jitter(width = .05), size = 3, shape = 20
      ) +
      geom_boxplot(aes(x = sex, y = body_mass_g, fill = species),
        outlier.shape = NA, alpha = .5, width = .1, colour = "black"
      ) +
      scale_colour_manual("Species", values = penpal) +
      scale_fill_manual("Species", values = penpal) +
      xlab("Sex") +
      ylab("Body mass (g)") +
      coord_flip() +
      facet_wrap(~island, scale = "free_x", ncol = 1) +
      pentheme
    p1
  })

  output$violinPlot <- renderPlot({
    penguins %>%
      filter(!is.na(sex)) %>%
      filter(island %in% input$island) %>%
      ggplot(aes(x = sex, y = body_mass_g, fill = species)) +
      geom_violin() +
      scale_colour_manual("Species", values = penpal) +
      scale_fill_manual("Species", values = penpal) +
      facet_wrap(~island) +
      pentheme
  })

  output$kitePlot <- renderPlot({
    dat <- penguins %>%
      filter(!is.na(sex)) %>%
      filter(island %in% input$island) %>%
      group_by(species, sex) %>%
      summarize_at(c(
        "bill_length_mm",
        "bill_depth_mm",
        "flipper_length_mm",
        "body_mass_g"
      ), .funs = max) %>%
      ungroup() %>%
      mutate_at(vars(-species, -sex), scales::rescale) %>%
      mutate(lab = paste0(species, "_", sex)) %>%
      select(lab, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)

    ggradar::ggradar(
      plot.data = dat,
      group.colours = penpal,
      grid.label.size = 4,
      axis.label.size = 4,
      group.point.size = 3
    ) +
      theme(legend.position = "bottom")
  })

  numSelected <- reactive({
    length(unique(input$island))
  })

  output$numSelectedDisp <- renderText({
    paste0("Number of islands selected: ", numSelected())
  })
}

# Run the application
shinyApp(ui = ui, server = server)

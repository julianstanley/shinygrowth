library(shiny)
library(data.table)
library(DT)
library(plotly)
library(tidyr)


ui <- fluidPage(
  titlePanel("Quick growth curves"),

  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        type = "tabs",
        
        tabPanel(
          "Upload and Options",
          fileInput("growth_in", "Input growth file (.csv):"),
          checkboxInput("log2", "log2 scale", value = TRUE),
          checkboxInput("se", "Show error", value = FALSE),
          numericInput(inputId = "growth_precision", label = "Growth Rate Decimal Precision", value = 2, step = 1)
        ),
        
        tabPanel(
          "Instructions",
          h2("Format"), br(),
          "Upload a .csv file containing OD data with one column containing
      the elapsed time (in minutes) each other column representing 
      a single sample.",
          h2("Example"),
          dataTableOutput("exampleTable")
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Plot",
          plotlyOutput("main_plot")
        ),
        tabPanel(
          "Growth rates",
          dataTableOutput("growth_rates")
        ),
        tabPanel(
          "Growth data",
          dataTableOutput("tidy_datatable")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  get_raw_data <- reactive({
    if(isTruthy(input$growth_in)) {
      growth <- fread(input$growth_in$datapath, stringsAsFactors = F)
      return(growth)
    } else {
      growth <- structure(list(time = c(0L, 30L, 60L, 90L, 120L, 150L, 180L), 
                           Sample.1 = c(0.1, 0.2, 0.42, 0.8, 1.5, 3.2, 6.4), 
                           Sample.2 = c(0.1, 
                           0.15, 0.225, 0.3375, 0.52, 0.76375, 1.1390625), 
                           Sample.3 = c(0.1, 
                           0.12, 0.144, 0.1728, 0.21736, 0.288832, 0.2985984), 
                           Sample.4 = c(0.1, 
                           0.22, 0.484, 1.0648, 2.33256, 5.154632, 11.3379904)), 
                          row.names = c(NA, 
                          -7L), class = c("data.table", "data.frame"))
    }

  })

  get_tidy_data <- reactive({
    growth <- req(get_raw_data())
    colnames(growth)[1] <- "time"
    growth_tidy <- growth %>% gather(
      Sample, Growth, -time
    )
    growth_tidy$Growth_log2 <- log2(growth_tidy$Growth)
    return(growth_tidy)
  })

  output$exampleTable <- renderDataTable(
    {
      data.table(
        "time" = c(0, 30, 60, 90),
        "Sample 1" = c(0.1, 0.2, 0.4, 0.8),
        "Sample 2" = c(1, 2, 4, 8),
        "Sample 3" = c(10, 20, 40, 80)
      )
    },
    options = list(dom = "tp", searching = F, scrollX = T)
  )

  output$main_plot <- renderPlotly({
    req(get_tidy_data())
    data <- get_tidy_data()

    if (input$log2) {
      growth_col <- "Growth_log2"
      formula <- y ~ x
      method <- "lm"
      y_text <- "log2(Growth)"
    } else {
      growth_col <- "Growth"
      y_text <- "Growth (OD)"
    }

    plot <- get_tidy_data() %>%
      ggplot(aes_string(y = growth_col, x = "time", col = "Sample")) +
      ylab(y_text) +
      xlab("time(minutes)") +
      geom_point(size = 2) +
      theme_publication()

    if (input$log2) {
      plot <- plot +
        stat_smooth(method = "lm", formula = y ~ x, se = input$se)
    }
    ggplotly(
      plot
    )
  })

  output$growth_rates <- renderDataTable(
    {
      req(get_tidy_data())
      data <- get_tidy_data()
      rates <- list()

      for (sample in unique(data$Sample)) {
        growth_sd <- round(
          summary(lm(
            time ~ Growth_log2,
            data %>%
              filter(Sample == sample)
          ))$coeff[c(2, 4)],
          input$growth_precision
        )
        rates <- rbind(rates, list(
          sample = sample, "Growth Rate" = growth_sd[1],
          "+/- (error)" = growth_sd[2]
        ))
      }
      data.table(rates)
    },
    options = list(searching = T, scrollX = T)
  )

  output$tidy_datatable <- renderDataTable(
    {
      data <- req(get_raw_data())
      datatable(data)
    },
    options = list(searching = T, scrollX = T)
  )
}

#-------------------------------------------------------------------------------
# Publication theme
theme_publication <- function(base_size = 14, base_family = "helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size = base_size, base_family = base_family)
  + theme(
      plot.title = element_text(
        face = "bold",
        size = rel(1.2), hjust = 0.5
      ),
      text = element_text(),
      panel.background = element_rect(colour = NA),
      plot.background = element_rect(colour = NA),
      panel.border = element_rect(colour = NA),
      axis.title = element_text(face = "bold", size = rel(1)),
      axis.title.y = element_text(angle = 90, vjust = 2),
      axis.title.x = element_text(vjust = -0.2),
      axis.text = element_text(),
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(),
      panel.grid.major = element_line(colour = "#f0f0f0"),
      panel.grid.minor = element_blank(),
      legend.key = element_rect(colour = NA),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(0.2, "cm"),
      legend.margin = unit(0, "cm"),
      legend.title = element_text(face = "italic"),
      plot.margin = unit(c(10, 5, 5, 5), "mm"),
      strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
      strip.text = element_text(face = "bold")
    ))
}

scale_fill_Publication <- function(...) {
  library(scales)
  discrete_scale("fill", "Publication", manual_pal(values = c("#386cb0", "#fdb462", "#7fc97f", "#ef3b2c", "#662506", "#a6cee3", "#fb9a99", "#984ea3", "#ffff33")), ...)
}

scale_colour_Publication <- function(...) {
  library(scales)
  discrete_scale("colour", "Publication", manual_pal(values = c("#386cb0", "#fdb462", "#7fc97f", "#ef3b2c", "#662506", "#a6cee3", "#fb9a99", "#984ea3", "#ffff33")), ...)
}

shinyApp(ui, server)

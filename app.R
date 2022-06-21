#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#http://shiny.rstudio.com/gallery/file-download.html
#library(rsconnect)
#rsconnect::deployApp('~/Dropbox/Rpackages/SearchAQ/searchAccountability')


library(shiny)
library(data.table)
library(ggplot2)
library(scales)

## correct program names
prog.names <- c( 'actual'   = 'Actual',
                 'all'      = 'All EGU & mobile regulations',
                 'egu.all'  = 'All EGU regulations',
                 'mob.all'  = 'All mobile regulations',
                 'egu.arp'  = 'EGU - Acid Rain Program (ARP)',
                 'egu.nbp'  = 'EGU - NOx Budget Trading Program (NBTP)',
                 'egu.cair' = 'EGU - Clean Air Interstate Rule (CAIR)',
                 'mob.im'   = 'Mobile - Inspection and Maintenance (IM)',
                 'mob.gs'   = 'Mobile - Gasoline Programs (GSP)',
                 'mob.ds'   = 'Mobile - Diesel Programs (DSP)')

prog.names.conc <- c( "fill" = 'Actual',
                      "em"   = 'All EGU & mobile regulations',
                      "e"    = 'All EGU regulations',
                      "m"    = 'All mobile regulations',
                      "ar"   = 'EGU - Acid Rain Program (ARP)',
                      "nb"   = 'EGU - NOx Budget Trading Program (NBTP)',
                      "mp"   = 'EGU - Clean Air Interstate Rule (CAIR)',
                      "gs"   = 'Mobile - Gasoline Programs (GSP)',
                      "ds"   = 'Mobile - Diesel Programs (DSP)',
                      "im"   = 'Mobile - Inspection and Maintenance (IM)')
spec.labels <- c(o3 = 'O[3]-ppb^""',
                 co = 'CO[]-ppb^""',
                 no2 = 'NO[2]-ppb^""',
                 so2 = 'SO[2]-ppb',
                 pm = 'PM[2.5]-µg~m^"-3"',
                 so4 = 'SO[4]^"2-"-µg~m^"-3"',
                 no3 = 'NO[3]^"-"-µg~m^"-3"',
                 nh4 = 'NH[4]^"+"-µg~m^"-3"',
                 oc = 'OC[]-µg~m^"-3"',
                 ec = 'EC[]-µg~m^"-3"')
spec.labels.dt <- data.table( spec.plot = spec.labels,
                              species      = names( spec.labels),
                              unit = c( rep( 'ppb', 4),
                                        rep( 'ugm3', 6)))


scientific_10 <- function(x) {
  library('scales')
  parse(text=gsub("e", " %*% 10^", scientific_format(digits=4)(x)))
}

# load inputdata
load( 'data/concs.avg.RData')
load( 'data/emiss.avg.RData')
load( 'data/emiss.act.RData')


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Atlanta, GA Accountability Analysis"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      p(HTML(paste0( "This web application shows the actual ambient concentrations (top) and mobile and electricity generating unit ",
                     "(EGU) emissions (below). To view estimates of counterfactual (i.e., hypothetical) concentrations and emissions under various scenarios, ",
                     "select a regulation scenario. The regulation scenario names correspond to the names of the national program, ",
                     "but most programs are associated with national, state, and local rules."))),
      br(),
      p(HTML(paste0( 'The results presented here correspond to the data and analyses described by ',
                     '<a href=https://www.sciencedirect.com/science/article/pii/S0160412018323110>',
                     'Henneman et al. (2019)</a> and ',
                     '<a href=https://www.healtheffects.org/publication/impacts-regulations-air-quality-and-emergency-department-visits-atlanta-metropolitan>',
                     'Russell et al. (2018)</a> and applied by ',
                     '<a href=https://www.sciencedirect.com/science/article/pii/S0160412018328046>',
                     'Abrams et al. (2019)</a> to estimate hospitalizations avoided by each policy group. ',
                     'The data available for download below has been aggregated to facilitate ',
                     "sharing. For inqueries on access to the raw datasets in the published works, please contact ",
                     '<a href=mailto:lhenneman@gmail.com>Lucas Henneman</a>.'))),

      radioButtons("radio", label = h3("Select regulation scenario"),
                   choiceNames = as.vector( prog.names),
                   choiceValues = as.vector( prog.names),
                   selected = as.vector( prog.names)[1]),
      radioButtons("time", label = h3("Select aggregation"),
                   choiceNames = c( 'Month', 'Year'),
                   choiceValues = c( 'month', 'year'),
                   selected = 'month'),
      h3("Download Data"),
      # Button
      downloadButton("downloadConc", "Download Concentrations"),
      # Button
      downloadButton("downloadEmiss", "Download Emissions")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot.conc", height = 800)
    )
  ),

  plotOutput("plot.mob", height = 500),
  plotOutput("plot.egu", height = 300)

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ## ---------------------------------------------- ##
  ## Read in concentration data
  concs.dt <- reactive({
    concs.avg[[input$time]]
  })

  ## ---------------------------------------------- ##
  ## Read in emissions data
  emiss.dt <- reactive({
    emiss.avg[[input$time]]
  })

  ## ---------------------------------------------- ##
  ## Read in emissions data
  emiss.act.dt <- reactive({
    emiss.act[[input$time]]
  })

  ## ---------------------------------------------- ##
  # conc plot
  output$plot.conc <- renderPlot({
    # subset data based on input
    dt.use <- concs.dt()[prog.plot %in% c( "Actual", input$radio)]

    ## make the plot
    ggplot( data = dt.use) +
      facet_wrap( ~ spec.plot,
                  ncol = 2,
                  scales = 'free_y',
                  labeller = label_parsed) +
      geom_line( aes( x = date.plot,
                      y = conc,
                      color = prog.plot),
                 size = 1.5) +
      # scale_y_continuous( label = scientific_10) +
      ggtitle( "Ambient Concentrations in Atlanta, GA") +
      ylab( NULL) +
      theme_bw() +
      expand_limits( y = 0) +
      theme( axis.text = element_text( size = 14),
             axis.title = element_text(size = 24),
             axis.title.x = element_blank(),
             legend.position = 'bottom', #c(.5,0),
             legend.text = element_text( size = 20),
             legend.direction ='horizontal',
             legend.background = element_rect(),
             legend.key.size = unit( 1, 'cm'),
             legend.text.align = 0,
             legend.title = element_blank(),
             legend.key.width = unit( 3, 'line'),
             plot.title = element_text(size = 28, hjust = .5),
             strip.background = element_rect( fill = 'white'),
             strip.text = element_text( size = 16))
  })

  ## ---------------------------------------------- ##
  #MOB plot
  output$plot.mob <- renderPlot({
    # subset data based on input
    dt.use <- emiss.dt()[ as.character(prog.plot) %in% c( "Actual", input$radio)]

    ## make the plot
    ggplot( data = dt.use[grep( '.mob', variable)][value > 0]) +
      facet_wrap( ~ spec.plot,
                  ncol = 2,
                  scales = 'free_y',
                  labeller = label_parsed) +
      geom_area( aes( x = date.plot,
                      y = value,
                      fill = prog.plot),
                 position = position_stack( reverse = T)) +
      scale_y_continuous( label = scientific_10) +
      ggtitle( "Mobile source emissions") +
      ylab(expression(paste('Emissions, tons ',month^{-1}))) +
      xlab('Year') +
      theme_bw() +
      theme( axis.text = element_text( size = 14),
             axis.title = element_text(size = 24),
             axis.title.x = element_blank(),
             legend.position = c(.5,0),
             legend.text = element_text( size = 20),
             legend.direction ='vertical',
             legend.background = element_rect(),
             legend.key.size = unit( 1, 'cm'),
             legend.text.align = 0,
             legend.justification = c("left", "bottom"),
             legend.title = element_blank(),
             legend.key.width = unit( 3, 'line'),
             plot.title = element_text(size = 28, hjust = .5),
             strip.background = element_rect( fill = 'white'),
             strip.text = element_text( size = 16))
  }, height = 500)

  ## ---------------------------------------------- ##
  # EGU plot
  output$plot.egu <- renderPlot({
    # subset data based on input
    dt.use <- emiss.dt()[prog.plot %in% c( "Actual", input$radio)]

    ggplot( data = dt.use[grep( '.egu|.reg', variable)]) +
      facet_wrap( ~ spec.plot,
                  nrow = 2,
                  scales = 'free_y',
                  labeller = label_parsed) +
      geom_area( aes( x = date.plot,
                      y = value,
                      fill = prog.plot),
                 position = position_stack( reverse = T)) +
      scale_y_continuous( label = scientific_10) +
      ggtitle( "EGU source emissions") +
      ylab(expression(paste('Emissions, tons ',month^{-1}))) +
      xlab('Year') +
      theme_bw() +
      theme( axis.text = element_text( size = 14),
             axis.title = element_text(size = 24),
             axis.title.x = element_blank(),
             legend.position = 'none', #c(.8,.07),
             plot.title = element_text(size = 28, hjust = .5),
             strip.background = element_rect( fill = 'white'),
             strip.text = element_text( size = 16))

  }, height = 300)

  # Downloadable csv of selected dataset ----
  output$downloadEmiss <- downloadHandler(
    filename = function() {
      paste0( 'Emissions_', input$time, ".csv")
    },
    content = function(file) {
      dt <- emiss.act.dt()

      dt[, `:=` (species = gsub( '\\..*', '', variable),
                 source = gsub( '.*\\.', '', variable),
                 value  = value.act + emiss.diff)]

      setnames( dt,
                c('prog.plot', 'value'),
                c('program', 'emissions_tons'))

      write.csv( dt[, .(date.plot, program, source, species, emissions_tons)],
                 file, row.names = FALSE)
    }
  )
  output$downloadConc <- downloadHandler(
    filename = function() {
      paste0( 'Concentrations_', input$time, ".csv")
    },
    content = function(file) {
      dt <- concs.dt()
      dt <- merge( dt, spec.labels.dt, by = 'spec.plot')
      setnames( dt, c( 'conc', 'prog.plot'),
                c( 'concentration', 'program'))

      write.csv( dt[, .( date.plot, program, species, unit, concentration)],
                 file, row.names = FALSE)
    }
  )


}

# Run the application
shinyApp(ui = ui, server = server)


library(imager)
library(jpeg)
library(ggplot2)
library(ggvis)
library(grid)
library(dplyr)
library(shiny)


# im <- load.image("SmallTop.jpg")
# im2 <- load.image("SmallTop2.jpg")
# 
# im <- load.image("fm11.jpg")
# im2 <- load.image("fm22.jpg")
# 
# im.m <- grayscale(im)
# im2.m <- grayscale(im2)
# 
# im.m <- isoblur(im.m, 2)
# im2.m <- isoblur(im2.m, 2)
# 
# 
# imDF <- as.data.frame(im.m)
# imDF2 <- as.data.frame(im2.m)
# 
# imDF$value1 <- imDF$value - imDF2$value
# imDF$value <- ifelse(abs(imDF$value1) > 0.3, 0, 1)
# imDF$value1 <- NULL
# 
# # as.cimg(imDF) %>% display()
# # isoblur(as.cimg(imDF),sigma=50) %>% display()
# 
# # as.cimg(imDF) %>% plot()
# # as.cimg(imDF) %>% display()
# 
# imPlotDf <- imDF %>% 
#             filter(value == 0) %>%
#             mutate(y = dim(im)[2] - y)
# 
# 
# dim(imPlotDf)[1]
# 
# if (dim(imPlotDf)[1] > 30000) {
#   print("Too much shit!!")
# } else {
#   imPlotDf %>%
#     ggplot(aes(x=x,y=y)) + 
#     annotation_custom(rasterGrob(im2, width=unit(1,"npc"), height=unit(1,"npc")), 
#                       -Inf, Inf, -Inf, Inf) +
#     scale_x_continuous(expand=c(0,0), 
#                        limits = c(0,dim(im2)[1])) +
#     scale_y_continuous(expand=c(0,0), 
#                        limits = c(0,dim(im2)[2])) +
#     coord_fixed(ratio = 1) +
#     geom_point(colour = "red", 
#                alpha = 0.03,
#                size = 6) +
#     theme(axis.line=element_blank(),
#           axis.text.y=element_blank(),
#           axis.title.x=element_blank(),
#           axis.title.y=element_blank(),
#           axis.ticks.y = element_blank())  
# }



###################################################################################################
################################ FIDUCIAL FINDING APP #############################################
###################################################################################################


im <- load.image("fm22.jpg")
im <- grayscale(im)

subim(im,
      x > 0,
      x < 70,
      y > 0,
      y < 70) -> searchArea


# subim(im,
#       x > 0,
#       x < 300,
#       y > 0,
#       y < 300) -> area

area <- im


ui <- fluidPage(
  fluidRow(
    column(width = 4, class = "well",
           h4("Brush and double-click to zoom"),
           plotOutput("plot1", height = 300,
                      dblclick = "plot1_dblclick",
                      brush = brushOpts(
                        id = "plot1_brush",
                        resetOnNew = TRUE
                      ),
                      hover = hoverOpts(
                        id = "plot1_hover"
                      )
           ),
           plotOutput("plot3", height = 300),
           actionButton("calculateCorrelations","Calculate correlations"),
           verbatimTextOutput("plot_brushinfo")
    ),
    column(width = 4, class = "well",
           plotOutput("plot4", height = 300,
                      dblclick = "plot4_dblclick",
                      brush = brushOpts(
                        id = "plot4_brush",
                        resetOnNew = TRUE
                      )),
           verbatimTextOutput("plot2_dim_info"),
           tableOutput("plot2_cor_info")
    )
    
  )
)

server <- function(input, output) {
  

  output$plot_brushinfo <- renderPrint({
    paste("dim:", dim(searchAreaR2$pic)[1],dim(searchAreaR2$pic)[2], 
          "stencil val: ",searchAreaR2stencil())
  })
  
  output$plot2_dim_info <- renderPrint({
    paste("dim:", dim(areaR$pic)[1],dim(areaR$pic)[2])
  })
  
  output$plot2_cor_info <- renderTable({
    req(allPointsCor())
    allPointsCor()
  })
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  searchAreaR <- reactiveValues(pic = searchArea)
  searchAreaR2 <- reactiveValues(pic = searchArea)
  areaR <- reactiveValues(pic = area)

  output$plot1 <- renderPlot({

    data <- data.frame(x = dim(searchAreaR$pic)[1]/2,
                       y = dim(searchAreaR$pic)[2]/2)

    data %>%
      ggplot(aes(x=x,y=y)) + 
      annotation_custom(rasterGrob(searchAreaR$pic, width=unit(1,"npc"), height=unit(1,"npc")), 
                        -Inf, Inf, -Inf, Inf) +
      scale_x_continuous(expand=c(0,0), 
                         limits = c(0,dim(searchAreaR$pic)[1])) +
      scale_y_continuous(expand=c(0,0), 
                         limits = c(0,dim(searchAreaR$pic)[2])) +
      #coord_fixed(ratio = 1) +
      # geom_point(colour = "red", 
      #            alpha = 0.5,
      #            size = 5) +
      theme(axis.line=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank()
            ) 
  })

  output$plot3 <- renderPlot({
    
    data <- data.frame(x = dim(searchAreaR2$pic)[1]/2,
                       y = dim(searchAreaR2$pic)[2]/2)
    
    data %>%
      ggplot(aes(x=x,y=y)) + 
      annotation_custom(rasterGrob(searchAreaR2$pic, width=unit(1,"npc"), height=unit(1,"npc")), 
                        -Inf, Inf, -Inf, Inf) +
      scale_x_continuous(expand=c(0,0), 
                         limits = c(0,dim(searchAreaR2$pic)[1])) +
      scale_y_continuous(expand=c(0,0), 
                         limits = c(0,dim(searchAreaR2$pic)[2])) +
      #coord_fixed(ratio = 1) +
      geom_point(colour = "red", 
                 alpha = 0.7,
                 size = 10,
                 shape = 3) +
      theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank()
      ) 
  })
    
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      kazkas <<- c(brush$xmin,
                   brush$xmax - brush$xmin,
                   dim(searchAreaR$pic)[2] - brush$ymax,
                   brush$ymax - brush$ymin)
      searchAreaR$pic <- subim(searchAreaR$pic,
                            x > kazkas[1],
                            x < kazkas[2],
                            y > kazkas[3],
                            y < kazkas[4])
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
      searchAreaR$pic <- searchArea
    }
  })
  
  observe({
    brush <- input$plot1_brush
    
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      kazkas <<- c(brush$xmin,
                   brush$xmax - brush$xmin,
                   dim(searchAreaR$pic)[2] - brush$ymax,
                   brush$ymax - brush$ymin)
      searchAreaR2$pic <- subim(searchAreaR$pic,
                               x > kazkas[1],
                               x < kazkas[2],
                               y > kazkas[3],
                               y < kazkas[4])
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
      searchAreaR2$pic <- searchAreaR$pic
    }
  })
 
  stencil <- reactive({
    dimx <- dim(searchAreaR2$pic)[1]
    dimy <- dim(searchAreaR2$pic)[2]
    xbot <- ceiling(-dim(searchAreaR2$pic)[1]/2)
    xtop <- floor(dim(searchAreaR2$pic)[1]/2)
    ybot <- ceiling(-dim(searchAreaR2$pic)[2]/2)
    ytop <- floor(dim(searchAreaR2$pic)[2]/2)
    expand.grid(dx = seq(xbot,xtop,1),
                dy = seq(ybot,ytop,1))
  })

  searchAreaR2stencil <- reactive({
    get.stencil(searchAreaR2$pic,
                stencil(),
                x = abs(min(stencil()$dx))+1,
                y = abs(min(stencil()$dy))+1)
  })

  getFidCor <- function(x,y) {
    get.stencil(areaR$pic,
                stencil(),
                x = x,
                y = y) %>%
      cor(searchAreaR2stencil())
  }
  
  allPoints <- reactive({
    expand.grid(x = seq(1, dim(areaR$pic)[1], 1),
                y = seq(1, dim(areaR$pic)[2], 1)) 
  })
  
  allPointsCor <- reactive({
    req(isolate(allPoints()))

    if (is.null(input$calculateCorrelations) ||
        input$calculateCorrelations == 0) {
      isolate({
        return({
          allPoints()  %>%
            mutate(value = 0) %>%
            head(5)
        })
      })
    }
    
    dimGood <- isolate({
      if(dim(searchAreaR2$pic)[1] > dim(areaR$pic)[1] || 
         dim(searchAreaR2$pic)[2] > dim(areaR$pic)[2] || 
         dim(areaR$pic)[1] > 200 || 
         dim(areaR$pic)[2] > 200) {
        FALSE
      } else {
        TRUE
      }
    })
    
    print(paste("GOOD??",dimGood))
    
    if(!dimGood) {
      isolate({
        return({
          allPoints()  %>%
            mutate(value = 0) %>%
            head(5)
        })
      })      
    }
    
    isolate({
      return({
        allPoints()  %>%
          rowwise() %>%
          mutate(value = getFidCor(x,y)) %>%
          as.data.frame() %>%
          arrange(desc(value)) %>%
          head(5)
      })
    })

  }) 
  
  output$plot4 <- renderPlot({

    data <- allPointsCor() %>%
            head(1)

    if (data$value == 0 || is.na(data$value)) {
      posColor <- "red"
      data$value <- 0
    } else if (data$value < 0.8) {
      posColor <- "yellow"
    } else {
      posColor <- "green"
    }
    
    data$y <- dim(areaR$pic)[2] - data$y
    data$value <- round(data$value, 2)
    
    
    data %>%
      ggplot(aes(x = x,
                 y = y,
                 label = value)) + 
      annotation_custom(rasterGrob(areaR$pic, width=unit(1,"npc"), height=unit(1,"npc")), 
                        -Inf, Inf, -Inf, Inf) +
      scale_x_continuous(expand=c(0,0), 
                         limits = c(0,dim(areaR$pic)[1])) +
      scale_y_continuous(expand=c(0,0), 
                         limits = c(0,dim(areaR$pic)[2])) +
      #coord_fixed(ratio = 1) +
      geom_point(colour = posColor,
                 alpha = 0.5,
                 size = 8,
                 shape = 3) +
      geom_text(nudge_x = 3,
                nudge_y = -3) +
      theme(axis.line=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank()
      ) 
  })
  
  
  observeEvent(input$plot4_dblclick, {
    brush <- input$plot4_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      kazkas2 <<- c(brush$xmin,
                   brush$xmax - brush$xmin,
                   dim(areaR$pic)[2] - brush$ymax,
                   brush$ymax - brush$ymin)
      areaR$pic <- subim(areaR$pic,
                         x > kazkas2[1],
                         x < kazkas2[2],
                         y > kazkas2[3],
                         y < kazkas2[4])
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
      areaR$pic <- area
    }
  })
  
  
}

shinyApp(ui, server)































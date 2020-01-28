# algae color palette
pal_alg <- colorFactor(
  palette = RColorBrewer::brewer.pal(8,  'Dark2'),#c('#004F7E', '#00806E', '#427355', '#5C4A42', '#958984'),
  na.color = 'yellow',
  levels = c('Bacillariophyta', 'Cyanobacteria', 'Karenia brevis', 'Pseudo-nitzschia pungens', 'Pseudo-nitzschia sp.', 'Pyrodinium bahamense', 'Tripos hircus', 'other')
  )

# image nonsense because renderplot doesn't create correct font on shiny server
img_fun <- function(rct, session, imgnm){
  
  width  <- session$clientData[[paste0('output_', imgnm, '_width')]]
  height <- session$clientData[[paste0('output_', imgnm, '_height')]]
  
  # For high-res displays, this will be greater than 1
  pixelratio <- session$clientData$pixelratio
  
  # A temp file to save the output.
  outfile <- tempfile(fileext='.png')
  
  png(outfile, width=width*pixelratio, height=height*pixelratio,
      res=72*pixelratio)
  print(rct)
  dev.off()
  
  # Return a list containing the filename
  list(src = outfile,
       width = width,
       height = height,
       alt = "")

}

# plotly function for threshold plots
thrplotly <- function(epcdata, bay_segment, maxyr, family, themein){
  
  p1 <- show_thrplot(epcdata, bay_segment = bay_segment, thr = "chla", yrrng = c(1975, maxyr), family = family, txtlab = F, labelexp = F) + 
    ggtitle(NULL) +
    themein +
    scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(1975, maxyr))
  p2 <- show_thrplot(epcdata, bay_segment = bay_segment, thr = "la", yrrng = c(1975, maxyr), family = family, txtlab = F, labelexp = F) + 
    ggtitle(NULL) +
    themein + 
    scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(1975, maxyr))
  
  p3 <- show_segmatrix(epcdata, bay_segment = bay_segment, yrrng = c(1975, maxyr), txtsz = NULL) + 
    scale_y_continuous(expand = c(0,0), breaks = c(1975:maxyr)) +
    coord_flip() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text = element_text(size = 12), 
      text = element_text(family = family)
    ) 
  
  p3 <- ggplotly(p3, tooltip = 'Result') 
  for(i in 1:length(p3$x$data)) p3$x$data[[i]]$showlegend <- FALSE    

  p1 <- ggplotly(p1)
  p2 <- ggplotly(p2)
  p2$x$data[[1]]$showlegend <- FALSE
  p2$x$data[[2]]$showlegend <- FALSE
  p2$x$data[[3]]$showlegend <- FALSE
  p2$x$data[[4]]$showlegend <- FALSE
  
  out <- subplot(p1, p3, p2, nrows = 3, heights = c(0.4, 0.2, 0.4), shareX = T, titleY = TRUE)
  
  return(out)
  
}

# plotly function for boxplots
boxplotly <- function(epcdata, bay_segment, maxyr, family, themein){
  
  p1 <- show_boxplot(epcdata, param = 'chla', bay_segment = bay_segment, yrrng = c(1975, maxyr - 1), yrsel = maxyr, family = family, labelexp = F, txtlab = F) + 
    ggtitle(NULL) +
    themein

  p2 <- show_boxplot(epcdata, param = 'la', bay_segment = bay_segment, yrrng = c(1975, maxyr - 1), yrsel = maxyr, family = family, labelexp = F, txtlab = F) + 
    ggtitle(NULL) +
    themein
  
  p1 <- ggplotly(p1)
  for(i in 1:length(p1$x$data))
    p1$x$data[[i]]$name <- gsub('^\\((.*),.*,.*$', '\\1', p1$x$data[[i]]$name)

  p2 <- ggplotly(p2)
  for(i in 1:length(p2$x$data)) p2$x$data[[i]]$showlegend <- FALSE
  
  out <- subplot(p1, p2, nrows = 2, shareX = T, titleY = TRUE)
  
  return(out)
  
}

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
    themein
  p2 <- show_thrplot(epcdata, bay_segment = bay_segment, thr = "la", yrrng = c(1975, maxyr), family = family, txtlab = F, labelexp = F) + 
    ggtitle(NULL) +
    themein
  
  p1 <- ggplotly(p1)
  p2 <- ggplotly(p2)
  p2$x$data[[1]]$showlegend <- FALSE
  p2$x$data[[2]]$showlegend <- FALSE
  p2$x$data[[3]]$showlegend <- FALSE
  p2$x$data[[4]]$showlegend <- FALSE
  
  out <- subplot(p1, p2, nrows = 2, heights = c(0.5, 0.5), shareX = T, titleY = TRUE)
  
  return(out)
  
}
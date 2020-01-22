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

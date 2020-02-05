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
  for(i in 1:length(p1$x$data)){ # fix legend
    p1$x$data[[i]]$name <- gsub('^\\((.*),.*,.*$', '\\1', p1$x$data[[i]]$name)
  }
  
  p1$x$data[[1]]$marker = list(opacity = 0) # remove outlier from plot
  
  # remove hoverinfo for boxplots and line
  p1$x$data[[3]]$hoveron <- NULL
  p1$x$data[[4]]$hoveron <- NULL
  
  p2 <- ggplotly(p2)
  for(i in 1:length(p2$x$data)){ # fix legend
    p2$x$data[[i]]$showlegend <- FALSE
  }
  
  p2$x$data[[1]]$marker = list(opacity = 0) # remove outliers from plot
  # p2$x$data[[1]]$hoverinfo <- NULL
  
  # remove hoverinfo for boxplots and line
  p2$x$data[[3]]$hoveron <- NULL
  p2$x$data[[4]]$hoveron <- NULL
  
  out <- subplot(p1, p2, nrows = 2, shareX = T, titleY = TRUE)
  
  return(out)
  
}

# find data from plotly boxplot event selection
selfun <- function(selin, plodat){

  if(!selin$curveNumber %in% c(5, 6, 13, 14))
    return(NULL)
  
  # get selection data
  if(selin$curveNumber == 5) #top plot
    txt <- plodat$x$data[[6]]
  if(selin$curveNumber == 6) #top plot
    txt <- plodat$x$data[[7]]
  if(selin$curveNumber == 13) # bottom plot
    txt <- plodat$x$data[[14]]
  if(selin$curveNumber == 14) # bottom plot
    txt <- plodat$x$data[[15]]

  moval <- round(selin$x, 0)
  moval <- month(moval, label = T)
  moval <- as.character(moval)
  yval <- selin$y
  tosel <- paste0('mo: ', moval, '<br />val:\\s*', trunc(yval * 1e3)/ 1e3)
  txtsel <- txt$text[grepl(tosel, txt$text)]
  yrval <- gsub('^mo.*yr:\\s+([0-9]+)<br.*$', '\\1',txtsel)
  
  # output
  out <- c(moval = moval, yrval = yrval, yval = yval)
  
  return(out)
  
}

# selected month/year algae plot
algselplo <- function(clkrct, bay_segment, algdat, epcdata, algnms, cols){

  # selected algae month/year plot
  stas <- epcdata %>% 
    filter(bay_segment %in% !!bay_segment) %>% 
    pull(epchc_station) %>% 
    unique
  browser()
  # toplo <- algdat %>% 
  #   filter(epchc_station %in% stas) %>% 
  #   filter(mo %in% clkrct[['moval']]) %>% 
  #   filter(yr %in% clkrct[['yrval']]) %>% 
  #   mutate(name = factor(name, levels = algnms)) %>% 
  #   group_by(name) %>% 
  #   summarise(count = sum(count, na.rm = T)) %>% 
  #   spread(name, count, drop = F, fill = 0) %>% 
  #   mutate(
  #     x = paste0(clkrct[c('moval', 'yrval')], collapse = ' '),
  #     width = 0.5
  #     )

  toplo <- algdat %>% 
    filter(epchc_station %in% stas) %>% 
    filter(mo %in% clkrct[['moval']]) %>%
    filter(yr %in% clkrct[['yrval']])
  
  validate(
    need(nrow(toplo) > 0, 'No phytoplankton data')
  )
  
  toplo <- toplo %>% 
    mutate(name = factor(name, levels = rev(algnms))) %>% 
    group_by(name) %>% 
    summarise(count = sum(count, na.rm = T)) %>% 
    mutate(
      x = paste0(clkrct[c('moval', 'yrval')], collapse = ' '),
      width = 0.6, 
      color = factor(name, levels = rev(algnms), labels = rev(cols))
    )

  p <-  plot_ly(toplo, x = ~ x, y= ~count, color = ~name, marker = list(color = ~color)) %>% 
    add_bars(width = ~width) %>% 
    layout(
      yaxis = list(title = 'Count (0.1/ml)', zeroline = F, showgrid = F), #gridcolor = '#FFFFFF'),
      xaxis = list(title = '', zeroline = F, showgrid = F),
      legend = list(x = 0, y = 1.1, borderwidth = 0), 
      barmode = 'stack',
      showlegend = T
      # width = 300, 
    ) %>% 
    config(displayModeBar = F)

  # p <-  plot_ly(toplo, x = ~ x, y= ~ other, type = 'bar', name = 'other', color = cols['other'], text = 'other') %>% 
  #   add_trace(y = ~`Tripos hircus`, name = 'Tripos hircus', color = cols['Tripos hircus'], text = 'Tripos hircus') %>% 
  #   add_trace(y = ~`Pyrodinium bahamense`, name = 'Pyrodinium bahamense', color = cols['Pyrodinium bahamense'], text = 'Pyrodinium bahamense') %>% 
  #   add_trace(y = ~`Pseudo-nitzschia sp.`, name = 'Pseudo-nitzschia sp.', color = cols['Pseudo-nitzschia sp.'], text = 'Pseudo-nitzschia sp.') %>% 
  #   add_trace(y = ~`Pseudo-nitzschia pungens`, name = 'Pseudo-nitzschia pungens', color = cols['Pseudo-nitzschia pungens'], text = 'Pseudo-nitzschia pungens') %>% 
  #   add_trace(y = ~`Karenia brevis`, name = 'Karenia brevis', color = cols['Karenia brevis'], text = 'Karenia brevis') %>% 
  #   add_trace(y = ~Cyanobacteria, name = 'Cyanobacteria', color = cols['Cyanobacteria'], text = 'Cyanobacteria') %>% 
  #   add_trace(y = ~Bacillariophyta, name = 'Bacillariophyta', color = cols['Bacillariophyta'], text = 'Bacillariophyta') %>% 
  #   layout(
  #     yaxis = list(title = 'Count (0.1/ml)', zeroline = F, showgrid = F), #gridcolor = '#FFFFFF'),
  #     xaxis = list(title = '', zeroline = F, showgrid = F),
  #     barmode = 'stack'#, plot_bgcolor= '#ECECEC', 
  #     # width = 200
  #     )
  
  return(p)

}

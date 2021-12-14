# algae color palette
pal_alg <- colorFactor(
  palette = RColorBrewer::brewer.pal(8,  'Dark2'),#c('#004F7E', '#00806E', '#427355', '#5C4A42', '#958984'),
  na.color = 'yellow',
  levels = c('Bacillariophyta', 'Cyanobacteria', 'Karenia brevis', 'Pseudo-nitzschia pungens', 'Pseudo-nitzschia sp.', 'Pyrodinium bahamense', 'Tripos hircus', 'other')
  )

# plotly function for boxplots
boxplotly <- function(epcdata, bay_segment, maxyr, yrrng, family, themein){

  maxyr <- as.numeric(maxyr)

  p1 <- show_boxplot(epcdata, param = 'chla', bay_segment = bay_segment, yrrng = yrrng, yrsel = maxyr, family = family, labelexp = F, txtlab = F) + 
    ggtitle(NULL) +
    themein

  p2 <- show_boxplot(epcdata, param = 'la', bay_segment = bay_segment, yrrng = yrrng, yrsel = maxyr, family = family, labelexp = F, txtlab = F) + 
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

  # replace hover text weird label for year
  p1$x$data[[2]]$text <- gsub('names\\(cols\\)\\[1\\].*$', '', p1$x$data[[2]]$text)
  p1$x$data[[3]]$text <- gsub('names\\(cols\\)\\[2\\].*$', '', p1$x$data[[3]]$text)
  p2$x$data[[2]]$text <- gsub('names\\(cols\\)\\[1\\].*$', '', p2$x$data[[2]]$text)
  p2$x$data[[3]]$text <- gsub('names\\(cols\\)\\[2\\].*$', '', p2$x$data[[3]]$text)
  
  out <- subplot(p1, p2, nrows = 2, shareX = T, titleY = TRUE) %>%
    plotly::layout(legend = list(title = ''))
  
  return(out)
  
}

# find data from plotly boxplot event selection
selfun <- function(selin, plodat, algdat, epcdata, bay_segment){

  if(!selin$curveNumber %in% c(1, 2, 5, 6))
    return(NULL)
  
  # get selection data
  if(selin$curveNumber == 1) # top plot
    txt <- plodat$x$data[[2]]
  if(selin$curveNumber == 2) # top plot
    txt <- plodat$x$data[[3]]
  if(selin$curveNumber == 5) # bottom plot
    txt <- plodat$x$data[[6]]
  if(selin$curveNumber == 6) # bottom plot
    txt <- plodat$x$data[[7]]

  moval <- round(selin$x, 0)
  moval <- month(moval, label = T)
  moval <- as.character(moval)
  yval <- selin$y
  # tosel <- paste0('mo: ', moval, '<br />val:\\s*', trunc(yval * 1e3)/ 1e3)
  # txtsel <- txt$text[grepl(tosel, txt$text)]
  txtsel <- txt$text[which.min(abs(txt$y - yval))]
  yrval <- gsub('^mo.*yr:\\s+([0-9]+)<br.*$', '\\1',txtsel)
  
  # clicked parsed data
  clkrct <- c(moval = moval, yrval = yrval, yval = yval)
  
  # selected algae month/year plot
  stas <- epcdata %>% 
    filter(bay_segment %in% !!bay_segment) %>% 
    pull(epchc_station) %>% 
    unique
  
  out <- algdat %>% 
    filter(epchc_station %in% stas) %>% 
    filter(mo %in% clkrct[['moval']]) %>%
    filter(yr %in% clkrct[['yrval']])
  
  return(out)
  
}

# selected month/year algae plot
algselplo <- function(clkrct, algnms, cols, family){

  toplo <- clkrct %>% 
    mutate(name = factor(name, levels = rev(algnms))) %>% 
    unite('x', mo, yr, sep = ' ') %>% 
    group_by(name, x) %>% 
    summarise(count = sum(count, na.rm = T)) %>% 
    mutate(
      width = 0.6, 
      color = factor(name, levels = rev(algnms), labels = rev(cols))
    )

  p <-  plot_ly(toplo, x = ~ x, y= ~count, color = ~name, marker = list(color = ~color)) %>% 
    add_bars(width = ~width) %>% 
    layout(
      yaxis = list(title = 'Phytoplankton cell count (0.1/ml)', zeroline = F, showgrid = F, titlefont = list(size = 18)),
      xaxis = list(title = '', zeroline = F, showgrid = F, tickfont = list(size = 18)),
      legend = list(x = 0, y = 1.2, borderwidth = 0, font = list(size = 16)), 
      barmode = 'stack',
      showlegend = T, 
      font = list(family = family)
      # width = 300, 
    ) %>% 
    config(displayModeBar = F)
  
  return(p)

}

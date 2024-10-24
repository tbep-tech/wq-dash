downloadButtonRmd <- function (outputId, label = "Download", class = NULL, ...)  {
  tags$a(id = outputId, class = paste("btn btn-default shiny-download-link", 
                                      class), href = "", target = "_blank", download = NA, 
         shiny::icon("download"), label, ...)
}

# algae color palette
pal_alg <- leaflet::colorFactor(
  palette = RColorBrewer::brewer.pal(8,  'Dark2'),#c('#004F7E', '#00806E', '#427355', '#5C4A42', '#958984'),
  na.color = 'yellow',
  levels = c('Bacillariophyta', 'Cyanobacteria', 'Karenia brevis', 'Pseudo-nitzschia pungens', 'Pseudo-nitzschia sp.', 'Pyrodinium bahamense', 'Tripos hircus', 'other')
  )

# plotly function for boxplots
boxplotly <- function(epcdata, bay_segment, maxyr, yrrng, themein, family = NULL){

  maxyr <- as.numeric(maxyr)

  p1 <- tbeptools::show_boxplot(epcdata, param = 'chla', bay_segment = bay_segment, yrrng = yrrng, yrsel = maxyr, family = family, labelexp = F, txtlab = F) + 
    ggplot2::ggtitle(NULL) +
    themein

  p2 <- tbeptools::show_boxplot(epcdata, param = 'la', bay_segment = bay_segment, yrrng = yrrng, yrsel = maxyr, family = family, labelexp = F, txtlab = F) + 
    ggplot2::ggtitle(NULL) +
    themein

  p1 <- plotly::ggplotly(p1)
  for(i in 1:length(p1$x$data)){ # fix legend
    p1$x$data[[i]]$name <- gsub('^\\(|,solid\\)$|,1,NA\\)$', '', p1$x$data[[i]]$name)
  }
  
  p1$x$data[[1]]$marker = list(opacity = 0) # remove outlier from plot
  
  # remove hoverinfo for boxplots and line
  p1$x$data[[3]]$hoveron <- NULL
  p1$x$data[[4]]$hoveron <- NULL
  
  p2 <- plotly::ggplotly(p2)
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
  
  out <- plotly::subplot(p1, p2, nrows = 2, shareX = T, titleY = TRUE) %>%
    plotly::layout(legend = list(title = '')) %>% 
    plotly::config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "myplot"
      )
    )
  
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
    dplyr::filter(bay_segment %in% !!bay_segment) %>% 
    dplyr::pull(epchc_station) %>% 
    unique
  
  out <- algdat %>% 
    dplyr::filter(epchc_station %in% stas) %>% 
    dplyr::filter(mo %in% clkrct[['moval']]) %>%
    dplyr::filter(yr %in% clkrct[['yrval']])
  
  return(out)
  
}

# selected month/year algae plot
algselplo <- function(clkrct, algnms, cols, family = NULL){

  toplo <- clkrct %>% 
    dplyr::mutate(name = factor(name, levels = rev(algnms))) %>% 
    tidyr::unite('x', mo, yr, sep = ' ') %>% 
    dplyr::summarise(count = sum(count, na.rm = T), .by = c('name', 'x')) %>% 
    dplyr::mutate(
      width = 0.6, 
      color = factor(name, levels = rev(algnms), labels = rev(cols))
    )

  p <-  plotly::plot_ly(toplo, x = ~ x, y= ~count, color = ~name, marker = list(color = ~color)) %>% 
    plotly::add_bars(width = ~width) %>% 
    plotly::layout(
      yaxis = list(title = 'Phytoplankton\ncell count (0.1/ml)', zeroline = F, showgrid = F, titlefont = list(size = 18)),
      xaxis = list(title = '', zeroline = F, showgrid = F, tickfont = list(size = 18)),
      legend = list(x = 0, y = 1.2, borderwidth = 0, font = list(size = 16)), 
      barmode = 'stack',
      showlegend = T, 
      font = list(family = family)
      # width = 300, 
    ) %>% 
    plotly::config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "myplot"
      )
    )
  
  return(p)

}

# plot for time series of phyto, chl, and secchi from map selection (selsit)
selsit_plo <- function(selsit){
  
  if(is.null(selsit))
    selsit <- locs[1, ]$epchc_station 
  
  gridcol <- 'white'
  plotcol <- '#E5E5E5'
  
  # data to plot
  toplo <- epcdata %>% 
    dplyr::filter(epchc_station %in% selsit) %>% 
    dplyr::filter(yr >= 1975) %>% 
    dplyr::select(Date = SampleTime, sd_raw_m, sd_q, chla) %>% 
    dplyr::mutate(Date = as.Date(Date))
  
  algplo <- algdat %>% 
    dplyr::filter(epchc_station %in% selsit) %>% 
    dplyr::summarise(count = sum(count, na.rm = T), .by = c(yrqrt, name)) %>% 
    dplyr::mutate(
      name = factor(name, levels = algnms, labels = algnms)
    )
  
  # algplot
  p1 <- plotly::plot_ly(algplo, x = ~ yrqrt, y= ~count, color = ~name, text = ~paste0(name, ', ', yrqrt), hoverinfo = 'text') %>% 
    plotly::add_bars() %>% 
    plotly::layout(
      yaxis = list(title = 'Phytoplankton\ncell count (0.1/ml)', gridcolor = gridcol),
      barmode = 'stack',
      showlegend = T
    )
  
  # chl plot
  p2 <- plotly::plot_ly(data = toplo, 
                x = ~Date, 
                y = ~chla, 
                type = 'scatter', 
                mode = 'lines', 
                line = list(color = '#427355'), 
                name = 'Chlorophyll-a') %>%
    plotly::layout(
      yaxis = list(title = 'Concentration (ug/L)', gridcolor = gridcol),
      xaxis = list(title = NULL),
      legend = list(title = list(text = NULL))
    )

  # secchi plot
  p3 <- plotly::plot_ly() %>% 
    plotly::add_trace(data = toplo, 
                x = ~Date, 
                y = ~sd_raw_m, 
                type = 'scatter', 
                mode = 'lines', 
                line = list(color = '#0047FE'), 
                name = 'Secchi depth')

  if(any(!toplo$sd_q))
    p3 <- p3 %>%
      plotly::add_trace(
        data = toplo[!toplo$sd_q, ],
        x = ~Date,
        y = ~sd_raw_m,
        type = 'scatter',
        mode = 'markers',
        marker = list(color = '#FF6347', size = 6),  # Adjust size and color
        name = 'Secchi on bottom'
      )

  p3 <- p3 %>% 
    plotly::layout(
      title = list(text = paste0('Station ', selsit)),
      yaxis = list(title = 'Depth (m)', gridcolor = gridcol),
      xaxis = list(title = NULL),
      legend = list(title = list(text = NULL))
    )
  
  out <- plotly::subplot(p1, p2, p3, nrows = 3, shareX = T, titleY = T) %>% 
    plotly::rangeslider(thickness = 0.02) %>%
    plotly::layout(
      title = paste('Station', selsit),
      legend = list(
        font = list(size = 16),
        # itemclick = FALSE, 
        # itemdoubeclick = FALSE, 
        groupclick = TRUE,
        traceorder = 'normal'
      ), 
      xaxis = list(
        domain = c(0.02, 1),
        fixedrange = FALSE,
        title = NULL,
        range = range(toplo$Date),
        rangeselector = list(
          buttons = list(
            list(step = "all"),
            list(
              count = 20,
              label = "20 yr",
              step = "year",
              stepmode = "backward"),
            list(
              count = 10,
              label = "10 yr",
              step = "year",
              stepmode = "backward"),
            list(
              count = 5,
              label = "5 yr",
              step = "year",
              stepmode = "backward"),
            list(
              count = 1,
              label = "1 yr",
              step = "year",
              stepmode = "backward")
          )
        )
      ),
      plot_bgcolor = plotcol
    ) %>% 
    plotly::config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "myplot"
      )
    )
  
  return(out)
  
}

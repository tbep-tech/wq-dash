# README

[![build](https://github.com/tbep-tech/wq-dash/actions/workflows/databuild.yaml/badge.svg)](https://github.com/tbep-tech/wq-dash/actions/workflows/databuild.yaml)
[![DOI](https://zenodo.org/badge/223773148.svg)](https://zenodo.org/badge/latestdoi/223773148)

Materials for TBEP water quality dashboard.  

[Shiny](http://shiny.tbep.org/wq-dash/)

## How the dashboard runs

The dashboard is a flexdashboard document (`wq-dash.Rmd`) that uses `runtime: shiny_prerendered`. This affects how updates reach users.

* The page layout is rendered once to `wq-dash.html` (plus `wq-dash_files/`) and reused for every visitor. These files are created on the server and are not tracked by git.
* The `setup` chunk (`context='setup'`) runs once when an R process starts. It sources `R/global.R`, which loads the files in `data/` and the latest water quality and phytoplankton data from GitHub Pages. It also builds objects shared by the page and the server, such as the station map. The attainment matrices and segment annual trend plots come from `data/plotlys.RData` and are rendered directly into `wq-dash.html`.
* The chunks with `context='server'` run once for each visitor. They hold the reactives, plot and map outputs, and download handlers.
* The plots on each page are only computed when a visitor first opens that page.

## After a data update

The water quality and phytoplankton data (`data/epcdata.RData` and `data/algdat.RData`) are rebuilt daily by the GitHub Action in `.github/workflows/databuild.yaml`. The dashboard reads these from GitHub Pages when an R process starts, so no action is needed on the server. A process that was already running keeps the old data until Shiny Server shuts it down, which happens shortly after its last visitor leaves.

The other files in `data/` are updated by running `R/dat_proc.R`. The maximum year (`maxyr`) is set by hand in `R/global.R`. After changing these files, follow the steps below.

1.  Pull the updated repository to the TBEP server. No other files need to be deleted or rendered by hand. rmarkdown re-renders `wq-dash.html` on the next request if `wq-dash.Rmd`, `R/global.R`, `R/funcs.R`, the files in `data/` loaded by `R/global.R`, the header files, or the files in `www/` (including `styles.css`) are newer than `wq-dash.html`. Pulling new files gives them a newer modified time, so this happens automatically.
1.  Make sure running R processes pick up the new data. To restart the app right away, create or update a `restart.txt` file in the app directory on the server (e.g., `touch restart.txt`).
1.  Check the live dashboard. The year sliders should end at the new year and the attainment matrices should include the new year.

## Troubleshooting

* __The dashboard still shows old data.__ Update `restart.txt` as described above and reload the page. If that does not help, delete `wq-dash.html` and the `wq-dash_files/` folder on the server and update `restart.txt` again. This forces a fresh render on the next request.
* __The app fails to start with "Unable to write prerendered HTML file".__ The Shiny Server user needs write access to the app directory so it can create `wq-dash.html`. Fix the folder permissions, or render the page once on the server with `rmarkdown::render('wq-dash.Rmd')` from the app directory.
* __The first visit after an update is slow.__ This is expected. The first request renders `wq-dash.html`, which takes some time. Later visits reuse it.
* __A plot or map shows an error.__ Run the dashboard locally to see the full R error. In RStudio, open `wq-dash.Rmd` and click Run Document. From the command line, run `rmarkdown::run('wq-dash.Rmd')`. Outside RStudio this needs pandoc, which can be pointed to the copy that comes with RStudio by setting the `RSTUDIO_PANDOC` environment variable (e.g., `Sys.setenv(RSTUDIO_PANDOC = 'C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools')`).
* __Editing the dashboard.__ Code that loads data or creates objects needed by both the page and the server goes in `R/global.R` or the `setup` chunk. Reactives and `output$...` assignments go in a `context='server'` chunk. The page layout only contains the matching output placeholders (e.g., `plotly::plotlyOutput('selplo')`). Code placed in a regular chunk only runs when the page is rendered and is not available to the server.

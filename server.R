library(shiny,quietly=T)
library(data.table, quietly=T)
library(leaflet, quietly=T)
library(DBI, quietly = T)
library(plotly, quietly = T)
library(shinyDatetimePickers)

Sys.setenv(tz = "UTC")
db = DBI::dbConnect(RSQLite::SQLite(), "loggerdb.sqlite3")

read.hobotemp <- function(filename){
  lns = readLines(filename)
  title = strsplit(lns[1], ': ')[[1]][2]
  info = strsplit(lns[2], '","')[[1]]
  serial = stringr::str_extract(info[3], "\\d+")
  d = fread(text = lns, skip = 2, select = 1:3, col.names = c("scan", "dateTime", "value"))
  d[, dateTime := as.POSIXct(dateTime, format = "%m/%d/%y %I:%M:%S %p")]
  d[, serialnumber := serial]
  d[, title := title]
  return(d)
}

onStop(function() {
  DBI::dbDisconnect(db)
})

shinyServer(function(input, output, session) {
  dat = reactiveValues()
  dat$deployments = setDT(dbGetQuery(db, "SELECT * FROM deployments"))
  dat$instruments = setDT(dbGetQuery(db, "SELECT * FROM instruments"))
  dat$locations = setDT(dbGetQuery(db, "SELECT * FROM locations"))

  observe({
    updateSelectInput(session, "select_deployment", choices = dat$deployments$filename)
    updateSelectInput(session, "select_location", choices = dat$locations$name)
  })

  observeEvent(input$query, {
  # ---- search
    selected_deployment_ids = dat$deployments[filename %in% input$select_deployment]$deployment_id
    selected_deployment_ids = paste(selected_deployment_ids, collapse = ",")
    query = paste0("SELECT * FROM data WHERE deployment_id IN (", selected_deployment_ids, ")")
    print(query)
    dat$results = setDT(dbGetQuery(db, query))
    dat$results[, datetime := as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M")]
  })

  output$map = renderLeaflet({
    leaflet(dat$locations) |>
      addTiles() |>
      addCircleMarkers(popup = ~name)
  })

  output$timeseries_plot = renderPlotly({
    validate(need(dat$results, label = "results"))
    ggplot(dat$results) +
      geom_line(aes(datetime, value, color = site, group = deployment_id)) +
      labs(x=NULL, y = "Temperature") +
      theme_bw() + theme(legend.position = "top")
  })

  output$pre_upload_plot = renderPlotly({
    validate(need(dat$upload, label = ".csv"))
    plot_ly(dat$upload, x = ~dateTime, y = ~ value, type = 'scatter', mode = 'lines') |>
      layout(shapes = list(
        type = "rect", x0 = input$deployment_start, x1 = input$deployment_end,
        y0 = min(dat$upload$value, na.rm = T), y1 = max(dat$upload$value, na.rm = T),
        line = list(color = "rgba(255, 0, 0, 0)"),  # No border line
        fillcolor = "rgba(55, 55, 55, 0.2)", layer = "below"
      ))
  })

  output$deployments = DT::renderDT({
    dat$deployments
  })

  observeEvent(input$hobo_file, {
  # ---- load_file
    dat$upload = read.hobotemp(input$hobo_file$datapath)
    dat$upload$filename = input$hobo_file$name
    updateDatetimeMaterialPickerInput(session, "deployment_start", min(dat$upload$dateTime, na.rm=T))
    updateDatetimeMaterialPickerInput(session, "deployment_end", max(dat$upload$dateTime, na.rm=T))
    # TODO validate hobo
  })

  observeEvent(input$submit, {
    showNotification("not implemented", type="warning", duration = 5)
  })

  observeEvent(input$debug, {
    browser()
  })
})

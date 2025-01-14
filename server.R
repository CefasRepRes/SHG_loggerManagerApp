library(shiny,quietly=T)
library(data.table, quietly=T)
library(leaflet, quietly=T)
library(DBI, quietly = T)
library(plotly, quietly = T)
library(shinyDatetimePickers)
library(readxl)

Sys.setenv(tz = "UTC")
db = DBI::dbConnect(RSQLite::SQLite(), "loggerdb.sqlite3")
source("functions.R")

onStop(function() {
  DBI::dbDisconnect(db)
})

shinyServer(function(input, output, session) {
  dat = reactiveValues()
  dat$deployments = setDT(dbGetQuery(db, "SELECT * FROM deployments ORDER BY start DESC"))
  dat$instruments = setDT(dbGetQuery(db, "SELECT * FROM instruments"))
  dat$locations = setDT(dbGetQuery(db, "SELECT * FROM locations ORDER BY name"))

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
  # ---- load hobo
    if(grepl("\\.csv", input$hobo_file$name, ignore.case = T)){
      dat$upload = read.hoboV2(input$hobo_file$datapath)
    }
    if(grepl("\\.xlsx", input$hobo_file$name, ignore.case = T)){
      dat$upload = read.hoboMX(input$hobo_file$datapath)
    }
    dat$upload$filename = input$hobo_file$name
    updateDatetimeMaterialPickerInput(session, "deployment_start", min(dat$upload$dateTime, na.rm=T))
    updateDatetimeMaterialPickerInput(session, "deployment_end", max(dat$upload$dateTime, na.rm=T))
  })
  
  observeEvent(input$minidot_file, {
  # ---- load_minidot
    dat$upload = read.miniDOT(input$minidot_file$datapath)
    dat$upload$filename = input$minidot_file$name
    updateDatetimeMaterialPickerInput(session, "deployment_start", min(dat$upload$dateTime, na.rm=T))
    updateDatetimeMaterialPickerInput(session, "deployment_end", max(dat$upload$dateTime, na.rm=T))
  })
  
  

  observeEvent(input$submit, {
    new_deployment_id = dbGetQuery(db, "SELECT MAX(deployment_id)+1 AS id FROM deployments")$id
    new_deployment = data.table(filename = dat$upload$filename[1],
                                location_id = input$select_location,
                                deployment_id = new_deployment_id,
                                instrument_id = dat$instruments[serial == dat$upload$serialnumber[1]]$serial,
                                location_id = dat$locations[name == input$select_location]$location_id,
                                start = min(dat$upload$dateTime, na.rm = T),
                                end = max(dat$upload$dateTime, na.rm = T))
    results = dat$upload[,.(datetime = dateTime,
                            deployment_id = new_deployment_id,
                            value,
                            variable_id = variable_id,
                            scan,
                            flag = 0)]
    results = results[!is.na(value)]
    # apply flags
    results[datetime < input$deployment_start, flag := 1]
    results[datetime > input$deployment_end, flag := 1]
    dbWithTransaction(db, {
      tryCatch({
        dbAppendTable(db, "deployments", new_deployment)
        dbAppendTable(db, "results", results)
        showNotification("data written to database", type="message", duration = 5)
        },
        error = function(e) {
          cat("Error: ", conditionMessage(e))
          showNotification(paste("Error writing to database:", conditionMessage(e)), duration=NULL, type = "error")
          dbBreak()
        })
    })
  })

  observeEvent(input$debug, {
    browser()
  })
})

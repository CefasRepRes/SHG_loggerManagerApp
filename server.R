library(shiny,quietly=T)
library(data.table, quietly=T)
library(leaflet, quietly=T)
library(DBI, quietly = T)
library(plotly, quietly = T)
library(shinyDatetimePickers, quietly = T)
library(readxl, quietly = T)

Sys.setenv(tz = "UTC")
db = DBI::dbConnect(RSQLite::SQLite(), "loggerdb.sqlite3")
source("functions.R")

  # added up update database structure, can be removed in later versions
if(!("depth" %in% DBI::dbListFields(db, "deployments"))){
  print("depth field not found, adding to deployments table")
  DBI::dbExecute(db, "ALTER TABLE deployments
                 ADD COLUMN depth INTEGER;")
}

onStop(function() {
  DBI::dbDisconnect(db)
})

shinyServer(function(input, output, session) {
  dat = reactiveValues()
  dat$deployments = setDT(dbGetQuery(db, "SELECT * FROM deployments ORDER BY start DESC"))
  dat$instruments = setDT(dbGetQuery(db, "SELECT * FROM instruments"))
  dat$variables = setDT(dbGetQuery(db, "SELECT * FROM variables ORDER BY variable_id DESC"))
  dat$locations = setDT(dbGetQuery(db, "SELECT * FROM locations ORDER BY name"))
  dat$calibrations = setDT(dbGetQuery(db, "SELECT * FROM calibrations ORDER BY cal_date DESC"))

  observe({
    updateSelectInput(session, "select_deployment", choices = dat$deployments$filename)
    updateSelectInput(session, "select_location", choices = c("UNKNOWN", dat$locations$name))
  })

  observeEvent(input$query, {
  # ---- search
    selected_deployment_ids = dat$deployments[filename %in% input$select_deployment]$deployment_id
    selected_deployment_ids = paste(selected_deployment_ids, collapse = ",")
    query = paste0("SELECT * FROM data WHERE deployment_id IN (", selected_deployment_ids, ")")
    query = paste0(query, "AND variable = 'Temperature'")
    print(query)
    dat$data = setDT(dbGetQuery(db, query))
    dat$data[, datetime := as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M")]
  })

  output$map = renderLeaflet({
    dat$locations[, label := paste0(name, " <br>(", latitude, ", ", longitude, ")")]
    leaflet(dat$locations) |>
      addTiles() |>
      addCircleMarkers(lat = ~latitude, lng = ~longitude, popup = ~label)
  })

  output$timeseries_plot = renderPlotly({
    validate(need(dat$data, "Select data"))
    ggplot(dat$data) +
      geom_line(aes(datetime, value, color = site, group = deployment_id)) +
      labs(x=NULL, y = "Temperature") +
      theme_bw() + theme(legend.position = "top")
  })

  # pre upload plot ----
  output$pre_upload_plot = renderPlotly({
    validate(need(dat$upload, "Upload HOBO / miniDOT data to view"))
    d = merge(dat$upload, dat$variables, by = "variable_id")
    d[, lab := paste0(varname, " (", unit, ")")]
      plot_ly(d, x = ~dateTime, y = ~ value, type = 'scatter', mode = 'lines', name = ~lab) |>
        layout(shapes = list(
          list(
            type = "rect",
            x0 = min(dat$upload$dateTime-1800, na.rm = T), x1 = input$deployment_start,
            y0 = min(dat$upload$value, na.rm = T), y1 = max(dat$upload$value, na.rm = T),
            line = list(color = "rgba(255, 0, 0, 0)"),  # No border line
            fillcolor = "rgba(204, 51, 102, 0.5)", layer = "above"
            ),
          list(
            type = "rect",
            x1 = max(dat$upload$dateTime+1800, na.rm = T), x0 = input$deployment_end,
            y0 = min(dat$upload$value, na.rm = T), y1 = max(dat$upload$value, na.rm = T),
            line = list(color = "rgba(255, 0, 0, 0)"),  # No border line
            fillcolor = "rgba(204, 51, 102, 0.5)", layer = "above"
            )
          ),
          legend = list(x = 0, y = 100)
        )
  })

  output$deployments = DT::renderDT({
    dat$deployments
  })
  
  observeEvent(input$hobo_file, {
    if(input$hobo_file$name %in% dat$deployments$filename){
      showNotification("This file has already been uploaded to the database", duration=10, type = "error")
    }
  # load hobo ----
    if(grepl("\\.csv", input$hobo_file$name, ignore.case = T)){
      dat$upload = read.hoboV2(input$hobo_file$datapath)
    }
    if(grepl("\\.xlsx", input$hobo_file$name, ignore.case = T)){
      dat$upload = read.hoboMX(input$hobo_file$datapath)
    }
    
    if(all(is.na(dat$upload$dateTime))){
      showNotification("Unable to read date time, likely incorrect format. please make sure it is MM/DD/YY HH:MM:SS", duration=10, type = "error")
    }
    
    if(!dat$upload$serialnumber[1] %in% dat$instruments$serial){
      showNotification(paste("Sensor serial number", dat$upload$serialnumber[1], "not found in database"), type = "warning")
      # Check if new sensor
      if(!dat$upload$serialnumber[1] %in% dat$instruments$serial){
        showModal(modalDialog(
          title = "Unknown sensor",
          "This sensor has not been used before, do you wish to add it as a new HOBO",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("add_HOBO", "OK")
            )
          ))
      }
    }
    
    dat$upload$filename = input$hobo_file$name
    updateDatetimeMaterialPickerInput(session, "deployment_start", min(dat$upload$dateTime, na.rm=T))
    updateDatetimeMaterialPickerInput(session, "deployment_end", max(dat$upload$dateTime, na.rm=T))
  })
  
  observeEvent(input$minidot_file, {
    if(input$minidot_file$name %in% dat$deployments$filename){
      showNotification("This file has already been uploaded to the database", duration=10, type = "error")
    }
  # load_minidot ----
    dat$upload = read.miniDOT(input$minidot_file$datapath)
    dat$upload$filename = input$minidot_file$name
    if(!dat$upload$serialnumber[1] %in% dat$instruments$serial){
      showNotification(paste("Sensor serial number", dat$upload$serialnumber[1], "not found in database"), type = "warning")
      # Check if new sensor
      if(!dat$upload$serialnumber[1] %in% dat$instruments$serial){
        showModal(modalDialog(
          title = "Unknown sensor",
          "This sensor has not been used before, do you wish to add it as a new PME MiniDOT",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("add_miniDOT", "OK")
            )
          ))
      }
    }
    updateDatetimeMaterialPickerInput(session, "deployment_start", min(dat$upload$dateTime, na.rm=T))
    updateDatetimeMaterialPickerInput(session, "deployment_end", max(dat$upload$dateTime, na.rm=T))
  })
  
  observeEvent(input$add_miniDOT, {
    new_sensor = data.frame(
      instrument_id = dbGetQuery(db, "SELECT MAX(instrument_id)+1 AS id FROM instruments")$id,
      type = "PME",
      model = "MiniDOT",
      serial = dat$upload$serialnumber[1])
    dbAppendTable(db, "instruments", new_sensor)
    dat$instruments = setDT(dbGetQuery(db, "SELECT * FROM instruments"))
    removeModal()
  })
  
  observeEvent(input$add_HOBO, {
    new_sensor = data.frame(
      instrument_id = dbGetQuery(db, "SELECT MAX(instrument_id)+1 AS id FROM instruments")$id,
      type = "HOBO",
      model = "unknown",
      serial = dat$upload$serialnumber[1])
    dbAppendTable(db, "instruments", new_sensor)
    dat$instruments = setDT(dbGetQuery(db, "SELECT * FROM instruments"))
    removeModal()
  })
  
  observeEvent(input$submit, {
    # submit to database ----
    if(input$select_location == "UNKNOWN"){
      showNotification("You must select a valid location", type = "error")
      return()
    }
    if(input$depth == 0){
      showNotification("You must specify the depth the logger was at", type = "error")
      return()
    }
    if(input$deployment_start > input$deployment_end){
      showNotification("Deployment start time is after deployment end time!", type = "error")
      return()
    }
    if(input$deployment_start < min(dat$upload$dateTime, na.rm = T)){
      showNotification("Deployment start time is before first measurement!", type = "error")
      return()
    }
    if(input$deployment_end > max(dat$upload$dateTime, na.rm = T)){
      showNotification("Deployment start time is after last measurement!", type = "error")
      return()
    }
    
    new_deployment_id = dbGetQuery(db, "SELECT MAX(deployment_id)+1 AS id FROM deployments")$id
    new_deployment = data.table(filename = dat$upload$filename[1],
                                location_id = dat$locations[name == input$select_location]$location_id,
                                deployment_id = new_deployment_id,
                                instrument_id = dat$instruments[serial == dat$upload$serialnumber[1]]$instrument_id,
                                start = input$deployment_start,
                                end = input$deployment_end,
                                depth = input$depth)
    
    if(nrow(new_deployment) != 1){
      showNotification("New deployment record is invalid, unable to upload", type = "error")
      return()
    }
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
    
    # format dates for sqlite
    results[, datetime := strftime(datetime, "%FT%H:%M:%SZ")]
    new_deployment[, start := strftime(start, "%FT%H:%M:%SZ")]
    new_deployment[, end := strftime(end, "%FT%H:%M:%SZ")]
    
    dbWithTransaction(db, {
      tryCatch({
        dbAppendTable(db, "deployments", new_deployment)
        dbAppendTable(db, "results", results)
        showNotification("data written to database", type="message", duration = 5)
        dat$deployments = setDT(dbGetQuery(db, "SELECT * FROM deployments ORDER BY start DESC"))
        },
        error = function(e) {
          cat("Error: ", conditionMessage(e))
          showNotification(paste("Error writing to database:", conditionMessage(e)), duration=NULL, type = "error")
          dbBreak()
        })
    })
  })
  
  output$download_csv <- downloadHandler(
    # download CSV ----
    filename = function() {
      paste0("logger-", Sys.Date(), ".csv")
    },
    content = function(filename) {
      fwrite(dat$data, file = filename)
    }
  )
  
  output$download_db <- downloadHandler(
    # download db ----
    filename = function() {
      paste0("loggerdb-", Sys.Date(), ".sqlite3.zip")
    },
    content = function(filename) {
      zip(filename, "loggerdb.sqlite3")
    }
  )
  
  observeEvent(input$debug, {
    browser()
  })
})

library(shiny)
library(shinyFiles)
library(plotly)
library(leaflet)
library(shinyDatetimePickers)
library(shinybusy, quietly = T)

shinyUI(
  fluidPage(
    add_busy_spinner(spin = "fading-circle"),
    titlePanel("STH data loggers"),
    tabsetPanel(
      tabPanel("Map",
        # ---- map
        leafletOutput("map", height = "85vh")
      ),
      tabPanel("View",
        # ---- View Panel
        sidebarLayout(
          sidebarPanel(width = 3,
            selectizeInput("select_deployment", "Select Deployments", choices = "", multiple = T),
            actionButton("query", "Fetch data", icon = icon("database")),
            p(),
            downloadButton("download_csv", "Download .CSV")
          ),
          mainPanel(
            plotlyOutput("timeseries_plot", height = "80vh")
          )
        )
        ),
      tabPanel("Deployments",
        DT::DTOutput("deployments"),
        downloadButton("download_db", "Backup Database")
        ),
      tabPanel("Upload",
        # ---- View Upload
        column(3,
          fileInput("hobo_file", "Hobo", buttonLabel = "Upload...", accept = c(".csv", ".xlsx")),
          fileInput("minidot_file", "Minidot", buttonLabel = "Upload...", accept = ".txt"),
          selectizeInput("select_location", "Select location", choices = ""),
          h4("select in-water start:"),
          shinyDatetimePickers::datetimeMaterialPickerInput("deployment_start"),
          h4("select in-water end:"),
          shinyDatetimePickers::datetimeMaterialPickerInput("deployment_end"),
          h4("grey area will be marked as in-water"),
          actionButton("submit", "Submit", icon = icon("save"))
          ),
        column(9,
          plotlyOutput("pre_upload_plot", height = "80vh")
          )
        )
    ),
    actionButton("debug", "debug")
  ))

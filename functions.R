read.hoboV2 <- function(filename){
  lns = readLines(filename)
  title = strsplit(lns[1], ': ')[[1]][2]
  info = strsplit(lns[2], '","')[[1]]
  serial = stringr::str_extract(info[3], "\\d+")
  d = fread(text = lns, skip = 2, select = 1:3, col.names = c("scan", "dateTime", "value"))
  d[, dateTime := as.POSIXct(dateTime, format = "%m/%d/%y %I:%M:%S %p")]
  d[, value := as.numeric(value)]
  d[, serialnumber := as.integer(serial)]
  d[, title := title]
  d[, variable_id := 1]
  return(d)
}

read.hoboMX <- function(filename){
  meta = setDT(readxl::read_xlsx(filename, "Details", col_names = c("meta1", "meta2", "meta3", "meta4")))
  data = setDT(readxl::read_xlsx(filename, "Data", col_names = c("scan", "dateTime", "value"), skip = 1))
  serial = meta[grepl("Serial Number", meta3)]$meta4
  data[, value := as.numeric(value)]
  data[, serialnumber := as.integer(serial)]
  data[, title := ""]
  data[, variable_id := 1]
  return(data)
}

read.miniDOT_raw <- function(filename){
  meta = readLines(filename, n = 2)
  d = fread(filename, skip = 2, col.names = c("dateTime", "battery", "temp", "DO", "Q"))
  d[, dateTime := as.POSIXct(dateTime, origin = "1970-01-01", tz = "UTC")]
  d = melt(d, id.vars = "dateTime", value.name = "value")
  d[, serialnumber := as.integer(strsplit(meta[1], "-")[[1]][2])]
  d[, title := ""]
  d[variable == "temp", variable_id := 1]
  d[variable == "DO", variable_id := 2]
  d = d[!is.na(variable_id),-c("variable")]
  return(d)
}

read.miniDOT <- function(filename){
  meta = readLines(filename, n = 2)
  d = fread(filename, skip = 9, col.names = c("unix", "dateTime", "GMT", "Batt", "temp", "DO", "SAT", "Q"))
  d[, scan := 1:.N]
  d = melt(d, id.vars = c("dateTime", "scan"), measure.vars = c("temp", "DO"), value.name = "value")
  d[, serialnumber := as.integer(stringr::str_extract(meta[2], "\\d+-(\\d+)", group = 1))]
  d[variable == "temp", variable_id := 1]
  d[variable == "DO", variable_id := 2]
  d = d[!is.na(variable_id),-c("variable")]
  return(d)
}

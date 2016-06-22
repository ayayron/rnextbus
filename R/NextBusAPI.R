#' NextBus API
#'@param base_url The base url for the NextBus webservice
#'@return nextbus_url
#'@import httr XML data.table
#'@export
nextbus_url <- function(base_url = NULL) {
  if (!missing(base_url)) {
    options(nextbus_url = base_url)
  }
  getOption("nextbus_url", "http://webservices.nextbus.com/service/publicXMLFeed")
}

#' NextBus API
#'@param param a list of parameters for the GET query. Built as part of each of the functions.
#'@return xml_parse
#'@import httr XML
#'@export
nextbus_api <- function(param) {
  response = httr::GET(nextbus_url(), query = param)
  if (response$status_code != 200) {
    stop(httr::content(response))
  } else {
    # Errors in API still return as 200
    xml_parse = XML::xmlParse(httr::content(response, "text", encoding = "UTF-8"))
    xml_parse = XML::xmlRoot(xml_parse)
    if (names(XML::xmlChildren(xml_parse)) %in% "Error") {
      e = XML::xmlChildren(xml_parse)
      stop(xmlValue(e$Error[[1]]))
    } else {
      return(XML::xmlSApply(xml_parse, XML::xmlAttrs))
    }
  }
}

#'Agency List
#'@return agency_df
#'@import data.table
#'@export
list_agencies <- function() {
  cmd = "agencyList"
  response = nextbus_api(list(command = cmd))
  agency_list = lapply(response, as.list)
  agency_df = data.table::rbindlist(agency_list, fill = TRUE)
  return(agency_df)
}

#' Route list
#'@param agency The tag name of the bus agency. Can get from \code{list_agencies()} function
#'@return route_list
#'@export
list_routes <- function(agency) {
  cmd = "routeList"
  response = nextbus_api(list(command = cmd, a = agency))
  route_list = lapply(response, as.list)
  return(route_list)
}

#'Route Configuration
#'
#'@param agency The tag name of the bus agency. Can get from \code{list_agencies()} function
#'@param route The name of the route. Can get from \code{route_list()} function
#'@return route_config
#'@export
route_config <- function(agency, route) {
  cmd = "routeConfig"
  response = nextbus_api(list(command = cmd, a = agency, r = route))
  route_config = lapply(response, as.list)
  return(route_config)
}

#'Predictions
#'
#'Combine the two predictions for stops
#'@param agency The tag name of the bus agency. Can get from \code{list_agencies()} function
#'@param route The name of the route. Can get from \code{route_list()} function
#'@param stop The name of the stop. Can get from \code{route_config()} function.
#'@return predictions
#'@export
predictions <- function(agency, route, stop) {

  if (length(stop) > 1) {
    cmd = "predictionsForMultiStops"
  } else {
    cmd = "predictions"
  }
  response = nextbus_api(list(command = cmd, a = agency, r = route, s = stop))
  predictions = lapply(response, as.list)
  return(predictions)
}

#'Schedule
#'
#'Get the schedule for a particular route
#'@param agency The tag name of the bus agency. Can get from \code{list_agencies()} function
#'@param route The name of the route. Can get from \code{route_list()} function
#'@return schedule_df
#'@export
schedule <- function(agency, route) {
  cmd = "schedule"
  response = nextbus_api(list(command = cmd, a = agency, r = route))
  schedule = lapply(response, as.list)
  return(schedule)
}

#'Messages
#'
#'Get the messages for selected routes
#'
#'@param agency The tag name of the bus agency. Can get from \code{list_agencies()} function
#'@param route The name of the route. Can get from \code{route_list()} function
#'@return messages
#'@export
messages <- function(agency, route) {
  cmd = "messages"
  response = nextbus_api(list(command = cmd, a = agency, r = route))
  messages = lapply(response, as.list)
  return(messages)
}

#'Vehicle Location
#'
#'Get the location of a vehicle
#'
#'@param agency The tag name of the bus agency. Can get from \code{list_agencies()} function
#'@param route The name of the route. Can get from \code{route_list()} function
#'@param time The unix time in msec. Default of t = 0 returns the most recent 15 minutes of data.
#'@return vehicle_location
#'@export
vehicle_locations <- function(agency, route, time = 0) {
  cmd = "vehicleLocations"
  response = nextbus_api(list(command = cmd, a = agency, r = route, t = time))
  vehicle_locations = lapply(response, as.list)
  return(vehicle_locations)
}

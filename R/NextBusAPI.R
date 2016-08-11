#' NextBus API
#'
#' Base URL function
#'
#'@param base_url The base url for the NextBus webservice
#'@return nextbus_url
#'@export
nextbus_url <- function(base_url = NULL, retry = NULL) {
  if (!missing(base_url)) {
    options(nextbus_url = base_url)
  }
  getOption("nextbus_url", "http://webservices.nextbus.com/service/publicXMLFeed")

  # if (!missing(retry)) {
  #   options(retry = TRUE)
  # }
}

#' NextBus API
#'
#' Main call to the API
#'
#'@param param a list of parameters for the GET query. Built as part of each of the functions.
#'@return xml_parse
#'@importFrom httr GET content
#'@importFrom XML xmlParse xmlRoot xmlChildren xmlValue xmlSApply xmlAttrs
#'@export
nextbus_api <- function(param) {

  # can use gzip xml for improved speed
  #Accept-Encoding: gzip, deflate


  response = httr::GET(nextbus_url(), query = param)
  if (response$status_code != 200) {
    stop(httr::content(response))
  } else {
    # Errors in API still return as 200
    xml_parse = XML::xmlParse(httr::content(response, "text", encoding = "UTF-8"))
    xml_parse = XML::xmlRoot(xml_parse)
    if (names(XML::xmlChildren(xml_parse)) %in% "Error") {
      e = XML::xmlChildren(xml_parse)

      # if  shouldRetry="true" you can try again because
      # it could just be a temp server issue. Check the retry option

      # otherwise stop

      stop(XML::xmlValue(e$Error[[1]]))
    } else {
      return(xml_parse)
    }
  }
}

#'Agency List
#'
#'List of agencies in Nextbus
#'
#'@return agency_df
#'@importFrom data.table rbindlist
#'@export
nb_list_agencies <- function() {
  cmd = "agencyList"
  response = nextbus_api(list(command = cmd))
  response = XML::xmlSApply(response, XML::xmlAttrs)
  agency_list = lapply(response, as.list)
  agency_df = data.table::rbindlist(agency_list, fill = TRUE)
  return(agency_df)
}

#'Route list
#'
#'List of routes for a  given bus agency
#'
#'@param agency The tag name of the bus agency. Can get from \code{list_agencies()} function
#'@return route_list
#'@export
nb_list_routes <- function(agency) {
  cmd = "routeList"
  response = nextbus_api(list(command = cmd, a = agency))
  response = XML::xmlSApply(response, XML::xmlAttrs)
  route_list = lapply(response, as.list)
  route_list = unlist(route_list)
  return(route_list)
}

#'Route Configuration
#'
#'Configuration of a particular route for an agency
#'
#'@param agency The tag name of the bus agency. Can get from \code{list_agencies()} function
#'@param route The name of the route. Can get from \code{route_list()} function
#'@param path Set to TRUE if you want to include path information, this can slow down the query because it's a lot of data. default = FALSE
#'@return route_config list(route_info, stops, directions, paths)
#'@export
nb_route_config <- function(agency, route, path = FALSE) {
  cmd = "routeConfig"
  if (path) {
    "&terse"
  }
  response = nextbus_api(list(command = cmd, a = agency, r = route))

  route_desc = XML::xmlSApply(response, XML::xmlAttrs)

  response = XML::xmlSApply(response[["route"]], XML::xmlAttrs)
  route_config = lapply(response, as.list)
  stop_table = data.table::rbindlist(route_config, fill = TRUE)

  # for each direction. not necessarily two directions

  # combine the paths inside a single path, but not across

  return(list(route = route_desc, stops = stop_table, directions = NULL, paths = NULL))
}

#'Bus Predictions
#'
#'Combine the two predictions for stops
#'
#'@param agency The tag name of the bus agency. Can get from \code{list_agencies()} function
#'@param route The name of the route. Can get from \code{route_list()} function
#'@param stop The name of the stop. Can get from \code{route_config()} function.
#'@param short_titles Use short titles for shortening the names. Can be good for smaller displays.
#'@return predictions
#'@export
nb_predictions <- function(agency, route, stop, short_titles = FALSE) {

  if (length(stop) > 1) {
    cmd = "predictionsForMultiStops"

    #need to apply the route and stop in the form stops=N|3031&stops...
    stops = paste(route, stop, sep = "|")
    query_params = list(command = cmd, a = agency, r = route)
    query_params[4:(length(stops) + 3)] = stops
    names(query_params)  = c("command", "a", "r", rep("stops", length(stops)))

  } else {
    cmd = "predictions"
    query_params = list(command = cmd, a = agency, r = route, s = stop)
  }

  if (short_titles) {
    query_params[["useShortTitles"]] = "true"
  }

  response = nextbus_api(query_params)
  response = XML::xmlSApply(response, XML::xmlAttrs)
  predictions = lapply(response, as.list)

  # returns potential multiple directions and multiple messages

  # also additional attributes

  # convert epoch to datetime


  return(predictions)
}

#'Schedule
#'
#'Get the schedule for a particular route
#'
#'@param agency The tag name of the bus agency. Can get from \code{list_agencies()} function
#'@param route The name of the route. Can get from \code{route_list()} function
#'@return schedule_df
#'@export
nb_schedule <- function(agency, route) {
  cmd = "schedule"
  response = nextbus_api(list(command = cmd, a = agency, r = route))
  response = XML::xmlSApply(response, XML::xmlAttrs)
  schedule = lapply(response, as.list)
  return(schedule)
}

#'Agency Messages
#'
#'Get the messages for selected routes
#'
#'@param agency The tag name of the bus agency. Can get from \code{list_agencies()} function
#'@param route The name of the route. Can get from \code{route_list()} function
#'@return messages
#'@export
nb_messages <- function(agency, route) {
  cmd = "messages"
  response = nextbus_api(list(command = cmd, a = agency, r = route))
  response = XML::xmlSApply(response, XML::xmlAttrs)
  messages = lapply(response, as.list)
  return(messages)
}

#' Vehicle Location
#'
#'Get the location of a vehicle
#'
#'@param agency The tag name of the bus agency. Can get from \code{list_agencies()} function
#'@param route The name of the route. Can get from \code{route_list()} function
#'@param time The unix time in msec. Default of t = 0 returns the most recent 15 minutes of data.
#'@return vehicle_location
#'@export
nb_vehicle_locations <- function(agency, route, time = 0) {
  cmd = "vehicleLocations"
  response = nextbus_api(list(command = cmd, a = agency, r = route, t = time))
  response = XML::xmlSApply(response, XML::xmlAttrs)
  vehicle_locations = lapply(response, as.list)
  return(vehicle_locations)
}

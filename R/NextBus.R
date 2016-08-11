#' rnextbus: Wrapper for the NextBus API
#'
#' This package is a connection to the NextBus API.
#' @section NextBus functions:
#' The NextBus functions
#'nb_list_agencies
#'nb_list_routes
#'nb_route_config
#'nb_schedule
#'nb_messages
#'nb_vehicle_locations
#' @sections Data limitations
#' From the current NextBus API PDF, the current limitations exist:
#' Maximum characters per requester for all commands (IP address): 2MB/20sec
#' Maximum routes per "routeConfig" command: 100
#' Maximum stops per route for the "predictionsForMultiStops" command: 150
#' Maximum number of predictions per stop for prediction commands: 5
#' Maximum timespan for "vehicleLocations" command: 5min
#' @references \url{http://www.nextbus.com/xmlFeedDocs/NextBusXMLFeed.pdf}
#' @references \url{https://gist.github.com/grantland/7cf4097dd9cdf0dfed14}
#' @docType package
#' @name rnextbus
NULL

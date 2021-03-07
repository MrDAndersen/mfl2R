#' MFL Endpoint
#'
#' R6 Class representing the the endpoint for MFL. Used for low level operations
#' such as logging in and generating request urls.
mfl_endpoint <- R6::R6Class(
  "mfl_app",
  public = list(
    scheme = NULL,
    host = NULL,
    season = NULL,

    initialize = function(scheme = "https",
                          host = "api.myfantasyleague.com",
                          season = "2021"){
      self$scheme <- scheme
      self$host <- host
      self$season <- season
    },

    print = function(){
      cat('Connecting to:\t', private$base_url())
      cat('\nSeason:\t\t', self$season)

      if(self$logged_in())
        cat("\nLogged in")
      else
        cat("\nNot Logged in")

      invisible(self)
    },

    login = function(user_name, password = NULL){
      if(is.null(password))
        password <- rstudioapi::askForPassword("Enter your MyFantasyLeague password")
      login_url <- httr::modify_url("https://api.myfantasyleague.com",
                                    path = file.path(self$season, "login"),
                                    query = list(USERNAME = user_name,
                                                 PASSWORD = password,
                                                 XML = "1"))

      login_resp <- httr::GET(login_url)
      login_content <- httr::content(httr::GET(login_url))

      if(httr::status_code(login_resp) == 200){
        error_text <- xml2::xml_text(xml2::xml_find_all(login_content, "//error"))

        if(length(error_text) > 0)
          stop(error_text, call. = FALSE)

        private$user_cookie <<- xml2::xml_attrs(xml2::xml_find_first(login_content, "//status"))[["mfl_user_id"]]

      }
    },

    export_query = function(qry_type, qry_params = list()){
      ex_url <- self$export_url()

      qry_params <- append(list(TYPE = qry_type), qry_params)

      ex_url <- httr::modify_url(ex_url, query = qry_params)

      return(ex_url)
    },

    import_query = function(qry_type, qry_params = list()){
      im_url <- self$import_url()

      qry_params <- append(list(TYPE = qry_type), qry_params)

      im_url <- httr::modify_url(im_url, query = qry_params)

      return(im_url)
    },

    request = function(request_url){

      req_type <- basename(httr::parse_url(request_url)$path)


      if(req_type == "export"){
        req_query <- httr::parse_url(request_url)$query
        request_cookie <- NULL
        if(!is.null(private$user_cookie))
          request_cookie <- httr::set_cookies(.cookies = c(MFL_USER_ID = private$user_cookie))

        req_query$JSON <- 1
        request_url <- httr::modify_url(request_url, query = req_query)

        mfl_response <- httr::GET(request_url, request_cookie, add_headers(`User-Agent` = "MFL2R CLIENT"))

        if(httr::status_code(mfl_response) == 200){
          mfl_content <- httr::content(mfl_response, "text")
          mfl_content <- jsonlite::fromJSON(mfl_content, simplifyVector = FALSE)

          return(mfl_content)
        }
      }
    },
    export_url = function(){
      httr::modify_url(private$base_url(), path = private$request_path("export"))
    },
    import_url = function(){
      httr::modify_url(private$base_url(), path = private$request_path("import"))
    },
    logged_in = function(){
      structure(!is.null(private$user_cookie), user_cookie = private$user_cookie)
    }
  ),
  private = list(
    user_cookie = NULL,
    base_url = function(){
      .url <- structure(
        list(scheme = self$scheme, hostname = self$host),
        class = "url"
      )
      return(httr::build_url(.url))
    },
    request_path = function(type){
      file.path(self$season, type, fsep = "/")
    }
  )
)


#' Generic endpoint object
#'
#' The enpoint is generated at package startup and provides the basis for the
#' functions in the package.
#' @export mfl_app
mfl_app <- mfl_endpoint$new()

#' Define the season
#'
#' Defines the season that will be used
#' @export set_season
set_season <- function(season){
  mfl_app$season <- season
}

#' Get bye weeks
#'
#' Finds bye weeks based on the NFL schedule that is loaded into MFL.
#'
#' @param season The season that bye weeks are requested for
#' @param gameWeek The week that bye weeks are requested for. Leave blank to get
#' full season.
#'
#' @export bye_weeks
bye_weeks <- function(season = NULL, gameWeek = NULL){
  nfl_app <- mfl_app$clone()

  if(missing(season) | is.null(season))
    season <- nfl_app$season
  else if(season != nfl_app$season)
    nfl_app$season <- season

  url_query <- list()

  if(!is.null(gameWeek))
    url_query$W <- gameWeek

  byes <- nfl_app$request(nfl_app$export_query("nflByeWeeks", url_query))

  byes %>% .[[c("nflByeWeeks", "team")]] %>%
    map(as_tibble) %>% bind_rows() %>% rename(team = "id")
}

#' Get NFL Schedule
#'
#' Retreives NFL Schedule from MFL
#'
#' @param season Season that schedule should be retreived for.
#' @param gameWeek The week that the NFL schedule shoud be retreived for. Use
#' \code{"ALL"} to retrieve full season
#'
#' @return List of lists with matchups
#' @export nfl_schedule
nfl_schedule <- function(season = NULL, gameWeek = NULL){
  if(is.null(season)){
    nfl_app <- mfl_app$clone()
  } else {
    nfl_app <- mfl_endpoint$new(season = season)
  }

  url_query <- list()
  if(is.null(gameWeek))
    gameWeek <- "ALL"

  url_query$W <- gameWeek



  nfl_url <- nfl_app$export_query("nflSchedule", url_query)

  schedule <- nfl_app$request(nfl_url)

  if(gameWeek != "ALL"){
    return(schedule$nflSchedule)
  } else {
    return(schedule$fullNflSchedule)
  }
}

#' Login to MFL
#'
#' Logs a user into MFL and enables user specific data to be retreieved.
#' @param user_name User name for user
#' @param password Password for user
#'
#' @export mfl_login
mfl_login <- function(user_name, password = NULL){
  mfl_app$login(user_name, password)
  invisible(mfl_app)
}

#' List users leagues
#'
#' Returns a list of leagues that belong to the user that is logged in via \link{mfl_login}.
#'
#' @export my_leagues
my_leagues <- function(franchise_names = FALSE){
  qry_param <- list(FRANCHISE_NAMES = 0)
  if(franchise_names)
    qry_param$FRANCHISE_NAMES = 1

  lg_req <- mfl_app$export_query("myleagues", qry_param)

  lg_list <- mfl_app$request(lg_req)$leagues

  lg_list %>%
    map(~ prepend(.x, list(league_id = basename(httr::parse_url(.x$url)$path)))) %>%
    map(as_tibble) %>% bind_rows()
}

#' Player list
#'
#' Get list of players from MFL
#' @param details \code{TRUE/FALSE} indicating whether full detailed information
#' should be retreived
#' @param since Unix Timestamp to retrieve only changes to the player database
#' since that time
#' @param player_id Player IDs to retrieve information for
#' @return Tibble with player information
#' @export players
players <- function(details = FALSE, since = NULL, player_id = NULL){
  qry_param <- list()

  if(details)
    qry_param$DETAILS <- 1

  if(!is.null(since))
    qry_param$SINCE <- since

  if(!is.null(player_id))
    qry_param$PLAYERS <- paste(player_id, collapse = ",")

  player_req <- mfl_app$export_query("players", qry_param)

  player_list <-  mfl_app$request(player_req)$players

  if(is.null(player_id) || length(player_id) > 1){
    player_list$player %>% map(as_tibble) %>%
      bind_rows() %>% `attr<-`("timestamp", player_list$timestamp)
  } else {
    as_tibble(player_list$player)
  }
}

#' Injury information
#'
#' Retrieves injury information from MFL.
#' @return Tibble with player id, injury status and injury detail
#' @export injuries
injuries <- function(){
  mfl_app$request(mfl_app$export_query("injuries"))$injuries$injury %>%
    map(as_tibble) %>% bind_rows() %>%
    mutate(details = str_remove(details, "[:cntrl:]")) %>%
    select(id, status, details)
}


#' Rule Dictionary
#'
#' Retrieves data from MFL that describes the rules that can be applied in the
#' leagues.
#' @export
rule_dictionary <- function(){
  mfl_app$export_query(qry_type = "allRules") %>%
    mfl_app$request() %>%
    `[[`(c("allRules", "rule")) %>%
    map_df(as_tibble) %>%
    unnest(c("detailedDescription", "shortDescription", "abbreviation"))
}

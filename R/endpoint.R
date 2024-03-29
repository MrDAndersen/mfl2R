#' Season Information
#'
#' Information on current state of season from Sleeper.
season_state <- httr::GET("https://api.sleeper.app/v1/state/nfl") %>% httr::content()

current_season <- season_state$season
current_week <- season_state$week

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
                          season = current_season){
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

    logout = function(){
      private$user_cookie <<- NULL
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
#' The endpoint is generated at package startup and provides the basis for the
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
  league_app$login(user_name, password)
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

#' Player Profile
#'
#' Returns a list with summary of information regarding a player, including DOB,
#' ADP ranking, height/weight.
#'
#' @param player_id A single player ID or vector of multiple player IDs to get information on
#' @export
player_profile <- function(player_id){
  qry_parms <- list(P = paste(player_id, collapse = ","))
  profiles <- mfl_app$export_query(qry_type = "playerProfile", qry_parms) %>%
    mfl_app$request()

  if(length(player_id) == 1){
    return(profiles$playerProfile)
  } else {
    return(profiles[[c("playerProfiles", "playerProfile")]])
  }
}

#' Player Ranks
#'
#' Overall player rankings from the experts at FantasySharks.com. These rankings
#' can be used instead of Average Draft Position (ADP) rankings for guidance during
#' your draft, or when generating your own draft list.
#'
#' @param position Filters the rankings to this position only (optional)
#' @export
player_ranks <- function(position = NULL){
  qry_parms <- list(POS = NULL)
  if(!is.null(position)){
    qry_parms <- list_modify(qry_parms, POS = position)
  }

  mfl_app$export_query(qry_type = "playerRanks", qry_parms) %>%
    mfl_app$request() %>% `[[`(c("player_ranks", "player")) %>%
    map(as_tibble) %>% bind_rows()
}

#' Average Draft information
#'
#' ADP or AAV results, including when the result were last updated, how many drafts or
#' auctions the player was selected in and the average auction value or average, maximum
#' and minimum pick position. Average auction value is relative to an auction where a total
#' of $1000 is available across all franchises.
#' @param type Return average draft position (adp) or average auction values (aav)
#' @param period This returns draft data for just drafts that started after the specified
#' period. Valid values are ALL, RECENT, DRAFT, JUNE, JULY, AUG1, AUG15, START, MID, PLAYOFF.
#' @param num_franchises 	This returns draft data from just leagues with this number of franchises.
#' Valid values are 8, 10, 12, 14 or 16. If the value is 8, it returns data from leagues with 8 or
#' less franchises. If the value is 16 it returns data from leagues with 16 or more franchises.
#' @param PPR 	Filters the data returned as follows: If set to 0, data is from leagues that not
#' use a PPR scoring system; if set to 1, only from PPR scoring system; if set to -1, all leagues.
#' @param keeper Pass a string with some combination of N, K and R: if N specified, returns data
#' from redraft leagues, if 'K' is specified, returns data for keeper leagues and if 'R' is specified,
#' return data from rookie-only drafts. You can combine these. If you specify c('K','R') it will return
#' rookie and keeper drafts only. Default is c('N', 'K', 'R').
#' @param mock If set to 1, returns data from mock draft leagues only. If set to 0, excludes data
#' from mock draft leagues. If set to -1, returns all
#' @param cutoff Only returns data for players selected in at least this percentage of drafts.
#' So if you pass 10, it means that players selected in less than 10% of all drafts will not be returned.
#' Note that if the value is less than 5, the results may be unpredictable.
#' @param details If set to 1, it returns the leagues that were included in the results.
#' @export
average_draft <- function(type = c("adp", "aav"),
                          period = c("ALL", "RECENT", "DRAFT", "JUNE", "JULY", "AUG1", "AUG15", "START", "MID", "PLAYOFF"),
                          num_franchises = c("12", "8", "10", "14", "16"), PPR = c("-1", "0", "1"), keeper = c("N", "K", "R"),
                          mock = c("-1", "0", "1"), cutoff = NULL, details = NULL){
  type <- match.arg(type)
  period <- match.arg(period)
  num_franchises <- match.arg(num_franchises)
  PPR = match.arg(PPR)
  keeper = match.arg(keeper, several.ok = TRUE)
  mock = match.arg(mock)

  qry_parms <- list(PERIOD = period, IS_PPR = PPR, IS_KEEPER = paste(keeper, collapse = ""))

  if(type == "adp")
    qry_parms <- list_modify(qry_parms, FCOUNT = num_franchises, IS_MOCK = mock, CUTOFF = cutoff, DETAILS = details)

  draft_data <- mfl_app$export_query(qry_type = type, qry_parms) %>%
    mfl_app$request() %>% `[[`(type)

  if(type == "adp" & details == 1)
    draft_data <- list_modify(draft_data, league_info = draft_data[[c("leagues", "league")]] %>% map_df(as_tibble), leagues = NULL)

  draft_data %>% list_modify(player_tbl = draft_data$player %>% map_df(as_tibble), player = NULL)
}

#' @include endpoint.R

#' League Class
#'
#' R6 Class representing an MFL League. This class is used to build league
#' specfic functions.
mfl_league <- R6::R6Class(
  "league",
  inherit = mfl_endpoint,
  public = list(
    currentWaiverType = NULL,
    playerLimitUnit	= NULL,
    taxiSquad	= NULL,
    survivorPool = NULL,
    lastRegularSeasonWeek = NULL,
    endWeek = NULL,
    lockout = NULL,
    nflPoolStartWeek = NULL,
    injuredReserve = NULL,
    standingsSort = NULL,
    commish_username = NULL,
    id = NULL,
    startWeek = NULL,
    nflPoolType = NULL,
    survivorPoolStartWeek = NULL,
    survivorPoolEndWeek = NULL,
    rosterSize = NULL,
    name = NULL,
    rostersPerPlayer = NULL,
    h2h = NULL,
    loadRosters = NULL,
    baseURL = NULL,
    nflPoolEndWeek = NULL,
    precision	= NULL,
    auctionStartAmount = NULL,
    minBid = NULL,
    bidIncrement = NULL,
    bbidConditional = NULL,
    bbidSeasonLimit = NULL,
    draftLimitHours = NULL,
    draftPlayerPool = NULL,
    leagueLogo = NULL,
    bestLineup = NULL,
    conferences = list(),
    divisions = list(),
    franchises = list(),
    starters = list(),
    history = list(),

    initialize = function(league_id = NULL, season = NULL){
      if(is.null(season))
        self$season <- mfl_app$season
      else
        self$season <- as.character(season)

      self$host <- mfl_app$host
      self$scheme <- mfl_app$scheme

      if(!is.null(league_id)){
        league_query <- self$export_query("league", list(L = league_id))

        response_header <- httr::HEAD(league_query)
        league_host <- httr::parse_url(response_header$url)$hostname
        league_response <- self$request(league_query)

        self$host <- league_host

        for(league_var in intersect(names(league_response$league), names(self))){
          switch(league_var,
                 "franchises" = {
                   self$franchises <- league_response$league$franchises$franchise %>%
                     map(as.tibble) %>% bind_rows() %>%
                     mutate(name = trimws(cleanFun(name)))
                 },
                 "divisions" = {
                   self$divisions <- league_response$league$divisions$division %>%
                     map(as.tibble) %>% bind_rows() #%>%
                     #mutate(name = trimws(cleanFun(name)))
                 },
                 "conferences" = {
                   self$conferences <-  league_response$league$conferences$conference %>%
                     map(as.tibble) %>% bind_rows() %>%
                     mutate(name = trimws(cleanFun(name)))
                 },

                 "starters" = {
                                self$starters <- league_response$league$starters$position %>%
                     map(as.tibble) %>% bind_rows() %>%
                     extract(limit, c("min", "max"), "([0-9]+)\\-([0-9]+)") %>%
                     `attr<-`("max_starters", league_response$league$starters$count) %>%
                     `attr<-`("idp_starters", league_response$league$starters$idp_starters)
                 },
                 self[[league_var]] <- league_response$league[[league_var]]
          )
        }

        if(mfl_app$logged_in())
          private$user_cookie <- attr(mfl_app$logged_in(), "user_cookie")
      } else {
        self$currentWaiverType <- NULL
        self$playerLimitUnit	<- NULL
        self$taxiSquad	<- NULL
        self$survivorPool <- NULL
        self$lastRegularSeasonWeek <- NULL
        self$endWeek <- NULL
        self$lockout <- NULL
        self$nflPoolStartWeek <- NULL
        self$injuredReserve <- NULL
        self$standingsSort <- NULL
        self$commish_username <- NULL
        self$id <- NULL
        self$startWeek <- NULL
        self$nflPoolType <- NULL
        self$survivorPoolStartWeek <- NULL
        self$survivorPoolEndWeek <- NULL
        self$rosterSize <- NULL
        self$name <- NULL
        self$rostersPerPlayer <- NULL
        self$h2h <- NULL
        self$loadRosters <- NULL
        self$baseURL <- NULL
        self$nflPoolEndWeek <- NULL
        self$precision	<- NULL
        self$auctionStartAmount <- NULL
        self$minBid <- NULL
        self$bidIncrement <- NULL
        self$bbidConditional <- NULL
        self$bbidSeasonLimit <- NULL
        self$draftLimitHours <- NULL
        self$draftPlayerPool <- NULL
        self$leagueLogo <- NULL
        self$bestLineup <- NULL
        self$conferences <- list()
        self$divisions <- list()
        self$franchises <- list()
        self$starters <- list()
        self$history <- list()
      }

      invisible(self)
    },

    print = function(){
      cat('League:\t\t', self$name)
      cat('\nLeague ID:\t', self$id)
      cat('\nConnecting to:\t', private$base_url())
      cat('\nSeason:\t\t', self$season, "\n")

      if(self$logged_in())
        cat("\nLogged in")
      else
        cat("\nNot Logged in")

      invisible(self)
    },

    home_url  = function(){
      paste(self$baseURL, self$season, "home", self$id , sep = "/")
    },
    standings = function(){
      lg_standings <- self$request(private$league_url("leagueStandings"))

      lg_standings$leagueStandings$franchise %>%
        map(as.tibble) %>% bind_rows()
    },

    rules = function(){
      self$request(private$league_url("rules")) %>%
        `[[`(c("rules", "positionRules"))  %>%
        modify_depth(2, ~ map(.x, ~ map(.x, `[[`, 1))) %>%
        map( ~ map(.x$rule, as.tibble)  %>% map(add_column, positions = unlist(.x$positions))) %>%
        map(bind_rows) %>% bind_rows() %>% split(.$positions) %>%
        map(select, -positions)
    },

    draft_results = function(){
      draft <-  self$request(private$league_url("draftResults"))

      if(self$playerLimitUnit != "LEAGUE"){
        results <- draft %>% .[[c("draftResults", "draftUnit")]] %>%
          modify_depth(2, ~ map(.x, as.tibble)) %>% modify_depth(2, bind_rows) %>%
          map(`[[`, "draftPick") %>%
          map(~ if("comments" %in% names(.x)){
            mutate(.x, comments = trimws(cleanFun(str_replace_all(comments, "[:cntrl:]", " "))))
            }) %>%
          imap(~ structure(.x, unit = draft[[c("draftResults", "draftUnit")]][[.y]][["unit"]],
                           draftType = draft[[c("draftResults", "draftUnit")]][[.y]][["draftType"]],
                           round1DraftOrder = draft[[c("draftResults", "draftUnit")]][[.y]][["round1DraftOrder"]]))
      } else {
        results <- draft %>% .[[c("draftResults", "draftUnit", "draftPick")]] %>%
          map(as.tibble) %>% bind_rows() %>%
          structure(., unit = draft[[c("draftResults", "draftUnit", "unit")]],
                    draftType = draft[[c("draftResults", "draftUnit", "draftType")]],
                    round1DraftOrder = draft[[c("draftResults", "draftUnit", "round1DraftOrder")]])

        if("comments" %in% names(results)){
          results <- mutate(results , comments = trimws(cleanFun(str_replace_all(comments, "[:cntrl:]", " "))))
        }
      }

      return(results)
    },

    future_draft_picks = function(){
      draft_pick_list <- self$request(private$league_url("futureDraftPicks"))

      draft_pick_list[[c("futureDraftPicks", "franchise")]] %>%
        map(~ map(.x$futureDraftPick, as.tibble) %>% map(mutate, franchise = .x$id)) %>%
        map(bind_rows) %>% bind_rows() %>% select(-matches("^value$"))
    },

    auction_results = function(){
      auction <- self$request(private$league_url("auctionResults"))

      if(self$playerLimitUnit != "LEAGUE"){
        results <- auction %>% .[[c("auctionResults", "auctionUnit")]] %>%
          modify_depth(2, ~ map(.x, as.tibble)) %>% modify_depth(2, bind_rows) %>%
          map(`[[`, "auction") %>%
          map(~ if("comments" %in% names(.x)){
            mutate(.x, comments = trimws(cleanFun(str_replace_all(comments, "[:cntrl:]", " "))))
          }) %>%
          imap(~ structure(.x, unit = auction[[c("auctionResults", "auctionUnit")]][[.y]][["unit"]]))
      } else {
        results <- auction %>% .[[c("auctionResults", "auctionUnit", "auction")]] %>%
          map(as.tibble) %>% bind_rows() %>%
          structure(., unit = auction[[c("auctionResults", "auctionUnit", "unit")]])

        if("comments" %in% names(results)){
          results <- mutate(results , comments = trimws(cleanFun(str_replace_all(comments, "[:cntrl:]", " "))))
        }
      }
      results
    },

    league_request = function(req_type, req_param = list()){
      req <- private$league_url(req_type)

      if(length(req_param) > 0){
        qry_param <- httr::parse_url(req)$query
        qry_param <- append(qry_param, req_param)
        req <- httr::modify_url(req, query = qry_param)
      }

      return(req)
    }
  ),
  private = list(

    league_url = function(req_type){
      if(is.null(self$id))
        stop("No League ID available. Please use set_league to set league ID and retrieve league info", call. = FALSE)

      api_url <- self$export_query(req_type, list(L = self$id))
      return(api_url)
    },
    home_path = function(){
      file.path(self$season, "home", self$id, fsep = "/")
    }
  )
)

#' Basic league object that is generated at package startup
league_app <- mfl_league$new()

#' Identify league
#'
#' Sets the league id to be used.
#' @param league_id Id of the league to use
#' @param season season to use if different from current season
#' @export set_league
set_league <- function(league_id, season = NULL){
  league_app$initialize(league_id, season)
  invisible(league_app)
}

#' Retrieve league standings
#'
#' Pulls league standings from active league.
#' @export league_standings
league_standings <- function(){
  league_app$standings()
}

#' League Franchises
#'
#' Table of league franchises in active league
#' @export franchises
franchises <- function(){
  return(league_app$franchises %>% mutate_all(str_trim))
}

#' League Divisions
#'
#' Table of league divisons in active league
#' @export divisions
divisions <- function(){
  return(league_app$divisions %>% mutate_all(str_trim))
}

#' League Conferences
#'
#' Table of league conferences in active league
#' @export conferences
conferences <- function(){
  return(league_app$conferences %>% mutate_all(str_trim))
}

#' Lineup requirements for active league
#'
#' Tibble with a row for each position with min require
#' @export starters
starters <- function(){
  return(league_app$starters)
}

#' Fantasy points allowed
#'
#' Fantasy points allowed by each NFL team based on current league scoring
#' settings, broken out by position.
#' @export points_allowed
points_allowed <- function(){
  pts_allow <- league_app$request(league_app$league_request("pointsAllowed"))

  if(length(pts_allow$pointsAllowed) == 0)
    return(tibble())

  pts_allow[[c("pointsAllowed", "team")]] %>%
    modify_depth(3, as.tibble, .ragged = TRUE) %>%
    map( ~ bind_rows(.x$position) %>% mutate(team = unlist(.x$id))) %>%
    bind_rows() %>% spread(name, points) %>%
    modify_at(which(names(.) != "team"), as.numeric)
}

#' Rosters for current league
#'
#' @export league_rosters
league_rosters <- function(){
  roster_list <- league_app$request(league_app$league_request("rosters"))

  roster_list[[c("rosters", "franchise")]] %>%
    map(as_tibble) %>%
    map(rename, franchise_id = id) %>%
    map(rowwise) %>%
    map(do, add_column(data.frame(.$player), franchise_id = .$franchise_id, .before = 1)) %>%
    map(ungroup) %>% bind_rows() %>% mutate(status = as.character(status))
}

#' 	Player scores for active league
#'
#' 	All player scores for the active league in a givenweek, including all
#' 	rostered players as well as all free
#' 	@param week The week to pull the scores from. Use \code{"YTD"} for year-to=date
#' 	and \code{"AVG"] for average scores.
#' 	@param season The season to pull the scores from.
#' 	@param player_id Player Id(s) to retrieve scores for
#' 	@param status Set to \code{"freeagent"} to return only free agents in the
#' 	active league
#' 	@param count Used to limit the number of players to retreive score for.
#'
#' @export player_scores
player_scores <- function(week = NULL, season = NULL, player_id = NULL, position = NULL,
                          status = NULL, rules = NULL, count = NULL){
  req_param <- list()
  if(!is.null(week))
    req_param$W <- week

  if(!is.null(season))
    req_param$YEAR <- season

  if(!is.null(player_id))
    req_param$PLAYERS <- paste(player_id, collapse = ",")

  if(!is.null(position))
    req_param$POSITION <- position

  if(!is.null(status))
    req_param$STATUS <- status

  if(!is.null(rules))
    req_param$RULES <- rules

  if(!is.null(count))
    req_param$COUNT <- count

  score_list <- league_app$request(league_app$league_request("playerScores", req_param))

  score_list[[c("playerScores", "playerScore")]] %>%
    map(as.tibble) %>% bind_rows() %>%
    mutate(gameWeek = score_list[[c("playerScores", "week")]])
}


#' Projected Scores for active league
#'
#' Get projected scores based on active leagues scoring rules.
#' @param week The week to get scores from
#' @param player_id Player Id(s) for players to get projected scores for
#' @param position Position to get projected scores for
#' @param status Set to \code{"freeagent"} to only return projected scores for
#' free agents
#' @param count Set to limit number of players returned
#' @export projected_scores
projected_scores <- function(week = NULL,  player_id = NULL, position = NULL,
                          status = NULL, count = NULL){
  req_param <- list()
  if(!is.null(week))
    req_param$W <- week

  if(!is.null(player_id))
    req_param$PLAYERS <- paste(player_id, collapse = ",")

  if(!is.null(position))
    req_param$POSITION <- position

  if(!is.null(status))
    req_param$STATUS <- status

  if(!is.null(count))
    req_param$COUNT <- count

  score_list <- league_app$request(league_app$league_request("projectedScores", req_param))

  score_list[[c("projectedScores", "playerScore")]] %>%
    map(as.tibble) %>% bind_rows() %>%
    mutate(gameWeek = score_list[[c("projectedScores", "week")]])
}


#' Free agents for active league
#'
#' Lists free agent for th active league. Will take into account the league unit.
#' @param position Position to return free agents for
#' @export free_agents
free_agents <- function(position = NULL){
  req_param <- list(JSON=1)

  if(!is.null(position))
    req_param$POSITION <- position

  fa_list <- league_app$request(league_app$league_request("freeAgents", req_param))#$freeAgents

  if(league_app$playerLimitUnit == "LEAGUE"){
    fa_list[[c("freeAgents", "leagueUnit", "player")]] %>% map(as_tibble) %>%
      map(mutate, league_unit = fa_list[[c("freeAgents", "leagueUnit", "unit")]]) %>%
      bind_rows()
  } else {
    fa_list[[c("freeAgents", "leagueUnit")]] %>%
      map(as_tibble) %>%
      map(rowwise) %>%
      map(do, add_column(data.frame(.$player), unit = .$unit, .before = 1)) %>%
      map(ungroup) %>% bind_rows()
  }
}

#' Schedule for active league
#'
#' Lists schedule for the active league
#' @param week The week to get the schedule for
#' @param franchise The franchise to get schedule for
#' @export
league_schedule <- function(week = NULL, franchise = NULL){
  req_param <- list(JSON=1)

  if(!is.null(week))
    req_param$W <- week

  if(!is.null(franchise))
    req_param$F <- franchise

  schedule <- league_app$request(league_app$league_request("schedule", req_param)) %>%
    `[[`(c("schedule", "weeklySchedule"))

  if(is.null(week)){
    schedule <- map(schedule, `[[`, "matchup")
    l_depth <- if_else(is.null(franchise), 4, 3)
  } else {
    schedule <- schedule %>% `[[`("matchup")
    l_depth <- if_else(is.null(franchise), 3, 2)
  }

  schedule <- schedule %>%
    keep(~ length(.) > 0) %>% modify_depth(l_depth, as.tibble) %>%
    modify_depth(l_depth - 1, bind_rows)

  if(is.null(week)){
    if(!is.null(franchise))
      return(map(schedule, `[[`, "franchise"))

    schedule %>% map(~ `names<-`(.x, 1:length(.x))) %>%
    modify_depth(l_depth - 2, `[[`, "franchise") %>% map(bind_rows, .id = "matchup")
  } else {
    if(!is.null(franchise))
      return(schedule[["franchise"]])

    schedule %>% `names<-`(., 1:length(.)) %>%
      map(`[[`, "franchise") %>% bind_rows(.id = "matchup")
  }
}

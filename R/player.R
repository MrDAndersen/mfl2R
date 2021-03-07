#' @include  endpoint.R

draft_info <- R6::R6Class(
  "draft_info",
  public = list(
    year = NULL,
    team = NULL,
    round = NULL,
    pick = NULL,
    initialize = function(year = mfl_app$season, team = NULL, round = NULL, pick = NULL){
      self$year <- year
      self$team <- team
      self$round <- round
      self$pick <- pick
    }
  )
)

other_ids <- R6::R6Class(
  "other_ids",
  public = list(
    stats_id = NULL,
    cbs_id = NULL,
    fleaflicker_id = NULL,
    nfl_id = NULL,
    espn_id = NULL,
    rotowire_id = NULL,
    sportsdata_id = NULL,
    rotoworld_id = NULL,
    stats_global_id = NULL,

    initialize = function(stats_id = NULL, cbs_id = NULL, fleaflicker_id = NULL,
                          nfl_id = NULL, espn_id = NULL, rotowire_id = NULL,
                          sportsdata_id = NULL, rotoworld_id = NULL,
                          stats_global_id = NULL){
      self$stats_id <- stats_id
      self$cbs_id <- cbs_id
      self$fleaflicker_id <- fleaflicker_id
      self$nfl_id <- nfl_id
      self$espn_id <- espn_id
      self$rotowire_id <- rotowire_id
      self$sportsdata_id <- sportsdata_id
      self$rotoworld_id <- rotoworld_id
      self$stats_global_id <- stats_global_id
    }
  )
)


#' @export mfl_player
mfl_player <- R6::R6Class(
  "mfl_player",
  public = list(
    id = NULL,
    name = NULL,
    position = NULL,
    team = NULL,
    draft = NULL,
    ids = NULL,
    weight = NULL,
    height = NULL,
    birthdate = NULL,
    college = NULL,
    jersey = NULL,
    status = NULL,
    twitter_username = NULL,
    initialize = function(id = NULL, name = NULL, position = NULL, team = NULL,
                          weight = NULL, height = NULL, college = NULL, jersey = NULL,
                          status = NULL, stats_id = NULL, cbs_id = NULL, birthdate = NULL,
                          fleaflicker_id = NULL, nfl_id = NULL, espn_id = NULL,
                          rotowire_id = NULL, sportsdata_id = NULL, rotoworld_id = NULL,
                          stats_global_id = NULL,
                          draft_year = NULL, draft_team = NULL, draft_round = NULL,
                          draft_pick = NULL, twitter_username = NULL) {
      self$id <- id
      self$name <- name
      self$position <- position
      self$team <- team
      self$draft <- draft_info$new(year = draft_year, team = draft_team, round = draft_round, pick = draft_pick)
      self$ids <- other_ids$new(stats_id, cbs_id, fleaflicker_id, nfl_id, espn_id, rotowire_id, sportsdata_id, rotoworld_id, stats_global_id)
      self$weight <- weight
      self$height <- height
      self$birthdate <- birthdate
      self$college <- college
      self$jersey <- jersey
      self$status <- status
      self$twitter_username <- twitter_username
    },
    get = function(player_id){
      player_info <- players(details = TRUE, player_id = player_id) %>% as.list()
      do.call(self$initialize, player_info)
    },
    profile = function(){
      api_url <- mfl_app$apiUrl("export")

    }
  )
)

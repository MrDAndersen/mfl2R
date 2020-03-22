#' @include  endpoint.R
#' @export mfl_player
mfl_player <- R6::R6Class(
  "player",
  public = list(
    id = NULL,
    name = NULL,
    position = NULL,
    team = NULL,
    draft_year = NULL,
    draft_team = NULL,
    draft_round = NULL,
    draft_pick = NULL,
    rotoworld_id = NULL,
    stats_id = NULL,
    stats_gobal_id = NULL,
    nfl_id = NULL,
    kffl_id = NULL,
    espn_id = NULL,
    fleaflicker_id = NULL,
    cbs_id = NULL,
    sportsdata_id = NULL,
    weight = NULL,
    height = NULL,
    college = NULL,
    jersey = NULL,
    status = NULL,
    get = function(player_id){
      api_url <- mfl_app$apiUrl("export")

    },
    profile = function(){
      api_url <- mfl_app$apiUrl("export")

    }
  )
)

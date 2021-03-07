#' Base R6 Class for scoring rules
#'
#' @description
#' Stores base information for a single scoring rule
#'
#' @details
#' Include method for calculating scoring.
scoring_rule <- R6::R6Class(
  "scoring_rule",
  public = list(

    #' @field event Stores the scoring event for the rule
    event = NULL,

    #' @field range Stores the range for the event that is valid for the rule
    range = NULL,

    #' @field points Stores how many points is given for the event. If first character
    #'  is \code{*} then it is a multiplier rule. If the points is in the format
    #'  \code{x/y} then x points is given for every y. If points is just a number then
    #'  that is what is scored if the value is within the range.
    points = NULL,

    #' @description
    #' Create a new rule
    #'
    #' @details
    #' Creates a new rule based on the event, range and points specified.
    #'
    #' @param event The scoring event to create the rule for.
    #' @param range The event range to create the scoring rule for.
    #' @param points The points setting for the scoring rule.
    initialize = function(event = NULL, range = NULL, points = NULL){
      self$event <- event
      self$range <- range
      self$points <- points
    },

    #' @description Displays Rule Details
    print = function(){
      cat("Event:\t", self$event, "\n")
      cat("Range:\t", self$range, "\n")
      cat("Points:\t", self$points, "\n")
    },

    #' @description
    #' Calculates the value resulting from the value x happening
    #' for the event.
    #' @param x The value for the event that needs to be scored.
    #' @param season Is the value x for an entire season?
    #' @param season_weeks Number of weeks included in the season.
    value = function(x, season = FALSE, season_weeks = 16){
      if(season)
        x <- x / season_weeks

      range_values <- stringr::str_extract_all(self$range, "^-?\\d+|\\d+")[[1]]
      names(range_values) <- c("left", "right")

      range_values <- as.list(range_values)

      range_values <- purrr::list_modify(range_values, x = x)

      in_range <- do.call(dplyr::between, range_values)

      if(in_range){

        if(stringr::str_starts(self$points, "\\*")){
          pt_value <- as.numeric(stringr::str_remove(self$points, "^\\*"))
          val <- x * pt_value
        } else if(stringr::str_detect(self$points, "/*")) {
          pt_value <- as.numeric(str_split(self$points)[[1]])
          val <- pt_value[1] * floor(x / pt_value[2])
        } else {
          val <- as.numeric(self$value)
        }
      } else {
        val <- 0
      }

      if(season)
        val <- val * season_weeks

      return(val)
    },

    #' @description
    #' Determines if supplied event and range matches with the current rule.
    #'
    #' @details
    #' Can be used to find a specific rule
    #'
    #' @param event Which event to look for
    #' @param range Which range to look for
    match = function(event, range){
      self$event == event & self$range == range
    }
  )
)

position_rules <- R6::R6Class(
  "position_rules",
  public = list(
    positions = NULL,
    rules = NULL,
    initialize = function(positions = c("QB", "RB", "WR", "TE", "PK", "Def", "DL", "LB", "DB"),
                          rules = list()){
      stopifnot("Positions provided not valid." = all(positions %in% c("QB", "RB", "WR", "TE", "PK", "Def", "DL", "LB", "DB")))

      self$positions <- match.arg(positions, several.ok = TRUE)

      self$rules <- purrr::map(rules, ~ do.call(scoring_rule$new, .x))
    },

    print = function(){
      cat("Position(s):\t", paste(self$positions, collapse = ", "), "\n")
      print(purrr::map_df(self$rules, ~ dplyr::tibble(Event = .x$event, Range = .x$range, Points = .x$points)))
    },

    value = function(x, season = FALSE){
      stopifnot("Elements have to be named" = !is.null(names(x)))

      val_list <- purrr::imap(x, ~ {
        ev <- .y

        scoring_event <- purrr::keep(self$rules, ~ .x$event == ev)
        if(length(scoring_event) == 0)
          return(0)

        r <- purrr::keep(self$rules, ~ .x$event == ev)[[1]]

        r$value(.x, season)
      })

      purrr::reduce(val_list, `+`)
    },

    remove_rule = function(event, range){
      if(!private$rule_exists(event, range))
        message("Rule doesn't exist. No changes made")
      else
        self$rules <- discard(self$rules, ~ .x$event == event & .x$range == range)
    },
    add_rule = function(event, range, points){

      if(private$rule_exists(event, range))
        stop("A rule already exists for this event and range", call. = FALSE)

      self$rules <- append(self$rules, list(list(event = event, range = range, points = points)))
    },

    update_rule = function(event, range, new_points = NULL, new_range = NULL){
      if(is.null(new_points) & is.null(new_range)){
        message("New values not provided. No changes made")
        return()
      }

      if(!private$rule_exists(event, range))
        message("Rule doesn't exist. No changes made")
      else {
        r <- private$which_rule(event, range)

        new_vals <- list()

        if(!is.null(new_range))
          self$rules[[r]] <- purrr::list_modify(self$rules[[r]], range = new_range)

        if(!is.null(new_points))
          self$rules[[r]] <- purrr::list_modify(self$rules[[r]], points = new_points)
      }
    }
  ),
  private = list(
    match_rule = function(event, range){
      purrr::map_lgl(self$rules, ~ .x$match(event, range))
    },
    rule_exists = function(event, range){
      any(private$match_rule(event, range))
    },
    which_rule = function(event, range){
      which(private$match_rule(event, range))
    }
  )
)

league_rules <- R6::R6Class(
  "league_rules",
  public = list(
    rules = NULL,
    initialize = function(pos_rules = list()){
      self$rules <- purrr::imap(pos_rules, ~ {
        pos <- stringr::str_split(.y, "\\|")[[1]]

        stopifnot("Positions provided not valid." = all(pos %in% c("QB", "RB", "WR", "TE", "PK", "Def", "DL", "LB", "DB")))

        position_rules$new(positions = pos, rules = .x)
      })
    },
    print = function(){
      purrr::walk(self$rules, print)
    },
    value = function(x = list(), position =  c("QB", "RB", "WR", "TE", "PK", "Def", "DL", "LB", "DB"), season = FALSE){
      position = match.arg(position)

      pos_rules <- purrr::keep(self$rules, ~ position %in% .x$positions)

      rule_vals <- purrr::map_dbl(pos_rules, ~ .x$value(x, season))

      purrr::reduce(rule_vals, `+`)
    },

    add_rule = function(positions = c("QB", "RB", "WR", "TE", "PK", "Def", "DL", "LB", "DB"), rules = list()){

      stopifnot("Positions provided not valid." = all(positions %in% c("QB", "RB", "WR", "TE", "PK", "Def", "DL", "LB", "DB")))

      positions = match.arg(positions, several.ok = TRUE)

      rule_check <- keep(self$rules, ~  identical(sort(.x$positions), sort(positions)))

      if(length(rule_check) >= 1)
        stop("A rule already exists for positions.", call. = FALSE)

      self$rules <- append(self$rules, position_rules$new(positions, rules))
    },

    remove_rule = function(positions = c("QB", "RB", "WR", "TE", "PK", "Def", "DL", "LB", "DB")){
      stopifnot("Positions provided not valid." = all(positions %in% c("QB", "RB", "WR", "TE", "PK", "Def", "DL", "LB", "DB")))

      positions = match.arg(positions, several.ok = TRUE)

      rule_check <- keep(self$rules, ~  identical(sort(.x$positions), sort(positions)))

      if(length(rule_check) == 0)
        stop("Rule doesn't exists. No changes made", call. = FALSE)

      self$rules <- discard(self$rules, ~  identical(sort(.x$positions), sort(positions)))
    }
  )
)

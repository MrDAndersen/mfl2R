cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}


mfl_query <- function(resource, ...){
  mfl_params <- c("W", "L", "PLAYERS", "DETAILS", "SINCE",
                  "DAYS", "TIME", "FRANCHISES", "IS_PPR", "IS_KEEPER",
                  "IS_MOCK", "INJURED", "CUTOFF", "FRANCHISE_NAMES", "YEAR",
                  "POSITION", "RULES", "STATUS", "COUNT", "TRANS_TYPE",
                  "FRANCHISE", "SEARCH", "THREAD", "P", "FRANCHISE_ID",
                  "INCLUDE_DRAFT_PICKS", "F", "POOLTYPE")


  query_vars <- as.list(match.call())[-1]

  query_list <- list(TYPE = resource)

  for(var in intersect(names(query_vars), mfl_params)){
    query_list[[var]] <- as.character(query_vars[[var]])
  }
  return(query_list)
}

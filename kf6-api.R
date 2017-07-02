##########################
### Library for KF API ###
##########################

### Load libraries
library(httr)
library(jsonlite)
library(dplyr)

### Load utility functions
# source("utils.R")
ua <- user_agent("https://github.com/meefen/kf6-shinydashboard")

# token = NA
# communityId = NA

### API URLs
baseURL = "https://kf6.ikit.org/"
loginURL = "auth/local/"
userURL = "api/users/me"
communityListURL = "api/users/myRegistrations"
# communityURL = paste0("api/communities/", communityId)
# asAuthorURL = paste0("api/authors/", communityId, "/me")
# communityViewsURL = paste0("api/communities/", communityId, "/views")
# communityAuthorsURL = paste0("api/communities/", communityId, "/authors")
# communityViewsURL = paste0("api/communities/", communityId, "/views")
# communityGroupsURL = paste0("api/communities/", communityId, "/groups")
# communityObjectsURL = paste0("api/contributions/", communityId, "/search")
# communityRecordsURL = paste0("api/records/search/", communityId)

#' Generate a API call url by combining the base url and the path
#' 
#' @param API path
#' 
#' @return Full url
get_url <- function(path) {
  modify_url(baseURL, path = path)
}

#' Generic function for GET requests
#' 
#' @param url API URL
#' 
#' @param token API token
#' 
#' @return Object from the GET request
GET_request <- function(url, token=NULL) {
  resp = GET(url, ua, add_headers(authorization = paste0("Bearer ", token)))
  
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
}

POST_request <- function(url, token, body) {
  resp = POST(url,
              ua,
              accept_json(),
              body = body,
              add_headers(authorization = paste0("Bearer ", token)),
              encode = "json")
  
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
}

#' Login / authenticate
#' 
#' @param uname Username
#' 
#' @param pword Password
#' 
#' @return An API token to be used for API calls
login <- function(host, uname, pword) {
  baseURL <<- host
  
  url = get_url(loginURL)
  
  resp = POST(url, query = list(userName = uname, password=pword))
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  # token = jsonlite::fromJSON(content(resp, "text"))$token
  
  return(jsonlite::fromJSON(content(resp, "text"))$token)
}

#' List communities the user is a member of
#' 
#' @param token The API token
#' 
#' @return A 2-col data frame with community `id` and `title`
get_communities <- function(token) {
  url = get_url(communityListURL)
  parsed = GET_request(url, token)
  
  results = sapply(parsed, function(c) {
    c(id=c$communityId, title=c[["_community"]]$title)
  }) %>% t() %>% data.frame()
  
  structure(
    list(
      content = results,
      url = url
    ),
    class = "kf6_api"
  )
}

#' Select a community
#' 
#' @param id Community id
#' 
#' @return A list object containing info about the community
set_community <- function(id) {
  communityId = id
  # communityId = "5924524e58316b59392b3edd"
}

#' Get information about a community
#' 
#' @param token The API token
#' 
#' @return A list containing relevant info
get_community <- function(token) {
  url = get_url(paste0("api/communities/", communityId))
  parsed = GET_request(url, token)
  
  structure(
    list(
      content = parsed,
      url = url
    ),
    class = "kf6_api"
  )
}

#' Get info about the current authenticated user
#' 
#' @param token Authenticated token
#' 
#' @return A list object
get_myself <- function(token) {
  url = get_url("api/users/me")
  parsed = GET_request(url, token)
  
  structure(
    list(
      content = parsed,
      url = url
    ),
    class = "kf6_api"
  )
}

#' Get the author object of the current authenticated user
#' 
#' @param token Authenticated token
#' 
#' @return A list object
get_myself_as_author <- function(token) {
  url = get_url(paste0("api/authors/", communityId, "/me"))
  GET_request(url, token)
}

#' Get all views in the community
#' 
#' @param token Authenticated token
#' 
#' @return A list of views in the community
get_views <- function(token) {
  url = get_url(paste0("api/communities/", communityId, "/views"))
  GET_request(url, token)
}

#' Get all authors in the community
#' 
#' @param token Authenticated token
#' 
#' @return A list of authors in the community
get_authors <- function(token) {
  url = get_url(paste0("api/communities/", communityId, "/authors"))
  GET_request(url, token)
}

#' Get all user groups in the community
#' 
#' @param token Authenticated token
#' 
#' @return A list of user groups in the community
get_groups <- function(token) {
  url = get_url(paste0("api/communities/", communityId, "/groups"))
  GET_request(url, token)
}

#' Count objects in the community
#' 
#' @param token Authenticated token
#' 
#' @return Count of all objects in the community
get_objects_count <- function(token, body=NULL) {
  url = get_url(paste0("api/contributions/", communityId, "/search/count"))
  body = list("query" = list(
    "pagesize" = 10000,
    "searchMode" = "title"))
  
  resp = POST_request(url, token, body)
  
  structure(
    list(
      content = resp,
      url = url
    ),
    class = "kf6_api"
  )
}

#' Get all objects from the community
#' 
#' @param token Authenticated token
#' 
#' @return A list of all objects from the community
get_objects <- function(token, body=NULL) {
  url = get_url(paste0("api/contributions/", communityId, "/search"))
  body = list(
    "contentType" = "application/json",
    "dataType" = "json",
    "query" = list(
      "pagesize" = 1000,
      "searchMode" = "title"))
  # resp = POST(url,
  #             accept_json(),
  #             body = body,
  #             add_headers(authorization = paste0("Bearer ", token)),
  #             encode = "json")
  
  resp = POST_request(url, token, body)
  
  structure(
    list(
      content = resp,
      url = url
    ),
    class = "kf6_api"
  )
}

#' Get all records from the community
#' 
#' @param token Authenticated token
#' 
#' @return All records in this community
get_records <- function(token, body="") {
  url = get_url(paste0("api/records/search/", communityId))
  body = list("contentType" = "application/json",
              "dataType" = "json")
  
  resp = POST_request(url, token, body)
  
  structure(
    list(
      content = resp,
      url = url
    ),
    class = "kf6_api"
  )
}

get_links_from <- function(fromId, token) {
  # fromId = "5938343358316b59392ebc15"
  url = get_url(paste0("api/links/from/", fromId))
  GET_request(url, token)
}
##########################
### Library for KF API ###
##########################

### Load libraries
library(httr)
library(jsonlite)
library(dplyr)

### Load utility functions
# source("../lib/utils/utils.R")
# ua <- user_agent("http://github.com/meefen/")

baseURL = "https://kf6.ikit.org/"
token = NA

communityId = NA
uname = "bodong.chen@gmail.com"
pword = "000000"

### API URLs
loginURL = "auth/local/"
userURL = "api/users/me"
communityListURL = "api/users/myRegistrations"
communityURL = paste0("api/communities/", communityId)
asAuthorURL = paste0("api/authors/", communityId, "/me")
communityViewsURL = paste0("api/communities/", communityId, "/views")
communityAuthorsURL = paste0("api/communities/", communityId, "/authors")
communityViewsURL = paste0("api/communities/", communityId, "/views")
communityGroupsURL = paste0("api/communities/", communityId, "/groups")
communityObjectsURL = paste0("api/contributions/", communityId, "/search")
communityRecordsURL = paste0("api/records/search/", communityId)

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
GET_request <- function(url, token) {
  resp = GET(url, add_headers(authorization = paste0("Bearer ", token)))
  
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
}

POST_request <- function(url, query, token) {
  resp = POST(url, query = query, 
              add_headers(authorization = paste0("Bearer ", token)))
  
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
  
  sapply(parsed, function(c) {
    c(id=c$communityId, title=c[["_community"]]$title)
  }) %>% t() %>% data.frame()
}

#' Select a community
#' 
#' @param id Community id
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
  GET_request(url, token)
}

get_me <- function(token) {
  url = get_url("api/users/me")
  GET_request(url, token)
}

get_me_as_author <- function(token) {
  url = get_url(paste0("api/authors/", communityId, "/me"))
  GET_request(url, token)
}

get_views <- function(token) {
  url = get_url(paste0("api/communities/", communityId, "/views"))
  GET_request(url, token)
}

get_authors <- function(token) {
  url = get_url(paste0("api/communities/", communityId, "/authors"))
  GET_request(url, token)
}

get_groups <- function(token) {
  url = get_url(paste0("api/communities/", communityId, "/groups"))
  GET_request(url, token)
}

get_object_counts <- function(token, query=NULL) {
  query = list(pagesize = 10000, searchMode = "title")
  url = get_url(paste0("api/communities/", communityId, "/search/count"))
  POST_request(url, query, token)
}

get_objects <- function(token, query=NULL) {
  query = list(pagesize = "10000", 
               searchMode = "title",
               contentType = 'application/json',
               dataType = 'json')
  url = get_url(paste0("api/communities/", communityId, "/search"))
  
  resp = POST(url, query = query,
              add_headers(authorization = paste0("Bearer ", token)))
  POST_request(url, query, token)
}

get_links_from <- function(fromId, token) {
  # fromId = "5938343358316b59392ebc15"
  url = get_url(paste0("api/links/from/", fromId))
  GET_request(url, token)
}
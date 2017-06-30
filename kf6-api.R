##########################
### Library for KF API ###
##########################

### Load libraries
library(httr)
suppressMessages(library(jsonlite))
suppressMessages(library(dplyr))
### Load utility functions
# source("../lib/utils/utils.R")

ua <- user_agent("http://github.com/meefen/")

baseURL = "https://kf6.ikit.org/"
token = NA

communityId = NA
uname = "bodong.chen@gmail.com"
pword = "000000"

### API URLs
loginURL = "auth/local/"
userURL = "api/users/me"
communityListURL = "api/users/myRegistrations"
communityURL = "api/communities/"
asAuthorURL = paste0("api/authors/", communityId, "/me")
communityViewsURL = paste0("api/communities/", communityId, "/views")
communityAuthorsURL = paste0("api/communities/", communityId, "/authors")
communityViewsURL = paste0("api/communities/", communityId, "/views")
communityGroupsURL = paste0("api/communities/", communityId, "/groups")
communityObjectsURL = paste0("api/contributions/", communityId, "/search")
communityRecordsURL = paste0("api/records/search/", communityId)

### Generate a API call url by combining the base url and the path
get_url <- function(path) {
  modify_url(baseURL, path = path)
}

### Login / authenticate
### Returns the token
login <- function(path) {
  url = get_url(loginURL)
  
  resp = POST(url, query = list(userName = uname, password=pword))
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  return(jsonlite::fromJSON(content(resp, "text"))$token)
}

### List communities
get_communities <- function(token) {
  url = get_url(communityListURL)
  
  resp = GET(url, add_headers(authorization = paste0("Bearer ", token)))
  parsed = jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE, flatten=T)
  
  sapply(parsed, function(c) {
    c(id=c$communityId, title=c[["_community"]]$title)
  }) %>% t() %>% data.frame()
}


set_baseURL <- function(url) {
  baseURL <<- url;
}

set_community <- function(id) {
  communityId = id;
}





connect <- function(method, path, data) {
  url = get_url(path)
  
  req = list(
    url = url,
    type = method,
    headers = list(
      authorization = paste0('Bearer ', token)
    )
  )
  if(data) {
    req$contentType = 'application/json'
    req$dataType = 'json'
    req$data = JSON.stringify(data)
  }
  
  # req.cache = false;
  # $.ajax(req)
  # .done(success)
  # .fail(failure);
}

# CreateCurlHandle <- function() {
#   ### Create a curl handle that will be shared among API calls
#   
#   library(RCurl)
#   curl = getCurlHandle()
#   curlSetOpt(cookiejar = "", 
#              followlocation = TRUE, 
#              curl = curl) # do not need to read the cookies
#   return(curl)
# }
# 
# EnsureHost <- function(host) {
#   ### Ensure host ends with '/'
#   
#   if(grepl("\\/$", host))
#     host
#   else
#     paste0(host, "/")
# }

SelectCommunity <- function(host, sectionId, curl) {
  ### Tell the server that we are going to use data from this community
  ### Required for any futher data queries
  
  host = EnsureHost(host)
  tryCatch({  
    vURL = paste0(host, "rest/account/selectSection/", sectionId)
    fromJSON(getURL(vURL, curl=curl), flatten=TRUE)
  }, error = function(e) {
    return(e)
  })
}

GetSectionPosts <- function(host, sectionId, curl) {
  ### Get posts in a section
  
  host = EnsureHost(host)
  pURL = paste0(host, postsURL, sectionId)
  text = getURL(pURL, curl=curl)
  text = CleanJSONText(text)
  df = fromJSON(text, flatten = TRUE)
  #   df = rjson::fromJSON(text)
  df$body_text = StripHTMLTags(df$body)
  return(df)
}

GetSectionViews <- function(host, sectionId, curl) {
  ### Get views in a section
  
  host = EnsureHost(host)
  vURL = paste0(host, viewsURL, sectionId)
  views = fromJSON(getURL(vURL, curl=curl), flatten=TRUE)
  views$created = StrpKFTime(views$created) # convert time
  return(views)
}

GetView <- function(host, viewId, curl) {
  ### Get view info
  
  host = EnsureHost(host)
  vURL = paste0(host, viewURL, viewId)
  view = fromJSON(getURL(vURL, curl=curl), flatten=TRUE)
  view$viewPostRefs$postInfo.body_text = StripHTMLTags(view$viewPostRefs$postInfo.body)
  return(view)
}

GetLogs <- function(host, viewIds, curl) {
  ### Get post histories from views
  ### Note: viewIds is a vector
  
  host = EnsureHost(host)
  tryCatch({
    logs = lapply(viewIds, function(viewId) {
      vURL = paste0(host, "rest/mobile/getPostHistoriesForView/", viewId)
      df = tryCatch(
        fromJSON(getURL(vURL, curl=curl), flatten=TRUE),
        error = function(e) NULL
      )
      if(!is.null(df) && ncol(df) == 5) 
        df$userName <- NA # strangely some views returned df with 5 cols
      return(df)
    })
    do.call("rbind", logs)
  }, error = function(e) {
    print(e)
  })
}

GetAllAuthors <- function(host, sectionId, curl) {
  ### Get all authors in a section / community
  
  host = EnsureHost(host)
  tryCatch({  
    vURL = paste0(host, "rest/mobile/getAllAuthors/", sectionId)
    fromJSON(getURL(vURL, curl=curl), flatten=TRUE)
  }, error = function(e) {
    return(e)
  })
}

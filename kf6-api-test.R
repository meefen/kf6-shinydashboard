
resp <- kf6_api(loginURL)
http_type(resp)


GET(get_url(loginURL), authenticate(uname, pword), add_headers(authorization = token))

resp = POST(get_url(loginURL), query = list(userName = uname, password=pword), encode = "json")
http_type(resp)
token = jsonlite::fromJSON(content(resp, "text"))$token


comms = GET(get_url(communityListURL), 
            add_headers(authorization = paste0("Bearer ", token)))
results = jsonlite::fromJSON(content(comms, "text"), simplifyVector = FALSE)

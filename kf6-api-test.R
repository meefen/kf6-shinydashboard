
source("kf6-api.R")

baseURL = "https://kf6.ikit.org/"
token = NA
communityId = NA
uname = "bodong.chen@gmail.com"
pword = "xxx"

token = login(baseURL, uname, pword)
get_communities(token)

communityId = "5924524e58316b59392b3edd"
comm = get_community(token)

user = get_myself(token)
author = get_myself_as_author(token)

views = get_views(token)

comm_objs = get_objects(token)
authors = get_authors(token)

get_records(token)

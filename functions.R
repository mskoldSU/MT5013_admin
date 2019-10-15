library(tidyverse)
library(gh)
TOKEN <- "your github token here"

org_name <-  "MT5013-HT18" 
private <-  TRUE
auto_init <-  TRUE
.token <-  TOKEN

init_student <- function(user_name, org_name, team_id){
    # Add student to organisation
    add_user_org(user_name, org_name)
    # Add student to students team (for access to Class_files repo)
    # find team id by e.g. gh("GET /orgs/MT5013-HT18/teams", .token = TOKEN)
    add_user_team(user_name, team_id)
    # Create homework repo for student
    add_repo(paste0("HW_", user_name), org_name)
    # Give student access to homework repo
    add_user_repo(user_name, paste0("HW_", user_name), org_name)
}

add_repo <- function(repo_name, org_name, 
                     private = TRUE, 
                     auto_init = TRUE,
                     .token = TOKEN){
    # Adds a repo "repo_name" to organisation "org_name"
    API_call <- paste("POST /orgs", org_name, "repos", sep = "/")
    gh(API_call, name = as.character(repo_name), private = private, auto_init = auto_init, .token = TOKEN)
}

add_user_repo <- function(user_name, repo_name, org_name,
                          user_permission = "push", 
                          .token = TOKEN){
    # Adds permissisons for user "user_name" to repo "repo_name"
    API_call <- paste("PUT /repos", org_name, repo_name, "collaborators", user_name, sep ="/")
    gh(API_call, permission = user_permission, .token = TOKEN)
}

add_user_org <- function(user_name, org_name, user_role = "member", .token = TOKEN){
    # Adds user "user_name" to organization "org_name"
    API_call <- paste("PUT /orgs", org_name, "memberships", as.character(user_name), sep = "/")
    gh(API_call, role = user_role, .token = TOKEN)
}

add_user_team <- function(user_name, team_id, 
                          .token = TOKEN){
    # Adds user "user_named" to team with "team_id"
    API_call <- paste("PUT /teams", team_id, "members", user_name)
    gh(API_call, .token = TOKEN)
}

get_issues_org <- function(org_name, 
                           .token = TOKEN){
    # Gets all open issues in the organiusation "org_name"
    API_call <- paste("GET /orgs", org_name, "issues", sep = "/")
    issues <- gh(API_call, .token = TOKEN, 
                 filter = "all", per_page = 100) # Note maximum 100 issues per call
    tibble(
        repo = map(issues, "repository") %>% map_chr("name"),
        user = map(issues, "user") %>% map_chr("login"),
        title = map_chr(issues, "title"),
        created = as.Date(map_chr(issues, "created_at"))
    )
                 
}

add_peer_review <- function(org_name, lag = 1){
    # Gets repos with open issues, rotate users lag and give pull permissions to allow peer review
    issues <- get_issues_org(org_name) %>% 
        filter(str_sub(repo, 4) == user) %>% 
        filter(str_sub(repo, 1, 2) == "HW") %>% 
        mutate(reviewer = rep(user, 2)[(1 + lag):(n() + lag)])
    walk2(issues$reviewer, issues$repo, add_user_repo, 
          org_name = org_name,
          user_permission = "pull")
}

get_students <- function(org_name, teachers){
    # List student logins (all org members - teachers)
    gh("GET /orgs/MT5013-HT18/members", .token = TOKEN, per_page = 100) %>% 
        map_chr("login") %>% 
        setdiff(teachers)
}

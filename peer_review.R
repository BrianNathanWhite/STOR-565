library(tidyverse)
library(reshape2)
library(readxl)

# import data
class_roster <- read_excel("data/Class_Roster.xlsx")
final_project_teams <- read_excel("data/Final_Project_Teams.xlsx")
student_id <- read.csv("~/Desktop/projects/r/unc/STOR-565/data/gradebook_export-aa6c97c0-3331-452b-9568-ade4270c8d00.csv")

# remove excess columns
student_id <- student_id[, 1:3]


# create data sets for use in peer-review assignments
teams <- data.frame(member = 1:5, # used only to melt data
                   marshmello = as.character(final_project_teams[1, 2:6]),
                   Outliers = as.character(final_project_teams[2, 2:6]),
                   EcoStatistician = as.character(final_project_teams[3, 2:6]),
                   SauceValidation = as.character(final_project_teams[4, 2:6]),
                   TeamJELDS = as.character(final_project_teams[5, 2:6]),
                   ThePHackers = as.character(final_project_teams[6, 2:6]),
                   'NA' = as.character(final_project_teams[7, 2:6]),
                   teamname = as.character(final_project_teams[8, 2:6]),
                   GoodTeam = as.character(final_project_teams[9, 2:6]),
                   Undecidedfornow = as.character(final_project_teams[10, 2:6]),
                   ThePrincipalComponents = as.character(final_project_teams[11, 2:6]),
                   Nameless = as.character(final_project_teams[12, 2:6]),
                   TheMichaelJordansofMachineLearning = as.character(final_project_teams[13, 2:6]))

teams_long <- teams %>%
  melt(id.vars = c('member')) %>%
  rename(team = variable, name = value) %>%
  select(-member) %>%
  drop_na() # 61 students participating in total

teams <- teams %>%
  select(-member)

team_names <- labels(teams)[[2]]

# takes random seed as input and returns peer-review assignments for each student along with a table with the total number of peer-reviews each team received.
assign_to_teams <- function(x) {
  
  set.seed(x)
  
  peer <- list(length = nrow(teams_long))
  
  for(i in 1:nrow(teams_long)) {
    
    # do not assign student to peer review their own team
    excluded_team <- which(team_names == teams_long[i,1])
    team_names <- team_names[-excluded_team]
    
    peer[[i]] <- sample(team_names, 3)
    
    # re-set team_names for next iteration
    team_names <- labels(teams)[[2]]
  }
  
  
  # replace student row id with student name
  names(peer) <- teams_long$name
  
  # coerce to tibble
  peer <- as.tibble(peer)
  
  # output results
  results <- list(peer, table(unlist(peer)))
  
  return(results)
  
}

# initialize while loop
range <- 20
x <- 1
n <- 1

# search for seed that makes total number of peer reviews per team roughly equal
while(range > 5 & n < 9999) {
  x <- x + 1
  n <- n + 1
  
  j <- assign_to_teams(x) # bi-weekly report 1 peer review assignments
  k <- assign_to_teams(10) # bi-weekly report 2 peer review assignments
  l <- assign_to_teams(20) # bi-weekly report 3 peer review assignments
  
 sum <- j[[2]] + k[[2]] + l[[2]] # total number of peer reviews each team receives over the three reports
 
 upper <- range(sum)[2]
 lower <- range(sum)[1]
 range <- upper - lower
  
}

# x = 2853 is found
sum

# peer assignments  for bi-weeekly reports as data frames
bi_weekly_assignments1 <- as.data.frame(j[[1]])
bi_weekly_assignments2 <- as.data.frame(k[[1]])
bi_weekly_assignments3 <- as.data.frame(l[[1]])

# initialize while loop
range <- 20
x <- 1
n <- 1
  
while(range > 3 & n < 9999) {
  x <- x + 1
  n <- n + 1
  
  m <- assign_to_teams(x) # project proposal peer review assignments
  
  upper <- range(m[[2]])[2]
  lower <- range(m[[2]])[1]
  range <- upper - lower
  
}

# x = 5499

# peer assignments for project proposal as data frame
project_proposal_assignments <- as.data.frame(m[[1]])


bi_weekly_assignments1 %>%
  select('Winnie Ren')

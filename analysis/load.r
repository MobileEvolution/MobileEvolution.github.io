setwd("/Users/gianlucascoccia/Desktop/svn2/gianluca/SANER2019/analysis")
#setwd(".")
library(plyr) 

mperm_apps = read.csv("data/mperm_analyzed.csv", stringsAsFactors = TRUE)
plint_apps = read.csv("data/plint_analyzed.csv", stringsAsFactors = TRUE)
commits = read.csv("data/commits.csv")
issues = read.csv("data/issues.csv")

all_apps = unique(c(as.vector(unique(mperm_apps$app_name)), as.vector(unique(plint_apps$app_name))))

# Commits per app
require(data.table)
summary(as.vector(table(commits$APP)))
sd(as.vector(table(commits$APP)))
IQR(as.vector(table(commits$APP)))
sum(as.vector(table(commits$APP)))

# Avg commits for analyzed app
mean(as.vector(table(droplevels(commits$APP)))) 

# Total number of unique committers 
length(unique(commits$AUTHOR_EMAIL))

# Mean unique committers for analyzed app
require(data.table)
ans = as.data.table(commits)[, commiterCount := uniqueN(AUTHOR_EMAIL), by = APP]
ans = ans[!duplicated(ans$APP),] 
summary(ans$commiterCount)
sd(ans$commiterCount)
IQR(ans$commiterCount)

# Load gplay data
gplay_data = read.csv2('data/google_play_data.csv', stringsAsFactors = TRUE, dec = '.')
gplay_data$description = NULL
gplay_data = subset(gplay_data, gplay_data$appId %in% all_apps)

# Remove issues of unmantained apps
issues = subset(issues, issues$AppName %in% all_apps)
affected_apps = unique(issues$AppName)
commits = subset(commits, commits$APP %in% affected_apps)


mperm_apps$is_dangerous = ifelse(mperm_apps$permission_name == "android.permission.READ_CALENDAR" | 
                                 mperm_apps$permission_name == "android.permission.WRITE_CALENDAR" |
                                 mperm_apps$permission_name == "android.permission.READ_CALL_LOG" |
                                 mperm_apps$permission_name == "android.permission.WRITE_CALL_LOG" |
                                 mperm_apps$permission_name == "android.permission.PROCESS_OUTGOING_CALLS" |
                                 mperm_apps$permission_name == "android.permission.CAMERA" |
                                 mperm_apps$permission_name == "android.permission.READ_CONTACTS" |
                                 mperm_apps$permission_name == "android.permission.WRITE_CONTACTS" |
                                 mperm_apps$permission_name == "android.permission.GET_ACCOUNTS" |
                                 mperm_apps$permission_name == "android.permission.ACCESS_FINE_LOCATION" |
                                 mperm_apps$permission_name == "android.permission.ACCESS_COARSE_LOCATION" |
                                 mperm_apps$permission_name == "android.permission.RECORD_AUDIO" |
                                 mperm_apps$permission_name == "android.permission.READ_PHONE_STATE" |
                                 mperm_apps$permission_name == "android.permission.READ_PHONE_NUMBERS" |
                                 mperm_apps$permission_name == "android.permission.CALL_PHONE" |
                                 mperm_apps$permission_name == "android.permission.ANSWER_PHONE_CALLS" |
                                 mperm_apps$permission_name == "android.permission.ADD_VOICEMAIL" |
                                 mperm_apps$permission_name == "android.permission.USE_SIP" |
                                 mperm_apps$permission_name == "android.permission.BODY_SENSORS" |
                                 mperm_apps$permission_name == "android.permission.SEND_SMS" |
                                 mperm_apps$permission_name == "android.permission.RECEIVE_SMS" |
                                 mperm_apps$permission_name == "android.permission.READ_SMS" |
                                 mperm_apps$permission_name == "android.permission.RECEIVE_WAP_PUSH" |
                                 mperm_apps$permission_name == "android.permission.RECEIVE_MMS" |
                                 mperm_apps$permission_name == "android.permission.RECEIVE_WAP_PUSH" |
                                 mperm_apps$permission_name == "android.permission.READ_EXTERNAL_STORAGE" |
                                 mperm_apps$permission_name == "android.permission.WRITE_EXTERNAL_STORAGE" 
                                 , TRUE, FALSE)

issues$WasFixed <- as.factor(revalue(issues$WasFixed, c("True" = TRUE, "False" = FALSE)))
issues$Type = regmatches(issues$IssueId, regexpr("[A-Za-z]+", issues$IssueId))

# Remove non-informative rows 
mperm_apps = subset(mperm_apps, mperm_apps$is_over == TRUE |
                          mperm_apps$is_under == TRUE | 
                          mperm_apps$RQ4 == "Removed" | 
                          mperm_apps$RQ1 == "Removed")

plint_apps = subset(plint_apps, plint_apps$is_missing == TRUE | 
                          plint_apps$is_multiple == TRUE | 
                          plint_apps$UC5 == "Removed" | 
                          plint_apps$UC7 == "Removed")

commits = commits[commits$COMMIT_SHA %in% mperm_apps$commit_id |
                    commits$COMMIT_SHA %in% plint_apps$commit_guid,]

# Count of issues
length(unique(issues[issues$Type == "O",]$AppName))
length(unique(issues[issues$Type == "U",]$AppName))
length(unique(issues[issues$Type == "Mi",]$AppName))
length(unique(issues[issues$Type == "Mu",]$AppName))

# Apps with dangerous commits
dang = subset(mperm_apps, mperm_apps$is_dangerous == TRUE)
length(unique(dang$app_name))

# Avg. Over Permissions Per Analyzed App
over_iss = subset(issues, issues$Type == "O")
nrow(over_iss)/length(unique(over_iss$AppName))

# Avg. Dangerous Permissions Per App 
require(data.table)
dang = subset(dang, !duplicated(dang[c("app_name", "permission_name")])) 
dang = as.data.table(dang)[, dangCount := uniqueN(permission_name), by = app_name]
dang = subset(dang, !duplicated(dang$app_name)) 
mean(dang$dangCount)

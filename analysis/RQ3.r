source("load.r")
library(ggplot2)
library(extrafont)
require(reshape2)
library(dplyr)
library(conover.test) 
library(nortest)
library(dunn.test)
library(effsize)
library(PMCMRplus)
library(questionr)
library(splitstackshape)

loadfonts()
options(max.print=50)
fontSize = 10

cv <- function(data, percentage){
  sd <- sd(data, na.rm=TRUE)
  mean <- mean(data, na.rm=TRUE)
  result <- sd/mean
  if(percentage) {
    result <- result * 100
  }
  return(result)
}

n_analyzed_commits <- 0

set_n_commits <- function(app, timestamp, author) {
  app_specific_commits <- commits[which(commits$APP == app),]
  total_commits = nrow(app_specific_commits)
  timestamp <- as.numeric(timestamp)
  n_commits <- nrow(app_specific_commits[which((app_specific_commits$author_unified == author) & (app_specific_commits$timestamp <= timestamp)),])
  n_analyzed_commits <<- n_analyzed_commits + 1
  print(n_analyzed_commits)
  return(n_commits)
}

set_n_days <- function(app, timestamp, author) {
  app_specific_commits <- commits[which(commits$APP == app),]
  author_specific_commits = app_specific_commits[which(app_specific_commits$author_unified == author),]
  first_commit_timestamp <- head(author_specific_commits[order(timestamp),], n = 1)$timestamp
  timestamp <- as.numeric(timestamp)
  diff <- floor((timestamp - first_commit_timestamp) / 3600 / 24)
  n_analyzed_commits <<- n_analyzed_commits + 1
  print(n_analyzed_commits)
  return(diff)
}

strip_author_email <- function(author_email) {
  return(unlist(strsplit(toString(author_email), "@"))[1])
}

## SKIP

# commits$author_unified <- as.character(lapply(commits$AUTHOR_EMAIL, function(x) strip_author_email(x)))
# 
# n_commits <- apply(commits[,c('APP','timestamp', 'author_unified')], 1, function(x) set_n_commits(x[1],x[2], x[3]))
# commits <- cbind(commits, n_commits)
# n_analyzed_commits <- 0
# 
# n_days <- apply(commits[,c('APP','timestamp', 'author_unified')], 1, function(x) set_n_days(x[1],x[2], x[3]))
# commits <- cbind(commits, n_days)
# n_analyzed_commits <- 0
# 
# commits$n_days <- commits$n_days + 1
# 
# write.csv(commits, file = "commitsAllApps.csv")

## END SKIP

commits = read.csv("commits.csv")

commits_nona <- commits[which(!is.na(commits$n_days)),]

fixed_issues <- issues[which(issues$WasFixed == "TRUE"),]

open_issues <- issues[which(issues$WasFixed == "FALSE"),]

stripped_commits <- subset(commits_nona, select = c(APP, COMMIT_SHA, MESSAGE, timestamp, n_commits, n_days))
joined_start <- inner_join(issues, stripped_commits, by=c(AppName = "APP", StartCommit = "COMMIT_SHA", StartTime = "timestamp"))
colnames(joined_start)[which(names(joined_start) == "n_commits")] <- "start_n_commits"
colnames(joined_start)[which(names(joined_start) == "n_days")] <- "start_n_days"

joined <- inner_join(joined_start, stripped_commits, by=c(AppName = "APP", EndCommit = "COMMIT_SHA", EndTime = "timestamp"))
colnames(joined)[which(names(joined) == "n_commits")] <- "end_n_commits"
colnames(joined)[which(names(joined) == "n_days")] <- "end_n_days"

joined$Type = regmatches(joined$IssueId, regexpr("[A-Za-z]+", joined$IssueId))
joined$Type = gsub('Mi', 'MRP', joined$Type)
joined$Type = gsub('Mu', 'MC', joined$Type)
joined$Type = factor(joined$Type, c('O', 'U', 'MC', 'MRP'))

f1 <- function(x) {
  log10(mean(10 ^ x))
}

joined <- joined[which(joined$WasFixed == "TRUE"),]
joined.m <- melt(joined, id.vars=c('IssueId', 'Type'), measure.vars=c('start_n_days','end_n_days'))

ggplot(data=joined.m, aes(x=Type, y=value,color=variable)) +
  scale_y_continuous(trans = "log10", breaks=c(1,2,5,10,25,50,100,200,500,1000,2000, 4000),
                     limits = c(1,4000)) +
  geom_boxplot(position=position_dodge(0.9)) +
  # stat_summary(fun.y=f1, geom="point", shape=5, size=2) +
  theme_linedraw() +
  theme(legend.position="bottom") +
  xlab("Type of permission-related issue") + ylab("Developer experience (days)") +
  guides(color=guide_legend(title="")) +
  scale_color_discrete(name="",
                      breaks=c("start_n_days", "end_n_days"),
                      labels=c("PRI introduction", "PRI fix")) +
  theme(axis.text=element_text(size=fontSize), axis.title=element_text(size=fontSize))

ggsave("./plots/RQ3_devExperience.pdf", scale = 0.6, height = 14, unit = "cm")


joined <- inner_join(joined_start, stripped_commits, by=c(AppName = "APP", EndCommit = "COMMIT_SHA", EndTime = "timestamp"))
colnames(joined)[which(names(joined) == "n_commits")] <- "end_n_commits"
colnames(joined)[which(names(joined) == "n_days")] <- "end_n_days"

joined$Type = regmatches(joined$IssueId, regexpr("[A-Za-z]+", joined$IssueId))
joined$Type = gsub('Mi', 'MRP', joined$Type)
joined$Type = gsub('Mu', 'MC', joined$Type)
joined$Type = factor(joined$Type, c('O', 'U', 'MC', 'MRP'))

f1 <- function(x) {
  log10(mean(10 ^ x))
}


# RQ3

get_dev_type <- function(n_commits) {
  result = "Regular"
  if(n_commits <= 3) {
    result = "Newcomer"    
  }
  return(result)
}

joined$start_dev_type <- as.factor(unlist(lapply(joined2$start_n_commits, function(x) get_dev_type(x))))
joined$end_dev_type <- as.factor(unlist(lapply(joined2$end_n_commits, function(x) get_dev_type(x))))

ggplot(data=joined, aes(x=Type, fill=start_dev_type)) +
  geom_bar(aes(y = (..count..)), position=position_dodge(1)) +
  geom_text(aes( label = ..count..,
                 y= ..count.. ), stat= "count", vjust = -.5, position = position_dodge(1), size=3) +
  theme_bw() +
  theme(legend.position=c(0.9, 0.8), legend.title = element_blank(), legend.box.background = element_rect(colour = "black")) +
  xlab("Type of introduced PRIs") + ylab("Introduced PRIs") +
  guides(color=guide_legend(title="")) +
  scale_color_discrete(name="",
                       breaks=c("Newcomer", "Regular"),
                       labels=c("Newcomer", "Regular")) +
  ylim(0, 2250) +
  theme(axis.text=element_text(size=fontSize), axis.title=element_text(size=fontSize))

ggsave("./plots/RQ3_newcomers.pdf", scale = 0.6, height = 10, unit = "cm")

d_start_table <- table(joined$start_dev_type, joined$Type)
d_start_table
prop.table(d_start_table)
summary(d_start_table)

chisquare <- chisq.test(d_start_table, correct = T, simulate.p.value = F)
cramer <- cramer.v(d_start_table)

chisquare
cramer

# Here we start working on the percentage of commits among newcomers and regular contributors

commits$author_with_app <- paste(commits$author_unified, commits$APP, sep = "___")
commits_per_contributor <- dplyr::count(commits, author_with_app, name="total_commits")

stripped_joined <- dplyr::select(joined, start_dev_type, StartCommit, Type)
stripped_joined$author_with_app <- inner_join(joined, commits, by=c(StartCommit = "COMMIT_SHA"))$author_with_app 

# START NEW PART

expanded_commits_per_contributor <- expandRows(commits_per_contributor, 8, count.is.col = FALSE)
expanded_commits_per_contributor$issue_type <- rep(c("O", "U", "MC", "MRP"), length.out = nrow(expanded_commits_per_contributor))
expanded_commits_per_contributor$start_dev_type <- rep(c("Newcomer", "Newcomer", "Newcomer", "Newcomer", "Regular", "Regular", "Regular", "Regular"), length.out = nrow(expanded_commits_per_contributor))

stripped_joined_counts <- summarise(group_by(stripped_joined, author_with_app, Type), num_issues =n())
stripped_joined <- distinct(inner_join(stripped_joined, stripped_joined_counts, by=c(author_with_app = "author_with_app", Type = "Type")), start_dev_type, Type, author_with_app, num_issues)

stripped_joined <- left_join(expanded_commits_per_contributor, stripped_joined, by=c(author_with_app = "author_with_app", "issue_type" = "Type", "start_dev_type" = "start_dev_type"))
stripped_joined$num_issues[is.na(stripped_joined$num_issues)] <- 0

# stripped_joined <- inner_join(stripped_joined, commits_per_contributor, by=c(author_with_app = "author_with_app"))

# here we count only once each pair (commit, issue type) because here we are focussing on issue-introducing commits.
# stripped_joined <- stripped_joined %>% distinct(StartCommit, Type, .keep_all = TRUE)

# stripped_joined <- dplyr::rename(dplyr::count(stripped_joined, start_dev_type, Type, author_with_app), num_issues = n)
# stripped_joined <- inner_join(stripped_joined, commits_per_contributor, by=c(author_with_app = "author_with_app"))
stripped_joined$issues_per_commit <- stripped_joined$num_issues/stripped_joined$total_commits
stripped_joined <- dplyr::rename(stripped_joined, Type = issue_type)

ggplot(data=stripped_joined, aes(x=Type, y=issues_per_commit, fill=start_dev_type)) +
  # scale_y_continuous(trans = "log10", breaks=c(1,2,5,10,25,50,100,200,500,1000,2000, 4000), limits = c(1,4000)) +
  geom_boxplot(position=position_dodge(0.9), outlier.size = 0.5) +
  # stat_summary(fun.y=f1, geom="point", shape=5, size=2) +
  theme_bw() +
  theme(legend.position=c(0.9, 0.8), legend.title = element_blank(), legend.box.background = element_rect(colour = "black")) +
  xlab("Type of introduced PRIs") + ylab("PRIs introduced per commit") +
  guides(color=guide_legend(title="")) +
  # scale_color_discrete(name="",
  #                      breaks=c("start_n_days", "end_n_days"),
  #                      labels=c("PRI introduction", "PRI fix")) +
  theme(axis.text=element_text(size=fontSize), axis.title=element_text(size=fontSize))

ggsave("./plots/RQ3_issues_per_commit.pdf", scale = 0.6, height = 10, unit = "cm")

round(summary(stripped_joined[which(stripped_joined$start_dev_type == "Newcomer" & stripped_joined$Type == "O"),]$issues_per_commit), digits=3)
round(summary(stripped_joined[which(stripped_joined$start_dev_type == "Newcomer" & stripped_joined$Type == "U"),]$issues_per_commit), digits=3)
round(summary(stripped_joined[which(stripped_joined$start_dev_type == "Newcomer" & stripped_joined$Type == "MC"),]$issues_per_commit), digits=3)
round(summary(stripped_joined[which(stripped_joined$start_dev_type == "Newcomer" & stripped_joined$Type == "MRP"),]$issues_per_commit), digits=3)
round(summary(stripped_joined[which(stripped_joined$start_dev_type == "Regular" & stripped_joined$Type == "O"),]$issues_per_commit), digits=3)
round(summary(stripped_joined[which(stripped_joined$start_dev_type == "Regular" & stripped_joined$Type == "U"),]$issues_per_commit), digits=3)
round(summary(stripped_joined[which(stripped_joined$start_dev_type == "Regular" & stripped_joined$Type == "MC"),]$issues_per_commit), digits=3)
round(summary(stripped_joined[which(stripped_joined$start_dev_type == "Regular" & stripped_joined$Type == "MRP"),]$issues_per_commit), digits=3)

round(sd(stripped_joined[which(stripped_joined$start_dev_type == "Newcomer" & stripped_joined$Type == "O"),]$issues_per_commit), digits=3)
round(sd(stripped_joined[which(stripped_joined$start_dev_type == "Newcomer" & stripped_joined$Type == "U"),]$issues_per_commit), digits=3)
round(sd(stripped_joined[which(stripped_joined$start_dev_type == "Newcomer" & stripped_joined$Type == "MC"),]$issues_per_commit), digits=3)
round(sd(stripped_joined[which(stripped_joined$start_dev_type == "Newcomer" & stripped_joined$Type == "MRP"),]$issues_per_commit), digits=3)
round(sd(stripped_joined[which(stripped_joined$start_dev_type == "Regular" & stripped_joined$Type == "O"),]$issues_per_commit), digits=3)
round(sd(stripped_joined[which(stripped_joined$start_dev_type == "Regular" & stripped_joined$Type == "U"),]$issues_per_commit), digits=3)
round(sd(stripped_joined[which(stripped_joined$start_dev_type == "Regular" & stripped_joined$Type == "MC"),]$issues_per_commit), digits=3)
round(sd(stripped_joined[which(stripped_joined$start_dev_type == "Regular" & stripped_joined$Type == "MRP"),]$issues_per_commit), digits=3)

round(IQR(stripped_joined[which(stripped_joined$start_dev_type == "Newcomer" & stripped_joined$Type == "O"),]$issues_per_commit), digits=3)
round(IQR(stripped_joined[which(stripped_joined$start_dev_type == "Newcomer" & stripped_joined$Type == "U"),]$issues_per_commit), digits=3)
round(IQR(stripped_joined[which(stripped_joined$start_dev_type == "Newcomer" & stripped_joined$Type == "MC"),]$issues_per_commit), digits=3)
round(IQR(stripped_joined[which(stripped_joined$start_dev_type == "Newcomer" & stripped_joined$Type == "MRP"),]$issues_per_commit), digits=3)
round(IQR(stripped_joined[which(stripped_joined$start_dev_type == "Regular" & stripped_joined$Type == "O"),]$issues_per_commit), digits=3)
round(IQR(stripped_joined[which(stripped_joined$start_dev_type == "Regular" & stripped_joined$Type == "U"),]$issues_per_commit), digits=3)
round(IQR(stripped_joined[which(stripped_joined$start_dev_type == "Regular" & stripped_joined$Type == "MC"),]$issues_per_commit), digits=3)
round(IQR(stripped_joined[which(stripped_joined$start_dev_type == "Regular" & stripped_joined$Type == "MRP"),]$issues_per_commit), digits=3)

round(summary(stripped_joined[which(stripped_joined$start_dev_type == "Newcomer"),]$issues_per_commit), digits = 3)
round(summary(stripped_joined[which(stripped_joined$start_dev_type == "Regular"),]$issues_per_commit), digits = 3)

round(sd(stripped_joined[which(stripped_joined$start_dev_type == "Newcomer"),]$issues_per_commit), digits = 3)
round(sd(stripped_joined[which(stripped_joined$start_dev_type == "Regular"),]$issues_per_commit), digits = 3)

round(IQR(stripped_joined[which(stripped_joined$start_dev_type == "Newcomer"),]$issues_per_commit, na.rm = TRUE), digits = 3)
round(IQR(stripped_joined[which(stripped_joined$start_dev_type == "Regular"),]$issues_per_commit, na.rm = TRUE), digits = 3)

round(summary(stripped_joined$issues_per_commit), digits = 3)
round(sd(stripped_joined$issues_per_commit), digits = 3)
round(IQR(stripped_joined$issues_per_commit, na.rm = TRUE), digits = 3)

# Mann Whitney U test
wilcox.test(stripped_joined[which(stripped_joined$start_dev_type == "Newcomer"),]$issues_per_commit, stripped_joined[which(stripped_joined$start_dev_type == "Regular"),]$issues_per_commit, p=0.001, paired = FALSE)
cliff.delta(stripped_joined[which(stripped_joined$start_dev_type == "Newcomer"),]$issues_per_commit, stripped_joined[which(stripped_joined$start_dev_type == "Regular"),]$issues_per_commit,return.dm=TRUE)

# RQ4

joined_fixed <- joined[which(joined$WasFixed == "TRUE"),]

ggplot(data=joined_fixed, aes(x=Type, fill=end_dev_type)) +
  geom_bar(aes(y = (..count..)), position=position_dodge(1)) +
  geom_text(aes( label = ..count.., y= ..count.. ), stat= "count", vjust = -.5, position = position_dodge(1), size=3) +
  theme_bw() +
  theme(legend.position=c(0.9, 0.8), legend.title = element_blank(), legend.box.background = element_rect(colour = "black")) +
  xlab("Type of fixed PRIs") + ylab("Fixed PRIs") +
  guides(color=guide_legend(title="")) +
  scale_color_discrete(name="",
                       breaks=c("Newcomer", "Regular"),
                       labels=c("Newcomer", "Regular")) +
  ylim(0, 2250) +
  theme(axis.text=element_text(size=fontSize), axis.title=element_text(size=fontSize))

ggsave("./plots/RQ4_newcomers.pdf", scale = 0.6, height = 10, unit = "cm")

d_end_table_fixed <- table(joined_fixed$end_dev_type, joined_fixed$Type)
d_end_table_fixed
prop.table(d_end_table_fixed)

chisquare2 <- chisq.test(d_end_table_fixed, correct = T, simulate.p.value = F)
cramer2 <- cramer.v(d_end_table_fixed)

chisquare2
cramer2

# Here we start working on the percentage of commits among newcomers and regular contributors

stripped_joined2 <- dplyr::select(joined_fixed, end_dev_type, EndCommit, Type)
stripped_joined2$author_with_app <- inner_join(joined_fixed, commits, by=c(EndCommit = "COMMIT_SHA"))$author_with_app 

# START NEW PART

expanded_commits_per_contributor <- expandRows(commits_per_contributor, 8, count.is.col = FALSE)
expanded_commits_per_contributor$issue_type <- rep(c("O", "U", "MC", "MRP"), length.out = nrow(expanded_commits_per_contributor))
expanded_commits_per_contributor$end_dev_type <- rep(c("Newcomer", "Newcomer", "Newcomer", "Newcomer", "Regular", "Regular", "Regular", "Regular"), length.out = nrow(expanded_commits_per_contributor))

stripped_joined_counts2 <- summarise(group_by(stripped_joined2, author_with_app, Type), num_issues =n())
stripped_joined2 <- distinct(inner_join(stripped_joined2, stripped_joined_counts2, by=c(author_with_app = "author_with_app", Type = "Type")), end_dev_type, Type, author_with_app, num_issues)

stripped_joined2 <- left_join(expanded_commits_per_contributor, stripped_joined2, by=c(author_with_app = "author_with_app", "issue_type" = "Type", "end_dev_type" = "end_dev_type"))
stripped_joined2$num_issues[is.na(stripped_joined2$num_issues)] <- 0

# stripped_joined <- inner_join(stripped_joined, commits_per_contributor, by=c(author_with_app = "author_with_app"))

# here we count only once each pair (commit, issue type) because here we are focussing on issue-introducing commits.
# stripped_joined <- stripped_joined %>% distinct(StartCommit, Type, .keep_all = TRUE)

# stripped_joined <- dplyr::rename(dplyr::count(stripped_joined, start_dev_type, Type, author_with_app), num_issues = n)
# stripped_joined <- inner_join(stripped_joined, commits_per_contributor, by=c(author_with_app = "author_with_app"))
stripped_joined2$issues_per_commit <- stripped_joined2$num_issues/stripped_joined2$total_commits
stripped_joined2 <- dplyr::rename(stripped_joined2, Type = issue_type)


# stripped_joined2 <- dplyr::select(joined, end_dev_type, EndCommit, Type)
# stripped_joined2$author_with_app <- inner_join(joined, commits, by=c(EndCommit = "COMMIT_SHA"))$author_with_app  
# stripped_joined2 <- inner_join(stripped_joined2, commits_per_contributor, by=c(author_with_app = "author_with_app"))
# 
# # here we count only once each pair (commit, issue type) because here we are focussing on issue-introducing commits.
# # stripped_joined2 <- stripped_joined2 %>% distinct(EndCommit, Type, .keep_all = TRUE)
# 
# stripped_joined2 <- dplyr::rename(dplyr::count(stripped_joined2, end_dev_type, Type, author_with_app), num_issues = n)
# stripped_joined2 <- inner_join(stripped_joined2, commits_per_contributor, by=c(author_with_app = "author_with_app"))
# stripped_joined2$issues_per_commit <- stripped_joined2$num_issues/stripped_joined2$total_commits

ggplot(data=stripped_joined2, aes(x=Type, y=issues_per_commit, fill=end_dev_type)) +
  geom_boxplot(position=position_dodge(0.9), outlier.size = 0.5) +
  theme_bw() +
  theme(legend.position=c(0.9, 0.8), legend.title = element_blank(), legend.box.background = element_rect(colour = "black")) +
  xlab("Type of fixed PRIs") + ylab("PRIs fixed per commit") +
  guides(color=guide_legend(title="")) +
  theme(axis.text=element_text(size=fontSize), axis.title=element_text(size=fontSize))

ggsave("./plots/RQ4_issues_per_commit.pdf", scale = 0.6, height = 10, unit = "cm")

round(summary(stripped_joined2[which(stripped_joined2$end_dev_type == "Newcomer" & stripped_joined2$Type == "O"),]$issues_per_commit), digits=3)
round(summary(stripped_joined2[which(stripped_joined2$end_dev_type == "Newcomer" & stripped_joined2$Type == "U"),]$issues_per_commit), digits=3)
round(summary(stripped_joined2[which(stripped_joined2$end_dev_type == "Newcomer" & stripped_joined2$Type == "MC"),]$issues_per_commit), digits=3)
round(summary(stripped_joined2[which(stripped_joined2$end_dev_type == "Newcomer" & stripped_joined2$Type == "MRP"),]$issues_per_commit), digits=3)
round(summary(stripped_joined2[which(stripped_joined2$end_dev_type == "Regular" & stripped_joined2$Type == "O"),]$issues_per_commit), digits=3)
round(summary(stripped_joined2[which(stripped_joined2$end_dev_type == "Regular" & stripped_joined2$Type == "U"),]$issues_per_commit), digits=3)
round(summary(stripped_joined2[which(stripped_joined2$end_dev_type == "Regular" & stripped_joined2$Type == "MC"),]$issues_per_commit), digits=3)
round(summary(stripped_joined2[which(stripped_joined2$end_dev_type == "Regular" & stripped_joined2$Type == "MRP"),]$issues_per_commit), digits=3)

round(sd(stripped_joined2[which(stripped_joined2$end_dev_type == "Newcomer" & stripped_joined2$Type == "O"),]$issues_per_commit), digits=3)
round(sd(stripped_joined2[which(stripped_joined2$end_dev_type == "Newcomer" & stripped_joined2$Type == "U"),]$issues_per_commit), digits=3)
round(sd(stripped_joined2[which(stripped_joined2$end_dev_type == "Newcomer" & stripped_joined2$Type == "MC"),]$issues_per_commit), digits=3)
round(sd(stripped_joined2[which(stripped_joined2$end_dev_type == "Newcomer" & stripped_joined2$Type == "MRP"),]$issues_per_commit), digits=3)
round(sd(stripped_joined2[which(stripped_joined2$end_dev_type == "Regular" & stripped_joined2$Type == "O"),]$issues_per_commit), digits=3)
round(sd(stripped_joined2[which(stripped_joined2$end_dev_type == "Regular" & stripped_joined2$Type == "U"),]$issues_per_commit), digits=3)
round(sd(stripped_joined2[which(stripped_joined2$end_dev_type == "Regular" & stripped_joined2$Type == "MC"),]$issues_per_commit), digits=3)
round(sd(stripped_joined2[which(stripped_joined2$end_dev_type == "Regular" & stripped_joined2$Type == "MRP"),]$issues_per_commit), digits=3)

round(IQR(stripped_joined2[which(stripped_joined2$end_dev_type == "Newcomer" & stripped_joined2$Type == "O"),]$issues_per_commit), digits=3)
round(IQR(stripped_joined2[which(stripped_joined2$end_dev_type == "Newcomer" & stripped_joined2$Type == "U"),]$issues_per_commit), digits=3)
round(IQR(stripped_joined2[which(stripped_joined2$end_dev_type == "Newcomer" & stripped_joined2$Type == "MC"),]$issues_per_commit), digits=3)
round(IQR(stripped_joined2[which(stripped_joined2$end_dev_type == "Newcomer" & stripped_joined2$Type == "MRP"),]$issues_per_commit), digits=3)
round(IQR(stripped_joined2[which(stripped_joined2$end_dev_type == "Regular" & stripped_joined2$Type == "O"),]$issues_per_commit), digits=3)
round(IQR(stripped_joined2[which(stripped_joined2$end_dev_type == "Regular" & stripped_joined2$Type == "U"),]$issues_per_commit), digits=3)
round(IQR(stripped_joined2[which(stripped_joined2$end_dev_type == "Regular" & stripped_joined2$Type == "MC"),]$issues_per_commit), digits=3)
round(IQR(stripped_joined2[which(stripped_joined2$end_dev_type == "Regular" & stripped_joined2$Type == "MRP"),]$issues_per_commit), digits=3)

round(summary(stripped_joined2[which(stripped_joined2$end_dev_type == "Newcomer"),]$issues_per_commit), digits = 3)
round(summary(stripped_joined2[which(stripped_joined2$end_dev_type == "Regular"),]$issues_per_commit), digits = 3)

round(sd(stripped_joined2[which(stripped_joined2$end_dev_type == "Newcomer"),]$issues_per_commit), digits = 3)
round(sd(stripped_joined2[which(stripped_joined2$end_dev_type == "Regular"),]$issues_per_commit), digits = 3)

round(IQR(stripped_joined2[which(stripped_joined2$end_dev_type == "Newcomer"),]$issues_per_commit, na.rm = TRUE), digits = 3)
round(IQR(stripped_joined2[which(stripped_joined2$end_dev_type == "Regular"),]$issues_per_commit, na.rm = TRUE), digits = 3)

round(summary(stripped_joined2$issues_per_commit), digits = 3)
round(sd(stripped_joined2$issues_per_commit), digits = 3)
round(IQR(stripped_joined2$issues_per_commit, na.rm = TRUE), digits = 3)

# Mann Whitney U test
wilcox.test(stripped_joined2[which(stripped_joined2$end_dev_type == "Newcomer"),]$issues_per_commit, stripped_joined2[which(stripped_joined2$end_dev_type == "Regular"),]$issues_per_commit, p=0.001, paired = FALSE)
cliff.delta(stripped_joined2[which(stripped_joined2$end_dev_type == "Newcomer"),]$issues_per_commit, stripped_joined2[which(stripped_joined2$end_dev_type == "Regular"),]$issues_per_commit,return.dm=TRUE)

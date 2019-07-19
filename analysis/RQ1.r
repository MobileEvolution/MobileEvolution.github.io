setwd("/Users/gianlucascoccia/Desktop/svn2/gianluca/SANER2019/analysis")
source("load.r")
library(ggplot2)
library(extrafont)
require(reshape2)
library(conover.test)
library(fifer) 
library(dunn.test) 
library(PMCMRplus)
library(effsize)
loadfonts()
options(max.print=1000)
fontSize = 12

#################################### RQ1 ####################################################

# Issues by type
issues_o = subset(issues, issues$Type == 'O')
issues_u = subset(issues, issues$Type == 'U')
issues_mi = subset(issues, issues$Type == 'Mi')
issues_mu = subset(issues, issues$Type == 'Mu')

# Indipendence test
fisher.test(table(issues$AppName, issues$Type), simulate.p.value = TRUE)
chisq.post.hoc(table(issues$AppName, issues$Type), test = c("fisher.test"), popsInRows = FALSE,
               control = c("holm"), digits = 4, simulate.p.value = TRUE)

# Mean occurrences for PRI type
nrow(issues_o)/length(all_apps)
nrow(issues_u)/length(all_apps)
nrow(issues_mi)/length(all_apps)
nrow(issues_mu)/length(all_apps)
nrow(issues)/length(all_apps)

# Count of apps for each issue
length(unique(issues_o$AppName))
length(unique(issues_u$AppName))
length(unique(issues_mi$AppName))
length(unique(issues_mu$AppName))

# Total count of unique apps with issues
length(unique(c(as.vector(issues_o$AppName),
                as.vector(issues_u$AppName),
                as.vector(issues_mi$AppName),
                as.vector(issues_mu$AppName))))

# For each app, count issues of each kind 
require(data.table)
issues_o = as.data.table(issues_o)[, issuesCount := uniqueN(IssueId), by = AppName]
issues_u = as.data.table(issues_u)[, issuesCount := uniqueN(IssueId), by = AppName]
issues_mi = as.data.table(issues_mi)[, issuesCount := uniqueN(IssueId), by = AppName]
issues_mu = as.data.table(issues_mu)[, issuesCount := uniqueN(IssueId), by = AppName]
issues = as.data.table(issues)[, issuesCount := uniqueN(IssueId), by = AppName]

issues_o_count = issues_o[!duplicated(issues_o$AppName)]
issues_u_count = issues_u[!duplicated(issues_u$AppName)]
issues_mi_count = issues_mi[!duplicated(issues_mi$AppName)]
issues_mu_count = issues_mu[!duplicated(issues_mu$AppName)]

apps_issues_counts = data.frame('name' = all_apps)
apps_issues_counts = merge(apps_issues_counts, issues_o_count[, c('AppName', 'issuesCount')], by.x = 'name', by.y ='AppName', all.x = TRUE)
colnames(apps_issues_counts) = c('name', 'OCount')
apps_issues_counts = merge(apps_issues_counts, issues_u_count[, c('AppName', 'issuesCount')], by.x = 'name', by.y ='AppName', all.x = TRUE)
colnames(apps_issues_counts) = c('name', 'OCount', 'UCount')
apps_issues_counts = merge(apps_issues_counts, issues_mi_count[, c('AppName', 'issuesCount')], by.x = 'name', by.y ='AppName', all.x = TRUE)
colnames(apps_issues_counts) = c('name', 'OCount', 'UCount', 'MiCount')
apps_issues_counts = merge(apps_issues_counts, issues_mu_count[, c('AppName', 'issuesCount')], by.x = 'name', by.y ='AppName', all.x = TRUE)
colnames(apps_issues_counts) = c('name', 'OCount', 'UCount', 'MiCount', 'MuCount')
apps_issues_counts$OCount[is.na(apps_issues_counts$OCount)] <- 0
apps_issues_counts$UCount[is.na(apps_issues_counts$UCount)] <- 0
apps_issues_counts$MiCount[is.na(apps_issues_counts$MiCount)] <- 0
apps_issues_counts$MuCount[is.na(apps_issues_counts$MuCount)] <- 0
apps_issues_counts$allCount = apps_issues_counts$OCount + apps_issues_counts$UCount + 
  apps_issues_counts$MiCount + apps_issues_counts$MuCount
summary(apps_issues_counts$OCount)
summary(apps_issues_counts$UCount)
summary(apps_issues_counts$MiCount)
summary(apps_issues_counts$MuCount)
summary(apps_issues_counts$allCount)

sd(apps_issues_counts$OCount)
sd(apps_issues_counts$UCount)
sd(apps_issues_counts$MiCount)
sd(apps_issues_counts$MuCount)
sd(apps_issues_counts$allCount)

IQR(apps_issues_counts$OCount)
IQR(apps_issues_counts$UCount)
IQR(apps_issues_counts$MiCount)
IQR(apps_issues_counts$MuCount)
IQR(apps_issues_counts$allCount)

# Boxplot to show the distribution of each PRI 
# Group issues in a single data frame
counts = data.frame(unique(apps_issues_counts$name), apps_issues_counts$allCount, apps_issues_counts$OCount, 
                    apps_issues_counts$UCount, apps_issues_counts$MiCount, apps_issues_counts$MuCount)

# Set column names and replace values at 0 for log scale plot
colnames(counts) = c("App", "All types", "O", "U", "MC", "MRP") 
data = melt(counts)

# Boxplot of disrtibutions for each issue
ggplot(data=data, aes(x=variable, y=value)) +
  scale_y_continuous(trans = "log10",
                     breaks = c(1,2,5,10,25,50,100,200),
                     limits = c(0.99,250)) +
  geom_violin(fill='#A4A4A4', color='#A4A4A4') +
  geom_boxplot(width=0.1, outlier.size = 0.3, lwd=0.2) +
  theme_linedraw() +
  xlab("Type of permission-related issue") + ylab("Issue occurrences (log-scaled)") +
  theme(axis.text=element_text(size=fontSize), axis.title=element_text(size=fontSize))
  
ggsave("./plots/issuesDistribution.pdf", scale = 0.75, height = 12, unit = "cm")
embed_fonts("./plots/issuesDistribution.pdf", outfile="./plots/issuesDistribution.pdf")

shapiro.test(apps_issues_counts$OCount)
shapiro.test(apps_issues_counts$UCount)
shapiro.test(apps_issues_counts$MiCount)
shapiro.test(apps_issues_counts$MuCount)
shapiro.test(apps_issues_counts$allCount)

qqnorm(apps_issues_counts$allCount)

y <- matrix(c(counts$O, counts$U, counts$MC, counts$MRP),nrow=length(counts$MRP), ncol=4,
  dimnames=list(1:574, c('O', 'U', 'MC', 'MRP')))

friedman.test(y, simulate.p.value = TRUE)
frdAllPairsExactTest(y=y, p.adjust = "holm")

cliff.delta(y[,'O'],y[,'U'],return.dm=TRUE)
cliff.delta(y[,'O'],y[,'MC'],return.dm=TRUE)
cliff.delta(y[,'O'],y[,'MRP'],return.dm=TRUE)

cliff.delta(y[,'U'],y[,'MC'],return.dm=TRUE)
cliff.delta(y[,'U'],y[,'MRP'],return.dm=TRUE)

cliff.delta(y[,'MC'],y[,'MRP'],return.dm=TRUE)


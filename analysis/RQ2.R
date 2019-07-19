setwd("/Users/gianlucascoccia/Desktop/svn2/gianluca/SANER2019/analysis")
source("load.r")
library(ggplot2)
library(extrafont)
library(stringr)
library(PMCMRplus)
require(reshape2)
library(effsize)
loadfonts()
options(max.print=50)
fontSize = 12

#################################### RQ2 ####################################################

issues$DaysForFix = ceiling(issues$TimesDiff / (3600 * 24)) # 1 day

summary(issues$DaysForFix)

issues = subset(issues, issues$WasFixed == "TRUE")
issues = subset(issues, issues$DaysForFix > 0)

# Plot of all issues
issues_m = subset(issues, select=c("DaysForFix", "Type"))
issues_m$Type = gsub('Mi', 'MC', issues_m$Type)
issues_m$Type = gsub('Mu', 'MRP', issues_m$Type)
issues_m$Type = factor(issues_m$Type, c('O', 'U', 'MC', 'MRP'))

nrow(issues_m[issues_m$Type == 'MC',])
factor(issues_m$Type)
 

# Boxplot of distributions for each issue
 ggplot(data=issues_m, aes(x=Type, y=DaysForFix)) +
   scale_y_continuous(trans = "log10", breaks=c(1,2,5,10,25,50,100,200,500,1000,2000, 4000),
                      limits = c(1,4000)) +
   geom_violin(fill='#A4A4A4', color='#A4A4A4') +
   geom_boxplot(width=0.1, outlier.size = 0.3, lwd=0.2) +
   stat_summary(fun.y=f1, geom="point", shape=5, size=1, lwd=0.2) +
   theme_linedraw() +
   xlab("Type of permission-related issue") + ylab("Days of existance") +
   theme(axis.text=element_text(size=fontSize), axis.title=element_text(size=fontSize))

 f1 <- function(x) {
   log10(mean(10 ^ x))
 }

ggplot(issues_m, aes(Type, DaysForFix)) + 
  geom_violin(fill='#A4A4A4', color='#A4A4A4') +
  geom_boxplot(width=0.1, outlier.size = 0.3, lwd=0.2) +
  stat_summary(fun.y='mean', geom="point", shape=2, size=2) +
  coord_trans(y='log10', limy = c(1,4000)) +
  scale_y_continuous( breaks=c(1,2,7,14,28,56,84,168,365,730,1460, 3650), label=c("1 day","2 days","1 week", "2 weeks","4 weeks","8 weeks","12 weeks","24 weeks","1 year","2 years","4 years", "10 years")) +
  theme_linedraw() +
  xlab("Type of permission-related issue") + ylab("Days of existance (log-scaled)") +
  theme(axis.text=element_text(size=fontSize), axis.title=element_text(size=fontSize))

ggsave("./plots/issuesDaysDistribution.pdf", scale = 0.75, height = 12, unit = "cm")
embed_fonts("./plots/issuesDaysDistribution.pdf", outfile="./plots/issuesDaysDistribution.pdf")

# Counts of fixed issues
length(issues[issues$Type == 'O',]$DaysForFix)
length(issues[issues$Type == 'U',]$DaysForFix)
length(issues[issues$Type == 'Mi',]$DaysForFix)
length(issues[issues$Type == 'Mu',]$DaysForFix)
length(issues$DaysForFix)

# Counts of affected apps apps when considering only fixed issues
length(unique(issues[issues$Type == 'O',]$AppName))
length(unique(issues[issues$Type == 'U',]$AppName))
length(unique(issues[issues$Type == 'Mi',]$AppName))
length(unique(issues[issues$Type == 'Mu',]$AppName))
length(unique(issues$AppName))

# Stats for fixed issues
summary(issues[issues$Type == 'O',]$DaysForFix, na.rm = TRUE)
summary(issues[issues$Type == 'U',]$DaysForFix, na.rm = TRUE)
summary(issues[issues$Type == 'Mi',]$DaysForFix, na.rm = TRUE)
summary(issues[issues$Type == 'Mu',]$DaysForFix)
summary(issues$DaysForFix, na.rm = TRUE)

sd(issues[issues$Type == 'O',]$DaysForFix, na.rm = TRUE)
sd(issues[issues$Type == 'U',]$DaysForFix, na.rm = TRUE)
sd(issues[issues$Type == 'Mi',]$DaysForFix, na.rm = TRUE)
sd(issues[issues$Type == 'Mu',]$DaysForFix, na.rm = TRUE)
sd(issues$DaysForFix, na.rm = TRUE)

IQR(issues[issues$Type == 'O',]$DaysForFix, na.rm = TRUE)
IQR(issues[issues$Type == 'U',]$DaysForFix, na.rm = TRUE)
IQR(issues[issues$Type == 'Mi',]$DaysForFix, na.rm = TRUE)
IQR(issues[issues$Type == 'Mu',]$DaysForFix, na.rm = TRUE)
IQR(issues$DaysForFix, na.rm = TRUE)

qqnorm(issues$DaysForFix)

shapiro.test(issues[issues$Type == 'O',]$DaysForFix)
shapiro.test(issues[issues$Type == 'U',]$DaysForFix)
shapiro.test(issues[issues$Type == 'Mi',]$DaysForFix)
shapiro.test(issues[issues$Type == 'Mu',]$DaysForFix)
shapiro.test(issues$DaysForFix)

issues$Type <- as.factor(issues$Type)


t = table(
  factor(issues$DaysForFix, levels = min(issues$DaysForFix):max(issues$DaysForFix)), issues$Type)

friedman.test(t)
frdAllPairsConoverTest(y=t, p.adjust = "holm")

cliff.delta(t[,'O'],t[,'U'],return.dm=TRUE)
cliff.delta(t[,'O'],t[,'Mi'],return.dm=TRUE)
cliff.delta(t[,'O'],t[,'Mu'],return.dm=TRUE)

cliff.delta(t[,'U'],t[,'Mi'],return.dm=TRUE)
cliff.delta(t[,'U'],t[,'Mu'],return.dm=TRUE)

cliff.delta(t[,'Mi'],t[,'Mu'],return.dm=TRUE)


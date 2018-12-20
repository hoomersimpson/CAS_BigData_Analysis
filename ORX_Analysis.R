## Review and Analyze Banking Operational Risk Data
## supplied by ORX (ORX News)
## 
## https://managingrisktogether.orx.org/orx-news
## 
## The ORX News team searches financial media sources worldwide to identify
## operational risk loss events as they appear. They're a dedicated team of
## multilingual researchers working to provide global coverage of
## operational risk loss events.
## The service provides detailed and timely reporting of
## operational risk loss events in the financial services industry,
## including the banking, insurance and asset management sectors.
## ORX News also covers significant cyber events from all industry sectors.
## Our subscribers use the service to raise awareness of losses, inform their scenario
## programmes and challenge assumptions.
## The service is available to ORX members and non-members.
##
## by thomas.stump@bluewin.ch
## CAS Big Data Analysis
## HWZ - Zurich - Switzerland
## 2018-Q4

## PREPARATION
## ===========

# Cleaup all stored elements in rStudio

rm(list = ls())

# Load Libraries

library(readxl) # XLS File manipulation
library(magrittr) # Contains pipe %>%
library(readxl) # XLS read-write
library(ggplot2) # Graphics and Visualization
library(hexbin) # Needed by ggplot2

## READ EXTERNAL DATA (EXCEL)
## ==========================

# Read ORX data from XLS in data frame "ORX"

ORX_path <-"/Users/tstump/Dropbox/CAS/IAS/data/"
ORX_filename <- "ORX_News.xlsx"
ORX_file=paste(ORX_path,ORX_filename,sep="")
ORX <- read_excel(ORX_file)

## CLEANUP DATA
## ============

# Subset ORX data.frame to active loss numbers

ORX_loss_w_0 <- ORX[!is.na(as.numeric(as.character(ORX$`Loss Amount (USD)`))),] # only numericals
ORX_loss <- ORX_loss_w_0[which(ORX_loss_w_0$`Loss Amount (USD)`!=0),] # only non-0 values

# Establish data.frame with selected and meaningful columns

ORX_clean <- data.frame(
  ORX_loss$`Story Reference Number`,
  ORX_loss$Headline,
  round(as.numeric(ORX_loss$`Loss Amount (USD)`),0),
  ORX_loss$`ORX Standard`,
  ORX_loss$`Business Line Level 1 Name`,
  ORX_loss$`Product Level 1 Name`,
  ORX_loss$`Process Level 1 Name`,
  ORX_loss$`Event Type Level 1 Name`,
  ORX_loss$`Scenario Category Name`,
  ORX_loss$`Basel Event Type Level 1 Name`,
  ORX_loss$`Cause 1 Level 1 Name`,
  ORX_loss$`Region Name`)

ORX_clean.col_names = c("Ref_ID",
                        "Headline",
                        "Loss_USD",
                        "ORX_Std",
                        "Business",
                        "Product",
                        "Process",
                        "Event",
                        "Scenario",
                        "Basel_II",
                        "Cause",
                        "Region")

colnames(ORX_clean) <- ORX_clean.col_names

# Remove rows with NA values

ORX_NA_removed <- ORX_clean[complete.cases(ORX_clean),]

# Remove outliners, define a value 0.0 to 1.0 in the quantile (e.g. 0.9 = 90%)

loss_quantile <- quantile(ORX_NA_removed$Loss_USD,1,names=FALSE) # 100%, no data reduction
ORX_select <- subset(ORX_NA_removed, Loss_USD < loss_quantile)

## DISPLAY DATA
## =============

# Basics

str(ORX_select)
summary(ORX_select)
boxplot(ORX_select$Loss_USD)

# Diplay data in grid (Table View)

library(gridExtra)
library(grid)

ORX_head_10 <- head(ORX_clean[,1:3],10)
grid.table(ORX_head_10)

tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
grid.table(ORX_head_10, theme=tt)

tt1 <- ttheme_default()
tt2 <- ttheme_minimal()
tt3 <- ttheme_minimal(
  core=list(bg_params = list(fill = blues9[1:4], col=NA),
            fg_params=list(fontface=3)),
  colhead=list(fg_params=list(col="navyblue", fontface=4L)),
  rowhead=list(fg_params=list(col="orange", fontface=3L)))

grid.table(ORX_head_10, theme=tt1)
grid.table(ORX_head_10, theme=tt2)
grid.table(ORX_head_10, theme=tt3)

tt1 <- ttheme_default()
tt2 <- ttheme_default(core=list(fg_params=list(hjust=1, x=0.9)),
                      rowhead=list(fg_params=list(hjust=1, x=0.95)))
tt3 <- ttheme_default(core=list(fg_params=list(hjust=0, x=0.1)),
                      rowhead=list(fg_params=list(hjust=0, x=0)))

grid.table(ORX_head_10, theme=tt1)
grid.table(ORX_head_10, theme=tt2)
grid.table(ORX_head_10, theme=tt3)

grid.arrange(
  tableGrob(ORX_head_10, theme=tt1),
  nrow=1)

# Graphical representation of input variables vs. "Loss_USD"

# Tried various options
# g_Business_geom <- g_Business + geom_point()
# g_Business_geom <- g_Business + geom_bin2d()
# g_Business_geom <- g_Business + stat_bin_2d()
# g_Business_geom <- g_Business + geom_boxplot()
# g_Business_geom <- g_Business + geom_hex()
# g_Business_geom <- g_Business + geom_jitter() # best 
# g_Business_geom <- g_Business + geom_violin()

ggplot(ORX_select, aes(x=Loss_USD,y=Business)) + 
  geom_jitter(colour="darkblue") + 
  labs(title="Loss (USD) vs Business", 
    subtitle="Operational Risk Losses by Business Line", 
    x="Loss [USD]", y="Business", 
    caption="ORX News (https://managingrisktogether.orx.org/orx-news)")

ggplot(ORX_select, aes(x=Loss_USD,y=Product)) + 
  geom_jitter(colour="darkblue") + 
  labs(title="Loss (USD) vs Product", 
    subtitle="Operational Risk Losses by Product", 
    x="Loss [USD]", y="Product", 
    caption="ORX News (https://managingrisktogether.orx.org/orx-news)")

ggplot(ORX_select, aes(x=Loss_USD,y=Process)) +
  geom_jitter(colour="darkblue") + 
  labs(title="Loss (USD) vs Process", 
    subtitle="Operational Risk Losses by Process", 
    x="Loss [USD]", y="Process", 
    caption="ORX News (https://managingrisktogether.orx.org/orx-news)")

ggplot(ORX_select, aes(x=Loss_USD,y=Event)) +
  geom_jitter(colour="darkblue") + 
  labs(title="Loss (USD) vs Event", 
       subtitle="Operational Risk Losses by Event", 
       x="Loss [USD]", y="Event", 
       caption="ORX News (https://managingrisktogether.orx.org/orx-news)")

ggplot(ORX_select, aes(x=Loss_USD,y=Scenario)) +
  geom_jitter(colour="darkblue") + 
  labs(title="Loss (USD) vs Scenario", 
       subtitle="Operational Risk Losses by Scenario", 
       x="Loss [USD]", y="Scenario", 
       caption="ORX News (https://managingrisktogether.orx.org/orx-news)")

ggplot(ORX_select, aes(x=Loss_USD,y=Basel_II)) +
  geom_jitter(colour="darkblue") + 
  labs(title="Loss (USD) vs Basel_II", 
       subtitle="Operational Risk Losses by Basel_II", 
       x="Loss [USD]", y="Basel_II", 
       caption="ORX News (https://managingrisktogether.orx.org/orx-news)")

ggplot(ORX_select, aes(x=Loss_USD,y=Cause)) +
  geom_jitter(colour="darkblue") + 
  labs(title="Loss (USD) vs Cause", 
       subtitle="Operational Risk Losses by Cause", 
       x="Loss [USD]", y="Cause", 
       caption="ORX News (https://managingrisktogether.orx.org/orx-news)")

## LINEAR REGRESSION
## =================
##
## Review ORX_Select data for usage in a linear regression (lm) exercise

ORX_lm_Business <- lm(Loss_USD ~ Business, data=ORX_select)
summary(ORX_lm_Business)
# Rsq = 0.0084

ORX_lm_Process <- lm(Loss_USD ~ Process, data=ORX_select)
summary(ORX_lm_Process)
# Rsq = 0.019

ORX_lm_Event <- lm(Loss_USD ~ Event, data=ORX_select)
summary(ORX_lm_Event)
# Rsq = 0.0048

ORX_lm_Scenario <- lm(Loss_USD ~ Scenario, data=ORX_select)
summary(ORX_lm_Scenario)
# Rsq = 0.025

ORX_lm_Basel_II <- lm(Loss_USD ~ Basel_II, data=ORX_select)
summary(ORX_lm_Basel_II)
# Rsq = 0.005

ORX_lm_Cause <- lm(Loss_USD ~ Cause, data=ORX_select)
summary(ORX_lm_Cause)
# Rsq = 0.0023

ORX_lm_Region <- lm(Loss_USD ~ Region, data=ORX_select)
summary(ORX_lm_Region)
# Rsq = 0.00042

# For all predictors/response variants, the R-Saquare values are far from close to 1.
# NONE of the predictors (e.g. Business, Product, etc.) are having a direct corrlation with the response (Loss_USD)

# Best case is ..
# > ORX_lm_Scenario <- lm(Loss_USD ~ Scenario, data=ORX_select)
# .. with Rsq = 0.025

## TEXT MINING
## ===========
##
## Anylysis of unstructured data

library(tm)
library(stringi)
library(proxy)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(ggplot2)

# Get text data from ORX data frame (Headline and Digest Text)

summary(ORX$Headline)
t_headline <- ORX$`Headline`
t_headline[1:5]

# Join the elements of the character vectors into one string

t_headline_flat <- stri_flatten(t_headline, col=" ")
substr(t_headline_flat,1,500)

# Check outome in case we stem the text vector

t_headline_stem <- stemDocument(t_headline_flat)
substr(t_headline_stem,1,500)

# text gets massively distracted:
# serverance > server
# Nationwide > Nationwid
# Insurance > Insuranc
# Financial > Financi

# Do a stem-completion > does not work at all > out-of-memory and invalid expression error
# spent approx one hour in the internet for fixing this problem
# no soultion found .. so I stopped further investiation and do not StemCompetion

# These code lines are used to fix the out-of-memory issue with stemCompletion
# content_transformer <- function(x) iconv(x, to='UTF-8-MAC', sub='byte')
# docs <- tm_map(docs_corp, content_transformer)

# t_headline_complete <- stemCompletion(t_headline_stem,Corpus(VectorSource(t_headline_flat)))


# Cleanup text

docs <- Corpus(VectorSource(t_headline_flat))
docs2 <- tm_map(docs, function(x) stri_replace_all_regex(x, "<.+?>", " "))
docs3 <- tm_map(docs2, function(x) stri_replace_all_fixed(x, "\t", " "))
docs4 <- tm_map(docs3, PlainTextDocument)
docs5 <- tm_map(docs4, stripWhitespace)
docs6 <- tm_map(docs5, removePunctuation)
docs7 <- tm_map(docs6, tolower)
docs8 <- tm_map(docs7, removeWords, c(stopwords("english"),
                                      "banking","risk","operational", "finance",
                                      "event","million","usd","bank","eur",
                                      "billion", "banks","financial","banco","pays",
                                      "pay","data","inr","former"))
# Do stemming > docs9

docs9 <- tm_map(docs8, stemDocument)

# Error in stemComplete > out of memory > disabled this command
# docs10 <- tm_map(docs9, stemCompletion, t_headline_flat)

# Prepare and display word clound / bar-plot - Top 10 counted words

dtm1 <- TermDocumentMatrix(docs8)
m1 <- as.matrix(dtm1)
v1 <- sort(rowSums(m1),decreasing=TRUE)
d1 <- data.frame(word = names(v1),freq=v1)
head(d1, 10)

set.seed(1234)

wordcloud(words = d1$word, freq = d1$freq, min.freq = 5,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"), scale = c(5, 1))

barplot(d1[1:10,]$freq, las = 2, names.arg = d1[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

# Prepare and display word clound > with stemming, using docs9

dtm <- TermDocumentMatrix(docs9)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)

wordcloud(words = d$word, freq = d$freq, min.freq = 5,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"), scale = c(5, 1))

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


############################################
## Classification and Regression Tree - CART
############################################

# Running a CART on a sub-set of ORX loss data
# Subsetting is needed to get the regression tree in a right shape (too much data)

library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(magrittr) # pipe

# Remove outliners of Loss_USD
# define a value 0.0 to 1.0 in the quantile (e.g. 0.75 = 75%)

par(mfrow=c(1,2)) 
boxplot(ORX_NA_removed$Loss_USD)
str(ORX_NA_removed)

loss_quantile <- quantile(ORX_NA_removed$Loss_USD,0.75,names=FALSE)
ORX_RM_Outliners <- subset(ORX_NA_removed, Loss_USD < loss_quantile)

boxplot(ORX_RM_Outliners$Loss_USD)
str(ORX_RM_Outliners)

# Setup data.frame ORX_CART with sub-set of columns from ORX_NA_removed, covering
# 3 : Loss USD
# 5 : Business
# 6 : Product
# 8 : Event
# 11 : Cause

ORX_CART_all <- ORX_RM_Outliners[,c(3,5,6,8,11)]

# Establish Sample / currently we don't sample, ie. we take full data-set

n_Daten <- nrow(ORX_CART_all)
sample_selection <- sample(nrow(ORX_CART_all), 1 * n_Daten)
ORX_CART_sample <- ORX_CART_all[sample_selection,]

# Reduce test data.frame to specific business we do in banking

ORX_CART_Business <- ORX_CART_sample    [ ORX_CART_sample$Business == "Retail Banking" 
                                        | ORX_CART_sample$Business == "Commercial Banking"
                                        | ORX_CART_sample$Business == "Private Banking",]
ORX_CART_Product <- ORX_CART_Business   [ ORX_CART_Business$Product == "Commercial Credit" 
                                        | ORX_CART_Business$Product == "Deposits"
                                        | ORX_CART_Business$Product == "Retail Credit",]
ORX_CART_Event <- ORX_CART_Product      [ ORX_CART_Product$Event == "External Fraud" 
                                        | ORX_CART_Product$Event == "Internal Fraud"
                                        | ORX_CART_Product$Event == "Clients, Products & Business Practices",]
ORX_CART_Cause <- ORX_CART_Event        [ ORX_CART_Event$Cause == "External" 
                                        | ORX_CART_Event$Cause == "People / Staff"
                                        | ORX_CART_Event$Cause == "Processes",]
ORX_CART <- ORX_CART_Cause
summary(ORX_CART)

# rpart (rpart)
# Recursive Partitioning And Regression Trees
# Fit a rpart model

fit <- rpart(Loss_USD ~ Business + Product + Event + Cause, method="anova", data = ORX_CART)
fit

# Display Regression Tree

fancyRpartPlot(fit,main="Regression Tree for ORX Loss-USD",sub="Sub-Set of ORX-News data - External OpRisk Incidents")

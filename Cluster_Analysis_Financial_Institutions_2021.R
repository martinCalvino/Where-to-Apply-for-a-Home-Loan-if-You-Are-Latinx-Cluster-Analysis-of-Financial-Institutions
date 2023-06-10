# Script Name:                    Cluster_Analysis_Financial_Institutions_2021.R
# Created on:                     June_9_2023
# Author:                         Dr. Martin Calvino
# Purpose:                        Cluster Financial Institutions that processed home loan applications from latinos
#                                 to uncover subgroups of banks with high percentage of accepted loans and low interest rates
# Version:                        v1.06.09.2023
# Data Source:                    https://ffiec.cfpb.gov/data-browser/data/2021?category=states
# Data Fields:                    https://ffiec.cfpb.gov/documentation/publications/loan-level-datasets/lar-data-fields
# Data LEIs to common_names:      https://ffiec.cfpb.gov/documentation/faq/identifiers-faq
# R Version:                      4.2.1
# RStudio Version:                2022.07.1 Build 554


### load libraries
library(tidyverse)
library(NbClust)
library(cluster)
library(fMultivar)
library(patchwork)
library(MetBrewer)
library(plot3D)
library(ggrepel)


### load datasets

# load dataset: home loans nationwide for 2021
banks <- read.csv(file.choose()) # 19,182,225 observations x 99 variables
length(levels(factor(banks$lei))) # 4,292 financial institutions

# load csv file with common bank names mapped to leis by year
bank.names <- read.csv(file.choose())
View(bank.names)
# select leis for 2020 (the most recent year)
bank.names <- bank.names[, c(1,5)] 
View(bank.names)
# rename variable lei_2020 to lei
names(bank.names)[2] <- "lei" # this will help in left_join operation
View(bank.names) # 5,399 entries


# only consider banks that have common names assigned to leis
my.banks <- filter(bank.names, lei %in% banks$lei)
length(levels(factor(my.banks$respondent_name))) # 3,731 financial institutions
View(my.banks)
# add common names to banks data frame
banks <- inner_join(my.banks, banks, by = c("lei"))
length(levels(factor(banks$respondent_name))) # 3,731 financial institutions

 

### latino home loan applicants

# consider latino home loan applicants only
banks <- filter(banks, derived_ethnicity == "Hispanic or Latino") # 2,243,013 observations


# inspect data frame
str(banks)

# convert interest_rate variable to numeric
typeof(banks$interest_rate)
class(banks$interest_rate)
banks$interest_rate <- as.numeric(banks$interest_rate)
class(banks$interest_rate)


### create new variables per bank

# variable > percentage of accepted home loans per bank

# group all home loan applications per bank and arrange banks in descending order
banks.Arranged <- banks %>%
  group_by(respondent_name) %>%
  summarize(c0 = n()) %>%
  arrange(desc(c0))

View(banks.Arranged) # 3,456 financial institutions processed home loan applications from latinos in 2021

# group accepted home loan applications per bank and arrange banks in descending order
applications.accepted.per.bank <- banks  %>%
  filter(action_taken == 1) %>%
  group_by(respondent_name) %>%
  summarize(c1 = n()) %>%
  arrange(desc(c1))

View(applications.accepted.per.bank) # 3,338 financial institutions accepted home loan applications

# consider banks with accepted home loan applications
all.applications.per.bank <- banks.Arranged %>%
  filter(respondent_name %in% applications.accepted.per.bank$respondent_name)

View(all.applications.per.bank) # 3,338 financial institutions

# join data frames
pct.accepted.per.bank <- inner_join(all.applications.per.bank, applications.accepted.per.bank, by = c("respondent_name"))
View(pct.accepted.per.bank)
# create new variable containing the percentage of accepted home loans per bank
pct.accepted.per.bank <- mutate(pct.accepted.per.bank, pct.accepted = (c1*100)/c0)
View(pct.accepted.per.bank)


# variable > mean interest_rate per bank

# calculate the mean interest rate per bank
# values for interest rates are only present in home loan applications that were accepted by the bank
mean.ir.per.bank <- banks %>%
  filter(respondent_name %in% pct.accepted.per.bank$respondent_name) %>%
  filter(action_taken == 1) %>%
  group_by(respondent_name) %>%
  summarize(mean.ir = as.numeric(mean(interest_rate)))

View(mean.ir.per.bank)

# join data frames
banks.to.be.clustered <- inner_join(pct.accepted.per.bank, mean.ir.per.bank, by = c("respondent_name"))
View(banks.to.be.clustered)


# variable >  mean loan_amount per bank
mean.la.per.bank <- banks %>%
  filter(respondent_name %in% pct.accepted.per.bank$respondent_name) %>%
  filter(action_taken == 1) %>%
  group_by(respondent_name) %>%
  summarize(mean.la = mean(loan_amount))

View(mean.la.per.bank)

# join data frames
banks.to.be.clustered <- inner_join(banks.to.be.clustered, mean.la.per.bank, by = c("respondent_name"))
View(banks.to.be.clustered)


### identify and remove missing values
sum(is.na(banks.to.be.clustered))
banks.to.be.clustered <- na.omit(banks.to.be.clustered)
View(banks.to.be.clustered) # 2,377 financial institutions


### inspect summary statistics
summary(banks.to.be.clustered[, c(4:6)])

# inspect the distribution of percentage of accepted loans per bank
plot1 <- ggplot(data = banks.to.be.clustered, mapping = aes(x = pct.accepted)) +
  geom_histogram()
# inspect the distribution of mean interest rate per bank
plot2 <- ggplot(data = banks.to.be.clustered, mapping = aes(x = mean.ir)) +
  geom_histogram()
# inspect the distribution of mean loan amount per bank
plot3 <- ggplot(data = banks.to.be.clustered, mapping = aes(x = mean.la)) +
  geom_histogram()

plot1 + plot2 + plot3


### HIERARCHICAL CLUSTERING

# consider the top100 financial institutions in terms of processed home loans (c0 variable)
top100 <- banks.to.be.clustered[1:100, ]
View(top100)

# visualize these
ggplot(data = top100) +
  geom_col(mapping = aes(x = reorder(respondent_name, c0), y = c0)) +
  coord_flip() +
  theme_bw()

# inspect summary statistics
summary(top100[, c(4:6)])
# the median for percentage of accepted loan applications per bank is: 55.9%
# the median interest rate per bank is: 3.0%
# the median loan amount per bank is: $267,447

# remove columns c0 and c1
top100 <- top100[, -c(2,3)]
View(top100)


# scale numeric variables prior to clustering
# the scale() function standarize the variables to a mean of 0 and standard deviation of 1
# (x-mean(x)) / sd(x)
top100.scaled <- scale(top100[, c(2,3,4)])
View(top100.scaled)
# add bank common names as row names to scaled data frame
row.names(top100.scaled) <- top100$respondent_name
View(top100.scaled)


# calculate distance
?dist()
distance <- dist(top100.scaled, method = "euclidean")
# implement clustering
hc.top100.scaled <- hclust(distance, method = "average")
# visualize dendrogram
plot(hc.top100.scaled, hang = -1, cex = 0.6, main = "Hierarchical Clustering of top100 financial institutions")


# inspect metrics for leftmost cluster
financial_institution <- c("Discover_Bank", "21st_Mortgage_Corp.", "Triad_Financial", "Vanderbilt_Mortgage", "Credit_Human_Federal")
pct_accepted_loans <- c(8.709925, 15.14854, 15.84744, 13.95384, 13.89522)
mean_interest_rate <- c(6.877177, 7.903087, 7.565589, 6.404317, 5.461475)
mean_loan_amount <- c(86579.79, 89930.14, 79428.49, 79428.49, 98302.11)
# create data frame with financial institutions from leftmost cluster
leftmost_cluster <- data.frame(financial_institution, pct_accepted_loans, mean_interest_rate, mean_loan_amount)
leftmost_cluster
# inspect summary statistics
summary(leftmost_cluster[, c(2,3,4)])


### PARTITIONING CLUSTERING

# consider financial institutions that processed at least 1,000 home loan applications
top245 <- filter(banks.to.be.clustered, c0 >= 1000)
View(top245)

# scale data
top245.scaled <- scale(top245[, c(4,5,6)])
# add row names
row.names(top245.scaled) <- top245$respondent_name
View(top245.scaled)

# determine the best number of clusters
?NbClust()
nc <- NbClust(top245.scaled, distance = "euclidean", min.nc = 2, max.nc = 15, method = "kmeans") # 2 clusters were suggested

# PAM: Partitioning Around Medoids
set.seed(1234)
?pam()
# fit PAM
fit.pam <- pam(top245.scaled, k = 2, metric = "euclidean", stand = FALSE)
# identify Medoids
fit.pam$medoids
# visualize partition in bivariate plot
clusplot(fit.pam)
# obtain info about clusters
fit.pam$clusinfo # one cluster contains 139 financial institutions whereas the other contains 106 institutions
# inspect cluster assignment for each financial institution
fit.pam$clustering 

# create a new variable in top245 data frame that contains cluster assignments
top245 <- mutate(top245, cluster_assignment = fit.pam$clustering)
View(top245)

# visualize banks in 3D colored by cluster assignments
x <- top245$pct.accepted
y <- top245$mean.ir  
z <- top245$mean.la
# define dimensions of plot in pixels
width <- 10000
height <- 10000
# open a new graphics device with desired width and height
dev.new(width = width, height = height)
# create scatter plot in 3D
scatter3D(x, y, z, colvar = top245$cluster_assignment,
          bty = "g", xlab = "percentage of accepted home loans", ylab = "mean interest rate", zlab = "mean loan amount",
          colkey = list(side = 1, length = 0.5),
          phi = 0,
          pch = 20,
          cex = 3,
          ticktype = "detailed", theta = 40)
text3D(x, y, z, labels = top245$respondent_name, add = TRUE, cex = 0.6)
# save plot to file
filename <- "3Dplot_top245_financial_institutions.clustered.png"
dev.copy(png, filename)
dev.off()

# visualize distribution of variables per cluster
# percentage of accepted home loans
plot4 <- ggplot(data = top245, aes(x = factor(cluster_assignment), y = pct.accepted)) +
  geom_boxplot()
# mean interest rate
plot5 <- ggplot(data = top245, aes(x = factor(cluster_assignment), y = mean.ir)) +
  geom_boxplot()
# mean loan amount
plot6 <- ggplot(data = top245, aes(x = factor(cluster_assignment), y = mean.la)) +
  geom_boxplot()
# view all three plots
plot4 + plot5 + plot6

### show financial institutions from cluster 1
cluster1.banks <- top245[top245$cluster_assignment == 1, ]
View(cluster1.banks)

# create new variable distinguishing banks with mean interest rates higher than 3.5%
cluster1.banks <- cluster1.banks %>%
  mutate(banks_high_ir = ifelse(mean.ir > 3.5, ">3.5%", "<= 3.5%"))

# visualized them based on their percentage of accepted home loans and interest rates
ggplot(data = cluster1.banks, aes(x = pct.accepted, y = mean.ir, color = banks_high_ir)) +
  scale_color_manual(values = met.brewer("Demuth", 2)) +
  geom_point() +
  geom_rug() +
  geom_text_repel(aes(label = respondent_name), size = 2.5) +
  theme(legend.position = "none")
  
# select financial institutions from cluster 1 with interest rates below 3.5
cluster1.low.ir <- filter(cluster1.banks, mean.ir < 3.5)
cluster1.low.ir$respondent_name
# visualize banks from cluster 1 according to percentage of accepted loans and mean interest rate per bank
ggplot(data = cluster1.low.ir, aes(x = pct.accepted, y = mean.ir, color = banks_high_ir)) +
  scale_color_manual(values = met.brewer("Demuth", 2)) +
  geom_point() +
  geom_rug() +
  geom_label_repel(aes(label = respondent_name), size = 2.5) +
  theme(legend.position = "none")



### show financial institutions from cluster 2
cluster2.banks <- top245[top245$cluster_assignment == 2, ]
View(cluster2.banks)

# create new variable distinguishing banks with mean interest rates higher than 3.5%
cluster2.banks <- cluster2.banks %>%
  mutate(banks_high_ir = ifelse(mean.ir > 3.5, ">3.5%", "<= 3.5%"))

# visualized them based on their percentage of accepted home loans and interest rates
ggplot(data = cluster2.banks, aes(x = pct.accepted, y = mean.ir, color = banks_high_ir)) +
  scale_color_manual(values = met.brewer("Demuth", 2)) +
  geom_point() +
  geom_rug() +
  geom_text_repel(aes(label = respondent_name), size = 2.5) +
  theme(legend.position = "none")

# select financial institutions from cluster 1 with interest rates below 3.5
cluster2.low.ir <- filter(cluster2.banks, mean.ir < 3.5)
cluster2.low.ir$respondent_name
# visualize banks from cluster 2 according to percentage of accepted loans and mean interest rate per bank
ggplot(data = cluster2.low.ir, aes(x = pct.accepted, y = mean.ir, color = banks_high_ir)) +
  scale_color_manual(values = met.brewer("Demuth", 2)) +
  geom_point() +
  geom_rug() +
  geom_label_repel(aes(label = respondent_name), size = 2.5) +
  theme(legend.position = "none")
  


#### Q-method through R - random benchmark
#### FrK; 2019-10-04; created
#### FrK; 2019-10-18; updated

#### Content:
## 1) Loading bootStrapped datafiles
## 2) Performing QMethod
## 3) Storing important results
## 4) Apropriatng data
## 5) Producing graphs

## Erasing memory and setting the working directory
setwd("c:/Users/Kalvas/ownCloud/!Luznak/")
#rm(list=ls())
getwd()

## Important variables
# distances - dataFrame storing distances of sentences over all 150 bootStrapped files
# scores - dataFrame storing factor scores of sentences over all 150 bS files
# factor1 - dataFrame storing scores of Factor1 over all 150bS files
# factor2 - dataFrame storing scores of Factor2 over all 150bS files
# fed - factors' Eucleidian distances 

#### 1) Loading bootStrapped datafiles
#### 2) Performing QMethod
#### 3) Storing important results
## Needed libraries
library(qmethod)
library(foreign)
library(data.table)

## We have to initialize FED variable for storing Factors' Eucleidian Distances
fed = c()

#### We iterate over 150 main random files 
for(iter in 1:150) {

## The first columns of two files
# Loading data prepared in Stata
results = qmethod(read.dta(paste0("randomized", iter, "RQM1.dta")), nfactors = 2, rotation = "cluster")

# Storing scores from 1st file
scores = results[[6]]
names(scores) = c("fac1_fil1", "fac2_fil1")

# Storing Factor1,2 from 1st file
factor1 = as.data.frame(rbind(scores$fac1_fil1))
factor2 = as.data.frame(rbind(scores$fac2_fil1))

# Storing distances from 1st file
x = results[[8]]
distances = as.data.frame(rbind(abs(x$f1_f2)))

## Loading all other 1499 rows/columns
for(n in 2:1500) {
  # Loading data
  file = read.dta(paste0("randomized", iter, "RQM", n, ".dta"))
  results = qmethod(file, nfactors = 2, rotation = "cluster")
  
  # Adding scores and factor1,2
  x = results[[6]]
  factor1 = rbind.data.frame(factor1, x$fsc_f1)
  factor2 = rbind.data.frame(factor2, x$fsc_f2)

  # Adding distances 
  x = results[[8]]
  distances = rbind.data.frame(distances, abs(x$f1_f2))
}

# Vector with all variable names
vNames = c("V01", "V02", "V03", "V04", "V05", "V06", "V07", "V08", "V09", "V10", "V11", "V12")
vNames = c(vNames, "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", "V22", "V23", "V24")
vNames = c(vNames, "V25", "V26", "V27", "V28", "V29", "V30", "V31", "V32", "V33", "V34", "V35", "V36")
vNames = c(vNames, "V37", "V38", "V39", "V40", "V41", "V42", "V43", "V44", "V45", "V46", "V47", "V48")
names(distances) = vNames
names(factor1) = vNames
names(factor2) = vNames


#### 4) Apropriatng data
#### 5) Producing graphs
## Needed libraries
library(tidyverse)
library(stats)
library(here)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(psych)


## Scatterplot of factors' scores
# Factor2 averages ...
y = factor2 %>%
  gather("var", "y") %>% 
  group_by(var) %>%
  summarise(
    y = mean(y)
  ) 
# Factor1 averages ...
x = factor1 %>%
  gather("var", "x") %>% 
  group_by(var) %>%
  summarise(
    x = mean(x)
  ) 
# Combining both factors ...
xy = cbind.data.frame(x, y$y)  
names(xy) = c("var", "factor1", "factor2")

# !!! Computing Factors' Eucleidian Distances and storing them to the FED !!!
fed = c(fed, sqrt(sum((xy$factor1 - xy$factor2) ^ 2)))

# Defining scatterplot:
bootStrapped.scores = xy %>%
ggplot(aes(x = factor1, y = factor2)) +
  geom_point() +
  geom_text(aes(label = var), nudge_y = -0.25) +
  geom_curve(
    aes(x = -4, y = -1, xend = 1, yend = 4),
    curvature = -0.1,
    angle = 90,
    ncp = 15,
    colour = "red"
  ) +
  geom_curve(
    aes(x = 4, y = 1, xend = -1, yend = -4),
    curvature = -0.1,
    angle = 90,
    ncp = 15,
    colour = "red"
  ) +
  geom_curve(
    aes(x = -3, y = -4, xend = 4, yend = 3),
    curvature = 0.05,
    angle = 90,
    ncp = 15,
    colour = "blue"
  ) +
  geom_curve(
    aes(x = 3, y = 4, xend = -4, yend = -3),
    curvature = 0.05,
    angle = 90,
    ncp = 15,
    colour = "blue"
  ) +
  geom_hline(aes(yintercept = 3)) +
  geom_hline(aes(yintercept = -3)) +
  geom_vline(aes(xintercept = 3)) +
  geom_vline(aes(xintercept = -3)) +
  theme_minimal()

## Defining box plot with differences of sentences in z-scores
bootStrapped.distances = distances %>%
  gather("Sentence", "Distance") %>% 
  ggplot(aes(x = reorder(Sentence, -Distance), y = Distance)) +
  xlab("Sentence") +
  geom_boxplot() +
  geom_hline(aes(yintercept = 2)) +
  geom_hline(aes(yintercept = 1.5)) +
  geom_hline(aes(yintercept = 1)) +
  theme_minimal()

#### Exporting graphs
ggarrange(
  bootStrapped.distances, 
  ncol = 1,
  labels = c(paste0("Random boot strapped sample no. ", iter, ": Sentences' distances"))
  ) %>%
  ggexport(
    filename = paste0("distances2FactorsRandomized", iter, ".png"),
    width = 1600,
    height = 600)

ggarrange(
  bootStrapped.scores, 
  ncol = 1,
  labels = c(paste0("Random boot strapped sample no. ", iter, ": Factors' scores"))
  ) %>%
  ggexport(
    filename = paste0("scores2FactorsRandomized", iter, ".png"),
    width = 800,
    height = 800)
} ## End of iteration over 150 main random files

## Defining box plot with Factors' Euclidean distances
randomFactors.distances = as.data.frame(fed) %>%
  ggplot(aes(y = fed)) +
  ylab("Factors' Euclidean distance") +
  geom_boxplot() +
  geom_hline(aes(yintercept = factEuDist)) +
  theme_minimal()

## Exporting graph
ggarrange(
  randomFactors.distances, 
  ncol = 1,
  labels = "Factors' Euclidean distances: Random samples vs. Real sample (line)"
) %>%
  ggexport(
    filename = "distances2FactorsEuclidean.png",
    width = 800,
    height = 800)

## Description statistics
describe(as.data.frame(fed), 
         IQR = TRUE, 
         quant = c(.05, .1, .25, .5, .75, .9, .95))

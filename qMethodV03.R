#### Q-method through R
#### FrK; 2019-10-04; created
#### FrK; 2019-10-10; updated

#### Content:
## 1) Loading bootStrapped datafiles
## 2) Performing QMethod
## 3) Storing important results
## 4) Apropriatng data
## 5) Producing graphs
## 6) Just for sure, doing the same with whole file

## Erasing memory and setting the working directory
setwd("c:/Users/Kalvas/ownCloud/!Luznak/")
rm(list=ls())
getwd()

## Important variables
# distances - dataFrame storing distances of sentences over all 150 bootStrapped files
# scores - dataFrame storing factor scores of sentences over all 150 bS files
# factor1 - dataFrame storing scores of Factor1 over all 150bS files
# factor2 - dataFrame storing scores of Factor2 over all 150bS files

#### 1) Loading bootStrapped datafiles
#### 2) Performing QMethod
#### 3) Storing important results
## Needed libraries
library(foreign)
library(qmethod)

## The first columns of two files
# Loading data prepared in Stata
results = qmethod(read.dta("RQM1.dta"), nfactors = 3, rotation = "cluster")

# Storing scores from 1st file
scores = results[[6]]
names(scores) = c("fac1_fil1", "fac2_fil1", "fac3_fil1")
#str(scores)

# Storing Factor1,2 from 1st file
factor1 = as.data.frame(rbind(scores$fac1_fil1))
factor2 = as.data.frame(rbind(scores$fac2_fil1))
factor3 = as.data.frame(rbind(scores$fac3_fil1))
#str(factor1)
#str(factor2)

# Storing distances from 1st file
x = results[[8]]
distances.XY = as.data.frame(rbind(abs(x$f1_f2)))
distances.XZ = as.data.frame(rbind(abs(x$f1_f3)))
distances.ZY = as.data.frame(rbind(abs(x$f2_f3)))
#str(distances)

## Loading all other columns
for(n in 2:1500) {
  # Loading data
  file = read.dta(paste0("RQM", n, ".dta"))
  results = qmethod(file, nfactors = 3, rotation = "cluster")
  
  # Adding scores and factor1,2
  x = results[[6]]
  factor1 = rbind.data.frame(factor1, x$fsc_f1)
  factor2 = rbind.data.frame(factor2, x$fsc_f2)
  factor3 = rbind.data.frame(factor3, x$fsc_f3)
  names(x) = c(paste0("fac1_fil", n), paste0("fac2_fil", n), paste0("fac3_fil", n))
  scores = data.frame(scores, x)

  # Adding distances 
  x = results[[8]]
  distances.XY = rbind.data.frame(distances.XY, abs(x$f1_f2))
  distances.XZ = rbind.data.frame(distances.XZ, abs(x$f1_f3))
  distances.ZY = rbind.data.frame(distances.ZY, abs(x$f2_f3))
}

# Vector with all variable names
vNames = c("V01", "V02", "V03", "V04", "V05", "V06", "V07", "V08", "V09", "V10", "V11", "V12")
vNames = c(vNames, "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", "V22", "V23", "V24")
vNames = c(vNames, "V25", "V26", "V27", "V28", "V29", "V30", "V31", "V32", "V33", "V34", "V35", "V36")
vNames = c(vNames, "V37", "V38", "V39", "V40", "V41", "V42", "V43", "V44", "V45", "V46", "V47", "V48")
names(distances) = vNames
names(factor1) = vNames
names(factor2) = vNames
names(factor3) = vNames
#str(scores)
#str(factor1)
#str(factor2)
#str(distances)


#### 4) Apropriatng data
#### 5) Producing graphs
## Needed libraries
library(tidyverse)
library(stats)
library(magrittr)
library(ggplot2)
library(ggpubr)

#sNames = c("V01", "V02", "V03", "V04", "V05", "V06", "V07", "V08", "V09", "V10", "V11", "V12")
#sNames = c(sNames, "V13", "V14", "V15", "V16", "V17", "V18", "V19", "11-V20", "10-V21", "06-V22", "12-V23", "07-V24")
#sNames = c(sNames, "03-V25", "V26", "02-V27", "05-V28", "V29", "V30", "V31", "V32", "V33", "01-V34", "08-V35", "V36")
#sNames = c(sNames, "V37", "V38", "V39", "V40", "V41", "V42", "V43", "09-V44", "04-V45", "V46", "V47", "V48")
#names(distances) = sNames

## Graphs with differences of sentences in z-scores
dxy = distances.XY %>%
gather("Sentence", "Distance") %>% 
ggplot(aes(x = reorder(Sentence, -Distance), y = Distance)) +
  xlab("Sentence") +
  geom_boxplot() +
  geom_hline(aes(yintercept = 2)) +
  geom_hline(aes(yintercept = 1.5)) +
  geom_hline(aes(yintercept = 1)) +
  theme_minimal()
dxy

dxz = distances.XZ %>%
gather("Sentence", "Distance") %>% 
ggplot(aes(x = reorder(Sentence, -Distance), y = Distance)) +
  xlab("Sentence") +
  geom_boxplot() +
  geom_hline(aes(yintercept = 2)) +
  geom_hline(aes(yintercept = 1.5)) +
  geom_hline(aes(yintercept = 1)) +
  theme_minimal()
dxz

dzy = distances.ZY %>%
gather("Sentence", "Distance") %>% 
ggplot(aes(x = reorder(Sentence, -Distance), y = Distance)) +
  xlab("Sentence") +
  geom_boxplot() +
  geom_hline(aes(yintercept = 2)) +
  geom_hline(aes(yintercept = 1.5)) +
  geom_hline(aes(yintercept = 1)) +
  theme_minimal()
dzy

## Scatterplot of factors' scores
# Factor3 averages ...
z = factor3 %>%
  gather("var", "z") %>% 
  group_by(var) %>%
  summarise(
    z = mean(z)
  ) 
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
xz = cbind.data.frame(x, z$z)  
names(xz) = c("var", "factor1", "factor3")
zy = cbind.data.frame(z, y$y)  
names(zy) = c("var", "factor3", "factor2")


# Scatterplot1:
sxy = xy %>%
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
sxy  

# Scatterplot2:
sxz = xz %>%
  ggplot(aes(x = factor1, y = factor3)) +
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
sxz

# Scatterplot3:
szy = zy %>%
  ggplot(aes(x = factor3, y = factor2)) +
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
szy

## Combining and exporting graphs
ggarrange(
  dxy, dxz, dzy,
  ncol = 1,
  labels = c("Factor 1 vs. Factor 2", 
             "Factor 1 vs. Factor 3",
             "Factor 3 vs. Factor 2")) %>%
  ggexport(
    filename = "distances3Factors.png",
    width = 1800,
    height = 1800)

ggarrange(
  sxy, sxz, szy,
  ncol = 3,
  labels = c("Factor 1 vs. Factor 2", 
             "Factor 1 vs. Factor 3",
             "Factor 3 vs. Factor 2")) %>%
  ggexport(
    filename = "scores3Factors.png",
    width = 1800,
    height = 600)

## Toto je skript, který pomáhá urèit, zda výsledky Q-metody mohly vzniknout náhodou, pøípadnì do jaké míry

## Vytvoøil: 2020-10-22 FrK
## Upravil:  2020-10-23 FrK

## Encoding: windows-1250

## Hlavièka
rm(list=ls())
dir = getwd()

library(dplyr)
library(qmethod)
library(foreign)
library(data.table)
library(tidyverse)
library(stats)
library(here)
library(magrittr)
library(ggplot2)
library(ggpubr)

## Jak moc se budou lišit dva pohledy, když je vygenerujeme náhodnì?
# Toto je distribuce pseudonormáního rozdìlení pro Q-metodu
v = c(rep(-4, 3), rep(4, 3), rep(-3, 4), rep(3, 4), rep(-2, 6), rep(2, 6), rep(-1, 7), rep(1, 7), rep(0, 8))
length(v)  # test zda má 48 prvkù

# Pøipravíme si matici pro numerickou simulaci - použijeme matici, protože je rychlejší ve výpoètech
rows = 1000000  # poèet øádkù matice
df = matrix(rep(0, rows * 4), ncol = 4)  # inicializace matice
colnames(df) = c('low', 'middle', 'high', 'vHigh')  # pojmenování sloupcù matice

for (i in 1:rows) {
  x = sample(v)  # vytvoøíme náhodnou kombinaci pseudonormálního rozdìlení pro první pohled
  y = sample(v)  # totéž pro druhý
  d = abs(x - y)  # spoèteme si rozdíly mezi pohledy v jednotlivých výrocích
  df[i,4] = (d > 4) %>% sum()  # Spoèítáme, kolik "výrokù" je odlišných extrémnì,
  df[i,3] = (d <= 4 & d >= 3) %>% sum()  # kolik velmi,
  df[i,2] = (d < 3 & d > 1) %>% sum()  # kolik støednì,
  df[i,1] = (d <= 1) %>% sum()  # kolik málo - všechny hranice jsou zvolené, aby odpovídali zpracování Q-grafu
}

# Histogramy s poèty výrokù v pøíslušných kategoriích odlišnosti
hist(df[,1])
hist(df[,2])
hist(df[,3])
hist(df[,4])

# Prùmìrné poèty výrokù v pøíslušných kategoriích odlišnosti
df %>% data.frame() %>% 
  summarise(low = mean(low), middle = mean(middle), high = mean(high), vHigh = mean(vHigh))

## Spoètení, kolik procent simulovaných distribucí vyšlo konsenzuálnìji než ta naše
# Nejprve kategorie jednotlivì
((df[,4] <= 5) %>% sum()) / rows * 100
((df[,3] <= 12) %>% sum()) / rows * 100
((df[,2] >= 10) %>% sum()) / rows * 100
((df[,1] >= 21) %>% sum()) / rows * 100
# A koneènì zároveò!
(((df[,1] >= 21) & (df[,4] <= 5)) %>% sum()) / rows * 100



## Další numerické simulace
# Jak je pravdìpodobné, že z 24 náb. a 24 nenáb. výrokù vybereme 5 náboženských?
n = c(rep(0, 24), rep(1, 24))  # Udìláme si vektor, 0 bude reprezentovat nenáb., 1 náb. výroky
length(n)  # ovìøení, že má vektor 48 prvkù

# pøipravíme si vektor, kam budeme ukládat údaj, kolikrát jsme vybrali 5 náboženských výrokù
pos = 1000000  # poèet pozic, tj. délka vektoru
ex = rep(FALSE, pos)  # Pøipravíme prázdný vektor pro extrémní kontroverze
sl = rep(FALSE, pos)  # a pro shody dolní hranice CI
sh = rep(FALSE, pos)  # a pro shody, horní hranice CI
vl = rep(FALSE, pos)  # a pro vyhrocené shody, dolní
vh = rep(FALSE, pos)  # a pro vyhrocené shody, horní

# naplníme vektor
for (i in 1:pos) {
  ex[i] = (sample(n, 5) %>% sum())==5 
  sl[i] = (sample(n, 21) %>% sum())<8 
  sh[i] = (sample(n, 21) %>% sum())>13 
  vl[i] = (sample(n, 10) %>% sum())>7 
  vh[i] = (sample(n, 10) %>% sum())<3 
}

# Spoèítáme procentní podíly a hranice konfidenèního intervalu
(ex %>% sum()) / pos * 100
(sl %>% sum()) / pos * 100
(sh %>% sum()) / pos * 100
(vl %>% sum()) / pos * 100
(vh %>% sum()) / pos * 100








### Zcela náhodné vygenerování Q-sortù
## Definování potøebných funkcí 
# 24 stabilních
stb24 = function() {
  m = c(rep(1:48, 24)) %>% matrix(nrow = 48, byrow = FALSE) %>% data.frame()
  for (i in 1:24){
    m[,i] = sample(v)  
  }
  m
}

# 22 støídaných
str22 = function() {
  m = c(rep(1:48, 22)) %>% matrix(nrow = 48, byrow = FALSE) %>% data.frame()
  for (i in 1:22){
    m[,i] = sample(v)  
  }
  as.data.frame(m)
}

b = stb24()
r = str22()

# Vzorek 29 resp. ze 46
vz29 = function(stab = b, stri = r) {
  df = cbind(stri[,sample(1:22, 5)], stab)
  names(df) = rNames
  df
}


do.graph = function(do, xy, analyza){
  if (do) draw.graph(xy, analyza)
}

draw.graph = function(xy, analyza){
  # Scatterplot:
  bootStrapped.scores = xy %>%
    ggplot(aes(x = factor1, y = factor2)) +
    coord_cartesian(xlim=c(-4, +4),ylim=c(-4, +4)) +
    geom_point() +
    geom_text(aes(label = var), nudge_x = 0.1 , nudge_y = -0.15) +
    geom_curve(
      aes(x = -4, y = -1, xend = 1, yend = 4),
      curvature = -0,
      angle = 90,
      ncp = 15,
      colour = "red"
    ) +
    geom_curve(
      aes(x = 4, y = 1, xend = -1, yend = -4),
      curvature = -0,
      angle = 90,
      ncp = 15,
      colour = "red"
    ) +
    geom_curve(
      aes(x = -3, y = -4, xend = 4, yend = 3),
      curvature = 0,
      angle = 90,
      ncp = 15,
      colour = "blue"
    ) +
    geom_curve(
      aes(x = 3, y = 4, xend = -4, yend = -3),
      curvature = 0,
      angle = 90,
      ncp = 15,
      colour = "blue"
    ) +
    geom_hline(aes(yintercept = 3)) +
    geom_hline(aes(yintercept = -3)) +
    geom_vline(aes(xintercept = 3)) +
    geom_vline(aes(xintercept = -3)) +
    labs(x = "Sekulární", 
         y = "Spirituální", 
         title = "") +
    theme_minimal()
  
  ## Exporting graphs
  ggarrange(
    bootStrapped.scores, 
    ncol = 1,
    labels = c("")
  ) %>%
    ggexport(
      filename = paste0("scores2FactorsReal", analyza,".png"),
      width = 800,
      height = 800,
      res = 100)
}



## Definování potøebných konstant
# Pseudo-normální rozdìlení
v = c(rep(-4, 3), rep(4, 3), rep(-3, 4), rep(3, 4), rep(-2, 6), rep(2, 6), rep(-1, 7), rep(1, 7), rep(0, 8))
length(v)  # test zda má 48 prvkù

# Jména výrokù
vNames = c(paste0("V0", 1:9), paste0("V", 10:48))

# Jména respondentù
rNames = c(paste0("R0", 1:9), paste0("R", 10:29))

# Parametry opakovaní
anals = 10  # poèet simulovaných analýz
samps = 150  # poèet vzorkù pro jednu simulovanou analýzu

# Matice pro záznam rozdílù
rozdily = c(rep(0, 48 * anals)) %>% matrix(ncol = 48)
rozdily2 = c(rep(0, 48 * anals)) %>% matrix(ncol = 48)


## Samotná simulace
for (analyza in 1:anals) {
  # Definice souboru
  b = stb24()
  r = str22()
  
  ## The first columns of two files
  # Loading data prepared in Stata
  results = qmethod(vz29(stab = b, stri = r), nfactors = 2, rotation = "cluster")
  
  # Storing Factor1,2 from 1st file
  factor1 = results[[6]][[1]] %>% matrix (ncol = 48) %>% data.frame()
  factor2 = results[[6]][[2]] %>% matrix (ncol = 48) %>% data.frame()
  names(factor1) = vNames
  names(factor2) = vNames
  
  ## Loading all other columns
  if (samps>1) {
  
    for(n in 2:samps) {
      # Loading data
      results = qmethod(vz29(stab = b, stri = r), nfactors = 2, rotation = "cluster")
   
      # Adding scores and factor1,2
      factor1 = rbind.data.frame(factor1, results[[6]][[1]])
      factor2 = rbind.data.frame(factor2, results[[6]][[2]])
    }
  }


## Scatterplot of factors' scores
# Factor2 averages ...
y = factor2 %>%
  gather("var", "y") %>% 
  group_by(var) %>%
  summarise(y = mean(y)) 
# Factor1 averages ...
x = factor1 %>%
  gather("var", "x") %>% 
  group_by(var) %>%
  summarise(x = mean(x)) 
# Combining both factors ...
xy = cbind.data.frame(x, y$y)  
names(xy) = c("var", "factor1", "factor2")

# Vykreslení grafu z každé analýzy
do.graph(TRUE, xy, analyza)

# Uložení výsledkù do matice rozdílù
rozdil = abs(factor1 - factor2) %>% gather("var", "v") %>% group_by(var) %>%  summarise(v = mean(v))
rozdily[analyza, ] = rozdil$v
rozdily2[analyza, ] = abs(x$x - y$y)
}

## Výpoèty na základì rozdílù
# Pøíprava matice
d = c(rep(0, 4 * anals)) %>% matrix(ncol = 4)


# Spoèteme a uložíme výskyty odlišností
for (i in 1:anals) {
  d[i,4] = (rozdily[i, ] > 4) %>% sum()  # Spoèítáme, kolik "výrokù" je odlišných extrémnì,
  d[i,3] = (rozdily[i, ] <= 4 & rozdily[i, ] >= 3) %>% sum()  # kolik velmi,
  d[i,2] = (rozdily[i, ] < 3 & rozdily[i, ] > 1) %>% sum()  # kolik støednì,
  d[i,1] = (rozdily[i, ] <= 1) %>% sum()  # kolik málo - všechny hranice jsou zvolené, aby odpovídali zpracování Q-grafu
}

# Nejprve kategorie jednotlivì
((d[,4] >= 5) %>% sum()) / anals * 100
((d[,3] >= 12) %>% sum()) / anals * 100
((d[,2] <= 10) %>% sum()) / anals * 100
((d[,1] <= 21) %>% sum()) / anals * 100
# A koneènì zároveò!
(((d[,1] <= 21) & (d[,4] >= 5)) %>% sum()) / anals * 100
d





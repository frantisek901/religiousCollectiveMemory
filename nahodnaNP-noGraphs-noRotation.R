## Toto je skript, kter� pom�h� ur�it, zda v�sledky Q-metody mohly vzniknout n�hodou, p��padn� do jak� m�ry

## Vytvo�il: 2020-10-22 FrK
## Upravil:  2020-11-05 FrK

## Encoding: windows-1250

## Hlavi�ka
rm(list=ls())
dir = getwd()

library(dplyr)
library(qmethod)
library(foreign)
library(data.table)
library(stats)
library(here)
library(magrittr)
library(ggplot2)
library(ggpubr)



### Zcela n�hodn� vygenerov�n� Q-sort�
## Definov�n� pot�ebn�ch funkc� 
# 23 stabiln�ch
stb23 = function() {
  m = c(rep(1:48, 23)) %>% matrix(nrow = 48, byrow = FALSE) %>% data.frame()
  for (i in 1:23){
    m[,i] = sample(v)  
  }
  m
}

# 22 st��dan�ch
str22 = function() {
  m = c(rep(1:48, 22)) %>% matrix(nrow = 48, byrow = FALSE) %>% data.frame()
  for (i in 1:22){
    m[,i] = sample(v)  
  }
  as.data.frame(m)
}

# Vzorek 28 resp. ze 45
vz28 = function(stab = b, stri = r) {
  df = cbind(stri[,sample(1:22, 5)], stab)
  names(df) = rNames
  df
}


## Definov�n� pot�ebn�ch konstant
# Pseudo-norm�ln� rozd�len�
v = c(rep(-4, 3), rep(4, 3), rep(-3, 4), rep(3, 4), rep(-2, 6), rep(2, 6), rep(-1, 7), rep(1, 7), rep(0, 8))
length(v)  # test zda m� 48 prvk�
# Jm�na v�rok�
vNames = c(paste0("V", str_pad(1:48, 2, 'left', '0')))

# Jm�na respondent�
rNames = c(paste0("R", str_pad(1:28, 2, 'left', '0')))

# Parametry opakovan�
anals = 1000  # po�et simulovan�ch anal�z
samps = 1500  # po�et vzork� pro jednu simulovanou anal�zu

# Matice pro z�znamy rozd�l�
rozdilyPrumeruFaktoru  = c(rep(0, 48 * anals)) %>% matrix(ncol = 48)
prumerneRozdilyFaktoru = c(rep(0, 48 * anals)) %>% matrix(ncol = 48)


# Za��tek simulace
start = Sys.time()

## Samotn� simulace
for (analyza in 1:anals) {
  # Definice souboru
  b = stb23()
  r = str22()
  
  ## The first columns of two files
  # Loading data prepared in Stata
  results = qmethod(vz28(stab = b, stri = r), nfactors = 2, rotation = "none")[[6]]
  
  # Storing Factor1,2 from 1st file
  factor1 = results[[1]] %>% matrix (ncol = 48) %>% data.frame()
  factor2 = results[[2]] %>% matrix (ncol = 48) %>% data.frame()

  ## Loading all other columns
  if (samps>1) {
  
    for(n in 2:samps) {
      # Loading data
      results = qmethod(vz28(stab = b, stri = r), nfactors = 2, rotation = "none")[[6]]
   
      # Adding scores and factor1,2
      factor1 = rbind.data.frame(factor1, results[[1]])
      factor2 = rbind.data.frame(factor2, results[[2]])
    }
  }


  # Ulo�en� v�sledk� do matice 'Pr�m�rn�ch rozd�l� faktor�'
  names(factor1) = vNames
  names(factor2) = vNames
  rozdil = abs(factor1 - factor2) %>% gather("var", "v") %>% group_by(var) %>%  summarise(v = mean(v))
  prumerneRozdilyFaktoru[analyza, ] = rozdil$v %>% round(digits = 3)

  # Ulo�en� v�sledk� do matice 'Rozd�l� pr�m�rnych faktor�'
  pf1 = factor1 %>% gather("var", "v") %>% group_by(var) %>%  summarise(v = mean(v)) %>% select(2)
  pf2 = factor2 %>% gather("var", "v") %>% group_by(var) %>%  summarise(v = mean(v)) %>% select(2)
  rozdilyPrumeruFaktoru[analyza, ] = abs(pf1$v - pf2$v) %>% round(digits = 3)

  # Po��tadlo stavu
  for (variable in 1:500) {
    paste0("Toto byla anal�za �. ", analyza, ".") %>% print()
  }
}

# Konec simulace
konec = Sys.time()
konec - start  # �as simulace

# Ulo��me si v�sledky simulace na pozd�ji
saveRDS(rozdilyPrumeruFaktoru ,  file = 'RPFbezRotace_2020-11-27.rds', ascii = TRUE)
saveRDS(prumerneRozdilyFaktoru , file = 'PRFbezRotace_2020-11-27.rds', ascii = TRUE)

rozdilyPrumeruFaktoru - prumerneRozdilyFaktoru


## V�po�ty na z�klad� rozd�l�
# P��prava matice
dPRF = c(rep(0, 4 * anals)) %>% matrix(ncol = 4)
dRPF = c(rep(0, 4 * anals)) %>% matrix(ncol = 4)

# Spo�teme a ulo��me v�skyty odli�nost�
for (i in 1:anals) {
  d[i,4] = (rozdily[i, ] > 4) %>% sum()  # Spo��t�me, kolik "v�rok�" je odli�n�ch extr�mn�,
  d[i,3] = (rozdily[i, ] <= 4 & rozdily[i, ] >= 3) %>% sum()  # kolik velmi,
  d[i,2] = (rozdily[i, ] < 3 & rozdily[i, ] > 1) %>% sum()  # kolik st�edn�,
  d[i,1] = (rozdily[i, ] <= 1) %>% sum()  # kolik m�lo - v�echny hranice jsou zvolen�, aby odpov�dali zpracov�n� Q-grafu
}

# Nejprve kategorie jednotliv�
((d[,4] >= 05) %>% sum()) / anals * 100
((d[,4] <= 05) %>% sum()) / anals * 100
((d[,3] >= 12) %>% sum()) / anals * 100
((d[,3] <= 12) %>% sum()) / anals * 100
((d[,2] >= 10) %>% sum()) / anals * 100
((d[,2] <= 10) %>% sum()) / anals * 100
((d[,1] <= 21) %>% sum()) / anals * 100
((d[,1] >= 21) %>% sum()) / anals * 100
# A kone�n� z�rove�!
(((d[,1] <= 21) & (d[,4] >= 5)) %>% sum()) / anals * 100
(((d[,1] >= 21) & (d[,4] >= 5)) %>% sum()) / anals * 100
(((d[,1] <= 21) & (d[,4] <= 5)) %>% sum()) / anals * 100
(((d[,1] >= 21) & (d[,4] <= 5)) %>% sum()) / anals * 100
(((d[,1] < 21) & (d[,4] > 5)) %>% sum()) / anals * 100
(((d[,1] > 21) & (d[,4] > 5)) %>% sum()) / anals * 100
(((d[,1] < 21) & (d[,4] < 5)) %>% sum()) / anals * 100
(((d[,1] > 21) & (d[,4] < 5)) %>% sum()) / anals * 100
# V�pis matice odli�nost�
d

# Histogramy s po�ty v�rok� v p��slu�n�ch kategori�ch odli�nosti
hist(d[,1])
hist(d[,2])
hist(d[,3])
hist(d[,4])

hist(rozdily)

# Kolik by rozd�l� m�lo b�t?
((rozdily > 4) %>% sum()) / anals
((rozdily <= 4 & rozdily >= 3) %>% sum()) / anals
((rozdily < 3 & rozdily > 1) %>% sum()) / anals
((rozdily <= 1) %>% sum()) / anals
mean(d[,1])
mean(d[,2])
mean(d[,3])
mean(d[,4])

# T-testem srovn�me, zda se simulace li�� od na�ich v�sledk�
t.test(d[,1], alternative = "t", mu = 21)
t.test(d[,2], alternative = "t", mu = 10)
t.test(d[,3], alternative = "t", mu = 12)
t.test(d[,4], alternative = "t", mu = 5)





## Jak moc se budou li�it dva pohledy, kdy� je vygenerujeme n�hodn�?
# Toto je distribuce pseudonorm�n�ho rozd�len� pro Q-metodu
v = c(rep(-4, 3), rep(4, 3), rep(-3, 4), rep(3, 4), rep(-2, 6), rep(2, 6), rep(-1, 7), rep(1, 7), rep(0, 8))
length(v)  # test zda m� 48 prvk�

# P�iprav�me si matici pro numerickou simulaci - pou�ijeme matici, proto�e je rychlej�� ve v�po�tech
rows = 1000000  # po�et ��dk� matice
df = matrix(rep(0, rows * 4), ncol = 4)  # inicializace matice
colnames(df) = c('low', 'middle', 'high', 'vHigh')  # pojmenov�n� sloupc� matice

for (i in 1:rows) {
  x = sample(v)  # vytvo��me n�hodnou kombinaci pseudonorm�ln�ho rozd�len� pro prvn� pohled
  y = sample(v)  # tot� pro druh�
  d = abs(x - y)  # spo�teme si rozd�ly mezi pohledy v jednotliv�ch v�roc�ch
  df[i,4] = (d > 4) %>% sum()  # Spo��t�me, kolik "v�rok�" je odli�n�ch extr�mn�,
  df[i,3] = (d <= 4 & d >= 3) %>% sum()  # kolik velmi,
  df[i,2] = (d < 3 & d > 1) %>% sum()  # kolik st�edn�,
  df[i,1] = (d <= 1) %>% sum()  # kolik m�lo - v�echny hranice jsou zvolen�, aby odpov�dali zpracov�n� Q-grafu
}

# Histogramy s po�ty v�rok� v p��slu�n�ch kategori�ch odli�nosti
hist(df[,1])
hist(df[,2])
hist(df[,3])
hist(df[,4])

# Pr�m�rn� po�ty v�rok� v p��slu�n�ch kategori�ch odli�nosti
df %>% data.frame() %>% 
  summarise(low = mean(low), middle = mean(middle), high = mean(high), vHigh = mean(vHigh))

## Spo�ten�, kolik procent simulovan�ch distribuc� vy�lo konsenzu�ln�ji ne� ta na�e
# Nejprve kategorie jednotliv�
((df[,4] <= 5) %>% sum()) / rows * 100
((df[,3] <= 12) %>% sum()) / rows * 100
((df[,2] >= 10) %>% sum()) / rows * 100
((df[,1] >= 21) %>% sum()) / rows * 100
# A kone�n� z�rove�!
(((df[,1] >= 21) & (df[,4] <= 5)) %>% sum()) / rows * 100



## Dal�� numerick� simulace
# Jak je pravd�podobn�, �e z 24 n�b. a 24 nen�b. v�rok� vybereme 5 n�bo�ensk�ch?
n = c(rep(0, 24), rep(1, 24))  # Ud�l�me si vektor, 0 bude reprezentovat nen�b., 1 n�b. v�roky
length(n)  # ov��en�, �e m� vektor 48 prvk�

# p�iprav�me si vektor, kam budeme ukl�dat �daj, kolikr�t jsme vybrali 5 n�bo�ensk�ch v�rok�
pos = 1000000  # po�et pozic, tj. d�lka vektoru
ex = rep(FALSE, pos)  # P�iprav�me pr�zdn� vektor pro extr�mn� kontroverze
sl = rep(FALSE, pos)  # a pro shody doln� hranice CI
sh = rep(FALSE, pos)  # a pro shody, horn� hranice CI
vl = rep(FALSE, pos)  # a pro vyhrocen� shody, doln�
vh = rep(FALSE, pos)  # a pro vyhrocen� shody, horn�

# napln�me vektor
for (i in 1:pos) {
  ex[i] = (sample(n, 5) %>% sum())==5 
  sl[i] = (sample(n, 21) %>% sum())<8 
  sh[i] = (sample(n, 21) %>% sum())>13 
  vl[i] = (sample(n, 10) %>% sum())>7 
  vh[i] = (sample(n, 10) %>% sum())<3 
}

# Spo��t�me procentn� pod�ly a hranice konfiden�n�ho intervalu
(ex %>% sum()) / pos * 100
(sl %>% sum()) / pos * 100
(sh %>% sum()) / pos * 100
(vl %>% sum()) / pos * 100
(vh %>% sum()) / pos * 100








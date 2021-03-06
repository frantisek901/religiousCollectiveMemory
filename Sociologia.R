#### Replikace anal�zy n�bo�ensk� pam�ti pro �asopis Socil�gia

## Vytvo�il: FrK 2020-11-05
## Upravil:  FrK 2020-11-27

## Encoding: windows-1250

# Z�m�r a c�l:
# ------------
# Chci zreplikovat je�t� jednou p��mo v R celou anal�zu Q-metody, a to v�etn� n�hodn�ho
# v�b�ru 5 mlad�ch �en z 22. Uvid�me, jestli to dopadne stejn� jako mix Stata-R.
# Pak chci testovat rozd�l mezi dv�ma zp�soby v�po�tu rozd�lu mezi faktory/subjektivitami.
# Tj. zda za 1500 anal�z spo��tat pr�m�rn� hodnoty faktor� a pak spo��tat razd�ly,
# nebo zda spo��tat 1500 jednotliv�ch rozd�l�, a pak z nich spo��tat pr�m�r.
# ------------

# Hlavi�ka
rm(list=ls())
dir = getwd()

library(dplyr)
library(tidyr)
library(qmethod)
library(foreign)
library(data.table)
library(stats)
library(here)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(tibble)
library(PlaneGeometry)
library(stringr)
library(ggrepel)

# Pot�ebn� funkce, hodnoty a objekty:
## Hodnoty
RPFs = readRDS('RPF_2020-11-05.rds')  # Simulovan� v�sledky - Rozd�ly pr�m�rn�ch faktor�
PRFs = readRDS('PRF_2020-11-05.rds')  # Simulovan� v�sledky - Pr�m�rn� rozd�ly faktor�
RPFsbr = readRDS('RPFbezRotace_2020-11-27.rds')  # Simulovan� v�sledky - Rozd�ly pr�m�rn�ch faktor�
PRFsbr = readRDS('PRFbezRotace_2020-11-27.rds')  # Simulovan� v�sledky - Pr�m�rn� rozd�ly faktor�
stb23r = read.dta('A.dta') %>% select(4:51)  # Stabiln�ch 23 respondent� 
str22r = read.dta('B.dta') %>% select(4:51)  # 22 mlad�ch �en, ze kter�ch vybereme 5
vzorky = 1500  # Po�et n�hodn�ch vzork�

## Jm�na v�rok� a respondent�
vNames = c(paste0("V0", 1:9), paste0("V", 10:48))
rNames = c(paste0("R0", 1:9), paste0("R", 10:28))

# Zn�n� v�rok�
Zn�n� =  c('1. Kn�e V�clav se zaslou�il o roz���en� k�es�anstv�, �il zbo�n� a mravn�.',
           '2. Kn�e V�clav byl a je vn�m�n jako symbol �esk�ho patriotismu.',
           '3. Husit� byli vlastn� p�edch�dci komunist�.',
           '4. Husitstv� je pokl�d�no za specificky �eskou formu k�es�anstv�.',
           '5. Mistr Jan Hus byl tragicky nepochopen� reform�tor c�rkve.',
           '6. Jan Hus pat�il k v�znamn�m evropsk�m intelektu�l�m sv� doby.',
           '7. Jan �mos Komensk� byl jednou z nejv�razn�j��ch osobnost� star� Jednoty bratrsk�.',
           '8. Jan �mos Komensk� byl hlavn� pedagogem, a v tom je pro sv�t nejcenn�j��.',
           '9. V�clav Havel byl neofici�ln�m nejvy���m p�edstavitelem nec�rkevn� duchovnosti.',
           '10. V�clav Havel je symbolem toho, �e zd�nliv� bezmocn� mohou porazit mocn�.',
           '11. Cyril a Metod�j p�inesli v�ru a evangelium.',
           '12. V�znam mise Cyrila a Metod�je byl hlavn� politick�.',
           '13. Karel IV. hledal rozumnou dohodu s c�rkv�, a c�rkev hledala rozumnou dohodu s n�m.',
           '14. Karel IV. stav�l katedr�lu ve jm�nu n�roda.',
           '15. Tom� Garrigue Masaryk byl �lov�k hluboce v���c�, ale p�esn� rozli�oval mezi v�rou a c�rkv�.',
           '16. Tom� Garrigue Masaryk je dovr�itel na�eho n�rodn�ho obrozen�.',
           '17. Klement Gottwald ud�lal mnoho dobr�ho pro lidi.',
           '18. Klement Gottwald se sna�il o slu�n� vztahy s ��mskokatolickou c�rkv�.',
           '19. B�l� hora je symbolem por�ky, potupy a ne�t�st�.',
           '20. Katol�ci se po B�l� ho�e za�ali k jinov�rc�m chovat neb�vale krut�.',
           '21. Odsun N�mc� ze Sudet byla chyba.',
           '22. Dnes navr�t�me majetek c�rkvi a z�tra �lecht�. Odtud je u� jen kr��ek k navr�cen� Sudet.',
           '23. V listopadu 1989 jedna moc padla a jin� se za�aly pr�t o d�dictv�.',
           '24. Po listopadu 1989 c�rkve nevyu�ily z�jmu o n�bo�enstv�, tak nev�dan�ho pro ateistickou zemi.',
           '25. Pra�sk� hrad nem� b�t dot�en restitucemi.',
           '26. Pra�sk� hrad je pam�tn�k za�l� moci.',
           '27. V�noce jsou oslavou toho, �e Kristus, kter� se narodil, p�i�el jako Bo�� dar.',
           '28. V�noce jsou v posledn� dob� st�le komer�n�j��.',
           '29. Velikonoce jsou v �esku vn�m�ny jako oslava p��chodu jara, a i jako p��le�itost k dobr�mu j�dlu a pit�.',
           '30. Velikonoce jsou kombinac� oby�ej� z pohansk�ch dob a k�es�ansk�ch tradic.',
           '31. �esk� pravoslavn� obec byla v protektor�tu rozpu�t�na, majetek zkonfiskov�n, chr�my zape�et�ny a bohoslu�by zak�z�ny.',
           '32. Protektor�t pro �echy znamenal vyh�n�n� z pohrani��, zat�k�n�, popravy, poni�ov�n�, poko�ov�n�, nejhlub�� porobu, likvidaci inteligence.',
           '33. Nov� rok u� oded�vna vyvol�val v lidech pocit, �e maj� �anci ve sv�m �ivot� n�co zlep�it.',
           '34. Nov� rok je pro n�s jedna z nejv�t��ch slavnost� c�rkevn�ho roku, slavnost Matky Bo��.',
           '35. Jan Nepomuck� m� mezi v���c�mi velkou v�hu.',
           '36. Jan Nepomuck� vel� nejen v p��hodn� �as ml�et, ale tak� mluvit, pokud je to zapot�eb�.',
           '37. Jan �i�ka bojoval za �ist�� c�rkev.',
           '38. Jan �i�ka jako �sp�n� vojev�dce a politik pat�� k nejv�t��m postav�m �esk�ch d�jin.',
           '39. Karel Havl��ek Borovsk� p�i�kl jezuit�m negativn� n�dech.',
           '40. Karel Havl��ek Borovsk� byl vtipn� b�sn�k, schopn� obchodn�k a zast�nce svobody podnik�n�.',
           '41. Franti�ek Drtikol byl v�dy �lov�kem pr�ce, praxe a obdivuhodn� um�rn�nosti.',
           '42. Franti�ek Drtikol byl pr�kopn�kem buddhismu u n�s.',
           '43. B�n� �lov�k si pod pojmem jezuita u n�s vybav� p�edev��m Anton�na Koni�e.',
           '44. Anton�n Koni� p�lil jen liter�rn� brak.',
           '45. Jan Zrzav� byl hluboce v���c� �lov�k, p��le�itostn� s�m sebe ztoto��oval s postavou umu�en�ho Krista.',
           '46. Jan Zrzav� byl v�znamn� �esk� mal��.',
           '47. Jan Neruda nebyl ��dn� antisemita, kter� by nen�vid�l �idy a �idovstv�.',
           '48. Jan Neruda je jedn�m z pil��� novodob� �esk� n�rodn� literatury.')
n = 'N�b'
s = 'Sek'
Charakter = c(n, s, s, n, n, s, n, s, n, s, n, s, n, s, n, s, s, n, s, n, s, n, s, n, n, s, n, s, s, n, n, s,
              s, n, n, s, n, s, n, s, s, n, n, s, n, s, n, s)



## Vzorek 28 resp. ze 45
vz28r = function(stab = stb23r, stri = str22r) {
  df = rbind(stri[sample(1:22, 5),], stab)
  names(df) = vNames
  row.names(df) = rNames
  t(df)
}



# Samotn� anal�za
## Za��tek anal�zy
start = Sys.time()

## The first columns of two files
### Loading data prepared in Stata
results = qmethod(vz28r(stab = stb23r, stri = str22r), nfactors = 2, rotation = "none")[[6]]  # POZOR! Bez rotace!
  
### Storing Factor1,2 from 1st file
factor1 = results[[1]] %>% matrix (ncol = 48) %>% data.frame()
factor2 = results[[2]] %>% matrix (ncol = 48) %>% data.frame()
  
## Loading all other columns
if (vzorky>1) {
    
  for(n in 2:vzorky) {
    ### Loading data
    results = qmethod(vz28r(stab = stb23r, stri = str22r), nfactors = 2, rotation = "none")[[6]]  # POZOR! Bez rotace!
      
    ### Adding scores and factor1,2
    factor1 = rbind.data.frame(factor1, results[[1]])
    factor2 = rbind.data.frame(factor2, results[[2]])
  }
}
  
## Zpracov�n� v�sledk� do objektu 'Pr�m�rn�ch rozd�l� faktor�' (PRFr)
names(factor1) = vNames
names(factor2) = vNames
rozdil = abs(factor1 - factor2) %>% gather("var", "v") %>% group_by(var) %>%  summarise(v = mean(v))
PRFr = rozdil$v %>% round(digits = 3)
  
## Zpracov�n� v�sledk� do objektu 'Rozd�l� pr�m�rnych faktor�' (RPFr)
pf1 = factor1 %>% gather("var", "v") %>% group_by(var) %>%  summarise(v = mean(v)) %>% select(2)
pf2 = factor2 %>% gather("var", "v") %>% group_by(var) %>%  summarise(v = mean(v)) %>% select(2)
RPFr = abs(pf1$v - pf2$v) %>% round(digits = 3)
  
## Srovn�n� p��stup�:
soucetOdchylek = abs(PRFr - RPFr) %>% sum()

## Konec anal�zy a rozd�l �as�
konec = Sys.time()
konec - start

# Ulo�en� dat
data.frame(RPF = RPFr, PRF = PRFr) %>% 
  saveRDS(., file = 'realneRozdilyBezRotace_2020-11-27.rds', ascii = TRUE)  # POZOR! Bez rotace!
 

# Porovn�n� Bez/Rotace
bezRotace = readRDS('realneRozdilyBezRotace_2020-11-27.rds')
sRotaci = readRDS('realneRozdily_2020-11-05.rds')
abs(bezRotace - sRotaci) %>% rownames_to_column(var = "V�rok") %>% 
  mutate(V�rok = as.numeric(V�rok)) %>% 
  ggplot(aes(x = V�rok, y = PRF, label = V�rok)) +
  geom_point(size = 2, col = "red") +
  geom_label_repel() +
  scale_x_continuous(breaks=seq(1,48,2)) +
  scale_y_continuous(breaks=seq(0,2.4,0.2)) +
  theme_light()

srovnani = data.frame('Bez rotace' = bezRotace$PRF, 
                      'S rotac�' = sRotaci$PRF, 
                      Rozd�l = abs(bezRotace$PRF - sRotaci$PRF), 
                      'Zn�n�' = str_trunc(Zn�n�, 90), 
                      check.names = FALSE) %>% 
  rownames_to_column(var = "V�rok") %>% 
  mutate(V�rok = str_pad(V�rok, 2, 'left', '0')) %>% mutate(V�rok = paste0('V', V�rok)) 
 
srovnani %>% ggplot(aes(x=`S rotac�`, y = `Bez rotace`, label = V�rok)) +
  geom_point(size = 5, alpha = 0.4, color = 'red') +
  geom_text() +
  geom_abline(slope = 1, intercept = 0, col = "blue") +
  geom_abline(slope = 1, intercept = 1, col = "red") +
  geom_abline(slope = 1, intercept = -1, col = "red") +
  geom_abline(slope = 1, intercept = 2, col = "red") +
  geom_abline(slope = 1, intercept = -2, col = "red") +
  theme_light()
ggsave(filename = "rozdilRotace.png",
       width = 5, height = 5, dpi = 300)

srovnani %>% arrange(Rozd�l)
srovnani %>% arrange(`Bez rotace`)
srovnani %>% arrange(`S rotac�`)

# Histogramy
ggplot(data = (c(PRFr, RPFr) %>% data.frame(X = .)), aes(x = X)) + 
  geom_histogram(bins = 6) +
  theme_classic()

ggplot(data = (PRFsbr %>% as.vector() %>%  data.frame(X = .)), aes(x = X)) + 
  geom_histogram(bins = 12) +
  # scale_y_log10() +
  theme_classic()

ggplot(data = (RPFsbr %>% as.vector() %>%  data.frame(X = .)), aes(x = X)) + 
  geom_histogram(bins = 12) +
  # scale_y_log10() +
  theme_classic()

ggplot(data = (c(PRFsbr, RPFsbr) %>% as.vector() %>%  data.frame(X = .)), aes(x = X)) + 
  geom_histogram(bins = 12) +
  # scale_y_log10() +
  theme_classic()


(PRFr - RPFr) %>% abs() %>% sum()


# Scatter plot
ggplot(data = data.frame(Sekul�rn� = pf1$v, Spiritu�ln� = pf2$v, L = vNames), 
       aes(x = Sekul�rn�, y = Spiritu�ln�, label = L)) +
  # geom_text(check_overlap = TRUE, nudge_y = -0.25, size = 3) +
  geom_abline(slope = 1, intercept = 3, col = "red") +
  geom_abline(slope = 1, intercept = 1, col = "blue") +
  geom_abline(slope = 1, intercept = -3, col = "red") +
  geom_abline(slope = 1, intercept = -1, col = "blue") +
  geom_hline (yintercept = c(-3, 3)) +
  geom_vline (xintercept = c(-3, 3)) +
  geom_point(color = "red", alpha = 0.4, size = 5) +
  geom_label_repel() +
  scale_y_continuous(breaks=seq(-4,4,2)) +
  scale_x_continuous(breaks=seq(-4,4,2)) +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
  labs(x = "N�rodn�") +
  theme_light()  
ggsave(filename = "vyrokyNerotovane2.png",
       width = 6.27, height = 6.27, dpi = 300)

data.frame(Sekul�rn� = pf1$v %>% round(digits = 2), 
           Spiritu�ln� = pf2$v  %>% round(digits = 2), 
           Rozd�l = abs(pf1$v - pf2$v) %>% round(digits = 2), L = vNames) %>% 
  arrange(Rozd�l)



# Boxplot: 6 prom�nn�ch
all = data.frame('Rozd�l Pr�m�r� (rot)' = sample(RPFs, 1000), 
                 'Pr�m�r Rozd�l� (rot)' = sample(PRFs, 1000), 
                 'Re�ln� rozd�ly (rot)' = c((sRotaci %>% as.matrix()), rep(NA, (1000 - 96))),
                 'Rozd�l Pr�m�r� (bez)' = sample(RPFsbr, 1000), 
                 'Pr�m�r Rozd�l� (bez)' = sample(PRFsbr, 1000), 
                 'Re�ln� rozd�ly (bez)' = c((bezRotace %>% as.matrix()), rep(NA, (1000 - 96))),
                 check.names = FALSE) %>% 
  pivot_longer(cols = 1:6, names_to = 'Zdroj', values_to = 'Rozd�l') %>% filter(!is.na(Rozd�l)) %>% 
  group_by(Zdroj)
ggplot(data = all, aes(x = Zdroj, y = Rozd�l, fill = Zdroj, col = Zdroj)) +
  geom_boxplot(alpha = 0.2, width = 0.2) + 
  geom_jitter(alpha = 0.1, size = 2, width = 0.1, height = 0) +
  theme_classic()
table(all$Zdroj)  
# Na pohled se zd�, �e pr�m�rn� rozd�ly se neli�� podle toho, jestli je to spo�teno s/bez rotace.

## T-Test, jestli se pr�m�rn� rozd�ly li�� s/bez rotace
t.test((PRFs %>% as.vector()), (PRFsbr %>% as.vector()))  # Pr�m�r rozd�l�
# t.test((RPFs %>% as.vector()), (RPFsbr %>% as.vector()))  # Rozd�l pr�m�r�

## K-S test, jestli se dv� distribuce li��
ks.test((PRFs %>% as.vector() %>% sample(5000)), 
        (PRFsbr %>% as.vector() %>% sample(5000)))  # S v�b�rem to ukazuje shodu distribuc�
ks.test((PRFs %>% as.vector()), 
        (PRFsbr %>% as.vector()))  # Bez v�b�ru ne - je to kv�li shod�m, z�ejm�...

## QQ plot
qq = data.frame(Rotace = (PRFs %>% as.vector() %>% sample(1000) %>% sort()),
                'Bez rotace' = (PRFsbr %>% as.vector() %>% sample(1000) %>% sort()),
                check.names = FALSE)
qq = data.frame(Rotace = (PRFs %>% as.vector() %>% sort()),
                'Bez rotace' = (PRFsbr %>% as.vector() %>% sort()),
                check.names = FALSE)
ggplot(data = qq, aes(x = Rotace, y = `Bez rotace`)) +
  # geom_qq() +
  # geom_qq_line() +
  geom_point(alpha = 0.1, color = "blue") +
  geom_abline(slope = .953, intercept = 0.12) +
  theme_classic2()
summary(qq)
m = glm(`Bez rotace` ~ Rotace, data = qq)
summary(m)

# Boxplot: 2 prom�nn� s/bez rotac�
all = data.frame('Simulace (rot)' = sample(PRFs, 1000), 
                 'Simulace (bez)' = sample(PRFsbr, 1000), 
                 'Data (rot)' = c(sRotaci$PRF, rep(NA, (1000 - 48))),
                 'Data (bez)' = c(bezRotace$PRF, rep(NA, (1000 - 48))),
                 check.names = FALSE) %>% 
  pivot_longer(cols = 1:4, names_to = 'Zdroj', values_to = 'Rozd�l') %>% filter(!is.na(Rozd�l)) %>% 
  group_by(Zdroj)
ggplot(data = all, aes(x = Zdroj, y = Rozd�l, fill = Zdroj, col = Zdroj)) +
  geom_boxplot(alpha = 0.1, width = 0.2) + 
  geom_jitter(alpha = 0.1, size = 3, width = 0.08, height = 0) +
  theme_classic()
table(all$Zdroj)  


# Boxplot: 2 prom�nn� 
all = data.frame('Simulace' = sample(PRFs, 1000), 
                 'Data' = c(PRFr, rep(NA, (1000 - 48)))) %>% 
  pivot_longer(cols = 1:2, names_to = 'Zdroj', values_to = 'Rozd�l') %>% filter(!is.na(Rozd�l)) %>% 
  group_by(Zdroj)
ggplot(data = all, aes(x = Zdroj, y = Rozd�l, fill = Zdroj, col = Zdroj)) +
  geom_boxplot(alpha = 0.1, width = 0.2) + 
  geom_jitter(alpha = 0.1, size = 3, width = 0.08, height = 0) +
  theme_classic()
table(all$Zdroj)  


all %>% filter(Zdroj == "Simulace") %>% filter(Rozd�l > 4.1) %>% nrow()
((PRFs >= 4.1) %>% sum()) / 480

# V kolika simulac�ch z 1000 je shoda stejna jako v re�ln�ch datech? 
x = as.data.frame(PRFs)
names(x) = vNames
x$shoda = rep(NA, (1000))
for (i in 1:1000) {
  row = x[i,1:48]
  x[i, 'shoda'] = (row <= 1.31) %>% sum(na.rm = TRUE)
}
x$rozd�l = rep(NA, (1000))
for (i in 1:1000) {
  row = x[i,1:48]
  x[i, 'rozd�l'] = (row >= 4.1) %>% sum(na.rm = TRUE)
}
summary(x$shoda)
summary(x$rozd�l)
t = t.test(x$rozd�l, mu = 5)

ggplot(data = (x %>% pivot_longer(cols = 49:50, names_to = 'charakteristika', values_to = "hodnota")),
       aes(y = hodnota, x = charakteristika, fill = charakteristika, col = charakteristika)) +
  geom_boxplot(alpha = 0.2) +
  geom_jitter(data = (x %>% pivot_longer(cols = 49:50, names_to = 'charakteristika', values_to = "hodnota") %>% 
                      slice_sample(n = 500)),
              alpha = 0.05, size = 3, width = 0.3, height = 0.4) +
  theme_classic()

data.frame(L = vNames, Seku = (pf1$v %>% round(digits = 2)), 
           Spir=(pf2$v %>% round(digits = 2)), 
           dif = ((pf1$v - pf2$v) %>% round(digits = 2) %>% 
                    abs())) %>% arrange(dif) %>% arrange(L)

# CIs pro jednotliv� v�roky
rozdilFaktoru = abs(factor1 - factor2)
CIrf = 1:96 %>% matrix(ncol = 2) %>% as.data.frame()
for (i in 1:48) {
  t = t.test(as.vector(rozdilFaktoru[, i]), mu = 4)
  CIrf[i, ] = t[['conf.int']][1:2]
}
CIrf[17,] = c(0,0) 

# V re�ln�ch datech je 5 v�rok�, kter� maj� rozd�l v�t�� ne� 4 body, kolik simulac� je na tom stejn�?
((x$rozd�l >= 5) %>% sum()) / 10
# Hmm... 20%, to nen� nic moc...

# Jak moc velk� n�hoda je, �e z 5 v�rok� je 5 n�bo�ensk�ch?
pok = rep(NA, 100000)
for (i in 1:100000) {
  pok[i] = c(rep(1, 24), rep(0, 24)) %>% sample(5) %>% sum()
}
((pok==5) %>% sum()) / 1000


# Test jestli z�le�� na typu rotace
data = vz28r(stab = stb23r, stri = str22r) # Zmraz�me si data
# dataFix = data  # data = dataFix
# saveRDS(dataFix, "dataFix.rds", ascii = TRUE)
# dataFix = readRDS("dataFix.rds")
qmethod(dataset = data, nfactors = 2, rotation = "cluster")
qmethod(dataset = data, nfactors = 2, rotation = "none")


# define an ellipses
ell <- Ellipse$new(center = c(0.45,0.6), rmajor = 0.25, rminor = 0.15, alpha = 90)
# ellipse as a path
ellpath3 <- ell$path()
# the path is not closed; close it
ellpath3 <- rbind(ellpath3, ellpath3[1,]) %>% as.data.frame() %>%  cbind(lab = "skup 1")

# define an ellipses
ell <- Ellipse$new(center = c(0.55,0.2), rmajor = 0.20, rminor = 0.15, alpha = 0)
# ellipse as a path
ellpath4 <- ell$path()
# the path is not closed; close it
ellpath4 <- rbind(ellpath4, ellpath4[1,]) %>% as.data.frame() %>%  cbind(lab = "skup 2")


# Graf...
qmethod(dataset = dataFix, nfactors = 2, rotation = "none")$loa  %>%
  rownames_to_column(var = "lab") %>% 
  ggplot(aes(x=f1, y=f2, label = lab)) +
  geom_point(color = "red", size = 5, alpha =0.4) +
  # geom_text(check_overlap = TRUE) +
  geom_abline(slope = 0, intercept = 0, col = "blue") +
  geom_path(aes(x = x, y = y), data = as.data.frame(ellpath3), color = "green") +
  geom_path(aes(x = x, y = y), data = as.data.frame(ellpath4), color = "green") +
  labs(x = "N�rodn�", y = "Spiritu�ln�") +
  scale_y_continuous(breaks=c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8)) +
  scale_x_continuous(breaks=seq(0.3,0.9,0.2)) +
  coord_cartesian(xlim = c(0.3, 0.8), ylim = c(-0.6, 0.7)) +
  theme_light() 


# Graf... Prohod�me osy - to se bude v�c hodit do �l�nku kdy� to bude "na le�ato"
qmethod(dataset = dataFix, nfactors = 2, rotation = "none")$loa  %>%
  rownames_to_column(var = "lab") %>% 
  ggplot(aes(y=f1, x=f2, label = lab)) +
  geom_point(color = "red", size = 5, alpha =0.4) +
  # geom_text(check_overlap = TRUE) +
  geom_vline(xintercept = 0, col = "blue") +
  geom_path(aes(x = y, y = x), data = as.data.frame(ellpath3), color = "green") +
  geom_path(aes(x = y, y = x), data = as.data.frame(ellpath4), color = "green") +
  labs(y = "N�rodn�", x = "Spiritu�ln�") +
  scale_x_continuous(breaks=c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8)) +
  scale_y_continuous(breaks=seq(0.3,0.9,0.2)) +
  coord_cartesian(ylim = c(0.3, 0.8), xlim = c(-0.6, 0.7)) +
  theme_light() 

# SUPER! Ney�le��! Po��d dost�v�me strukturu 6 + 6 + 14! Jen je to pooto�en�...
  ggsave("respondentiNerotovano2.png", 
       width = 6, height = 2.6, dpi = 300)
  


dt = qmethod(vz28r(stab = stb23r, stri = str22r), nfactors = 2, rotation = "cluster")[["loa"]] %>%
  rownames_to_column(var = "lab") 
dt[c(15, 18),]
# dtFix = dt
# saveRDS(dtFix, "dtFix.rds", ascii = TRUE)
# dtFix = readRDS("dtFix.rds")

# define an ellipses
ell <- Ellipse$new(center = c(-0.1,0.75), rmajor = 0.25, rminor = 0.15, alpha = 150)
# ellipse as a path
ellpath1 <- ell$path()
# the path is not closed; close it
ellpath1 <- rbind(ellpath1, ellpath1[1,]) %>% as.data.frame() %>%  cbind(lab = "skup 1")

# define an ellipses
ell <- Ellipse$new(center = c(0.25,0.45), rmajor = 0.15, rminor = 0.15, alpha = 45)
# ellipse as a path
ellpath2 <- ell$path()
# the path is not closed; close it
ellpath2 <- rbind(ellpath2, ellpath2[1,]) %>% as.data.frame() %>%  cbind(lab = "skup 2")


qg = ggplot(dtFix, aes(x=f1, y=f2, label = lab)) +
  geom_point(color = "red", size = 5, alpha =0.4) +
  geom_text(check_overlap = FALSE) +
  geom_abline(slope = 1, intercept = 0, col = "blue") +
  geom_path(aes(x = x, y = y), data = as.data.frame(ellpath1), color = "green") +
  geom_path(aes(x = x, y = y), data = as.data.frame(ellpath2), color = "green") +
  labs(x = "Sekul�rn�", y = "Spiritu�ln�") +
  scale_y_continuous(breaks=seq(-0.3,0.9,0.3)) +
  scale_x_continuous(breaks=seq(-0.3,0.9,0.3)) +
  theme_light()
qg
qg + coord_cartesian(xlim = c(0.1, 0.6), ylim = c(0.1, 0.6))
qg + coord_cartesian(xlim = c(-0.3, 0.2), ylim = c(0.6, 1))
qg + coord_cartesian(xlim = c(-0.3, 0.9), ylim = c(-0.3, 0.9))

ggsave("respondenti.png",
       (qg + coord_cartesian(xlim = c(-0.3, 0.9), ylim = c(-0.3, 0.9))),
       width = 5, height = 5, dpi = 300)

read.dta('A.dta')
read.dta('A.dta')[c(10, 13), ]


## Test jestli maj� v re�ln�ch datech vybran� n�bo�ensk� v�roky v�t�� SD ne� nen�bo�ensk�
# pok = vz28r() %>% as.data.frame() %>% mutate(SD = 0, MAD = 0)
anals = 1000
SDs = matrix(1:(48 * anals), nrow = anals )
MADs = matrix(1:(48 * anals), nrow = anals )
Ms = matrix(1:(48 * anals), nrow = anals )
for (i in 1:anals) {
  pok = vz28r()
  for (j  in 1:48) {
    SDs[i, j] = pok[j, ] %>% sd()
    MADs[i, j] = pok[j, ] %>% mad()
    Ms[i, j] = pok[j, ] %>% mean() 
  }
}

P�ehled = data.frame(#ID = vNames,
  Zn�n� =  str_trunc(Zn�n�, 90),
  M = Ms %>% as.data.frame() %>% summarise_all(mean) %>% round(digits = 3) %>% as.matrix() %>% as.vector(),
  SD = SDs %>% as.data.frame() %>% summarise_all(mean) %>% round(digits = 3) %>% as.matrix() %>% as.vector(), 
  MAD = MADs %>% as.data.frame() %>% summarise_all(mean) %>% round(digits = 3) %>% as.matrix() %>% as.vector())
arrange(P�ehled, M)
arrange(P�ehled, SD)[30:48,]
arrange(P�ehled, MAD)



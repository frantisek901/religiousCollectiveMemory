#### Replikace analýzy náboženské pamìti pro èasopis Socilógia

## Vytvoøil: FrK 2020-11-05
## Upravil:  FrK 2020-11-27

## Encoding: windows-1250

# Zámìr a cíl:
# ------------
# Chci zreplikovat ještì jednou pøímo v R celou analýzu Q-metody, a to vèetnì náhodného
# výbìru 5 mladých žen z 22. Uvidíme, jestli to dopadne stejnì jako mix Stata-R.
# Pak chci testovat rozdíl mezi dvìma zpùsoby výpoètu rozdílu mezi faktory/subjektivitami.
# Tj. zda za 1500 analýz spoèítat prùmìrné hodnoty faktorù a pak spoèítat razdíly,
# nebo zda spoèítat 1500 jednotlivých rozdílù, a pak z nich spoèítat prùmìr.
# ------------

# Hlavièka
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

# Potøebné funkce, hodnoty a objekty:
## Hodnoty
RPFs = readRDS('RPF_2020-11-05.rds')  # Simulované výsledky - Rozdíly prùmìrných faktorù
PRFs = readRDS('PRF_2020-11-05.rds')  # Simulované výsledky - Prùmìrné rozdíly faktorù
RPFsbr = readRDS('RPFbezRotace_2020-11-27.rds')  # Simulované výsledky - Rozdíly prùmìrných faktorù
PRFsbr = readRDS('PRFbezRotace_2020-11-27.rds')  # Simulované výsledky - Prùmìrné rozdíly faktorù
stb23r = read.dta('A.dta') %>% select(4:51)  # Stabilních 23 respondentù 
str22r = read.dta('B.dta') %>% select(4:51)  # 22 mladých žen, ze kterých vybereme 5
vzorky = 1500  # Poèet náhodných vzorkù

## Jména výrokù a respondentù
vNames = c(paste0("V0", 1:9), paste0("V", 10:48))
rNames = c(paste0("R0", 1:9), paste0("R", 10:28))

# Znìní výrokù
Znìní =  c('1. Kníže Václav se zasloužil o rozšíøení køesanství, žil zbožnì a mravnì.',
           '2. Kníže Václav byl a je vnímán jako symbol èeského patriotismu.',
           '3. Husité byli vlastnì pøedchùdci komunistù.',
           '4. Husitství je pokládáno za specificky èeskou formu køesanství.',
           '5. Mistr Jan Hus byl tragicky nepochopený reformátor církve.',
           '6. Jan Hus patøil k významným evropským intelektuálùm své doby.',
           '7. Jan Ámos Komenský byl jednou z nejvýraznìjších osobností staré Jednoty bratrské.',
           '8. Jan Ámos Komenský byl hlavnì pedagogem, a v tom je pro svìt nejcennìjší.',
           '9. Václav Havel byl neoficiálním nejvyšším pøedstavitelem necírkevní duchovnosti.',
           '10. Václav Havel je symbolem toho, že zdánlivì bezmocní mohou porazit mocné.',
           '11. Cyril a Metodìj pøinesli víru a evangelium.',
           '12. Význam mise Cyrila a Metodìje byl hlavnì politický.',
           '13. Karel IV. hledal rozumnou dohodu s církví, a církev hledala rozumnou dohodu s ním.',
           '14. Karel IV. stavìl katedrálu ve jménu národa.',
           '15. Tomáš Garrigue Masaryk byl èlovìk hluboce vìøící, ale pøesnì rozlišoval mezi vírou a církví.',
           '16. Tomáš Garrigue Masaryk je dovršitel našeho národního obrození.',
           '17. Klement Gottwald udìlal mnoho dobrého pro lidi.',
           '18. Klement Gottwald se snažil o slušné vztahy s øímskokatolickou církví.',
           '19. Bílá hora je symbolem porážky, potupy a neštìstí.',
           '20. Katolíci se po Bílé hoøe zaèali k jinovìrcùm chovat nebývale krutì.',
           '21. Odsun Nìmcù ze Sudet byla chyba.',
           '22. Dnes navrátíme majetek církvi a zítra šlechtì. Odtud je už jen krùèek k navrácení Sudet.',
           '23. V listopadu 1989 jedna moc padla a jiné se zaèaly prát o dìdictví.',
           '24. Po listopadu 1989 církve nevyužily zájmu o náboženství, tak nevídaného pro ateistickou zemi.',
           '25. Pražský hrad nemá být dotèen restitucemi.',
           '26. Pražský hrad je památník zašlé moci.',
           '27. Vánoce jsou oslavou toho, že Kristus, který se narodil, pøišel jako Boží dar.',
           '28. Vánoce jsou v poslední dobì stále komerènìjší.',
           '29. Velikonoce jsou v Èesku vnímány jako oslava pøíchodu jara, a i jako pøíležitost k dobrému jídlu a pití.',
           '30. Velikonoce jsou kombinací obyèejù z pohanských dob a køesanských tradic.',
           '31. Èeská pravoslavná obec byla v protektorátu rozpuštìna, majetek zkonfiskován, chrámy zapeèetìny a bohoslužby zakázány.',
           '32. Protektorát pro Èechy znamenal vyhánìní z pohranièí, zatýkání, popravy, ponižování, pokoøování, nejhlubší porobu, likvidaci inteligence.',
           '33. Nový rok už odedávna vyvolával v lidech pocit, že mají šanci ve svém životì nìco zlepšit.',
           '34. Nový rok je pro nás jedna z nejvìtších slavností církevního roku, slavnost Matky Boží.',
           '35. Jan Nepomucký má mezi vìøícími velkou váhu.',
           '36. Jan Nepomucký velí nejen v pøíhodný èas mlèet, ale také mluvit, pokud je to zapotøebí.',
           '37. Jan Žižka bojoval za èistší církev.',
           '38. Jan Žižka jako úspìšný vojevùdce a politik patøí k nejvìtším postavám èeských dìjin.',
           '39. Karel Havlíèek Borovský pøiøkl jezuitùm negativní nádech.',
           '40. Karel Havlíèek Borovský byl vtipný básník, schopný obchodník a zastánce svobody podnikání.',
           '41. František Drtikol byl vždy èlovìkem práce, praxe a obdivuhodné umírnìnosti.',
           '42. František Drtikol byl prùkopníkem buddhismu u nás.',
           '43. Bìžný èlovìk si pod pojmem jezuita u nás vybaví pøedevším Antonína Koniáše.',
           '44. Antonín Koniáš pálil jen literární brak.',
           '45. Jan Zrzavý byl hluboce vìøící èlovìk, pøíležitostnì sám sebe ztotožòoval s postavou umuèeného Krista.',
           '46. Jan Zrzavý byl významný èeský malíø.',
           '47. Jan Neruda nebyl žádný antisemita, který by nenávidìl židy a židovství.',
           '48. Jan Neruda je jedním z pilíøù novodobé èeské národní literatury.')
n = 'Náb'
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



# Samotná analýza
## Zaèátek analýzy
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
  
## Zpracování výsledkù do objektu 'Prùmìrných rozdílù faktorù' (PRFr)
names(factor1) = vNames
names(factor2) = vNames
rozdil = abs(factor1 - factor2) %>% gather("var", "v") %>% group_by(var) %>%  summarise(v = mean(v))
PRFr = rozdil$v %>% round(digits = 3)
  
## Zpracování výsledkù do objektu 'Rozdílù prùmìrnych faktorù' (RPFr)
pf1 = factor1 %>% gather("var", "v") %>% group_by(var) %>%  summarise(v = mean(v)) %>% select(2)
pf2 = factor2 %>% gather("var", "v") %>% group_by(var) %>%  summarise(v = mean(v)) %>% select(2)
RPFr = abs(pf1$v - pf2$v) %>% round(digits = 3)
  
## Srovnání pøístupù:
soucetOdchylek = abs(PRFr - RPFr) %>% sum()

## Konec analýzy a rozdíl èasù
konec = Sys.time()
konec - start

# Uložení dat
data.frame(RPF = RPFr, PRF = PRFr) %>% 
  saveRDS(., file = 'realneRozdilyBezRotace_2020-11-27.rds', ascii = TRUE)  # POZOR! Bez rotace!
 

# Porovnání Bez/Rotace
bezRotace = readRDS('realneRozdilyBezRotace_2020-11-27.rds')
sRotaci = readRDS('realneRozdily_2020-11-05.rds')
abs(bezRotace - sRotaci) %>% rownames_to_column(var = "Výrok") %>% 
  mutate(Výrok = as.numeric(Výrok)) %>% 
  ggplot(aes(x = Výrok, y = PRF, label = Výrok)) +
  geom_point(size = 2, col = "red") +
  geom_label_repel() +
  scale_x_continuous(breaks=seq(1,48,2)) +
  scale_y_continuous(breaks=seq(0,2.4,0.2)) +
  theme_light()

srovnani = data.frame('Bez rotace' = bezRotace$PRF, 
                      'S rotací' = sRotaci$PRF, 
                      Rozdíl = abs(bezRotace$PRF - sRotaci$PRF), 
                      'Znìní' = str_trunc(Znìní, 90), 
                      check.names = FALSE) %>% 
  rownames_to_column(var = "Výrok") %>% 
  mutate(Výrok = str_pad(Výrok, 2, 'left', '0')) %>% mutate(Výrok = paste0('V', Výrok)) 
 
srovnani %>% ggplot(aes(x=`S rotací`, y = `Bez rotace`, label = Výrok)) +
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

srovnani %>% arrange(Rozdíl)
srovnani %>% arrange(`Bez rotace`)
srovnani %>% arrange(`S rotací`)

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
ggplot(data = data.frame(Sekulární = pf1$v, Spirituální = pf2$v, L = vNames), 
       aes(x = Sekulární, y = Spirituální, label = L)) +
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
  labs(x = "Národní") +
  theme_light()  
ggsave(filename = "vyrokyNerotovane2.png",
       width = 6.27, height = 6.27, dpi = 300)

data.frame(Sekulární = pf1$v %>% round(digits = 2), 
           Spirituální = pf2$v  %>% round(digits = 2), 
           Rozdíl = abs(pf1$v - pf2$v) %>% round(digits = 2), L = vNames) %>% 
  arrange(Rozdíl)



# Boxplot: 6 promìnných
all = data.frame('Rozdíl Prùmìrù (rot)' = sample(RPFs, 1000), 
                 'Prùmìr Rozdílù (rot)' = sample(PRFs, 1000), 
                 'Reálné rozdíly (rot)' = c((sRotaci %>% as.matrix()), rep(NA, (1000 - 96))),
                 'Rozdíl Prùmìrù (bez)' = sample(RPFsbr, 1000), 
                 'Prùmìr Rozdílù (bez)' = sample(PRFsbr, 1000), 
                 'Reálné rozdíly (bez)' = c((bezRotace %>% as.matrix()), rep(NA, (1000 - 96))),
                 check.names = FALSE) %>% 
  pivot_longer(cols = 1:6, names_to = 'Zdroj', values_to = 'Rozdíl') %>% filter(!is.na(Rozdíl)) %>% 
  group_by(Zdroj)
ggplot(data = all, aes(x = Zdroj, y = Rozdíl, fill = Zdroj, col = Zdroj)) +
  geom_boxplot(alpha = 0.2, width = 0.2) + 
  geom_jitter(alpha = 0.1, size = 2, width = 0.1, height = 0) +
  theme_classic()
table(all$Zdroj)  
# Na pohled se zdá, že prùmìrné rozdíly se neliší podle toho, jestli je to spoèteno s/bez rotace.

## T-Test, jestli se prùmìrné rozdíly liší s/bez rotace
t.test((PRFs %>% as.vector()), (PRFsbr %>% as.vector()))  # Prùmìr rozdílù
# t.test((RPFs %>% as.vector()), (RPFsbr %>% as.vector()))  # Rozdíl prùmìrù

## K-S test, jestli se dvì distribuce liší
ks.test((PRFs %>% as.vector() %>% sample(5000)), 
        (PRFsbr %>% as.vector() %>% sample(5000)))  # S výbìrem to ukazuje shodu distribucí
ks.test((PRFs %>% as.vector()), 
        (PRFsbr %>% as.vector()))  # Bez výbìru ne - je to kvùli shodám, zøejmì...

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

# Boxplot: 2 promìnné s/bez rotací
all = data.frame('Simulace (rot)' = sample(PRFs, 1000), 
                 'Simulace (bez)' = sample(PRFsbr, 1000), 
                 'Data (rot)' = c(sRotaci$PRF, rep(NA, (1000 - 48))),
                 'Data (bez)' = c(bezRotace$PRF, rep(NA, (1000 - 48))),
                 check.names = FALSE) %>% 
  pivot_longer(cols = 1:4, names_to = 'Zdroj', values_to = 'Rozdíl') %>% filter(!is.na(Rozdíl)) %>% 
  group_by(Zdroj)
ggplot(data = all, aes(x = Zdroj, y = Rozdíl, fill = Zdroj, col = Zdroj)) +
  geom_boxplot(alpha = 0.1, width = 0.2) + 
  geom_jitter(alpha = 0.1, size = 3, width = 0.08, height = 0) +
  theme_classic()
table(all$Zdroj)  


# Boxplot: 2 promìnné 
all = data.frame('Simulace' = sample(PRFs, 1000), 
                 'Data' = c(PRFr, rep(NA, (1000 - 48)))) %>% 
  pivot_longer(cols = 1:2, names_to = 'Zdroj', values_to = 'Rozdíl') %>% filter(!is.na(Rozdíl)) %>% 
  group_by(Zdroj)
ggplot(data = all, aes(x = Zdroj, y = Rozdíl, fill = Zdroj, col = Zdroj)) +
  geom_boxplot(alpha = 0.1, width = 0.2) + 
  geom_jitter(alpha = 0.1, size = 3, width = 0.08, height = 0) +
  theme_classic()
table(all$Zdroj)  


all %>% filter(Zdroj == "Simulace") %>% filter(Rozdíl > 4.1) %>% nrow()
((PRFs >= 4.1) %>% sum()) / 480

# V kolika simulacích z 1000 je shoda stejna jako v reálných datech? 
x = as.data.frame(PRFs)
names(x) = vNames
x$shoda = rep(NA, (1000))
for (i in 1:1000) {
  row = x[i,1:48]
  x[i, 'shoda'] = (row <= 1.31) %>% sum(na.rm = TRUE)
}
x$rozdíl = rep(NA, (1000))
for (i in 1:1000) {
  row = x[i,1:48]
  x[i, 'rozdíl'] = (row >= 4.1) %>% sum(na.rm = TRUE)
}
summary(x$shoda)
summary(x$rozdíl)
t = t.test(x$rozdíl, mu = 5)

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

# CIs pro jednotlivé výroky
rozdilFaktoru = abs(factor1 - factor2)
CIrf = 1:96 %>% matrix(ncol = 2) %>% as.data.frame()
for (i in 1:48) {
  t = t.test(as.vector(rozdilFaktoru[, i]), mu = 4)
  CIrf[i, ] = t[['conf.int']][1:2]
}
CIrf[17,] = c(0,0) 

# V reálných datech je 5 výrokù, které mají rozdíl vìtší než 4 body, kolik simulací je na tom stejnì?
((x$rozdíl >= 5) %>% sum()) / 10
# Hmm... 20%, to není nic moc...

# Jak moc velká náhoda je, že z 5 výrokù je 5 náboženských?
pok = rep(NA, 100000)
for (i in 1:100000) {
  pok[i] = c(rep(1, 24), rep(0, 24)) %>% sample(5) %>% sum()
}
((pok==5) %>% sum()) / 1000


# Test jestli záleží na typu rotace
data = vz28r(stab = stb23r, stri = str22r) # Zmrazíme si data
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
  labs(x = "Národní", y = "Spirituální") +
  scale_y_continuous(breaks=c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8)) +
  scale_x_continuous(breaks=seq(0.3,0.9,0.2)) +
  coord_cartesian(xlim = c(0.3, 0.8), ylim = c(-0.6, 0.7)) +
  theme_light() 


# Graf... Prohodíme osy - to se bude víc hodit do èlánku když to bude "na ležato"
qmethod(dataset = dataFix, nfactors = 2, rotation = "none")$loa  %>%
  rownames_to_column(var = "lab") %>% 
  ggplot(aes(y=f1, x=f2, label = lab)) +
  geom_point(color = "red", size = 5, alpha =0.4) +
  # geom_text(check_overlap = TRUE) +
  geom_vline(xintercept = 0, col = "blue") +
  geom_path(aes(x = y, y = x), data = as.data.frame(ellpath3), color = "green") +
  geom_path(aes(x = y, y = x), data = as.data.frame(ellpath4), color = "green") +
  labs(y = "Národní", x = "Spirituální") +
  scale_x_continuous(breaks=c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8)) +
  scale_y_continuous(breaks=seq(0.3,0.9,0.2)) +
  coord_cartesian(ylim = c(0.3, 0.8), xlim = c(-0.6, 0.7)) +
  theme_light() 

# SUPER! Neyáleží! Poøád dostáváme strukturu 6 + 6 + 14! Jen je to pootoèené...
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
  labs(x = "Sekulární", y = "Spirituální") +
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


## Test jestli mají v reálných datech vybrané náboženské výroky vìtší SD než nenáboženské
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

Pøehled = data.frame(#ID = vNames,
  Znìní =  str_trunc(Znìní, 90),
  M = Ms %>% as.data.frame() %>% summarise_all(mean) %>% round(digits = 3) %>% as.matrix() %>% as.vector(),
  SD = SDs %>% as.data.frame() %>% summarise_all(mean) %>% round(digits = 3) %>% as.matrix() %>% as.vector(), 
  MAD = MADs %>% as.data.frame() %>% summarise_all(mean) %>% round(digits = 3) %>% as.matrix() %>% as.vector())
arrange(Pøehled, M)
arrange(Pøehled, SD)[30:48,]
arrange(Pøehled, MAD)



## Correlation entre revenus et résultats 

<h1> Introduction </h1>
 <br/>
 
<p> La question de l’égalité des chances se pose depuis déjà un bon moment au sein de l’éducation nationale, le second principe de la devise de notre République, prônant l’égalité des chances (ou des résultats) au sein d’enfants issues de strates sociales différentes. L’éducation nationale s’attelle déjà à cette tâche depuis des années maintenant, mais quelles en sont les résultats ?
Ce projet a pour but d'analyser les résultats au baccalauréat des récentes années. (2012-2018)  </p>

<h1> Préparation des données </h1>

<p> Dans ce projet, nous avons étudié les données des BACs de 2012 à 2019 en cherchant à les relier aux revenus des foyers ainsi qu'à leur niveau.

Chargement des librairies utile à notre étude.
```{r Chargement des librairies, message=FALSE, warning=FALSE, include=TRUE}
library(corrplot)
library(zoom)
library(gplots)
library(psy)
library(FactoMineR)
library(prettyR)
```

Chargement du jeu de donnée des résultats aux BACs de 2012 à 2019 issue de l'INSEE (voir source)
```{r ouverture du jeu de donnée du bac}
donnee.bac <- read.csv2("fr-en-indicateurs-de-resultat-des-lycees-denseignement-general-et-technologique.csv") 
```
Nous avons utilisé un jeu de donnée proposé par l’INSEE sur les BAC de 2012 à 2019, 2019 compris, mais pas complet, car les épreuves n’avaient pas encore eu lieu à l’époque. (voir SOURCE)


Chargement du jeu de donnée des revenus par départements issue  journal du net (voir source)
```{r ouverture du jeu des revenus par département}
code.et.revenu <- read.csv("code et revenu.csv")
```
Les revenus moyen par département on été récupéré par nos soins via les données de journal du net et mis en page pour nous faciliter leur utilisation. (voir SOURCE)



Stockage du revenu par département dans le fichier principal : donnee.bac
```{r stockage revenu derniere ligne, echo=TRUE, message=FALSE, warning=FALSE, paged.print=TRUE}
if(!("revenu" %in% colnames(donnee.bac))){ #on vérifie que la colonne 
                                           #n'existe pas déjà dans le dataframe
donnee.bac$revenu <- rep(NA,16210)
i<-0
ii<-0

for(i in seq(1,16210,1))
{
  for(ii in seq(1,101,1))
  {
    if(is.na(donnee.bac$revenu[i]) & donnee.bac$Code.dÃ.partement[i]==code.et.revenu$Code[ii]){
      
      donnee.bac$revenu[i]<-code.et.revenu$revenu[ii]
    }
  }
}

write.csv2(donnee.bac, 
           "fr-en-indicateurs-de-resultat-des-lycees-denseignement-general-et-technologique.csv",
           row.names = FALSE, col.names = TRUE)
}
```

Stockage du revenu par département dans un data frame secondaire avec taux de mentions des séries générales : code.et.revenu.ex
```{r stockage revenu et  taux de mentions des séries générales, echo=TRUE, message=FALSE, warning=FALSE, paged.print=TRUE}
revenu <- read.csv2("revenu.et.taux.csv")
if(is.na(donnee.bac$revenu))
  {
revenu<-data.frame(departement=levels(donnee.bac$DÃ.partement),revenu=rep(0,101),mentionS=rep(0,101),mentionES=rep(0,101),mentionL=rep(0,101))
revenu$revenu<-c(2433,1825,1800,1887,2233,1880,1793,1689,1975,1710,1872,2254,2092,2047,1769,1887,1996,1903,1894,1965,2185,1982,1631,1952,1789,2194,1985,2531,2064,2150,2041,1878,1839,2150,1389,1424,2380,1780,2272,1846,1815,1848,2805,1924,1940,1802,3349,1942,2166,1805,2121,2243,2031,1976,2045,1958,2218,2161,1870,1802,1764,2025,1942,2213,1499,1992,789,2051,1851,2055,2053,1793,1934,2156,1825,3417,1779,2057,2110,1756,1458,2375,1966,1981,2216,2370,2022,1760,1895,1870,1801,2094,2449,2288,2106,1887,2020,1961,1836,1934,3178)


#on regarde les moyennes par departement 
total<-rep(0,101)
tot<-rep(0,101)
for(i in seq(1,16210,1)){
  for(ii in seq(1,101,1)){
    if(revenu$departement[ii] == donnee.bac$DÃ.partement[i] & !is.na(revenu$departement[ii]) & !is.na(donnee.bac$Taux_Mention_brut_serie_S[i])){
      total[ii]=total[ii]+donnee.bac$Taux_Mention_brut_serie_S[i]*donnee.bac$Effectif.PrÃ.sents.sÃ.rie.S[i]/100
      tot[ii]=tot[ii]+donnee.bac$Effectif.PrÃ.sents.sÃ.rie.S[i]
    }
  }
}
#calcul du taux de mentions par département en S
revenu$mentionS=total/tot

total<-rep(0,101)
tot<-rep(0,101)
for(i in seq(1,16210,1)){
  for(ii in seq(1,101,1)){
    if(revenu$departement[ii] == donnee.bac$DÃ.partement[i] & !is.na(revenu$departement[ii]) & !is.na(donnee.bac$Taux_Mention_brut_serie_ES[i] )){
      total[ii]=total[ii]+donnee.bac$Taux_Mention_brut_serie_ES[i]*donnee.bac$Effectif.PrÃ.sents.sÃ.rie.ES[i]/100
      tot[ii]=tot[ii]+donnee.bac$Effectif.PrÃ.sents.sÃ.rie.ES[i]
    }
    
  }
  
  
}
#calcul du taux de mentions par département en ES
revenu$mentionES=total/tot

total<-rep(0,101)
tot<-rep(0,101)
for(i in seq(1,16210,1)){
  for(ii in seq(1,101,1)){
    if(revenu$departement[ii] == donnee.bac$DÃ.partement[i] & !is.na(revenu$departement[ii]) & !is.na(donnee.bac$Taux_Mention_brut_serie_L[i] )){
      total[ii]=total[ii]+donnee.bac$Taux_Mention_brut_serie_L[i]*donnee.bac$Effectif.PrÃ.sents.sÃ.rie.L[i]/100
      tot[ii]=tot[ii]+donnee.bac$Effectif.PrÃ.sents.sÃ.rie.L[i]
    }
    
  }
  
  
}
#calcul du taux de mentions par département en L
revenu$mentionL=total/tot

donnee.bac$revenu<-rep(0,16210)
donnee.bac$numero<-c()
i<-0
ii<-0
for(i in seq(1,16210,1)){
  for(ii in seq(1,101,1)){
    if(donnee.bac$DÃ.partement[i]==revenu$departement[ii]){
      donnee.bac$revenu[i]<-revenu$revenu[ii]
    }
  }
} 
}
```

La partie cartographique a elle été réalisé essentiellement à l’aide de l’outil proposé par l’INSEE pour effectuer de la cartographies à différentes échelles au niveau de la France et de ses DOM-TOM, en lui soumettant des données classé selon l’échelle choisie :  régional, départementale...
</p>
 <br/>
 
<h1> Education: différences et similarité globales  </h1> 
 <br/>


<p>Notre beau territoire, modeste mais suffisamment  vaste pour observer des différences spatiale. </p>

<img src = "map par anamorphose des effectifs présent en terminal.png" title = "map par anamorphose des effectifs present en terminal" alt = "carte effectif présent">

carte par anamorphose des effectifs par departement

<p> Une carte par anamorphose est plus parlante que de simple gradient de couleurs quand on veut comparer des différence de taille. </p>


### Par Département

Taux de mentions par Départemenent en série générale

```{r taux mention dept, echo=FALSE}

plot(donnee.bac$Taux_Mention_brut_serie_S~donnee.bac$LibellÃ..dÃ.partement,xlab=NA, ylab = "taux de mention brut série S" ,notch =FALSE, horizontal = FALSE,cex.axis=0.4, las=2, col=4,)

plot(donnee.bac$Taux_Mention_brut_serie_ES~donnee.bac$LibellÃ..dÃ.partement,xlab=NA, ylab = "taux de mention brut série ES" ,notch =FALSE, horizontal = FALSE,cex.axis=0.4, las=2, col=3)

plot(donnee.bac$Taux_Mention_brut_serie_L~donnee.bac$LibellÃ..dÃ.partement,xlab=NA, ylab = "taux de mention brut série L" ,notch =FALSE, horizontal = FALSE,cex.axis=0.4, las=2, col=2)
```


Pour ce qui est des département, les cartes sont plus efficaces pour donner une vue d'ensemble des disparité.


## {-}

Des box plots, ont été utilisées pour pouvoir comparer à la fois les niveaux moyens(médian) et le degré auquel varie le niveau au sein d’une académie 

<br/>
<p>
<img src = "map revenu mensuel dept.png" 
  title = "map revenu mensuel par dept" 
  alt = "revenu mensuel"
  />
  
  carte revenu mensuel par departement
  
  <br/>
  
<img src = "map taux de mention.png"
 title = "map taux de mention par dept" 
 alt = "taux de mentions"
 />
 
  carte taux de mentions par departement 
 
  <br/>
</p>

<br/>

<p>
Des cartes avec gradients de couleurs ont été utilisé d’une part parce qu'une carte est utile pour représenter des données dans l’espace et d’autre parce que l’anamorphose n’aurait pas beaucoup de sens étant donnée que l’on parle de revenue par personne et d’un ratio (taux de mention) </p>
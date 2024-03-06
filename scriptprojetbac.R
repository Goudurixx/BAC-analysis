library(corrplot)
library(zoom)
library(gplots)
library(psy)
library(FactoMineR)
library(prettyR)
library(ggplot2)


donnee.bac <- read.csv2("fr-en-indicateurs-de-resultat-des-lycees-denseignement-general-et-technologique.csv")
code.et.revenu <- read.csv("code et revenu.csv")

if(!("revenu" %in% colnames(donnee.bac))){"yep it's there"}

annee_t<-rep(0,8); annee=2012;
while(annee<=2019){annee_t[annee-2011]=annee; annee=annee+1;}



#ici je calcule les proportions de lycées prive public et au total

cont_pu<-rep(0,8); cont_pr<-rep(0,8);  nb_lycee<-rep(0,8);
annee=2012;
while (annee<=2019) {
  i=1;
  while(i<=16210){
     if(!is.na(fr_en_indicateurs_de_resultat_des_lycees_denseignement_general_et_technologique$`Effectif Présents série S`[i])){
        if(fr_en_indicateurs_de_resultat_des_lycees_denseignement_general_et_technologique$`Secteur Public/Prive`[i]=="PU" & fr_en_indicateurs_de_resultat_des_lycees_denseignement_general_et_technologique$Année[i]==annee){
          cont_pu[annee-2011]<- cont_pu[annee-2011] +1;
        }
        if(fr_en_indicateurs_de_resultat_des_lycees_denseignement_general_et_technologique$`Secteur Public/Prive`[i]=="PR" & fr_en_indicateurs_de_resultat_des_lycees_denseignement_general_et_technologique$Année[i]==annee){
          cont_pr[annee-2011]<- cont_pr[annee-2011] +1;
        }
    }   
    i = i +1;
    
  }
  nb_lycee[annee-2011]=  cont_pr[annee-2011]+ cont_pu[annee-2011];
  annee=annee+1;

}

cont_pu; cont_pr; nb_lycee;

#effectif et taux de réussite par année

annee=2012; effectif<-rep(0,8); taux_reu<-rep(0,8);
while (annee<=2019) {
  i=1;
  while(i<=16210){
    if(!is.na(fr_en_indicateurs_de_resultat_des_lycees_denseignement_general_et_technologique$`Effectif Présents série S`[i])& fr_en_indicateurs_de_resultat_des_lycees_denseignement_general_et_technologique$Année[i]==annee){
      effectif[annee-2011] =   effectif[annee-2011] + fr_en_indicateurs_de_resultat_des_lycees_denseignement_general_et_technologique$`Effectif Présents série S`[i];
      taux_reu[annee-2011] =   taux_reu[annee-2011] + fr_en_indicateurs_de_resultat_des_lycees_denseignement_general_et_technologique$`Taux Brut de Réussite série S`[i];
    
    }
    i = i +1;
  }
  taux_reu[annee-2011] =  taux_reu[annee-2011]/ nb_lycee[annee-2011];
  annee=annee+1;
}

plot(annee_t,effectif); plot(annee_t,nb_lycee); plot(annee_t,taux_reu);
barplot(effectif,axisnames=TRUE,cex.names=annee_t)






#ici je calcule le taux de mention moyen par départements en série S

code<-c(code.et.revenu) #je récupère les données des revenu par départements
tauxmentionmedian<-rep(0,101) #je créé un vecteur pour recevoir les taux médians
i<-1 
j<-1 
tauxmention<-rep(NA,16210)
while(j<=101)
  {
      while (i<=16210) {
        if(code.et.revenu$Code[j] == donnee.bac$Code.dÃ.partement[i] 
           & !is.na(donnee.bac$Taux_Mention_brut_serie_S[i]) )
          {
          tauxmention[i]<-donnee.bac$Taux_Mention_brut_serie_S[i]
          }
        i = i +1;
      }
      tauxmentionmedian[j]<-median(tauxmention,TRUE)
      i<-1;
      tauxmention<-rep(NA,16210)
      j = j +1;
  }


code.et.revenu$taux_mention<-tauxmentionmedian

#ici je calcule l'effectif en terminale par départements en tout

code<-c(code.et.revenu) #je récupère les données des code des  départements
effectif<-rep(0,101) #je créé un vecteur pour recevoir  effectifs
i<-1 
j<-1 
while(j<=101)
{
  while (i<=16210) {
    if(code.et.revenu$Code[j] == donnee.bac$Code.dÃ.partement[i] 
       & !is.na(donnee.bac$Effectif.de.terminale[i]) )
    {
      effectif[j]<-effectif[j] + donnee.bac$Effectif.de.terminale[i]
    }
    i = i +1;
  }
  i<-1;
  j = j +1;
}


code.et.revenu$effectif<-effectif








donnee.bac$revenu<-rep(0,16210)
i<-0
ii<-0
for(i in seq(1,16210,1))
{
  for(ii in seq(1,101,1))
  {
    if(donnee.bac$Code.dÃ.partement[i]==code.et.revenu$Code[ii]){
      donnee.bac$revenu[i]<-code.et.revenu$revenu[ii]
    }
  }
}

#effectif par département
donnee.bac$revenu<-rep(0,16210)
i<-0
ii<-0
for(i in seq(1,16210,1))
{
  for(ii in seq(1,101,1))
  {
    if(donnee.bac$Code.dÃ.partement[i]==code.et.revenu$Code[ii]){
      donnee.bac$revenu[i]<-code.et.revenu$revenu[ii]
    }
  }
}


v <- rep(0,8)
v[1]<-cor(x=donnee.bac$Taux_Mention_brut_serie_L,y=donnee.bac$revenu,use="pairwise.complete.obs","pearson")
v[2]<-cor(x=donnee.bac$Taux_Mention_brut_serie_ES,y=donnee.bac$revenu,use="pairwise.complete.obs")
v[3]<-cor(x=donnee.bac$Taux_Mention_brut_serie_S,y=donnee.bac$revenu,use="pairwise.complete.obs")
v[4]<-cor(x=donnee.bac$Taux_Mention_brut_serie_STI2D,y=donnee.bac$revenu,use="pairwise.complete.obs")
v[5]<-cor(x=donnee.bac$Taux_Mention_brut_serie_STD2A,y=donnee.bac$revenu,use="pairwise.complete.obs")
v[6]<-cor(x=donnee.bac$Taux_Mention_brut_serie_STMG,y=donnee.bac$revenu,use="pairwise.complete.obs")
v[7]<-cor(x=donnee.bac$Taux_Mention_brut_serie_STL,y=donnee.bac$revenu,use="pairwise.complete.obs")
v[8]<-cor(x=donnee.bac$Taux_Mention_brut_serie_ST2S,y=donnee.bac$revenu,use="pairwise.complete.obs")



eff.region<-table(donnee.bac$LibellÃ..rÃ.gion)

nom.region<-levels(donnee.bac$LibellÃ..rÃ.gion)

par(mfrow=c(2,3))
aux<-data.frame(Academie=names(table(donnee.bac$AcadÃ.mie)))
#effectifs par series par academie
str(aux)   
aux$L<-rep(0,11)
par(mfrow=c(1,1)) #mise en page
for(ii in seq(1,31,1)){#specifi? avec le seq(a,b,1) quelle academie prendre
  x<-rep(0,12)
  for(i in seq(2,11,1)){
    str(ii)
    x[i-1]<-aux[ii,i]
  }
  barplot(x,names.arg=c("L","ES","S","STG","STI2D","ST2A","STMG","STI","STL","ST2S","musique/dance","hotellerie"),main=aux$Academie[ii],cex.names=0.8)
}





#◘boite à moustache par académie de toute les années taux brute de réussite
boxplot(fr_en_indicateurs_de_resultat_des_lycees_denseignement_general_et_technologique$`Taux Brut de Réussite série S`~fr_en_indicateurs_de_resultat_des_lycees_denseignement_general_et_technologique$Académie,ylab="taux de mention",xlab="academie",tl.cex=0.2,horizontal=TRUE)

revenue<-data.frame(departement=levels(fr_en_indicateurs_de_resultat_des_lycees_denseignement_general_et_technologique$Département),revenue=c(2433,1825,1800,1887,2233,1880,1793,1689,1975,1710,1872,2254,2092,2047,1769,1887,1996,1903,1894,1965,2185,1982,1631,1952,1789,2194,1985,2531,2064,2150,2041,1878,1839,2150,1389,1424,2380,1780,2272,1846,1815,1848,2805,1924,1940,1802,3349,1942,2166,1805,2121,2243,2031,1976,2045,1958,2218,2161,1870,1802,1764,2025,1942,2213,1499,1992,789,2051,1851,2055,2053,1793,1934,2156,1825,3417,1779,2057,2110,1756,1458,2375,1966,1981,2216,2370,2022,1760,1895,1870,1801,2094,2449,2288,2106,1887,2020,1961,1836,1934,3178))

plot(code.et.revenu$Libellé,code.et.revenu$revenu)

write.csv2(code.et.revenu, "code et revenu et taux mention.csv")
    



















revenue<-data.frame(departement=levels(donnee.bac$DÃ.partement),revenue=rep(0,101),mentionS=rep(0,101),mentionES=rep(0,101),mentionL=rep(0,101))
revenue$revenue<-c(2433,1825,1800,1887,2233,1880,1793,1689,1975,1710,1872,2254,2092,2047,1769,1887,1996,1903,1894,1965,2185,1982,1631,1952,1789,2194,1985,2531,2064,2150,2041,1878,1839,2150,1389,1424,2380,1780,2272,1846,1815,1848,2805,1924,1940,1802,3349,1942,2166,1805,2121,2243,2031,1976,2045,1958,2218,2161,1870,1802,1764,2025,1942,2213,1499,1992,789,2051,1851,2055,2053,1793,1934,2156,1825,3417,1779,2057,2110,1756,1458,2375,1966,1981,2216,2370,2022,1760,1895,1870,1801,2094,2449,2288,2106,1887,2020,1961,1836,1934,3178)


#on regarde les moyennes par departement 
total<-rep(0,101)
tot<-rep(0,101)
for(i in seq(1,16210,1)){
  for(ii in seq(1,101,1)){
    if(revenue$departement[ii] == donnee.bac$DÃ.partement[i] & !is.na(revenue$departement[ii]) & !is.na(donnee.bac$Taux_Mention_brut_serie_S[i])){
      total[ii]=total[ii]+donnee.bac$Taux_Mention_brut_serie_S[i]*donnee.bac$Effectif.PrÃ.sents.sÃ.rie.S[i]/100
      tot[ii]=tot[ii]+donnee.bac$Effectif.PrÃ.sents.sÃ.rie.S[i]
    }
  }
}
#calcul du taux de mentions par département en S
revenue$mentionS=total/tot

total<-rep(0,101)
tot<-rep(0,101)
for(i in seq(1,16210,1)){
  for(ii in seq(1,101,1)){
    if(revenue$departement[ii] == donnee.bac$DÃ.partement[i] & !is.na(revenue$departement[ii]) & !is.na(donnee.bac$Taux_Mention_brut_serie_ES[i] )){
      total[ii]=total[ii]+donnee.bac$Taux_Mention_brut_serie_ES[i]*donnee.bac$Effectif.PrÃ.sents.sÃ.rie.ES[i]/100
      tot[ii]=tot[ii]+donnee.bac$Effectif.PrÃ.sents.sÃ.rie.ES[i]
    }
    
  }
  
  
}
#calcul du taux de mentions par département en ES
revenue$mentionES=total/tot

total<-rep(0,101)
tot<-rep(0,101)
for(i in seq(1,16210,1)){
  for(ii in seq(1,101,1)){
    if(revenue$departement[ii] == donnee.bac$DÃ.partement[i] & !is.na(revenue$departement[ii]) & !is.na(donnee.bac$Taux_Mention_brut_serie_L[i] )){
      total[ii]=total[ii]+donnee.bac$Taux_Mention_brut_serie_L[i]*donnee.bac$Effectif.PrÃ.sents.sÃ.rie.L[i]/100
      tot[ii]=tot[ii]+donnee.bac$Effectif.PrÃ.sents.sÃ.rie.L[i]
    }
    
  }
  
  
}
#calcul du taux de mentions par département en L
revenue$mentionL=total/tot


donnee.bac$revenue<-rep(0,16210)
donnee.bac$numero<-c()
i<-0
ii<-0
for(i in seq(1,16210,1)){
  for(ii in seq(1,101,1)){
    if(donnee.bac$DÃ.partement[i]==revenue$departement[ii]){
      donnee.bac$revenue[i]<-revenue$revenue[ii]
    }
  }
}    


p<-rep(NA,16210)
p[which(donnee.bac$Secteur.Public.Prive=="PU")]<-0
p[which(donnee.bac$Secteur.Public.Prive=="PR")]<-1
cor(p,donnee.bac$Taux_Mention_attendu_serie_S,use="pairwise.complete.obs") #correlation entre revenue du departement et le fais d'etre privée

donnee.bac$priv


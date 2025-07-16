#question 1: D suit loi uniform U(6) E(D)=3.5 et V(D)=35/12
#question 2 :
De.faces<-function(N)
{
  return(sample(1:6, size = N, replace = TRUE))
}
#question 3 
echantillon <- De.faces(1000)
# Tracer l'histogramme
hist(echantillon, main = "Histogramme de l'échantillon", xlab = "Valeurs",
ylab = "Fréquence", col = "lightblue", border = "black")
#question4:
moy.emp<-function(X)
{
  return((1/length(X))*sum(X))
}
#question 5:
echantillon_2<-De.faces(10000)
s=moy.emp(echantillon_2)
print(s)#s=3.4999 est tres proches de E(X) 
#question 6:
N=1000
De.Moyennes<-function(n, N)
{
  moyennes <- numeric(N)
  for(i in 1:N )
  {
    D_i<-De.faces(n)
    D_n=moy.emp((D_i))
    moyennes[i]=D_n
  }
  return(moyennes)
}
#question 7 : 
n=100
moyennes<-De.Moyennes(n,N)
hist(moyennes, main = "Histogramme de moyennes ", xlab = "Valeurs", ylab = "Fréquence", col = "lightblue", border = "black")

#question 8 : 
# Superposer la courbe de la loi normale
mu <- 3.5
sigma <- sqrt(35/1200)
x <- seq(min(moyennes), max(moyennes), length = 1000)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y * N * diff(hist(moyennes, col = "lightblue",border = "black")$breaks[1:2]), col = "red", lwd = 2)
# Test statistique
shapiro.test(moyennes)
#question 9 : oui P(S=13)=1/6^3 
#question 10 : 
JeuDeDes<-function(N)
{
  scores=numeric(N)
  for(i in 1:N)
  {
    score<-0
    resultat=De.faces(1)# Lancer un dé
    while(resultat==6)
    {
      score=score+resultat #Ajouter le résultat au score
      resultat=De.faces(1) #Répéter le lancement de Dé
    }
    score=score+resultat
    scores[i]=score
    
  }
  return(scores)
}
#question 11 
N=10000
scores<-JeuDeDes(N)
hist(scores, main = "Histogramme de scores", xlab = "scores", ylab = "Fréquence", col = "lightblue", border = "black")

#question 12 
freq=0
for (i in 1:N) 
{
  if(scores[i]==13)
  {
    freq=freq+1
  }
}
freq_emp=freq/N
#freq_emp=0.0041
  
#question 13 
esp=sum(scores)/N
f=0
for (i in 1:N) 
{
  if(scores[i]>=10)
  {
    f=f+1
  }
}
freq_emp2=f/N
print(freq_emp2)

  









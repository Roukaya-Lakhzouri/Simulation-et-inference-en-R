#question 1 : 
Lp=c(19.5,22.1,21.5,21.9,22.0,21.0,22.3,21.0,20.3,20.9,22.0,22.0,23.8,21.2,21.0)
Lg=c(22.35,23.9,20.8,25.8,25.0,24.0,23.8,21.7,22.8,23.1,23.5,23.0,23.0,23.1)
# Test de normalité pour Lp
mu <- mean(Lp)
sigma <-sd(Lp)
x <- seq(min(Lp), max(Lp), length = 1000)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y * length(Lp)* diff(hist(Lp,
                                   main = "Histogramme de l'échantillon Dont des nids de petite taille ", 
                                   xlab = "tailles des œufs", ylab = "Fréquence", col = "lightblue",
                                   border = "black")$breaks[1:2]), col = "red", lwd = 2)

# Tests statistiques
shapiro.test(Lp)
#W = 0.95149, p-value = 0.5841
# Test de normalité pour Lg graphiquement 
mu2 <- mean(Lg)
sigma2 <-sd(Lg)
x <- seq(min(Lg), max(Lg), length = 1000)
y <- dnorm(x, mean = mu2, sd = sigma2)
lines(x, y * length(Lg)* diff(hist(Lg, main = "Histogramme de
                                   l'échantillon Dont des nids de grand taille ",
                                   xlab = "tailles des œufs", ylab = "Fréquence", 
                                   col = "lightblue", border = "black")$breaks[1:2]), col = "red", lwd = 2)
# Test statistique
shapiro.test(Lg)
#W = 0.97064, p-value = 0.3973

#question 2 
var.test(Lp,Lg)
#p_value=0.4676642

#question3
t.test(Lp,Lg,var.equal = T)




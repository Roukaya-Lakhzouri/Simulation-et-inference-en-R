score_av<-c(5 ,2, 8, 9, 5, 3, 25, 8, 8, 4, 1, 2)
score_ap<-c( 6 ,2 ,9 ,10 ,6 ,1 ,3 ,10 ,6 ,7 ,2 ,10)
diff <- score_ap - score_av
#difference
#hist(difference,prob=T)
#curve(dnorm(x,mean(difference),sd(difference)),add = T,col='blue',lwd=3)
mu <- mean(score_av)
sigma <-sd(score_ap)
x <- seq(min(score_av), max(score_av), length = 1000)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y * length(score_av)* diff(hist(diff, main = "Histogramme de l'échantillon des scores de différence  " , xlab = "scores", ylab = "Fréquence", col = "lightblue", border = "black") $breaks[1:2]), col = "red", lwd = 2)
shapiro.test(diff)


wilcox.test(score_ap, score_av, paired = T)$p.value


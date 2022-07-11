hut <- read.table("asfr_hutterites_raw.txt", skip = 3, header = F)
hut <- t(hut[-c(1,3,7,8,9),-1])
ages <- c(17, 22, 27, 32, 37, 42, 47)
plot(ages,hut[,1]/1000, ylim = c(0, 0.5), xlim = c(10, 55))
lines(ages,hut[,2]/1000)
lines(ages,hut[,3]/1000)
lines(ages,hut[,4]/1000)

hut <- apply(hut[,-3], 1, mean, na.rm = T)/1000

lines(ages, hut, col = "red")

sm_hut <- smooth.spline(c(10:15,ages,55), c(rep(0,6),hut,0))
lines(predict(sm_hut, 10:55), col = "orange")

sm_asfr <- as.data.frame(cbind(predict(sm_hut, 10:55)$x,
                 round(predict(sm_hut, 10:55)$y,2)))
names(sm_asfr) <- c("age", "fx")                 
write.csv(sm_asfr, file = "asfr_hutterites.csv", row.names = F)     

######

kappa <- .2
alpha <- 30
age <- 14:50
phi <- 0.2

phi_age <- function(age, phi, alpha, kappa){
  
  out <- phi / (1 + exp(kappa*(age - alpha)))
  return(out)
  
}

plot(age, sapply(14:50, phi_age, phi, alpha, kappa))


g_shape <- 5
g_rate <- 35
mean(rgamma(1000, g_shape, g_rate))
hist(rgamma(1000, g_shape, g_rate))

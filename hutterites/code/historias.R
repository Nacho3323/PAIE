#################################################################################
# Simulación de Procesos Demográficos ALAP 2020                                 #
# Daniel Ciganda - Diciembre 2020                                               # 
# Lab 3                                                                         #      
# Modelos generativos del proceso reproductivo                                  #
#################################################################################
library(ggplot2)

# Ejemplo simulacion evento a partir de probabilidad
runif(1)<0.9
runif(1)<0.1

# funciones 
plot_hst <- function(dat, ylim, n, save = F, legend = T){
  p <- ggplot(dat, aes(x = edad, y = id)) +
    geom_point(aes(colour = paridad), size = 2)+
    scale_colour_gradient(low = "orange", high = "red")+
    xlim(c(0,60)) + 
    ylim(c(ylim)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA))+
    theme(panel.border = element_blank(),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"))+
    theme(axis.text.y = element_blank(),
          axis.ticks = element_blank())+
    ylab(NULL) + xlab("Edad")
  for (i in 1:n) {p <- p + geom_segment(x=10, y=i, xend=50, yend=i)}
  if(save){
    pdf(file.path("..","..","imagenes", "rep_lines.pdf"), width=4, height=4) 
    print(p + theme(legend.position = "none"))
    dev.off()
  }
  if(!legend){
    p <- p + theme(legend.position = "none")
  }
  return(p)
}
plot_cum_fec <- function(dat, ylim, n, save = F){
  p <- ggplot(dat, aes(x = edad, y = cum_fec)) +
    geom_point(size = 2)+ 
    xlim(c(0,60)) + 
    ylim(c(ylim)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + 
    ylab("Fecundidad Acumulada") + xlab("Edad")
  if(save){
    pdf(file.path("..","..","imagenes", "cum_fec.pdf"), width=4, height=4) 
    print(p)
    dev.off()
  }
  p
}
plot_fx <- function(dat){
edad <- 0:50
dat <- merge(dat, as.data.frame(edad), by = "edad", all = T)
dat$cum_nac <- cumsum(!is.na(dat$id))
dat$cum_fec <- dat$cum_nac/max(dat$id, na.rm = T)
inferior <- dat[dat$edad %in% seq(10,49,1),c("edad", "cum_fec")]
superior <- dat[dat$edad %in% seq(11,50,1),c("edad", "cum_fec")]
tasas_edad <- superior$cum_fec - inferior$cum_fec 
plot(10:49, tasas_edad)
cat(paste("TFR:", sum(tasas_edad)), "\n")
}

plot_asfr <- function(dat, anios, type, save = F){
  dat[dat$Age == "12-", "Age"] <- "12"
  dat[dat$Age == "55+", "Age"] <- "55"
  dat[,2] <- as.numeric(dat[,2])
  dat <- dat[dat$Year %in% anios, c(1,2,3)]  
  names(dat) <- c("Año","edad","dat")
  
  p <- ggplot(dat, aes(x = edad, y = dat,
                       group = as.factor(Año),
                       colour = Año))
  if(type=="points"){
    p <- p + geom_point(size = 2)    
  }else{
    p <- p + geom_line()
  }
  p <- p +  scale_colour_gradient(low = "orange", high = "red")+
    theme_bw() +
    ylab("f(x)") + xlab("Edad")+
    theme(legend.position = c(0.85, 0.7),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) +
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA))
  
  if(save){
    pdf(file.path("..","..","imagenes", "asfr.pdf"), width=6, height=6) 
    print(p)
    dev.off()
  }
  p
}
plot_tfr <- function(dat, save = F){
  
  dat <- dat[dat$Year >= 1960, c(1,2)]
  names(dat) <- c("Año", "TFR")
 p <- ggplot(dat, aes(x = Año, y = TFR,
                  colour = Año)) +
    geom_line(size = 0.9)+
    scale_colour_gradient(low = "orange", high = "red")+
    theme_bw()+
   ylim(0.5,3.5) +
   theme(legend.position = c(0.85, 0.7),
                        legend.title = element_text(size = 12),
                        legend.text = element_text(size = 10),
         plot.background = element_rect(fill = "transparent",colour = NA))
  if(save){
    pdf(file.path("..","..","imagenes", "tfr.pdf"), width=6, height=6) 
    print(p)
    dev.off()
  }
  p
}

# plots asfr, tfr
fx_es <- read.table(file.path("..","datos","asfrs_hfd.txt"), skip = 2, header=T, stringsAsFactors = F)
plot_asfr(fx_es, anios = 1960:2016, type = "lines")

tfr_es <- read.table(file.path("..","datos","tfr_hfd.txt"), skip = 2, header = T, stringsAsFactors = F)
plot_tfr(tfr_es)

# generando historias reproductivas de una cohorte de mujeres observadas 
# desde la edad 10 a la edad 50, sin truncamiento
n <- 8
nac <- c(2,1,2,4,1,3,2,1)
edades <- c(34,37.2,38.5,21.8,24.3,26.5,28.5,32.3,
         38.3,20.1,21.5,23.4,27.7,22.4,25.6,15.9)

hst <- as.data.frame(cbind(id = rep(1:n, nac),
                   nac = rep(nac, nac),
                   edad = edades,
                   paridad = sequence(nac)))

edad <- 10:50
hst <- merge(hst, as.data.frame(edad), by = "edad", all= T)

plot_hst(hst, c(0.5, n), n)
  
# fecundidad acumulada a edad E(x) 
hst$cum_nac <- cumsum(!is.na(hst$nac))
hst$cum_fec <- hst$cum_nac/n

plot_cum_fec(hst, c(0, 2.2), n)

# tasas de fecundidad por edad f(x1, x2)
inferior <- hst[hst$edad %in% seq(10,49,1),c("edad", "cum_fec")]
superior <- hst[hst$edad %in% seq(11,50,1),c("edad", "cum_fec")]
cbind(inferior, superior)

fx <- superior$cum_fec - inferior$cum_fec 
cbind(inferior, superior, fx)

plot(edad[-length(edad)], fx)
TFR <- sum(fx)

# Cómo podemos generar unas historias reproductivas similares a las observadas en 
# distintos contextos

# Modelos del proceso reproductivo

# 1er nacimiento
# Método Gini
n <- 10^6
# fecundabilidad: probabilidad mensual de concebir en ausencia de prácticas anticonceptivas
fi <- 0.2 
m <- 9:30
gini_props <- (1 - fi)^(m-9) * fi
gini <- cbind(meses = m, gini_props)
plot(gini)

# generando meses hasta el primer hijo a partir de una 
# distribución binomial negativa
meses <- rnbinom(n, 1, fi) + 9
nbinom <- as.data.frame(prop.table(table(meses)))
names(nbinom)[2] <- "nbinom_props"
con <- merge(gini, nbinom, by = "meses", all = T)
con <- con[con$meses %in% m,]

plot(con$meses, con$gini_props)
points(con$meses, con$nbinom_props, col = "red")

# múltiples nacimientos
n <- 8
edades_union <- runif(n, 14*12, 30*12)
ns <- 6
fi <- 0.2
meses_1 <- rnbinom(n, 1, fi) + 9
edades_1 <- (edades_union + meses_1)

hst <- as.data.frame(cbind(id = 1:n,
                           edad = edades_1/12,
                           paridad = 1))

plot_hst(hst, c(0.5, n), n)

meses_2 <- rnbinom(n, 1, fi) + 9
edades_2 <- (ns + edades_1 + meses_2)

hst <- as.data.frame(cbind(id = rep(1:n, 2),
                           edad = c(edades_1, edades_2)/12,
                           paridad = rep(1:2,each = n)))

plot_hst(hst, c(0.5, n), n)

# función para generar historias reproductivas
gen_hst <- function(n, fi, ns){
  
  wt_u <- runif(n, 14*12, 20*12)
  
  meses <- lapply(1:50, function(x) rnbinom(n, 1, fi) + 9)
  
  wt_p <- list()
  wt_p[[1]] <- wt_u + meses[[1]]
  
  hst <- list()
  hst[[1]] <- as.data.frame(cbind(id = 1:n,
                             edad = wt_p[[1]]/12,
                             paridad = 1))
  
  for(i in 2:50){
    
    wt_p[[i]] <- ns + wt_p[[i-1]] + meses[[i]]
    
    nid <- which(wt_p[[i]]>50*12)
    
    wt_p[[i]][nid] <- NA 
    
    if(sum(is.na(wt_p[[i]])) == n){break}
    
    hst[[i]] <- as.data.frame(cbind(id = rep(1:n,i),
                               edad = unlist(wt_p)/12,
                               paridad = rep(1:i, each = n)))
    hst[[i]] <- hst[[i]][!is.na(hst[[i]]$edad),]
  }

   return(hst)
  
}
    
ls_hst <- gen_hst(n = 8, fi = 0.2, ns = 12)  
p <- lapply(ls_hst, function(x) plot_hst(x, c(0.5, n),n, legend = F))
dev.off()
print(p, newpage = F)

# n 
ls_hst <- gen_hst(n = 1000, fi = 0.2, ns = 12)

# plot f(x) - fi constante
plot_fx(ls_hst[[length(ls_hst)]])

# modelo con fi_i,t
# Relación Coale and Trussell (1974) 
df <- cbind(age = c(10,25,30,35,40,49)*12,
            series= c(100,94,86,70,36,5)/100)
sm <- smooth.spline(df)
plot(predict(sm, seq(10*12, 50*12,1)))

# ajustamos una función logística
# L/(1 + exp(-r*(x-x0)))

x0 <- 38*12      # punto de inflexión
r  <- 0.022      # tasa
max_fec <- 1      
age <- seq(10*12, 50*12,1)

lines(age, max_fec / (1 + exp(r*(age-x0))), col = "red", lwd = 2)

# Modelo con fecundabilidad dependiente de t
gen_hst_hf <- function(n, ns, x0, r, wt_u_b, id = vector(), wt_c = vector()){
  
  edad <- 1:600
  
  # fecundabilidad
  fi_t <- 0.2 / (1 + exp(r*(edad-x0)))
  fi_t[589:600] <- 0
  #plot(edad,fi_t)
  
  # tiempo de espera a la unión
  wt_u <- round(rlnorm(n, meanlog = log(wt_u_b), sdlog = 0.15),5)
  #plot(prop.table(table(round(rlnorm(10^6, meanlog = log(240), sdlog = 0.15),0))))
  
  fi_it <- lapply(1:n, function(x) fi_t[wt_u[x]:length(fi_t)])
  
  maxt <- max(sapply(fi_it, length))
  
  for(t in 1:maxt){ 
    
    is <- which(runif(n) < sapply(fi_it, function(x) x[t])) # nr id 
    
    if(length(is)!=0){ 
      
      wts <- sapply(is, function(x) (wt_u[x]-1) + t) # si hay concepcion
      
      id <- c(id, is) 
      wt_c <- c(wt_c, wts)
      
      fi_it[is] <- lapply(fi_it[is], function(x){x[t:(t+9+ns)] <- NA; return(x)})
      
    }
    
  }
  
  data <- as.data.frame(cbind(id = id, wt_c = wt_c))
  data <- data[order(data$id),]
  hst <- as.data.frame(cbind(id = data$id,
                             edad = (data$wt_c + 9)/12,
                             nac = rep(table(data$id), table(data$id)),
                             paridad = sequence(table(data$id))))
  return(hst)
}

hst <- gen_hst_hf(n = 1000, ns = 6, x0 = x0, r = r, wt_u_b = 25*12)

#plot_hst(hst, c(0.5, n),n)
plot_fx(hst)
plot_asfr(fx_es, anios = 1922, type = "lines", save = F)

# Número Deseado de Hijos 
# cargar datos del "International Value Survey"
obs_d <- read.table(file.path("..", "datos", "d_1990_ivs.csv"))
head(obs_d)
table(obs_d)
table(obs_d$ideal_nr, useNA = "always")

#
obs_d <- as.data.frame(table(obs_d$ideal_nr), stringsAsFactors = F)
names(obs_d) <- c("nr","freq")

# graficar frequencias
plot(x = obs_d$nr, y = obs_d$freq, type = "p")

# graficar probs.
plot(obs_d$nr, obs_d$freq/sum(obs_d$freq), type = "p")

# parametros de la distribucion Gamma
alpha <- 5.9 # shape
beta <- 2.3 # rate

sim_d <- round(rgamma(n = 1000, shape = alpha, rate = beta),0)
table(sim_d)

# frequencias
plot(table(sim_d), type = "p")

# probabilidades
plot(obs_d$nr, obs_d$freq/sum(obs_d$freq), type = "p")
points(table(sim_d)/sum(table(sim_d)), type = "p",
       ylim = c(0, 0.5), xlim = c(0,10), col = "red")

gen_hst_d <- function(n, ns, x0, r, wt_u_b, alpha, beta, c, id = vector(), wt_c = vector()){
  
  edad <- 1:600
  
  # fecundabilidad
  fi_t <- 0.2 / (1 + exp(r*(edad-x0)))
  fi_t[589:600] <- 0
  #plot(edad,fi_t)
  
  # tiempo de espera a la unión
  wt_u <- round(rlnorm(n, meanlog = log(wt_u_b), sdlog = 0.15),5)
  #plot(prop.table(table(round(rlnorm(10^6, meanlog = log(240), sdlog = 0.15),0))))
  
  fi_it <- lapply(1:n, function(x) fi_t[wt_u[x]:length(fi_t)])
  
  sim_d <- round(rgamma(n, shape = alpha, rate = beta),0)
  
  maxt <- max(sapply(fi_it, length))
  
  for(t in 1:maxt){ 
    
    is <- which(runif(n) < sapply(fi_it, function(x) x[t])) # nr id 
    
    if(length(is)!=0){ 
      
      wts <- sapply(is, function(x) (wt_u[x]-1) + t) # si hay concepcion
      
      id <- c(id, is) 
      wt_c <- c(wt_c, wts)
      
      fi_it[is] <- lapply(fi_it[is], function(x){x[t:(t+9+ns)] <- NA; return(x)})
      
      b_i <- sapply(is, function(x) length(which(id==x)))
      
      id_max_d <- is[which(b_i >= sim_d[is])]
      
      if(length(id_max_d)>0){
      
      fi_it[id_max_d] <- lapply(fi_it[id_max_d], function(x) x*c)
      
      }
      
    }
    
  }
  
  # data
  hst <- as.data.frame(cbind(id = id, wt_c = wt_c))
  hst <- hst[order(hst$id),]
  hst <- as.data.frame(cbind(id = hst$id,
                             edad = (hst$wt_c + 9)/12,
                             nac = rep(table(hst$id), table(hst$id)),
                             paridad = sequence(table(hst$id))))
  return(hst)
}

hst <- gen_hst_d(n = 1000, ns = 6, x0 = x0, r, wt_u_b = 22*12, alpha, beta, c = 1)

#plot_hst(hst, c(0.5, n),n)
plot_fx(hst)
plot_asfr(fx_es, anios = 1922, type = "lines", save = F)


library(stringr)
library(sos)
library(ggplot2)
library(tidyverse)

# Cargo los datos

setwd("C:\\Users\\Diego\\Desktop\\Estadistica (NACHO)\\Grupo de investigacion Actuarial-Demografico\\PAIE\\CAMPOP")
CAMPOP <- read.delim("26ParishesReconstitutions_ALL_DATA.txt", sep = "\t", header = TRUE)
names(CAMPOP)


# datos relevantes 

df <- CAMPOP[,c("marriages_parfrf", "wives_BirthDate", "children_ChildNumber", "children_BirthDate")]
names(df)
df <- df[!(df$wives_BirthDate==""),]
df <- df[!(df$children_BirthDate==""),]
df$wives_BirthDate <- as.numeric(str_sub(df$wives_BirthDate, -4, -1)) # se queda solo con el año de nacimiento de las mujeres
df$children_BirthDate <- as.numeric(str_sub(df$children_BirthDate, -4, -1)) # se queda solo con el año de nacimiento de los hijos

table(df$children_ChildNumber, useNA = "ifany") # 0 NA
df %>% count(children_ChildNumber)

# Frecuencia de entradas para cada registro de matrimonio
Freqs <- as.data.frame(table(df$marriages_parfrf)) # indica la cantidad de hijos por mujer
colnames(Freqs) <- c(colnames(df)[1], "lb_total")
df <- merge(df, Freqs, by="marriages_parfrf", all=T)

# edad de la madre al nacimiento

df$m_ageb <- paste0((df$children_BirthDate-df$wives_BirthDate),"_",df$nrc) # mothers age at birth

colnames(df) <- c("ID", "m_yob", "nrc", "c_yob", "lb_total", "m_ageb")
# ID, mother's year of birth, nr of child (order), child's year of birth, live births total, age of mother at birth of child

# list with cohort dfs
lc <- list()
asfr_list <-  list() # esto es la tasa de fecundidad por edad por cohorte?
TFR_list <- list() # esto es la tasa global de fecundidad por cohorte?
m_ageb <- 10:50

# mellizos los cuenta como dos nacimientos

for (i in unique(df$m_yob)){
  dfg <- grepFn(i, df, column="m_yob") # se toma la cantidad de mujeres por cohorte
  n <- length(unique(dfg$ID)) # numero de mujeres por cohorte
  print(n)
  dfg <- merge(dfg, as.data.frame(m_ageb), by = "m_ageb", all=T) # add all ages
  dfg$cum_lb_total <- cumsum(!is.na(dfg$lb_total)) # fetirlidad acumulada a la edad x E(x)
  dfg$cum_fec <- dfg$cum_lb_total/n
  lc[[as.character(i)]] <- as.data.frame(dfg) # dfs in list
  
  # tasa de fertilidad por edad
  inferior <- dfg[dfg$m_ageb %in% seq(10,49,1),c("m_ageb", "cum_fec")]
  superior <- dfg[dfg$m_ageb %in% seq(11,50,1),c("m_ageb", "cum_fec")]
  cbind(inferior, superior)
  fx <- superior$cum_fec - inferior$cum_fec
  fxdf <- cbind(inferior, superior, fx)
  fxdf$cohort <- i
  colnames(fxdf) <- c("m_agebi", "cum_feci", "m_agebs", "cum_fecs", "fx", "cohort")
  asfr_list[[as.character(i)]] <- fxdf
  
  TFR_list[[as.character(i)]] <- as.data.frame(cbind(i, sum(fx)))
} 

# plot
for (i in 1:length(asfr_list)){
  p <- ggplot(asfr_list[[i]], aes(x=m_agebi, y=fx)) + geom_point() + ggtitle(unique(asfr_list[[i]]$cohort))
  print(p)
  }


# mellizos como un nacimiento

asfr_list_nt <- list()

for (i in unique(df$m_yob)){
  dfg <- grepFn(i, df, column="m_yob") # cohort df
  dfg <- merge(dfg, as.data.frame(m_ageb), by = "m_ageb", all=T) # agregar para todas las edades
  n <- length(unique(dfg$colfam_nr)) # numero de mujeres por cohorte
  dfg$cum_lb_total <- cumsum(!is.na(dfg$lb_total)) # fertilidad acumulada a la edad x E(x)
  dfg$cum_fec <- dfg$cum_lb_total/n
  lc[[as.character(i)]] <- as.data.frame(dfg) # dfs in list
  
  # tasa de fertilidad por edad f(x1, x2)
  inferior <- dfg[dfg$m_ageb %in% seq(10,49,1),c("m_ageb", "cum_fec")]
  superior <- dfg[dfg$m_ageb %in% seq(11,50,1),c("m_ageb", "cum_fec")]
  cbind(inferior, superior)
  fx <- superior$cum_fec - inferior$cum_fec
  fxdf <- cbind(inferior, superior, fx)
  fxdf$cohort <- i
  colnames(fxdf) <- c("m_agebi", "cum_feci", "m_agebs", "cum_fecs", "fx", "cohort")
  asfr_list_nt[[as.character(i)]] <- fxdf
} 

# plot
# comparar asfr_list_nt con gemelos como dos nacimientos y como uno

for (i in 1:length(asfr_list)){
  p <- ggplot(asfr_list[[i]], aes(x=m_agebi, y=fx)) + geom_point() + ggtitle(unique(asfr_list[[i]]$cohort))
  p_nt <- ggplot(asfr_list_nt[[i]], aes(x=m_agebi, y=fx)) + geom_point() + ggtitle(paste0(unique(asfr_list_nt[[i]]$cohort),"_nt"))
  print(grid.arrange(p, p_nt))
}

# cohorte 1740
ggplot(asfr_list[[1]], aes(x=m_agebi, y=fx)) + geom_point() + ggtitle(unique(asfr_list[[1]]$cohort))

#cohorte 1666
ggplot(asfr_list[[223]], aes(x=m_agebi, y=fx)) + geom_point() + ggtitle(unique(asfr_list[[223]]$cohort))  
asfr_list[[223]]

library(stringr)
library(sos)
library(ggplot2)
library(tidyverse)

# Cargo los datos ----

setwd("C:\\Users\\mrocc\\OneDrive\\Desktop\\Facultad\\PAIE\\PAIE\\CAMPOP")
CAMPOP <- read.delim("26ParishesReconstitutions_ALL_DATA.txt", sep = "\t", header = TRUE)
names(CAMPOP)


# Datos relevantes ---- 

df <- CAMPOP[,c("marriages_parfrf", "wives_BirthDate", "children_ChildNumber", "children_BirthDate")]
names(df)

df %>% count(wives_BirthDate=="") # saca de la base de datos las fecha de nacimiento de las mujeres vacias
df <- df[!(df$wives_BirthDate==""),]

df %>% count(children_BirthDate=="") # Saca las fecha de nacimiento que estan vacias
df <- df[!(df$children_BirthDate==""),]

df$wives_BirthDate <- as.numeric(str_sub(df$wives_BirthDate, -4, -1)) # se queda solo con el año de nacimiento de las mujeres
df$children_BirthDate <- as.numeric(str_sub(df$children_BirthDate, -4, -1)) # se queda solo con el año de nacimiento de los hijos

table(df$children_ChildNumber, useNA = "ifany") # 0 NA, indica la cantidad de mujeres que tuvieron tantos hijos
df %>% count(children_ChildNumber)

# Frecuencia de entradas para cada registro de matrimonio ----

Freqs <- as.data.frame(table(df$marriages_parfrf))# indica la cantidad de hijos por mujer
df %>% count(marriages_parfrf)

colnames(Freqs) <- c(colnames(df)[1], "lb_total") # agrega columna con la cnatidad de hijos por mujer
df <- merge(df, Freqs, by="marriages_parfrf", all=T)

# lb_total: indica la cantidad de veces que figura en ID de las mujeres

# Edad de la madre al nacimiento ----

#####################
# Sacamos mujeres que registro de hijos fuera distinto de la cantidad que hay en la table4a
a <- df %>%
  group_by(marriages_parfrf) %>% 
  summarise(n = max(children_ChildNumber))

df <- left_join(df, a, by = "marriages_parfrf")

df <- df %>% 
  mutate(m = n - lb_total) %>%
  filter(m == 0) %>% 
  select(-c("m", "n"))

##################
df$m_ageb <- paste0((df$children_BirthDate-df$wives_BirthDate),"_",df$nrc) # edad de la madre en los nacimiento de sus hijos

colnames(df) <- c("ID", "m_yob", "nrc", "c_yob", "lb_total", "m_ageb")
# DNI, año de nacimiento de la madre, nº de hijo (pedido), año de nacimiento del hijo, 
# total de nacidos vivos, edad de la madre al nacimiento del hijo

# lista con cohorte dfs
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
  # print(grid.arrange(p, p_nt))
}

# cohorte 1740
ggplot(asfr_list[[1]], aes(x=m_agebi, y=fx)) + geom_point() + ggtitle(unique(asfr_list[[1]]$cohort))


##############

# Filtrar cohortes que tienen por lo menos 2 mujeres 

x <- df %>%
  select(ID, m_yob) %>% 
  group_by(m_yob, ID) %>% 
  count() %>% 
  filter(n >= 2)

df1 <- left_join(df, x, by = c("ID", "m_yob")) %>% 
  filter(!is.na(n)) %>% 
  select(-"n") 

lc1 <- list()
asfr_list1 <-  list() # esto es la tasa de fecundidad por edad por cohorte?
TFR_list1 <- list() # esto es la tasa global de fecundidad por cohorte?
m_ageb <- 10:50

# mellizos los cuenta como dos nacimientos

for (i in unique(df1$m_yob)){
  dfg1 <- grepFn(i, df1, column="m_yob") # se toma la cantidad de mujeres por cohorte
  n1 <- length(unique(dfg1$ID)) # numero de mujeres por cohorte
  print(n1)
  dfg1 <- merge(dfg1, as.data.frame(m_ageb), by = "m_ageb", all=T) # add all ages
  dfg1$cum_lb_total <- cumsum(!is.na(dfg1$lb_total)) # fetirlidad acumulada a la edad x E(x)
  dfg1$cum_fec <- dfg1$cum_lb_total/n1
  lc1[[as.character(i)]] <- as.data.frame(dfg1) # dfs in list
  
  # tasa de fertilidad por edad
  inferior1 <- dfg1[dfg1$m_ageb %in% seq(10,49,1),c("m_ageb", "cum_fec")]
  superior1 <- dfg1[dfg1$m_ageb %in% seq(11,50,1),c("m_ageb", "cum_fec")]
  cbind(inferior1, superior1)
  fx1 <- superior1$cum_fec - inferior1$cum_fec
  fxdf1 <- cbind(inferior1, superior1, fx1)
  fxdf1$cohort <- i
  colnames(fxdf1) <- c("m_agebi", "cum_feci", "m_agebs", "cum_fecs", "fx", "cohort")
  asfr_list1[[as.character(i)]] <- fxdf1
  
  TFR_list1[[as.character(i)]] <- as.data.frame(cbind(i, sum(fx)))
} 

# plot
for (i in 1:length(asfr_list1)){
  p1 <- ggplot(asfr_list1[[i]], aes(x=m_agebi, y=fx)) + geom_point() + ggtitle(unique(asfr_list1[[i]]$cohort))
  print(p1)
}

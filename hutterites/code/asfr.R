library(sos); library(data.table); library(ggplot2); library(grid); library(gridExtra); library(data.table)

# asfr per cohort

#####

# read in data
setwd("C:\\Users\\mrocc\\OneDrive\\Desktop\\Facultad\\PAIE\\PAIE\\hutterites")
famc <- readRDS("data/family_card.rds")
lbc <- readRDS("data/livebirth_card.rds")

## births data
df <- lbc[,c("deck_nr", "colfam_nr", "m_yob", "yb")] 
df %>%  select(colfam_nr) %>% unique() %>%  dim() # Hay 677 mujeres
# deck_nr: twins have a card for each child but identical deck_nr
  #          counts as one confinement but 2 LB
table(df$deck_nr, useNA = "ifany") # cantidad de hijos
df$nrc <- ifelse(df$deck_nr %in% unique(df$deck_nr)[1:9], substr(df$deck_nr,2,2), df$deck_nr) # delete 0 from numbers 1:9 
df$nrc <- as.numeric(df$nrc)
table(df$nrc, useNA = "ifany")
table(df$m_yob, useNA = "ifany")
df <- df[!is.na(df$m_yob),] # delete NA cases in m_yob


# family data
df2 <- famc[,c("colfam_nr", "lb_total")] 
  # lb_total: total nr of LB = twins counted as two

# merge
dfm <- merge(df, df2, by="colfam_nr", all=T)
dfm <- dfm[!is.na(dfm$m_yob),] # delete NA cases in m_yob
dfm$m_ageb <- dfm$yb-dfm$m_yob # age of mother at birth
table(dfm$m_ageb, useNA = "ifany")
dfm <- dfm[!is.na(dfm$m_ageb),] # delete NA cases in m_ageb
dfm$m_ageb <- paste0((dfm$yb-dfm$m_yob),"_",dfm$nrc) # age of mother at birth

df <- dfm[,c("colfam_nr", "m_yob", "lb_total", "m_ageb", "nrc")]
nrow(df)
# count only one twin birth
df_nt <- unique(df)
nrow(df_nt)

###########

# Partimos de 5001 datos, sacamos 10 NA
# Estudiamos todos los casos que estén todos los registros de los hijos, y nos quedamos con 4635 datos.

Freqs <- as.data.frame(table(df$colfam_nr))# indica la cantidad de hijos por mujer
df %>% count(colfam_nr)
colnames(Freqs) <- c(colnames(df)[1], "lb_total") # agrega columna con la cnatidad de hijos por mujer
df <- merge(df, Freqs, by="colfam_nr", all=T)
df = df %>% mutate(lb_total=lb_total.x) %>% 
  select(-c(lb_total.y,lb_total.x))

a <- df %>%
  group_by(colfam_nr) %>% 
  summarise(n = max(nrc))

df <- left_join(df, a, by = "colfam_nr")

df <- df %>% 
  mutate(m = n - lb_total) %>%
  filter(m == 0) %>% 
  select(-c("m", "n"))

df %>%  select(colfam_nr) %>% unique() %>%  dim() # Ahora hay 637 mujeres
#########

# list with cohort dfs
lc <- list()
asfr_list <-  list()
TFR_list <- list()
m_ageb <- 10:50

# twins as 2 births
for (i in unique(df$m_yob)){
  dfg <- grepFn(i, df, column="m_yob") # cohort df
  n <- length(unique(dfg$colfam_nr)) # nr of unique women
  dfg <- merge(dfg, as.data.frame(m_ageb), by = "m_ageb", all=T) # add all ages
  dfg$cum_lb_total <- cumsum(!is.na(dfg$lb_total)) # cumulative fertility at age E(x)
  dfg$cum_fec <- dfg$cum_lb_total/n
  lc[[as.character(i)]] <- as.data.frame(dfg) # dfs in list
  
  # fertility rates by age f(x1, x2)
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

# twins as one birth
asfr_list_nt <- list()
for (i in unique(df_nt$m_yob)){
  dfg <- grepFn(i, df_nt, column="m_yob") # cohort df
  dfg <- merge(dfg, as.data.frame(m_ageb), by = "m_ageb", all=T) # add all ages
  n <- length(unique(dfg$colfam_nr)) # nr of unique women
  dfg$cum_lb_total <- cumsum(!is.na(dfg$lb_total)) # cumulative fertility at age E(x)
  dfg$cum_fec <- dfg$cum_lb_total/n
  lc[[as.character(i)]] <- as.data.frame(dfg) # dfs in list
  
  # fertility rates by age f(x1, x2)
  inferior <- dfg[dfg$m_ageb %in% seq(10,49,1),c("m_ageb", "cum_fec")]
  superior <- dfg[dfg$m_ageb %in% seq(11,50,1),c("m_ageb", "cum_fec")]
  cbind(inferior, superior)
  fx <- superior$cum_fec - inferior$cum_fec
  fxdf <- cbind(inferior, superior, fx)
  fxdf$cohort <- i
  colnames(fxdf) <- c("m_agebi", "cum_feci", "m_agebs", "cum_fecs", "fx", "cohort")
  asfr_list_nt[[as.character(i)]] <- fxdf
  } 

###########

# plot
# compare asfrs with twins as two births and as one 
for (i in 1:length(asfr_list)){
  p <- ggplot(asfr_list[[i]], aes(x=m_agebi, y=fx)) + geom_point() + ggtitle(unique(asfr_list[[i]]$cohort))
  p_nt <- ggplot(asfr_list_nt[[i]], aes(x=m_agebi, y=fx)) + geom_point() + ggtitle(paste0(unique(asfr_list_nt[[i]]$cohort),"_nt"))
  print(grid.arrange(p, p_nt))
}
# difference for 1886, 1894, 1897, 1900, 1905, 1908, 1909, 1910, 1912, 1914, 1916, 1917, 1918, 1919, 1924, 1925, 1926, 1928, 1929, 1930, 1931, 1932



######## TFR

# df for tfr plotting
tfrdf <- rbindlist(TFR_list, use.names = TRUE)
colnames(tfrdf) <- c("cohort", "tfr")
plot(tfrdf$cohort, tfrdf$tfr)



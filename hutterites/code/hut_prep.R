library(sos); library(data.table)

# read hutterites data
hut <- read.table("../data/hutterites.dat", sep = "", header=FALSE)

hut$comp_nr <- hut$V1 # complete number
hut$deck_nr <- substr(hut$comp_nr, 1, 2) # deck number
table(hut$deck_nr)

##############################################################################

##### different datasets / cards

### family card
famc <- subset(hut, deck_nr=="00")

famc$colfam_nr <- substr(famc$comp_nr, 3, 7) # colony and family number
famc$comp_f_bd <- substr(famc$comp_nr, 8, 8) # completeness of information about fathers birth day
famc$f_dob <- substr(famc$comp_nr, 9, 14) # fathers birth date
famc$f_yob <- paste0("1", substr(famc$comp_nr, 9, 11)) # father year of birth
famc$f_yob <- ifelse(famc$f_yob==1000, NA, famc$f_yob)
famc$f_yob <- as.numeric(famc$f_yob)
famc$f_dmob <- substr(famc$comp_nr, 12, 14) # father day and month of birth
famc$comp_m_bd <- substr(famc$comp_nr, 15, 15) # completeness of information about mothers birth day
famc$m_dob <- substr(famc$comp_nr, 16, 21) # mothers birth date
famc$m_yob <- paste0("1", substr(famc$comp_nr, 16, 18)) # mother year of birth
famc$m_yob <- ifelse(famc$m_yob==1000 | famc$m_yob==1, NA, famc$m_yob)
famc$m_yob <- as.numeric(famc$m_yob)
famc$m_dmob <- substr(famc$comp_nr, 19, 21) # mother day and month of birth
famc$comp_dmarr <- substr(famc$comp_nr, 22, 22) # completeness of date of marriage
famc$dmarr <- substr(famc$comp_nr, 23, 28) # date of marriage 
famc$m_dmob <- substr(famc$comp_nr, 23, 25) # year of marriage
famc$r_endmarrrec <- substr(famc$comp_nr, 29, 29) # reasons for end of marriage record
famc$comp_d_endmarrrec <- substr(famc$comp_nr, 30, 30) # completeness of date of end of marriage record
famc$d_endmarrrec <- substr(famc$comp_nr, 31, 36) # date of end of marriage record
famc$y_endmarrrec <- substr(famc$comp_nr, 31, 33) # year of end of marriage record
famc$comp_dsurvey <- substr(famc$comp_nr, 37, 37) # completeness of date of survey
famc$dsurvey <- substr(famc$comp_nr, 38, 43) # date of survey
famc$ysurvey <- paste0("1", substr(famc$comp_nr, 38, 40)) # year of survey
famc$ysurvey <- as.numeric(famc$ysurvey)
famc$confinements_total <- substr(famc$comp_nr, 44, 45) # total number of confinements (LB) (Twins counted as one)
famc$pregnant_at_tsurvey <- substr(famc$comp_nr, 46, 46) # pregnancy at time of survey
famc$nr_multiplebirths <- substr(famc$comp_nr, 47, 47) # number of multiple births (number of times twins or triplets)
famc$lb_total <- substr(famc$comp_nr, 48, 49) # total number of live births (counting nr of children, twins count as two)
famc$lb_total <- as.numeric(famc$lb_total)
famc$marstat_at_marr <- substr(famc$comp_nr, 50, 50) # marital status at marriage
famc$other_marr <- substr(famc$comp_nr, 51, 51) # other marriages
famc$comp_dlastbirth <- substr(famc$comp_nr, 52, 52) # completeness of date of last birth
famc$dlastlb <- substr(famc$comp_nr, 53, 58) # date of last live birth
famc$ylastlb <- paste0("1", substr(famc$comp_nr, 53, 55)) # year of last live birth
famc$ylastpreg <- paste0("1", substr(famc$comp_nr, 59, 61)) # year of last pregnancy if not LB
famc$comp_dfdeath <- substr(famc$comp_nr, 62, 62) # completeness of date of father's death
famc$dfdeath <- substr(famc$comp_nr, 63, 68) # date of father's death
famc$yfdeath <- paste0("1", substr(famc$comp_nr, 63, 65)) # year of father's death
famc$comp_dmdeath <- substr(famc$comp_nr, 69, 69) # completeness of date of mother's death
famc$dmdeath <- substr(famc$comp_nr, 70, 75) # date of mother's death
famc$ymdeath <- paste0("1", substr(famc$comp_nr, 70, 72)) # year of mother's death

famc$V1 <- substring(famc$comp_nr, 73)

saveRDS(famc, "../data/family_card.rds")

################################################

#### live birth card, by order
lbcnr <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", 10:17)
lbc <- subset(hut, deck_nr %in% lbcnr)
table(lbc$deck_nr)

# col 3:45 duplicate from famc
lbc$colfam_nr <- substr(lbc$comp_nr, 3, 7) # colony and family number
lbc$comp_f_bd <- substr(lbc$comp_nr, 8, 8) # completeness of information about fathers birth day
lbc$f_dob <- substr(lbc$comp_nr, 9, 14) # fathers birth date
lbc$f_yob <- paste0("1", substr(lbc$comp_nr, 9, 11)) # father year of birth
lbc$f_dmob <- substr(lbc$comp_nr, 12, 14) # father day and month of birth
lbc$comp_m_bd <- substr(lbc$comp_nr, 15, 15) # completeness of information about mothers birth day
lbc$m_dob <- substr(lbc$comp_nr, 16, 21) # mothers birth date
lbc$m_yob <- paste0("1", substr(lbc$comp_nr, 16, 18)) # mother year of birth
lbc$m_yob <- ifelse(lbc$m_yob==1000 | lbc$m_yob==1, NA, lbc$m_yob)
lbc$m_yob <- as.numeric(lbc$m_yob)
lbc$m_dmob <- substr(lbc$comp_nr, 19, 21) # mother day and month of birth
lbc$comp_dmarr <- substr(lbc$comp_nr, 22, 22) # completeness of date of marriage
lbc$dmarr <- substr(lbc$comp_nr, 23, 28) # date of marriage 
lbc$m_dmob <- substr(lbc$comp_nr, 23, 25) # year of marriage
lbc$r_endmarrrec <- substr(lbc$comp_nr, 29, 29) # reasons for end of marriage record
lbc$comp_d_endmarrrec <- substr(lbc$comp_nr, 30, 30) # completeness of date of end of marriage record
lbc$d_endmarrrec <- substr(lbc$comp_nr, 31, 36) # date of end of marriage record
lbc$y_endmarrrec <- substr(lbc$comp_nr, 31, 33) # year of end of marriage record
lbc$comp_dsurvey <- substr(lbc$comp_nr, 37, 37) # completeness of date of survey
lbc$dsurvey <- substr(lbc$comp_nr, 38, 43) # date of survey
lbc$ysurvey <- paste0("1", substr(lbc$comp_nr, 38, 40)) # year of survey
lbc$confinements_total <- substr(lbc$comp_nr, 44, 45) # total number of confinements (LB) (Twins counted as one)
#
lbc$comp_db <- substr(lbc$comp_nr, 46, 46) # completeness of date of this birth
lbc$db <- substr(lbc$comp_nr, 47, 52) # date of birth
lbc$yb <- paste0("1", substr(lbc$comp_nr, 47, 49)) # year of birth
lbc$yb <- as.numeric(lbc$yb)
lbc$yb <- ifelse(lbc$yb==1000 | lbc$yb==1, NA, lbc$yb)
lbc$comp_ddeath <- substr(lbc$comp_nr, 53, 53) # completeness of date of death
lbc$ddeath <- substr(lbc$comp_nr, 54, 59) # date of death
lbc$ydeath <- substr(lbc$comp_nr, 54, 56) # year of death
lbc$sex_multp <- substr(lbc$comp_nr, 60, 60) # sex and multiplicity
lbc$order_preg <- substr(lbc$comp_nr, 61, 62) # rand order pregnancy from beginning counting all pregnancies
lbc$order_confinm <- substr(lbc$comp_nr, 63, 63) # rank order confinement from end
lbc$miscstill <- substr(lbc$comp_nr, 64, 64) # number of miscarriages and stillbirths between this birth and last preceding LB or marriage in the case of the first LB
lbc$comp_db_pb <- substr(lbc$comp_nr, 65, 65) # completeness of date of birth of preceding LB
lbc$db_pb <- substr(lbc$comp_nr, 66, 71) # date of birth of preceding birth
lbc$yb_pb <- substr(lbc$comp_nr, 66, 68) # year of birth of preceding birth
lbc$mortstat_pb <- substr(lbc$comp_nr, 72, 72) # information about mortality of previous child
lbc$special_codes <- substr(lbc$comp_nr, 73, 73) # special codes

lbc$V1 <- substring(lbc$comp_nr, 73)
table(lbc$V1)

saveRDS(lbc, "../data/livebirth_card.rds")

#######################################################

#### pregnancy card for the xth marriage for this woman
pregcnr <- (41:42)
pregc <- subset(hut, deck_nr %in% pregcnr)

pregc$deck_nr <- substr(pregc$deck_nr, 1, 1)
pregc$order_marr <- substr(pregc$comp_nr, 2, 2)
# 3:36 duplicate from famc
pregc$colfam_nr <- substr(pregc$comp_nr, 3, 7) # colony and family number
pregc$comp_f_bd <- substr(pregc$comp_nr, 8, 8) # completeness of information about fathers birth day
pregc$f_dob <- substr(pregc$comp_nr, 9, 14) # fathers birth date
pregc$f_yob <- paste0("1", substr(pregc$comp_nr, 9, 11)) # father year of birth
pregc$f_dmob <- substr(pregc$comp_nr, 12, 14) # father day and month of birth
pregc$comp_m_bd <- substr(pregc$comp_nr, 15, 15) # completeness of information about mothers birth day
pregc$m_dob <- substr(pregc$comp_nr, 16, 21) # mothers birth date
pregc$m_yob <- paste0("1", substr(pregc$comp_nr, 16, 18)) # mother year of birth
pregc$m_dmob <- substr(pregc$comp_nr, 19, 21) # mother day and month of birth
pregc$comp_dmarr <- substr(pregc$comp_nr, 22, 22) # completeness of date of marriage
pregc$dmarr <- substr(pregc$comp_nr, 23, 28) # date of marriage 
pregc$m_dmob <- substr(pregc$comp_nr, 23, 25) # year of marriage
pregc$r_endmarrrec <- substr(pregc$comp_nr, 29, 29) # reasons for end of marriage record
pregc$comp_d_endmarrrec <- substr(pregc$comp_nr, 30, 30) # completeness of date of end of marriage record
pregc$d_endmarrrec <- substr(pregc$comp_nr, 31, 36) # date of end of marriage record
pregc$y_endmarrrec <- substr(pregc$comp_nr, 31, 33) # year of end of marriage record
pregc$nr_marr <- substr(pregc$comp_nr, 37, 37) # number of marriages for woman
pregc$preg_total <- substr(pregc$comp_nr, 38, 44) # total number of pregnancies
# pregnancy outcomes of ith pregnancy
pregc[22:45] <- NA
pregnames <- c(47:70)
colnames(pregc)[22:45] <- pregnames
for (i in colnames(pregc)[22:45]){
  pregc[,paste0("",i,"")] <- substr(pregc$comp_nr, i, i) 
}
pregnames <- c(1:24)
pregnames <- paste0("preg",pregnames)
colnames(pregc)[22:45] <- pregnames
#
pregc$lb_conf_total <- substr(pregc$comp_nr, 71, 71) # total number of LB confinements

pregc$V1 <- substring(pregc$comp_nr, 45)

# no value in lb_conf_total
# no coding for columns/nrs 45:46
# nr for cols 38:44 unclear

saveRDS(pregc, "../data/pregnancy_card.rds")

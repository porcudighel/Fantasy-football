# Alessio Morelli
# October the 29th 2018
#
# Exercise R: data analysis on Serie A players
# Files from Fantagazzetta.com
# - Svincolati
# - Rose
# - Statistiche

# needed for readhtmltable
# install.packages("XML")
library("XML")
# to read excel files
# install.packages("readxl")
library(readxl)

setwd("C:/R data/Fantacalcio")

### Import all LegaSerieA and put them together
# Import data directly from the website Legacalcio.it
# http://www.legaseriea.it/en/serie-a/teams/bologna/players-stats
# function getting a team dataset from LegaSerieA
LegaSerieA <- function(x){
  # import table from legaseriea website with readlines (because for some reason readhtmltable gives Error: failed to load external entity)
  lineTeam <- readLines(paste0("http://www.legaseriea.it/en/serie-a/teams/", x, "/players-stats"))
  LSATeam <- as.data.frame(readHTMLTable(lineTeam)[5])
  # Add column with Team name
  LSATeam$Squadra <- x
  return(LSATeam)
}

Squadre <- as.list(c("atalanta", "bologna", "cagliari", "chievoverona", "empoli", "fiorentina", "frosinone", "genoa", "inter", "juventus", "lazio", "milan", "napoli", "parma", "roma", "sampdoria", "sassuolo", "spal", "torino", "udinese"))
# apply LegaSerieA on each team, get a list of matrices
# do.call of the rbind puts all of them together in dataframe
LSA <- as.data.frame(do.call(rbind, lapply(Squadre, function(i) rbind(LegaSerieA(i)))))
# Delete x., ROLE, APPEARANCES, GOAL, Bookings, Two.Bookings, Sending.Offs, Assists
# since they are repetitions (merging with data from Fantagazzetta they would be duplicated)
LSA[,c(1,3,4,6:9,12)] = NULL
# give decent names to the remaining columns
names(LSA)[1] <- "Nome"
names(LSA)[2] <- "Minuti"
names(LSA)[3] <- "Km"
names(LSA)[4] <- "Occasioni"
names(LSA)[5] <- "Tiri"
AAA <- LSA
LSA$Minuti <- as.numeric(as.character(AAA$Minuti)) 
LSA$Km <- as.numeric(as.character(AAA$Km))
LSA$Occasioni <- as.numeric(as.character(AAA$Occasioni)) 
LSA$Tiri <- as.numeric(as.character(AAA$Tiri)) 

table(LSA$Squadra)
class(LSA$Minuti)

#############################################################################################################################################
# WARNING: from time to time importing from LegaSerieA works weird. Find out why
# E.G. Sometimes it get all teams of 28 players (clearly wrong)
# need to insert check to control if the download is failing or not
#############################################################################################################################################

### Import Fantagazzetta data.
# import data from excel file. Need to find out how to get the file from the pop-up window which downloads the file from fantagazzetta.com
# need to specify there is a header, so it will use it for the names
# however original file has title in first line and header in second, hence I use the skip = 1 
dati201819 <- as.data.frame(read_xlsx(paste0(getwd(),"/Statistiche_Fantacalcio_2018-19_Fantagazzetta.xlsx"), skip = 1))
# Change name squadra for congruency with LegaSerieA data
dati201819$Squadra[dati201819$Squadra == "Chievo"] <- "Chievoverona"
# I want to get data for players free to buy
# use choose.files but ask for what file you want to open
svincolati <- as.data.frame(read_xlsx(paste0(getwd(),"/ListaSvincolati_fantavanzago.xlsx"), skip = 3))
#import my players
ATLPORCAO <- as.data.frame(read_xlsx(paste0(getwd(),"/Rose_fantavanzago.xlsx"), range = "F64:I89"))
 
### Prepare the dati201819 in order matching players between tables (preparation for the merge)
# cut the names in FGTeam (so i get rid of initials and the grep on set of full names can work)
cutnames <- sapply(dati201819$Nome, function(i) substr(i, 1, 6))
# remove spaces at end of cutnames (so the grep will work)
cutnames <- trimws(cutnames, "r")
# new df with cutnames instead of full surnames with initials
FGTagliata <- dati201819
FGTagliata$Nome <- cutnames

table(FGTagliata$Squadra)

### Add stats from LegaSerieA to dati201819
# Merge the two datasets
# In order to avoid players with similar name but different team to be picked up at the same time, need restrict grep to one team per time
# make function running on subset for each team
TeamMerge <- function(x){
  # subsets of FG and LSA data for given team
  FGTeam <- subset(FGTagliata, tolower(FGTagliata$Squadra) == x)
  LSATeam <- subset(LSA, tolower(LSA$Squadra) == x)
  # grep gives out a list containing the index of the found matches, otherwise integer(0) and NA
  # I use sapply to iterate searching all FGTagliata$Nome into LSATeam$NULL.PLAYER
  # then unlist, so I can work with single elements
  matchPlayers <- unlist(sapply(FGTeam$Nome, function(i) grep(i, LSATeam$Nome)[1]))
  # give LegaSerieA names to FG subset for the team in elaboration
  # to be sure make the names as characters
  LSATeam$Nome <- as.character(LSATeam$Nome)
  LSATeam$Squadra <- NULL
  # and now to the names of FGTag (which were found into LSATeam) I give the corresponding name in LSATeam  
  FGTeam$Nome[!is.na(matchPlayers)] <- LSATeam$Nome[matchPlayers[!is.na(matchPlayers)]]
  FGLSAMerged <- merge(FGTeam, LSATeam, by.x = "Nome",by.y = "Nome",  all.x = TRUE)
  return(FGLSAMerged)
}

FGandLSA <- NULL
# apply TeamMerge on each team, get a list of matrices
# do.call of the rbind puts all of them together in dataframe
FGandLSA <- do.call(rbind, lapply(Squadre, function(i) TeamMerge(i)))
# I got a dataframe for all players with combined data from FG and LSA

table(FGandLSA$Squadra)
class(FGandLSA$Au)

#########
# Check for players who have played but have no minutes (missed from the grep)
sum(is.na(FGandLSA$Minuti))
subset(FGandLSA, is.na(FGandLSA$Minuti) & FGandLSA$Pg !=0)
#########

### I want to get data for players free to buy
# Get the data for the svincolati from FGandLSA
liberi <- subset(FGandLSA, FGandLSA$Id %in% svincolati$Id & !is.na(FGandLSA$Minuti))
# next step: compare to my own players
# get the stats for my players
# but there is no id. I have to do the same as in AddStats1
cutnames <- sapply(ATLPORCAO$Calciatore, function(i) substr(i, 1, 6))
cutnames <- trimws(cutnames, "r")
matchPlayers <- unlist(sapply(ATLPORCAO$Calciatore, function(i) grep(i, FGandLSA$Nome)[1]))
StatsAP <- FGandLSA[matchPlayers,]

### Function returning players by role, comparing mine to available ones
# in FinalTable a list with all the players of mine followed by NA and a list of free players better than my worst one

selectPlayers <- function(x){
  # select my players for that role (and order them)
  xStatsAP <- subset(StatsAP, StatsAP$R == x)
  xStatsAP <- xStatsAP[order(-xStatsAP$Mf),]
  # select free players with better Mf than my worst
  # and at most 1 match less than max
  xBetter <- subset(liberi, liberi$R == x & liberi$Mf > min(xStatsAP$Mf))
  xBetter <- xBetter[order(-xBetter$Mf),]
  xBetter <- xBetter[order(-xBetter$Minuti),]
  # table all of my players, and the free ones better than the worst
  FinalTable <- rbind(xStatsAP, NA, xBetter)
  write.csv(FinalTable, paste0(x," Fantamercato di riparazione 3.csv", na = " "))
  return(FinalTable)
}

# get the return list into foo DF and save each part in DF for role (P goalkeeper, D defender, C midfielder, A forward)
foo <- selectPlayers("P")
PFinal <- as.data.frame(foo)
foo <- selectPlayers("D")
DFinal <- as.data.frame(foo)
foo <- selectPlayers("C")
CFinal <- as.data.frame(foo)
foo <- selectPlayers("A")
AFinal <- as.data.frame(foo)

######################################################################################################################
# ISSUES
# 1. minutes is a factor not numeric: what comes form LSA is factor: solved, but should check if there is way to import it already in right format
# 2. Column Squadra duplicated: I'm using wrong merge. Solved by deleting one of them before merge, but should check which merge can do that
######################################################################################################################

# Query line to compare players directly from main dataframe 
FGandLSA[unlist(sapply(c("ZAJC", "BETANCUR"), function(i) grep(i, FGandLSA$Nome)[1])),]
FGandLSA[unlist(sapply(c("CAICEDO"), function(i) grep(i, FGandLSA$Nome)[1])),]
provacentro <- subset(liberi, liberi$R == "C" & liberi$Gf > 0)
provacentro1 <- provacentro[order(liberi$Minuti),]
subset(FGandLSA, Squadra == "Bologna")

as.numeric(FGandLSA$Minuti[1])-as.numeric(FGandLSA$Minuti[2])




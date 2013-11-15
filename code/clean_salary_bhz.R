# This code is used to merge the salary data from Blackhawkzone,
# match player names with the ones in the roster data set, 
# and add the plus.minus value of players from buildgoals data set

# set the working directory
setwd("/Users/apple/Dropbox/Sen/hockey/salary/data")
# salary data for all seasons, obtained from blackhawkzone
meta_name <- c("0203","0304","0506","0607","0708","0809","0910","1011","1112","1213","1314")
salary <- list()
# format of the name we want: first_last
# the format in the data set is: last, first
# define a function to reverse the first and last name, and connect them with underlines
name_fun <- function(x){paste(x[2],x[1],sep="_")}
for (i in 1:length(meta_name)){
  # read in salary data of corresponding season
  salary[[i]] <- read.table(file=sprintf("salary_%s.txt", meta_name[i]),header=TRUE,sep="$", quote="")
  # clean the names of players
  salary[[i]]$Name <- gsub("\t","",salary[[i]]$Name,fix=TRUE)
  # change the format of names
  name <- strsplit(salary[[i]]$Name, split=', ', fix=TRUE)
  salary[[i]]$Name <- do.call(rbind,lapply(name,name_fun))
  salary[[i]]$Name <- toupper(salary[[i]]$Name)
  # change the unit of salary to million
  salary[[i]]$Salary <- as.numeric(gsub(",","",salary[[i]]$Salary,fix=TRUE)) / 1000000
}

# rename the columns of salaries to avoid duplicate column names when merging
salary <- Map(function(x, i) setNames(x, ifelse(names(x) %in% "Name",
                   names(x), sprintf('%s.%s', names(x), meta_name[i]))), salary, seq_along(salary))
# merge the data sets
salary_merge <- Reduce(function(...) merge(..., by="Name",all=TRUE), salary)
# replace NA with 0
salary_merge[is.na(salary_merge)] <- 0
# calculate the total salary
salary_merge <- cbind(salary_merge,rowSums(salary_merge[,-1]))
colnames(salary_merge)[length(salary_merge)] <- "Total"
# write to a file
write.table(salary_merge,file="nhlsalaries_bhz.txt", col.names=TRUE, row.names=FALSE, 
            quote=FALSE, sep='|')


#########################################################################################################
#########################################################################################################
### The following code matches the player names in the salary data with the names
### in the roster data we have before

## load the roster object containing player/game info
load("previous/roster.RData")
pnames <- paste(roster$roster.unique$first, roster$roster.unique$last, sep="_")
# read in the salary data
salary <- read.table(file="nhlsalaries_bhz.txt", header=TRUE, quote="", sep='|')
salary$Name <- as.character(salary$Name)
# common player names
common_players <- intersect(pnames,salary$Name)
# pull out data with dismatch player names
salary_nk <- salary[!(salary$Name %in% common_players),]
pnames_nk <- pnames[!(pnames %in% common_players)]

# split the first and last names
pnames_nk_split <- strsplit(pnames_nk, split='_', fix=TRUE)
salary_nk_split <- strsplit(salary_nk$Name, split='_', fix=TRUE)
pnames_nk_split <- data.frame(do.call("rbind",pnames_nk_split),stringsAsFactors = FALSE)
salary_nk_split <- data.frame(do.call("rbind",salary_nk_split),stringsAsFactors = FALSE)
colnames(pnames_nk_split) <- colnames(salary_nk_split) <- c("first","last")

# for the unmatching names, find the name in the roster data has the same last name but different first name 
# as the corresponding name in the salary data
name_check <- list()
j <- 0
for(i in 1:nrow(salary_nk_split)){
  if(salary_nk_split$last[i] %in% pnames_nk_split$last){
    j <- j+1
    name_check[[j]] <- c(paste(salary_nk_split$first[i],salary_nk_split$last[i],sep='_'),
                         paste(pnames_nk_split$first[which(pnames_nk_split$last == salary_nk_split$last[i])],salary_nk_split$last[i],sep='_'))
  } 
}
# set the lengths of elements in the list to the maximum length 
n <- max(do.call(rbind, lapply(name_check, function(x) length(x))))
for(i in 1:length(name_check)){
  length(name_check[[i]]) <- n
}
# data frame, 1st column: player name in the salary data, 2nd-5th column: player name with the same last name in the roster data
name_check <- do.call(rbind,name_check)

# change the player names
salary$Name[which(salary$Name == "AKI-PETTERI_BERG")] <- "AKI_BERG"
salary$Name[which(salary$Name == "ALEXANDER_AULD")] <- "ALEX_AULD"
salary$Name[which(salary$Name == "ALEXANDER_FROLOV")] <- "ALEX_FROLOV"
salary$Name[which(salary$Name == "ALEXANDER_KILLORN")] <- "ALEX_KILLORN"
salary$Name[which(salary$Name == "ALEXANDER_OVECHKIN")] <- "ALEX_OVECHKIN"
salary$Name[which(salary$Name == "ALEXANDER_ROACH")] <- "ALEX_ROACH"
salary$Name[which(salary$Name == "ALEXANDER_STEEN")] <- "ALEX_STEEN"
salary$Name[which(salary$Name == "ALEXANDRE_BURROWS")] <- "ALEX_BURROWS"
salary$Name[which(salary$Name == "ALEXEI_KOVALEV")] <- "ALEX_KOVALEV"
salary$Name[which(salary$Name == "ALEXSEY_MOROZOV")] <- "ALEKSEY_MOROZOV"
salary$Name[which(salary$Name == "ANDREW_WOZNIEWSKI")] <- "ANDY_WOZNIEWSKI"
salary$Name[which(salary$Name == "ANTTI SAKARI_PIHLSTROM")] <- "ANTTI_PIHLSTROM"
salary$Name[which(salary$Name == "CHRISTOPHER_HIGGINS")] <- "CHRIS_HIGGINS"
salary$Name[which(salary$Name == "CHRISTOPHER_NEIL")] <- "CHRIS_NEIL"
salary$Name[which(salary$Name == "DANIEL_CARCILLO")] <- "DAN_CARCILLO"
salary$Name[which(salary$Name == "DANIIL_MARKOV")] <- "DANNY_MARKOV"
salary$Name[which(salary$Name == "DANNY_TAYLOR")] <- "DANIEL_TAYLOR"
salary$Name[which(salary$Name == "DAVID_ANDREYCHUK")] <- "DAVE_ANDREYCHUK"
salary$Name[which(salary$Name == "DAVID_BOLLAND")] <- "DAVE_BOLLAND"
salary$Name[which(salary$Name == "DAVID_DZIURZYNSKI")] <- "DAVE_DZIURZYNSKI"
salary$Name[which(salary$Name == "DAVID_KARPA")] <- "DAVE_KARPA"
salary$Name[which(salary$Name == "DIMITRI_AFANASENKOV")] <- "DIMITRY_AFANASENKOV"
salary$Name[which(salary$Name == "DIMITRI_BYKOV")] <- "DMITRI_BYKOV"
salary$Name[which(salary$Name == "DIMITRI_KALININ")] <- "DMITRI_KALININ"
salary$Name[which(salary$Name == "DOMINICK_HASEK")] <- "DOMINIK_HASEK"
salary$Name[which(salary$Name == "DOUGLAS_GILMOUR")] <- "DOUG_GILMOUR"
salary$Name[which(salary$Name == "DWAYNE (D.J.)_KING")] <- "D.J._KING"
salary$Name[which(salary$Name == "ILYA_ZUBOV")] <- "ILJA_ZUBOV"
salary$Name[which(salary$Name == "J.P._ANDERSON")] <- "JON-PAUL_ANDERSON"
salary$Name[which(salary$Name == "J.P._DUMONT")] <- "JEAN-PIERRE_DUMONT"
salary$Name[which(salary$Name == "J.P._VIGIER")] <- "JEAN-PIERRE_VIGIER"
salary$Name[which(salary$Name == "J.T._WYMAN")] <- "JAMES_WYMAN"
salary$Name[which(salary$Name == "JACOB_DOWELL")] <- "JAKE_DOWELL"
salary$Name[which(salary$Name == "JAMES_DOWD")] <- "JIM_DOWD"
salary$Name[which(salary$Name == "JAMES_HOWARD")] <- "JIMMY_HOWARD"
salary$Name[which(salary$Name == "JEAN-SEBASTIEN_AUBIN")] <- "J-SEBASTIEN_AUBIN"
salary$Name[which(salary$Name == "JEFF_HAMILTON")] <- "JEFFREY_HAMILTON"
salary$Name[which(salary$Name == "JOHN COLIN_WHITE")] <- "COLIN_WHITE"
salary$Name[which(salary$Name == "JOHN_ODUYA")] <- "JOHNNY_ODUYA"
salary$Name[which(salary$Name == "JONATHAN_BLUM")] <- "JONATHON_BLUM"
salary$Name[which(salary$Name == "JONATHON_KALINSKI")] <- "JON_KALINSKI"
salary$Name[which(salary$Name == "JOSEPH_SACCO")] <- "JOE_SACCO"
salary$Name[which(salary$Name == "JOSHUA_BAILEY")] <- "JOSH_BAILEY"
salary$Name[which(salary$Name == "KENNETH_DANEYKO")] <- "KEN_DANEYKO"
salary$Name[which(salary$Name == "KEVAN_MILLER")] <- "KEVIN_MILLER"
salary$Name[which(salary$Name == "KRIS_FOUCAULT")] <- "KRISTOPHER_FOUCAULT"
salary$Name[which(salary$Name == "KRISTOPHER_LETANG")] <- "KRIS_LETANG"
salary$Name[which(salary$Name == "MARK DOUGLAS_MESSIER")] <- "MARK_MESSIER"
salary$Name[which(salary$Name == "MATT_CLARK")] <- "MAT_CLARK"
salary$Name[which(salary$Name == "MATTHEW_ANDERSON")] <- "MATT_ANDERSON"
salary$Name[which(salary$Name == "MATTHEW_CARLE")] <- "MATT_CARLE"
salary$Name[which(salary$Name == "MAXIME_SAUVE")] <- "MAX_SAUVE"
salary$Name[which(salary$Name == "MICHAEL_BLUNDEN")] <- "MIKE_BLUNDEN"
salary$Name[which(salary$Name == "MICHAEL_EASTWOOD")] <- "MIKE_EASTWOOD"
salary$Name[which(salary$Name == "MICHAEL_KNUBLE")] <- "MIKE_KNUBLE"
salary$Name[which(salary$Name == "MICHAEL_MODANO")] <- "MIKE_MODANO"
salary$Name[which(salary$Name == "MICHAEL_RUPP")] <- "MIKE_RUPP"
salary$Name[which(salary$Name == "MICHAEL_SANTORELLI")] <- "MIKE_SANTORELLI"
salary$Name[which(salary$Name == "MICHAEL_SILLINGER")] <- "MIKE_SILLINGER"
salary$Name[which(salary$Name == "MICHAEL_WEBER")] <- "MIKE_WEBER"
salary$Name[which(salary$Name == "MICHAEL_YORK")] <- "MIKE_YORK"
salary$Name[which(salary$Name == "MIKE_VERNACE")] <- "MICHAEL_VERNACE"
salary$Name[which(salary$Name == "NICHOLAS_BOYNTON")] <- "NICK_BOYNTON"
salary$Name[which(salary$Name == "NICHOLAS_DRAZENOVIC")] <- "NICK_DRAZENOVIC"
salary$Name[which(salary$Name == "NICHOLAS_PETRECKI")] <- "NICK_PETRECKI"
salary$Name[which(salary$Name == "NIKOLAI_ANTROPOV")] <- "NIK_ANTROPOV"
salary$Name[which(salary$Name == "PER JOHAN_AXELSSON")] <- "P. J._AXELSSON"
salary$Name[which(salary$Name == "PIERRE-ALEXANDRE_PARENTEAU")] <- "P.A._PARENTEAU"
salary$Name[which(salary$Name == "RICHARD_JACKMAN")] <- "RIC_JACKMAN"
salary$Name[which(salary$Name == "ROBERT_BLAKE")] <- "ROB_BLAKE"
salary$Name[which(salary$Name == "ROBERT_DIMAIO")] <- "ROB_DIMAIO"
salary$Name[which(salary$Name == "ROBERT_HOLIK")] <- "BOBBY_HOLIK"
salary$Name[which(salary$Name == "ROBERT_RAY")] <- "ROB_RAY"
salary$Name[which(salary$Name == "ROBERT_SCUDERI")] <- "ROB_SCUDERI"
salary$Name[which(salary$Name == "ROBERT_ZAMUNER")] <- "ROB_ZAMUNER"
salary$Name[which(salary$Name == "RONALD_FRANCIS")] <- "RON_FRANCIS"
salary$Name[which(salary$Name == "STANISLAV_NECKAR")] <- "STAN_NECKAR"
salary$Name[which(salary$Name == "STEPHEN_HEINZE")] <- "STEVE_HEINZE"
salary$Name[which(salary$Name == "STEPHEN_WEBB")] <- "STEVE_WEBB"
salary$Name[which(salary$Name == "STEVE_PINIZZOTTO")] <- "STEVEN_PINIZZOTTO"
salary$Name[which(salary$Name == "THOMAS_MOJZIS")] <- "TOMAS_MOJZIS"
salary$Name[which(salary$Name == "TIM R._TAYLOR")] <- "TIM_TAYLOR"
salary$Name[which(salary$Name == "VITALI_VISHNEVSKI")] <- "VITALY_VISHNEVSKI"
salary$Name[which(salary$Name == "ZACH_STORTINI")] <- "ZACK_STORTINI"

# 1960 common players now
common_players_now <- intersect(pnames,salary$Name)

# output the salary data with name corrections
write.table(salary,file="data/nhlsalaries_bhz_nc.txt", col.names=TRUE, row.names=FALSE, 
            quote=FALSE, sep='|')

#############################################################################################################
## merge the salary data with goals plus-minus data 
# read in the goals data, 2302 players
load("hockey_goals.rda")
# create a new data set including name of player, plus-minus value and salaries
salary_pm <- merge(player,salary, by.x="name",by.y="Name")
# write the result to a text file
write.table(salary_pm,file="../results/salary_pm.txt", col.names=TRUE, row.names=FALSE, quote=FALSE, sep='|')
# read back to check
#salary_pm_check <- read.table("results/salary_pm.txt", header=TRUE, sep='|', quote="")
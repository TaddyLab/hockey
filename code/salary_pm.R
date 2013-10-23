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

## merge the salary data with goals plus-minus data 
# read in the goals data, 2302 players
load("hockey_goals.rda")
# create a new data set including name of player, plus-minus value and salaries
salary_pm <- merge(player,salary_merge, by.x="name",by.y="Name")
# write the result to a text file
write.table(salary_pm,file="../results/salary_pm.txt", col.names=TRUE, row.names=FALSE, quote=FALSE, sep='|')
# read back to check
#salary_pm_check <- read.table("results/salary_pm.txt", header=TRUE, sep='|', quote="")
library(tcltk)
library(data.table)




script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)



names.last          <- read.table(file="https://www2.census.gov/topics/genealogy/1990surnames/dist.all.last", sep="", header=FALSE, stringsAsFactors=FALSE)[,1]
names.first.female  <- read.table(file="https://www2.census.gov/topics/genealogy/1990surnames/dist.female.first", sep="", header=FALSE, stringsAsFactors=FALSE)[,1]
names.first.male    <- read.table(file="https://www2.census.gov/topics/genealogy/1990surnames/dist.male.first", sep="", header=FALSE, stringsAsFactors=FALSE)[,1]
names.boat          <- scan(file="Names/boat.names.txt", what="", sep="\n")
names.colors          <- scan(file="Names/color.names.txt", what="", sep="\n")



num.sailors <- 40
num.boats <- 10
num.reservations <-100
num.reservations.per.day <- 50



Sailors <- NULL   # sid, lastname, firstname, rating
Boats <- NULL         # bid, color, rating
Reservations <- NULL  # sid, bid, date

sailors.colnames <- c("sid", "lname", "fname", "rating")
boats.colnames <- c("bid", "name", "color")
reservations.colnames <- c("sid", "bid", "date")

#Generate Sailors
set.seed(0)
for (sid in c(1:num.sailors)){
  female.list <- 1
  male.list <- 2
  sailor.fname.list <- sample(1:2, 1)
  sailor.fname <- NULL
  sailor.lname <- NULL
  sailor <- NULL
  
  if (sailor.fname.list ==  female.list){
    sailor.fname.index <- sample(1:length(names.first.female),1)
    sailor.fname <- names.first.female[sailor.fname.index]
  } else   if (sailor.fname.list ==  male.list){
    sailor.fname.index <- sample(1:length(names.first.male),1)
    sailor.fname <- names.first.male[sailor.fname.index]
  }
  
  sailor.lname.index <- sample(1:length(names.last),1)
  sailor.lname <- names.last[sailor.lname.index]
  
  sailor <- c(sid, sailor.lname, sailor.fname, sample(1:100,1))
  # print(sailor)
  
  if (is.null(Sailors))
    Sailors <- sailor
  else {
    Sailors <-  rbind(Sailors, sailor)
    }
}



# Geberate Boats
set.seed(1)
for (bid in c(1:num.boats)){
  boat.index <- sample(1:length(names.boat),1)
  boat.name <- names.boat[boat.index]
  
  color.index <- sample(1:length(names.colors),1)
  color.name <- names.colors[color.index]
  
  boat <- c(bid, boat.name, color.name)
  # print(boat)
  
  if (is.null(Boats))
    Boats <- boat
  else {
    Boats <-  rbind(Boats, boat)
  }
}



# Geberate Reservations
set.seed(2)
date.orgin <- "1/1/1970"
reservations.start.date <- as.Date("05/14/2000",format='%m/%d/%Y', origin=date.orgin)
reservations.current.date <- reservations.start.date
reservations.current.number.on.day <- 0



for (r in c(1:num.reservations)){
  sid <- sample(1:length(Sailors[,1]),1)
  bid <- sample(1:length(Boats[,1]),1)
  
  reservations.current.number.on.day <- reservations.current.number.on.day + 1
  
  if (reservations.current.number.on.day > num.reservations.per.day){
    reservations.current.date <- reservations.current.date + 1
    reservations.current.number.on.day <- 1
  } 
  
  date <- reservations.current.date
  
  reservation <- c(sid, bid, date)
  # print(reservation)
  
  if (is.null(Reservations))
    Reservations <- reservation
  else {
    Reservations <-  rbind(Reservations, reservation)
  }
}

rownames(Sailors) <- NULL
rownames(Boats) <- NULL
rownames(Reservations) <- NULL

colnames(Sailors) <- sailors.colnames
colnames(Boats) <- boats.colnames
colnames(Reservations) <- reservations.colnames

write.csv(Sailors, "sailors.csv",row.names=FALSE)
write.csv(Boats, "boats.csv",row.names=FALSE)
write.csv(Reservations, "reservations.csv",row.names=FALSE)



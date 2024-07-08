library(tidyverse)
randomwalks = list(0)
achtteilbar = integer()

#this small script creates "random walks" between 1 and 4 and checks if they fit the requirements,
# e.g. equall occurences of all numbers, and stores them if they fulfill the requirements. At the end
#the collection of random walks are tested again if they meet all requirements.

#these are used for difficulties in the experiment which has two conditions, a speed and an accuracy condition,
# because the difficulties should be equally distributed between both, we also have to filter for randomwalks
#that fulfill this condition.
achtteilbar = 24 
ntrials =24
for (ntrials in achtteilbar) {
  #equal amounts all 1,2,3,4 is wanted. So we have an ideal uniform distribution to check against
  #which is created here.
  #We have two comparisons to do, one for the total length of the walk and the other per condition.
  total <- as.matrix(table(c(rep(1,ntrials/4), rep(2,ntrials/4), rep(3,ntrials/4), rep(4,ntrials/4))))
  condition <-  as.matrix(table(c(rep(1,ntrials/8), rep(2,ntrials/8), rep(3,ntrials/8), rep(4,ntrials/8))))
  rw <- tibble(x=matrix(nrow=ntrials, ncol = 1)) #empty tibble
for (i in 1:100000) {
  #we bruteforce the process by creating lots of random walks and check if they meet the requirements.
  y = numeric() # random walk 
  x = sample(1:4, 1) # random starting point 

    for (j in 1:ntrials*2) { 
    x = x+sample(c(-1,1),1) #we go 1 step up or down.
    if (x == 0) { # if x is too low (there is no difficulty of 0), we go up to 2 instead
      x <- 2
      y <- c(y,x) # adds current value to random walk
    }
    else { # 
    if (x == 1) { #if X becomes one of the outer difficulties (1 or 4), it needs to be added 2 times.
    y <- c(y,rep(x,2)) #reason is otherwise there would alway be 2 inner difficulties (2,3) for every outer one, 2 -> 1 -> 2.
    }
    if (x == 2) {
      y <- c(y,x)
    }
    if (x == 3) {
      y <- c(y,x)
    }
    if (x == 4) { #same here, needs to be doubled.
      y <- c(y,rep(x,2))
    }
    if (x == 5) { # if x is too high, we go down to 3 instead
      x <- 3
      y <- c(y,x)
    }
    }
    if (length(y) == ntrials) {
      break
    }
    if (length(y) == ntrials+1) {
      y <- head(y,-1) #because we sometimes add 2 values to y it can become too big. 
      break
    }
    }
  #we now split the random walk into two, the first and third quarter and the second and fourth quarter.
  #This split happens according to the two conditions and then we create a table that outlines how many of each difficulty are in the 2 conditions.
  cond1 = as.matrix(table(c(y[1:(ntrials/4)], y[(ntrials/2+1):(ntrials*(3/4))])))
  cond2 = as.matrix(table(c(y[(ntrials/4+1):(ntrials/2)], y[(ntrials*(3/4)+1):ntrials])))
  total1 = as.matrix(table(y))
  
  if (identical(condition, cond1) == TRUE & identical(condition, cond2) == TRUE & identical(total, total1) == TRUE  ) { # checks if the uniform distributions are achieved
    rw[[paste("rw", i, sep = "")]] <-  y #if so we store the randomwalk 
    assign(paste0("rw", ntrials),rw) # rename it
    rwfinal <- cbind(rw,y) #add rw to list of rws

}


}
}


#some tests

##checks if there are jumps that are too long or sometimes only jumps where there shouldn't be any
for(k in 2:length(randomwalks)) {
  
for (j in 2:length(randomwalks[[k]])) {
boo <- apply(randomwalks[[k]], 2, FUN=rle) # rle erstellt 2 Vektoren in einer Liste. in der ersten wird der Wert festgestellt, in der zweiten wie oft er wiederholt wurde
#daher muss in der zweiten Liste immer für die Werte 1 &4 die Wiederholoung 2 stehen, für 2 &3 der Wert 1.
if(sum(boo[[1]][["lengths"]]>2) >0){
  print("aaaaaa")
}

#ich weiß nicht mehr was hier getestet wird...
for (i in 1:length(randomwalks[[k]])) {
  if (sum( boo[[i]] [[2]][1:length(boo[[i]] [[2]])-1]== 1 &  boo[[i]] [[1]][1:length(boo[[i]] [[1]])-1]==1) != 0) { # alle außer dem letzten Wert, der darf alleine Stehen.
    print(i)
  }
  if (sum( boo[[i]] [[2]][1:length(boo[[i]] [[2]])-1]== 4 &  boo[[i]] [[1]][1:length(boo[[i]] [[1]])-1]==1) != 0) {
    print(i)
  }
  if (sum( boo[[i]] [[2]]== 2 &  boo[[i]] [[1]]==2) != 0) {
    print(i)
  }
  if (sum( boo[[i]] [[2]]== 3 &  boo[[i]] [[1]]==2) != 0) {
    print(i)
  }
}

#check if jump larger than 1
if (sum(abs(diff(as.matrix(randomwalks[[k]])))>1) >= 1){
  print("eeeee")
  
}
}
}

# Find the Duplicated Columns
duplicated_columns <- duplicated(as.list(rw))
# Remove the Duplicated Columns
rwunique <- rw[!duplicated_columns]
# experiment is written in python, so we need to lower the values by 1. 
rwunique <- rwunique %>% -1

test <- as.data.frame(t(rwunique))
randwalkstart <-test$V1
test <- test %>% mutate(
  V1 = paste0(rep("[", times = nrow(test)), randwalkstart),
  V25 = rep("]", times = nrow(test))
  )
write_csv(test, "tatata.csv")


### Test, ob Code funktioniert mit nicht-random walk

# 
# for (ntrials in achtteilbar) {
#   #ideal table
#   condition <-  as.matrix(table(c(rep(1,ntrials/8), rep(2,ntrials/8), rep(3,ntrials/8), rep(4,ntrials/8)))) #Ziel pro Speed/Accuracy Bedingung
#   total <- as.matrix(table(c(rep(1,ntrials/4), rep(2,ntrials/4), rep(3,ntrials/4), rep(4,ntrials/4)))) # Ziel über gesammte Serie hinweg
#   rw <- tibble(x=matrix(nrow=ntrials, ncol = 1)) # Skellet zum einfügen
#   
#   for (i in 1:1000000) {#erstellt viele rws und testet, ob sie gut genug sind. Wenn ja, speicher in 
#     y = numeric() # random walk 
#     x = sample(1:4, 1) # random Startschwierigkeit
#     
#     for (j in 1:ntrials*2) { #erstellt ein random walk y zwischen Stufen 1:4
#       x = sample(1:4,1) #Unterschied
#       y = c(y,x)
#       
#       if (length(y) == ntrials) {
#         break
#       }
#       if (length(y) == ntrials+1) {
#         y <- head(y,-1) # manchmal kommt es durch die doppelten 4er und 1er zu zu langen ketten, dann abschneiden
#         break
#       }
#     }
#     cond1 = as.matrix(table(c(y[1:(ntrials/4)], y[(ntrials/2+1):(ntrials*(3/4))]))) #Wie sieht Verteilung in Accuracy Bedingung aus
#     cond2 = as.matrix(table(c(y[(ntrials/4+1):(ntrials/2)], y[(ntrials*(3/4)+1):ntrials]))) #in der Speed Bedingung
#     total1 = as.matrix(table(y))
#     
#     if (identical(condition, cond1) == TRUE & identical(condition, cond2) == TRUE & identical(total, total1) == TRUE  ) { # checkt, ob Bedingungen gleich sind und ob es insgesamt fair verteilt ist 
#       rw[[paste("rw", i, sep = "")]] <-  y
#       assign(paste0("rw", ntrials, sep=""),rw) # extra nur den Datensatz erzeugen, der Bedingung erzeugt
#       print("basdsdkjhkjfhsjkdhf")
#       
#     }
#     
#     
#   }
#   
# }

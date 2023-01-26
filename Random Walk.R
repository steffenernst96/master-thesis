library(tidyverse)
randomwalks = list(0)
achtteilbar = integer()
# for (i in 24:96) {
#   if (i %% 8 == 0) {
#     achtteilbar = c(achtteilbar, i)
#   }
#   
# }

achtteilbar = 24
ntrials =24
for (ntrials in achtteilbar) {
#ideal table
  condition <-  as.matrix(table(c(rep(1,ntrials/8), rep(2,ntrials/8), rep(3,ntrials/8), rep(4,ntrials/8)))) #Ziel pro Speed/Accuracy Bedingung
  total <- as.matrix(table(c(rep(1,ntrials/4), rep(2,ntrials/4), rep(3,ntrials/4), rep(4,ntrials/4)))) # Ziel über gesammte Serie hinweg
  rw <- tibble(x=matrix(nrow=ntrials, ncol = 1)) # Skellet zum einfügen
for (i in 1:100000) {#erstellt viele rws und testet, ob sie gut genug sind. Wenn ja, speicher in 
  y = numeric() # random walk 
  x = sample(1:4, 1) # random Startschwierigkeit

    for (j in 1:ntrials*2) { #erstellt ein random walk y zwischen Stufen 1:4, auf GRUPPENebene
    x = x+sample(c(-1,1),1)
    if (x == 0) { # x zu niedrig, springt von 1 auf 2 statt auf 0
      x <- 2
      y <- c(y,x) # hinzufügen von x zu Vektor y
    }
    else { # else notwendig, weil sonst wird x=0 zu x=2, was zu einem weiteren y=c(y,x) führt.
    if (x == 1) {
    y <- c(y,rep(x,2)) #kommt nur halb so häufig vor, daher doppelt hinzufügen
    }
    if (x == 2) { # hier normal
      y <- c(y,x)
    }
    if (x == 3) { #hier normal
      y <- c(y,x)
    }
    if (x == 4) { #kommt nur halb so häufig vor, daher doppelt hinzufügen
      y <- c(y,rep(x,2))
    }
    if (x == 5) { # x zu hoch, springt von 4 auf 3 statt auf 5
      x <- 3
      y <- c(y,x)
    }
    }
    if (length(y) == ntrials) {
      break
    }
    if (length(y) == ntrials+1) {
      y <- head(y,-1) # manchmal kommt es durch die doppelten 4er und 1er zu zu langen ketten, dann abschneiden
      break
    }
  }
  cond1 = as.matrix(table(c(y[1:(ntrials/4)], y[(ntrials/2+1):(ntrials*(3/4))]))) #Wie sieht Verteilung in Accuracy Bedingung aus
  cond2 = as.matrix(table(c(y[(ntrials/4+1):(ntrials/2)], y[(ntrials*(3/4)+1):ntrials]))) #in der Speed Bedingung
  total1 = as.matrix(table(y))
  
  if (identical(condition, cond1) == TRUE & identical(condition, cond2) == TRUE & identical(total, total1) == TRUE  ) { # checkt, ob Bedingungen gleich sind und ob es insgesamt fair verteilt ist 
    rw[[paste("rw", i, sep = "")]] <-  y
    assign(paste0("rw", ntrials),rw) # extra nur den Datensatz erzeugen, der Bedingung erzeugt
    # rw <- select(rw,-1)
    # randomwalks[[length(randomwalks)+1]] <- rw
    rwfinal <- cbind(rw,y)

}


}
}
#write.csv(randomwalks, "test")

#einige Tests

##check ob Kette sauber ist, d.h. immer konsekutiv 1 nach 1 und 4 nach 4 und nie 2 nach 2 oder 3 nach 3
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
# die Werte sind Schwierigkeiten für Python. Python zählt ab 0, nicht ab 1, weswegen hier bereits alle Werte um 1 erniedrigt werden
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

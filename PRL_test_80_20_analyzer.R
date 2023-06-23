
print(paste(format(Sys.time(), "%H:%M:%S"), "work start."))

# searching data 
print(paste ("searching data file"))

the.files <- as.character(list.files(".", pattern="([!])"))

print(paste("I found", length(the.files), "files."), quote = FALSE)

all.data <- vector()

for (k in 1:length(the.files)) {all.data <- append(all.data, readLines(the.files[k], encoding="UTF-8"))}



# WYSZUKIWANIE DANYCH Z TESTU PROBABLISTIC REVERSAL LEARNING Z DANYCH ZRODLOWYCH
print(paste("wyszukiwanie danych z test PRL z danych zrodlowych"))

require(stringr)

session.starts <- which(str_detect(all.data, "Start Date"))

PRL_test <- which(str_detect(all.data, "PRL_test_80_20_5s_ITI_Pellet_fan"))

print(paste(length(session.starts), "records detected."), quote = FALSE)

print(paste(length(PRL_test), "PRL_test_80_20_5s_ITI_Pellet_fan records detected."), quote = FALSE)



# WYSZUKIWANIE REGEX

number_detector <- "([[:digit:]]+)"

number_detector_dot <- "([[:digit:]]{1,})([/.])([[:digit:]]{1,})"

date_detector <- "([[:digit:]]{1,})([[:graph:]]{1})([[:digit:]]{2,})([[:graph:]]{1})([[:digit:]]{2,})"

text_detector <-"([[:word:]]{5,})"


# ZBIOR DANYCH
print(paste("okreslanie zbioru danych"))

events <- data.frame()

for (i in 1:length(PRL_test)) {
  
  event.counter <- as.numeric(str_extract((all.data[PRL_test[i]+7]), number_detector)) #Wartość Event Counter(G)
  
  Data.Array.N <- as.numeric(unlist(str_extract_all(all.data[(PRL_test[i]+ 25):(PRL_test[i]+25+ceiling(event.counter/5) - 1)], 
                                                    number_detector_dot)))
  
  Data.Array.N <- as.data.frame(matrix(Data.Array.N, nrow = event.counter/4, ncol=4, byrow = TRUE))
  
  colnames(Data.Array.N) <- c("Time", "Trial.Number", "Correct.Lever", "Event")

  events.temp <- data.frame(str_extract((all.data[PRL_test[i]-8]), date_detector), #Start date
                            str_extract((all.data[PRL_test[i]-2]), date_detector), # Start time
                            str_extract((all.data[PRL_test[i]]), text_detector), #MSN
                            as.numeric(str_extract((all.data[PRL_test[i]-6]), number_detector)), #Subject
                            as.numeric(str_extract((all.data[PRL_test[i]-3]), number_detector)), #Box
                            as.numeric(str_extract((all.data[PRL_test[i]-5]), number_detector)), #Experiment
                            as.numeric(str_extract((all.data[PRL_test[i]-4]), number_detector)), #Group
                            Data.Array.N)
  
  colnames(events.temp) <- c("Start.Date", "Start.Time", "MSN", "Subject", "Box", "Experiment", "Group",
                             "Time", "Trial.Number", "Correct.Lever", "Event")
  
  events <- rbind(events, events.temp)
  
}

events$Start.Date <- strptime(events$Start.Date, format="%m/%d/%y")

events$Start.Time <- as.POSIXct(strptime(paste(events$Start.Date, events$Start.Time), format="%Y-%m-%d %H:%M:%S"))

events <- events[order(events$Start.Date), ,drop = FALSE]


# ANALIZA POSZCZEGOLNYCH PROB W SESJI DLA WSZYSTKICH ZWIERZAT

trial.sum <- data.frame()

sesje <- unique(events$Start.Time)

print(paste("Wyciaganie danych z sesji"))

for (i in 1:length(sesje)) {
  
  events.sesja <- subset(events, Start.Time == sesje[i])  

  zwierzeta <- unique(events.sesja$Subject)
  
  for (j in 1:length(zwierzeta)) {
    
    events.sesja.zwierze <- subset(events.sesja, Subject == zwierzeta[j])
    liczba.triali <- unique(events.sesja.zwierze$Trial.Number)
    
    for (k in 1:(length(liczba.triali) - 1)) {
      
      trial <- subset(events.sesja.zwierze, Trial.Number == k) 
      
      ktory.wybor <- NA
      latencja <- NA
      czy.byla.nagroda <- NA
      dobra.strona <- NA
      wybor.strony <- NA
      
      
      if (5 %in% trial$Event) {
        
        ktory.wybor <- "Correct"
        
      }
      
      if (6 %in% trial$Event) {
        
        ktory.wybor <- "Incorrect"
        
      }
      
      if (7 %in% trial$Event) {
        
        ktory.wybor <- "Omission"
        
      }
      

      if (ktory.wybor == "Omission") {latencja = NA} else {
        
        latencja = trial[which(trial$Event %in% c(10,20)), "Time"] - trial[which(trial$Event == 15), "Time"]
      }
      

      if (10 %in% trial$Event) {
        
        czy.byla.nagroda <- "tak"
        
      }
 
      
      if (20 %in% trial$Event) {
        
        czy.byla.nagroda <- "nie"
        
      }
      
      
      if (trial$Correct.Lever[1] == 1) {dobra.strona = "Left"}
      if (trial$Correct.Lever[1] == 2) {dobra.strona = "Right"}
      

      if (ktory.wybor == "Omission") {wybor.strony = "Omission"}
      
      if (dobra.strona == "Left" & ktory.wybor == "Correct") {wybor.strony = "Left"}
      if (dobra.strona == "Left" & ktory.wybor == "Incorrect") {wybor.strony = "Right"}
      if (dobra.strona == "Right" & ktory.wybor == "Incorrect") {wybor.strony = "Left"}
      if (dobra.strona == "Right" & ktory.wybor == "Correct") {wybor.strony = "Right"}
      
      
      temp <- data.frame(as.POSIXct(trial$Start.Date[1]),
                         trial$Start.Time[1],
                         trial$MSN[1],
                         trial$Subject[1],
                         trial$Box[1],
                         trial$Experiment[1],
                         trial$Group[1],
                         trial$Trial.Number[1],
                         ktory.wybor,
                         latencja,
                         czy.byla.nagroda,
                         dobra.strona,
                         wybor.strony
      )
      
      trial.sum <- rbind(trial.sum, temp)
    
    }
    
  }
  
}

colnames(trial.sum) <- c("Start.Date", "Start.Time", "MSN", "Subject", "Box", "Experiment", "Group", "Trial",
                         "Choice",
                         "Choice.Latency",
                         "Reward", "Dobra.Strona", "Wybrana.Strona")


zebrane.p <- data.frame()

myszki <- unique(trial.sum$Subject)

for (i in 1:length(myszki)) {
  
  cala.jedna.mysz <- subset(trial.sum, Subject == myszki[i])
  
  print(paste(format(Sys.time(), "%H:%M:%S"), "analizuje p.left/p.right dla szczura", myszki[i]))
  
  for (j in 1:nrow(cala.jedna.mysz)) {
    
    if (j < 10) {
      p.left = NA
      p.right = NA
      p.omission = NA
    }
    
    if ((j >= 10)) {
      p.left = length(which(cala.jedna.mysz$Wybrana.Strona[(j-9):j] == "Left"))/10
      p.right = length(which(cala.jedna.mysz$Wybrana.Strona[(j-9):j] == "Right"))/10
      p.omission = length(which(cala.jedna.mysz$Wybrana.Strona[(j-9):j] == "Omission"))/10
    }
    
    linijka <- data.frame(cala.jedna.mysz[j, ], 
                          p.left,
                          p.right,
                          p.omission)
    
    zebrane.p <- rbind(zebrane.p, linijka)
  }
}


wsad <- zebrane.p


ostatni <- "jeszcze_nie"
analiza <- data.frame()
ostatnia.nagroda <- "jeszcze_nie"


print("Liczenie Win/Lose/Stay/Shift")

for (i in 1:nrow(wsad)) {
  
  Win.Lose <- NA
  Stay.Shift <- NA
  

  if (wsad$Choice[i] == "Omission") {
    Win.Lose = "Omission"
    Stay.Shift = "Omission"
    Shifted.Choice = "Omission"
  }
  

  if (wsad$Trial[i] == 1 & wsad$Choice[i] != "Omission") {
    Win.Lose = "First"
    Stay.Shift = "First"
    Shifted.Choice = "First"
    ostatni = wsad$Choice[i]
    ostatnia.nagroda = wsad$Reward[i]
    ostatni.rewersal = i
    
  }

  if (wsad$Trial[i] == 1 & wsad$Choice[i] == "Omission") {
    ostatni = "jeszcze_nie"
    ostatnia.nagroda = "jeszcze_nie"
    ostatni.rewersal = i
  }
  
 
  zamiana = 0
  
  if (wsad$Trial[i] > 1) {
    
    if (wsad$Dobra.Strona[i] != wsad$Dobra.Strona[i-1]) { 
      
      if (wsad$Choice[i-1] == "Correct") {ostatni = "Incorrect"}
      if (wsad$Choice[i-1] == "Incorrect") {ostatni = "Correct"} 
      zamiana = 1
      ostatni.rewersal = i
    }    
  }

  if (wsad$Trial[i] > 1 & wsad$Choice[i] != "Omission") {
    
    if (ostatnia.nagroda != "jeszcze_nie"){
      Win.Lose <- ifelse(ostatnia.nagroda == "tak", "Win", "Lose")
      
    }else{Win.Lose = "nie_wiem"}
      
    ostatnia.nagroda = wsad$Reward[i]
    
    
    if (ostatni != "jeszcze_nie") {
      Stay.Shift <- ifelse(wsad$Choice[i] == ostatni, "Stay", "Shift")
      Shifted.Choice = ostatni
    }else{Stay.Shift = "nie_wiem"}
    
    ostatni = wsad$Choice[i]
    
  }

  if (nrow(subset(wsad[ostatni.rewersal:i, ], Choice == "Correct")) > 0) {
    
    p.80 = nrow(subset(wsad[ostatni.rewersal:i, ], Choice == "Correct" & Reward == "tak"))/nrow(subset(wsad[ostatni.rewersal:i, ], Choice == "Correct"))
    
  }else{
    p.80 = NA
  }
  
  if (nrow(subset(wsad[ostatni.rewersal:i, ], Choice == "Incorrect")) > 0) {
    
    p.20 = nrow(subset(wsad[ostatni.rewersal:i, ], Choice == "Incorrect" & Reward == "tak"))/nrow(subset(wsad[ostatni.rewersal:i, ], Choice == "Incorrect"))
    
  }else{
    p.20 = NA
  }
  
  
 linijka <- data.frame(wsad[i, ], Shifted.Choice, Win.Lose, Stay.Shift, zamiana, p.80, p.20)
 analiza <- rbind(analiza, linijka)

}

levels(analiza$Shifted.Choice[analiza$zamiana==1]) <- sub("Incorrect", "Correct", levels(analiza$Shifted.Choice[analiza$zamiana==1]))

category <- paste(analiza$Shifted.Choice, analiza$Win.Lose, analiza$Stay.Shift, sep = "_")

category.simple <- paste(analiza$Win.Lose, analiza$Stay.Shift, sep = "_")

without.choice <- paste(analiza$Shifted.Choice, analiza$Win.Lose, sep = "_")


analiza <- data.frame(analiza, category, category.simple, without.choice)

require(plyr)

myszki <- unique(analiza$Subject)

analiza2 <- data.frame()

for (i in 1:length(myszki)) {
  
  jedna.mysz <- subset(analiza, Subject == myszki[i])
  dni <- data.frame(unique(jedna.mysz$Start.Date), c(1:length(unique(jedna.mysz$Start.Date))))
  colnames(dni) <- c("dzien", "sesja")
  
  temp <- ddply(jedna.mysz,
                colnames(jedna.mysz),
                transform,
                sesja = dni[which(dni$dzien == Start.Date), "sesja"])
  
  analiza2 <- rbind(analiza2, temp)
  
}


require(plyr)
require(stringr)

regulka_2 <- "[[:alpha:]]+[_]{1}[[:alpha:]]+[_]{1}Stay+[#]{1}[[:alpha:]]+[_]{1}[[:alpha:]]+[_]{1}[[:alpha:]]+"


sesje_uproszczone <- ddply(analiza2, c("sesja", "Subject"), summarize,
                           liczba.triali = length(Win.Lose),
                           liczba.Win.Stay = length(which(Win.Lose == "Win" & Stay.Shift == "Stay")),
                           liczba.Lose.Shift = length(which(Win.Lose == "Lose" & Stay.Shift == "Shift")),
                           REWARD.count = length(which(Reward == "tak")),
                           ULAMEK.WIN.STAY = liczba.Win.Stay/length(which(Win.Lose == "Win")),
                           ULAMEK.LOSE.SHIFT = liczba.Lose.Shift/length(which(Win.Lose == "Lose")),
                           OMISSION.count = length(which(Win.Lose == "Omission")),
                           triali.bez.omisji = liczba.triali - OMISSION.count,
                           liczba.Correct = length(which(Shifted.Choice == "Correct")),
                           ulamek.Correct = liczba.Correct/triali.bez.omisji,
                           liczba.Incorrect = length(which(Shifted.Choice == "Incorrect")),
                           liczba.Reversals = sum(zamiana)
)


write.csv2(sesje_uproszczone, file =  paste0(("wstepne_podsumowanie"),format(Sys.time(), "%d_%m_%Y_%H_%M_%S"),(".csv")))


srednie.wykres <- ddply(sesje_uproszczone, c("sesja"), summarize,
                        ile.obserwacji = length(Subject),
                        srednia.nagrod = mean(REWARD.count, na.rm = TRUE),
                        sredni.ULAMEK.WIN.STAY = mean(ULAMEK.WIN.STAY, na.rm = TRUE),
                        sredni.ULAMEK.LOSE.SHIFT = mean(ULAMEK.LOSE.SHIFT, na.rm = TRUE),
                        sredni.ulamek.correct = mean(ulamek.Correct, na.rm = TRUE),
                        srednia.reversal = mean(liczba.Reversals, na.rm = TRUE),
                        sem.nagrod = sd(REWARD.count, na.rm = TRUE)/sqrt(ile.obserwacji),
                        sem.ULAMEK.WIN.STAY = sd(ULAMEK.WIN.STAY, na.rm = TRUE)/sqrt(ile.obserwacji),
                        sem.ULAMEK.LOSE.SHIFT = sd(ULAMEK.LOSE.SHIFT, na.rm = TRUE)/sqrt(ile.obserwacji),
                        sem.ulamek.correct = sd(ulamek.Correct, na.rm = TRUE)/sqrt(ile.obserwacji),
                        sem.reversal = sd(liczba.Reversals, na.rm = TRUE)/sqrt(ile.obserwacji)
)

# diferentiated to correct/incorrect choice

sesje_z_podzialem_na_correct <- ddply(analiza2, c("sesja", "Subject", "Shifted.Choice"), summarize,
                                      liczba.triali = length(Win.Lose),
                                      liczba.nagrod = length(which(Reward == "tak")),
                                      liczba.Win.Stay = length(which(Win.Lose == "Win" & Stay.Shift == "Stay")),
                                      ULAMEK.WIN.STAY = liczba.Win.Stay/length(which(Win.Lose == "Win")),
                                      liczba.Lose.Shift = length(which(Win.Lose == "Lose" & Stay.Shift == "Shift")),
                                      ULAMEK.LOSE.SHIFT = liczba.Lose.Shift/length(which(Win.Lose == "Lose")),
                                      liczba.Ommission = length(which(Stay.Shift == "Omission")),
                                      liczba.Reversals = sum(zamiana),
                                      srednia.latencja = mean(Choice.Latency, na.rm = TRUE),
                                      liczba.Win = length(which(Win.Lose == "Win")),
                                      liczba.Lose = length(which(Win.Lose == "Lose"))
                                      
)

sesje_z_podzialem_na_correct <- subset(sesje_z_podzialem_na_correct, Shifted.Choice %in% c("Correct", "Incorrect"))


#table

tabela.cor.incor <- data.frame(sesje_z_podzialem_na_correct$Subject,
                               sesje_z_podzialem_na_correct$Shifted.Choice,
                               sesje_z_podzialem_na_correct$ULAMEK.WIN.STAY,
                               sesje_z_podzialem_na_correct$ULAMEK.LOSE.SHIFT,
                               sesje_z_podzialem_na_correct$srednia.latencja)

tabela.general <- data.frame(sesje_uproszczone$Subject, 
                             sesje_uproszczone$liczba.triali,
                             sesje_uproszczone$REWARD.count,
                             sesje_uproszczone$ulamek.Correct, 
                             sesje_uproszczone$liczba.Reversals,
                             sesje_uproszczone$OMISSION.count)

tabela.Correct <- split(tabela.cor.incor,
                        tabela.cor.incor$sesje_z_podzialem_na_correct.Shifted.Choice)

calosc <- cbind(tabela.general, 
                tabela.Correct$Correct[,3:5], 
                tabela.Correct$Incorrect[,3:5])

colnames(calosc)<- c("Subject","trials", "rewards", "% correct", "Reversals", "Omissions", "Win.Stay.Correct", "Lose.Shift.Correct", "Latency.Correct", "Win.Stay.InCorrect", "Lose.Shift.InCorrect", "Latency.InCorrect")

write.csv2(calosc, file =  paste0(("reszta_danych"),format(Sys.time(), "%d_%m_%Y_%H_%M_%S"),(".csv")))

print(paste("i tabelki gotowe :D"))

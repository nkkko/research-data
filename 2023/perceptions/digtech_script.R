library(here)
library(dplyr) 
library(readxl) # read excel
library(xlsx2dfs)
library(ltm) # chronbach alpha i t-test
library(likert)
library(cowplot)
library(RColorBrewer)
library(ggplot2)
library(grid)
library(psych)

devtools::install_github("matherion/userfriendlyscience", dependencies=T)
library(userfriendlyscience)

font_size = "12"
line_height = "0.8"

# setwd("/Users/nikola/Dropbox/20-29 Personal/20 PDS EIT/20.02 Dig_Teh_za_ucenje")
data <- read_xlsx("results-survey517778.xlsx")

data=as.data.frame(data)

data[data == "Izvanredni ili redoviti profesor" | data == "Uprava" | data == "Asistent ili docent" | data == "Predavač" ] <- "University teacher"
data[data == "Student prediplomskog studija" | data == "Student diplomskog studija" | data == "Student poslijediplomskog studija"] <- "Student"
data[data == "Ženski"] <- "Woman"
data[data == "Muški"] <- "Man"
data[data == "manje od 20"] <- "< 20"
data[data == "21 do 25"] <- "21 to 25"
data[data == "26 do 35"] <- "26 to 35"
data[data == "36 do 45"] <- "36 to 45"
data[data == "46 do 55"] <- "46 to 55"
data[data == "56 do 65"] <- "56 to 65"
data[data == "66 i više"] <- "> 65"
data[data == "Humanističke znanosti"] <- "Humanities"    
data[data == "Društvene znanost"] <- "Social Sciences"  
data[data == "Umjetničko područje"] <- "Arts"
data[data == "Tehničke znanosti"] <- "Technical Sciences"       
data[data == "Biomedicina i zdravstvo"] <- "Biomedicine and Health"     
data[data == "Biotehničke znanosti"] <- "Biotechnical Sciences"        
data[data == "Interdisciplinarno područje"] <- "Interdisciplinary"
data[data == "Prirodne znanosti"] <- "Natural Sciences"           

### novi stupac u kojem smo grupirali drustvene znanosti i umjetnicke naspram STEM
data <- data %>%
          mutate(STEMvsSSHA=if_else(`Područje znanosti ili umjetnosti`=="Humanities","SSHA", 
                                    if_else(`Područje znanosti ili umjetnosti`=="Social Sciences","SSHA",
                                            if_else(`Područje znanosti ili umjetnosti`=="Arts","SSHA","STEM"))),
                .after=`Područje znanosti ili umjetnosti`)

# head(data)
# tail(data)
# length(data)
# dim(data)
# str(data)
# levels(data$Rod)
# names(data)
# table(PROCIJENA$`Age group`)

### rekodiranje likertovih skala u brojčane vrijednosti ###
data.recoded <- data %>% mutate_at(vars(starts_with(c("Procijenite vlastitu digitalnu spremnost.",
                                                       "Smatram da digitalne tehnologije za učenje i podučavanje na daljinu",
                                                       "Koji su ključni izazovi učenja i podučavanja na daljinu?",
                                                       "Kako poboljšati učenje i podučavanje na daljinu?"))),
                                      funs(case_when(
                                                  .=="potpuno se ne slažem"~1,
                                                  .=="ne slažem se"~2,
                                                  .=="imam neutralan stav"~3,
                                                  .=="slažem se"~4,
                                                  .=="slažem se u potpunosti"~5)))

data.recoded <- data.recoded %>% mutate_at(vars(starts_with(c("Koje digitalne alate koristite za komunikaciju student-predavač?"))),
                                    funs(case_when(
                                      .=="Vrlo često"~4,
                                      .=="Ponekad"~3,
                                      .=="Rijetko"~2,
                                      .=="Nikada"~1)))

data.recoded <- data.recoded %>% mutate_at(vars(starts_with(c("Kakav je Vaš stav prema sljedećim aspektima učenja na daljinu?",
                                                       "Kakav je Vaš stav prema sljedećim aspektima podučavanja na daljinu?"))),
                                    funs(case_when(
                                      .=="potpuno se ne slažem"~1,
                                      .=="ne slažem se"~2,
                                      .=="neutralan"~3,
                                      .=="slažem se"~4,
                                      .=="potpuno se slažem"~5)))

data.recoded <- data.recoded %>% mutate_at(vars(starts_with(c("U kojoj mjeri smatrate da ..."))),
                                    funs(case_when(
                                      .=="nimalo"~1,
                                      .=="uglavnom ne"~2,
                                      .=="osrednje, da i ne"~3,
                                      .=="uglavnom da"~4,
                                      .=="u potpunosti"~5)))

### KRAJ rekodiranja ###

### cronbach alpha 
# the data columns contain 8 constructs in Likert form (each one contain several dimensions which are the heads of the column)
# here we will group questions in constructs with the given headings

ID = data.recoded[,c(8:10,12:13)] # incijalni podaci o ispitaniku poput dobi, spola, etc.
colnames(ID) <- c("Age group",
                         "Sex",
                         "Target group",
                         "Field of science",
                         "SSHAvsSTEM")

# merganje inicijalnih podataka samo sa setom pitanja za svaku pojedinu skupinu
PROCIJENA = cbind(ID,data.recoded[,c("Procijenite vlastitu digitalnu spremnost. [Korištenje računala ne uzrokuje mi nelagodu.]", ######! Popraviti i obrnuti skalu
                    "Procijenite vlastitu digitalnu spremnost. [Uživam u korištenju računala.]",
                    "Procijenite vlastitu digitalnu spremnost. [Bolje se snalazim na pametnom telefonu nego na računalu.]",
                    "Procijenite vlastitu digitalnu spremnost. [Računalo koristim aktivno svaki dan i van redovitih obveza.]",
                    "Procijenite vlastitu digitalnu spremnost. [Spremam i pronalazim datoteke  na računalu s iznimnom lakoćom.]",
                    "Procijenite vlastitu digitalnu spremnost. [Na internetu pronalazim stvari s iznimnom lakoćom.]",
                    "Procijenite vlastitu digitalnu spremnost. [Spremno koristim razne tehnologije za pronalaženje informacija, izradu digitalnih sadržaja i dijeljenje informacija s drugima.]",
                    "Procijenite vlastitu digitalnu spremnost. [Koristim sustave za rad i učenje na daljinu s iznimnom lakoćom.]",
                    "Procijenite vlastitu digitalnu spremnost. [Osjećam samopouzdanje kada koristim učenje na daljinu čak i ako nikada prije nisam koristio takav sustav.]")])

PROCIJENA_translate <- c("Computers aren't causing me unease",
                     "I enjoy working with computers",
                     "I am better with smartphone than computer",
                     "I use computers actively every day and outside of regular work",
                     "I can save and find files with ease",
                     "I can find things online with ease",
                     "I readily use technology to find and evaluate information, produce digital content, and share information with others",
                     "I use distance working and learning tools with ease",
                     "I am confident of using distance learning even if I have never used such a system before")
colnames(PROCIJENA)[6:14] <- PROCIJENA_translate

ALATI = cbind(ID,data.recoded[,c("Koje digitalne alate koristite za komunikaciju student-predavač? [Poruke u sustavu e-učenja]",
                "Koje digitalne alate koristite za komunikaciju student-predavač? [Dopisivanje putem e-maila]",
                "Koje digitalne alate koristite za komunikaciju student-predavač? [Izravna interakcija tijekom predavanja (video ili chat)]",
                "Koje digitalne alate koristite za komunikaciju student-predavač? [Video pozivi (konzultacije jedan na jedan)]",
                "Koje digitalne alate koristite za komunikaciju student-predavač? [Online forumi]",
                "Koje digitalne alate koristite za komunikaciju student-predavač? [Poruke na društvenim mrežama (npr. Facebook i dr.)]",
                "Koje digitalne alate koristite za komunikaciju student-predavač? [Tekstualne poruke (npr. WhatsApp, Telegram, Signal i dr.)]",
                "Koje digitalne alate koristite za komunikaciju student-predavač? [Glasovne ili video poruke (npr. WhatsApp, Telegram, Signal i dr.)]",
                "Koje digitalne alate koristite za komunikaciju student-predavač? [Timske komunikacijske platforme (npr. Slack, Discord, i dr.)]")])

ALATI_translate <- c("Messages in e-learning",
                           "E-mail corespondence",
                           "Direct interaction during lecture via video or chat",
                           "One on one video calls",
                           "Online forums",
                           "Messages on social media (e.g. Facebook, etc.)",
                           "Text messages (e.g. WhatsApp, Telegram, Signal, etc.)",
                           "Voice or video messages (e.g. WhatsApp, Telegram, Signal, etc.)",
                           "Team communication platforms (e.g. Slack, Discord, etc.)")
colnames(ALATI)[6:14] <- ALATI_translate
  
SMATRAM = cbind(ID,data.recoded[,c("Smatram da digitalne tehnologije za učenje i podučavanje na daljinu: [Poboljšavaju interakciju između studenta i predavača.]",
                  "Smatram da digitalne tehnologije za učenje i podučavanje na daljinu: [Osiguravaju bolje okruženje za učenje u odnosu na predavaonicu.]",
                  "Smatram da digitalne tehnologije za učenje i podučavanje na daljinu: [Osiguravaju veću fleksibilnost u podučavanju i učenju.]",
                  "Smatram da digitalne tehnologije za učenje i podučavanje na daljinu: [Štede trud i vrijeme.]",
                  "Smatram da digitalne tehnologije za učenje i podučavanje na daljinu: [Povećavaju kvalitetu učenja.]")])

SMATRAM_translate <- c("Increase interaction between students and lecturers",
                           "Ensure better learning environment than lecture rooms",
                           "Ensure greater flexibility in teaching and learning",
                           "Save effort and time",
                           "Increase quality of learning")
colnames(SMATRAM)[6:10] <- SMATRAM_translate
  
IZAZOVI = cbind(ID,data.recoded[,c("Koji su ključni izazovi učenja i podučavanja na daljinu? [Smetnje zbog okoline u kojoj se nalazim.]",
                  "Koji su ključni izazovi učenja i podučavanja na daljinu? [Smetnje zbog internetskih distrakcija.]",
                  "Koji su ključni izazovi učenja i podučavanja na daljinu? [Teže je koncentrirati se na zahtjevnije probleme.]",
                  "Koji su ključni izazovi učenja i podučavanja na daljinu? [Nedostatak ljudske interakcije.]",
                  "Koji su ključni izazovi učenja i podučavanja na daljinu? [Tehnički problemi.]",
                  "Koji su ključni izazovi učenja i podučavanja na daljinu? [Nedostatak zabave.]")])

IZAZOVI_translate <- c("External environmental distractions",
                           "Online distractions",
                           "It is harder to concentrate on challanging topics",
                           "Lack of human interaction",
                           "Technical issues",
                           "Lack of fun")
colnames(IZAZOVI)[6:11] <- IZAZOVI_translate

POBOLJSANJA = cbind(ID,data.recoded[,c("Kako poboljšati učenje i podučavanje na daljinu? [Boljom prilagodbom nastavnih materijala od strane predavača za novi medij.]",
                      "Kako poboljšati učenje i podučavanje na daljinu? [Kvalitetnijom pripremom predavača i studenata.]",
                      "Kako poboljšati učenje i podučavanje na daljinu? [Većom dostupnosti i raspoloživosti predavača.]",
                      "Kako poboljšati učenje i podučavanje na daljinu? [Dinamičnijom i interaktivnijom nastavom.]",
                      "Kako poboljšati učenje i podučavanje na daljinu? [Boljom računalnom opremom.]",
                      "Kako poboljšati učenje i podučavanje na daljinu? [Boljim iskustvom u korištenju postojećih programskih rješenja.]",
                      "Kako poboljšati učenje i podučavanje na daljinu? [Potpuno novim programskim rješenjima, jer postojeća nisu dovoljno dobra.]")])

POBOLJSANJA_translate <- c("Better adaptation of teaching materials by lecturers for new media",
                           "Better preparation of lecturers and students",
                           "Greater availability of lecturers",
                           "More dynamic and interactive teaching",
                           "Better computer equipement",
                           "Better experience in using existing software solutions",
                           "Brand new software solutions, because the existing ones are not good enough")
colnames(POBOLJSANJA)[6:12] <- POBOLJSANJA_translate

STAVST = cbind(ID,data.recoded[,c("Kakav je Vaš stav prema sljedećim aspektima učenja na daljinu? [Uživam u učenju u internetskom okruženju.]",
                         "Kakav je Vaš stav prema sljedećim aspektima učenja na daljinu? [Učenje na daljinu je učinkovitije.]",
                         "Kakav je Vaš stav prema sljedećim aspektima učenja na daljinu? [Više volim učenje na daljinu od tradicionalnog učenja u učionici.]",
                         "Kakav je Vaš stav prema sljedećim aspektima učenja na daljinu? [Učenje na daljinu lakše je od tradicionalnog učenja u učionici.]",
                         "Kakav je Vaš stav prema sljedećim aspektima učenja na daljinu? [Digitalni alati pomažu mi da bolje savladam gradivo.]",
                         "Kakav je Vaš stav prema sljedećim aspektima učenja na daljinu? [Digitalni alati koje koristim za učenje na daljinu su pouzdani.]",
                         "Kakav je Vaš stav prema sljedećim aspektima učenja na daljinu? [Digitalni alati koje koristim za učenje na daljinu nisu komplicirani.]",
                         "Kakav je Vaš stav prema sljedećim aspektima učenja na daljinu? [Moji nastavnici su aktivnije uključeni u proces učenja.]",
                         "Kakav je Vaš stav prema sljedećim aspektima učenja na daljinu? [Ne osjećam se da podučavam samoga sebe.]",
                         "Kakav je Vaš stav prema sljedećim aspektima učenja na daljinu? [Ne mislim da trebam više vremena za učenje.]")])
STAVST = STAVST[rowSums(is.na(STAVST[6:15])) != ncol(STAVST[6:15]), ] # ukloni sve prazne linije s NA

STAVST_translate <- c("I enjoy learning in an online environment",
                            "Distance learning is more efficient",
                            "I prefer distance learning to traditional classroom learning",
                            "Distance learning is easier than traditiona classroom learning",
                            "Digital tools help me master the material better",
                            "Digital tools I use for distance learning are reliable",
                            "Digital tools I use for distance learning are not complicated",
                            "My teachers are more actively involved in the learning process",
                            "I don't feel like teaching myself",
                            "I don't think I need more time")
colnames(STAVST)[6:15] <- STAVST_translate
  
STAVPR = cbind(ID,data.recoded[,c("Kakav je Vaš stav prema sljedećim aspektima podučavanja na daljinu? [Uživam podučavati u internetskom okruženju.]",
                         "Kakav je Vaš stav prema sljedećim aspektima podučavanja na daljinu? [Podučavanje na daljinu je učinkovitije.]",
                         "Kakav je Vaš stav prema sljedećim aspektima podučavanja na daljinu? [Više volim podučavanje na daljinu od tradicionalnog podučavanja u učionici.]",
                         "Kakav je Vaš stav prema sljedećim aspektima podučavanja na daljinu? [Podučavanje na daljinu lakše je od tradicionalnog podučavanja u učionici.]",
                         "Kakav je Vaš stav prema sljedećim aspektima podučavanja na daljinu? [Digitalni alati pomažu mi u boljem podučavanju.]",
                         "Kakav je Vaš stav prema sljedećim aspektima podučavanja na daljinu? [Digitalni alati koje koristim za podučavanje na daljinu su pouzdani.]",
                         "Kakav je Vaš stav prema sljedećim aspektima podučavanja na daljinu? [Digitalni alati koje koristim za podučavanje na daljinu nisu komplicirani.]",
                         "Kakav je Vaš stav prema sljedećim aspektima podučavanja na daljinu? [Moji studenti su aktivnije uključeni u proces učenja.]",
                         "Kakav je Vaš stav prema sljedećim aspektima podučavanja na daljinu? [Ne osjećam se da podučavam samoga sebe.]",
                         "Kakav je Vaš stav prema sljedećim aspektima podučavanja na daljinu? [Ne mislim da trebam više vremena.]")])
STAVPR = STAVPR[rowSums(is.na(STAVPR[6:15])) != ncol(STAVPR[6:15]), ] # ukloni sve prazne linije s NA

STAVPR_translate <- c("I enjoy teaching in an online environment",
                            "Distance teaching is more efficient",
                            "I prefer distance teaching to traditional classroom teaching",
                            "Distance teaching is easier than traditiona classroom teaching",
                            "Digital tools help me being a better teacher",
                            "Digital tools I use for distance teaching are reliable",
                            "Digital tools I use for distance teaching are not complicated",
                            "My students are more actively involved in the learning process",
                            "I don't feel like teaching myself",
                            "I don't think I need more time")
colnames(STAVPR)[6:15] <- STAVPR_translate

COVID =  cbind(ID,data.recoded[,c("U kojoj mjeri smatrate da ... [je vaša ustanova bila pripremljena za obavljanje obrazovnih aktivnosti na daljinu prije pandemije?]",
                        "U kojoj mjeri smatrate da ... [je vaša ustanova sada spremna za provođenje obrazovnih aktivnosti na daljinu?]",
                        "U kojoj mjeri smatrate da ... [će se većina digitalnih alata koje smo koristili prošle godine i dalje koristiti?]",
                        "U kojoj mjeri smatrate da ... [su predavači bili pripremljeni za provođenje obrazovnih aktivnosti na daljinu?]",
                        "U kojoj mjeri smatrate da ... [su predavači sada spremni za obavljanje obrazovnih aktivnosti na daljinu?]"#,
                        #"U kojoj mjeri smatrate da ... [će se obrazovne aktivnosti na daljinu vratit na pred-pandemijsko stanje u narednom periodu?]", # izbacio pitanje zbog Cronbach Alphe, ocito je ispitanicima proricanje buducnosti jako nekonzistentno
                        #"U kojoj mjeri smatrate da ... [će učenje na daljinu u prethodnoj godini utjecati negativno na ocjene?]"
                        )])

COVID_translate <- c("Your institution was prepared for carrying out online educational activities before the pandemic",
                           "Your institution is now prepared for carrying out online educational activities",
                           "Majority of digital tools that we have used this year will continue to be used",
                           "The teachers were prepared for carrying out online educational activities",
                           "The teachers are now ready for carrying out online educational activities")
colnames(COVID)[6:10] <- COVID_translate

### measure of internal consistency - Cronbach’s alpha is computed by correlating the score for each scale item with the total score for each observation (usually individual survey respondents or test takers), and then comparing that to the variance for all individual item scores.
print(cronbach.alpha(data.recoded[c(19,21:26,70:75,77,79,43:51, 52:56, 57:62, 63:69,90:94)], na.rm = TRUE)["alpha"])

print(cronbach.alpha(PROCIJENA[c(7,9:14)], na.rm = TRUE)["alpha"])
print(cronbach.alpha(ALATI[6:14], na.rm = TRUE)["alpha"])
print(cronbach.alpha(SMATRAM[6:10], na.rm = TRUE)["alpha"])
print(cronbach.alpha(IZAZOVI[6:11], na.rm = TRUE)["alpha"])
print(cronbach.alpha(POBOLJSANJA[6:12], na.rm = TRUE)["alpha"])
print(cronbach.alpha(STAVST[6:15], na.rm = TRUE)["alpha"])
print(cronbach.alpha(STAVPR[6:15], na.rm = TRUE)["alpha"])
print(cronbach.alpha(COVID[6:8], na.rm = TRUE)["alpha"])

userfriendlyscience::scaleReliability(PROCIJENA[c(7,9:14)])
userfriendlyscience::scaleReliability(ALATI[6:14])
userfriendlyscience::scaleReliability(SMATRAM[6:10])
userfriendlyscience::scaleReliability(IZAZOVI[6:11])
userfriendlyscience::scaleReliability(POBOLJSANJA[6:12])
userfriendlyscience::scaleReliability(STAVST[6:15])
userfriendlyscience::scaleReliability(STAVPR[6:15])
userfriendlyscience::scaleReliability(COVID[6:10])

# c(19,21:26), 43:51, 52:56, 57:62, 63:69, 70:79, 80:89, 90:94 
# c(70:75,77,79), c(80:85,87,89)

omega(data.recoded[80:89]) # ovo se odnosi na PROCIJENA set iz orig. dataseta jer tamo nisu faktori vec num

# ftable(xtabs(~ SMATRAM$`Target group` + SMATRAM$`Increase quality of learning` + SMATRAM$SSHAvsSTEM, data = SMATRAM))

### END reliability testing


mylevels1 <- c("Strongly Disagree",
              "Disagree",
              "Undecided",
              "Agree",
              "Strongly Agree")

mylevels2 <- c("Very frequently",
               "Occasionally",
               "Rarely",
               "Never")

mylevels3 <- c("Definitely Not",
               "Mostly Not",
               "Neutral",
               "Mostly Yes",
               "Definitely Yes")

###  pretvori odgovore u faktore
PROCIJENA[6:14] <- as.data.frame(lapply(PROCIJENA[6:14], factor, levels=1:5, labels = mylevels1))
ALATI[6:14] <- as.data.frame(lapply(ALATI[6:14], factor, levels=1:4, labels = mylevels2))
SMATRAM[6:10] <- as.data.frame(lapply(SMATRAM[6:10], factor, levels=1:5, labels = mylevels1))
IZAZOVI[6:11] <- as.data.frame(lapply(IZAZOVI[6:11], factor, levels=1:5, labels = mylevels1))
POBOLJSANJA[6:12] <- as.data.frame(lapply(POBOLJSANJA[6:12], factor, levels=1:5, labels = mylevels1))
STAVST[6:15] <- as.data.frame(lapply(STAVST[6:15], factor, levels=1:5, labels = mylevels1))
STAVPR[6:15] <- as.data.frame(lapply(STAVPR[6:15], factor, levels=1:5, labels = mylevels1))
COVID[6:10] <- as.data.frame(lapply(COVID[6:10], factor, levels=1:5, labels = mylevels3))

### napravi likert df
PROCIJENA_likert <- likert(PROCIJENA[6:14])
ALATI_likert <- likert(ALATI[6:14])

SMATRAM_likert_s <- likert(SMATRAM[6:10] %>% filter(SMATRAM$`Target group` == "Student"))
SMATRAM_likert_t <- likert(SMATRAM[6:10] %>% filter(SMATRAM$`Target group` == "University teacher"))

IZAZOVI_likert_s <- likert(IZAZOVI[6:11] %>% filter(SMATRAM$`Target group` == "Student"))
IZAZOVI_likert_t <- likert(IZAZOVI[6:11] %>% filter(SMATRAM$`Target group` == "University teacher"))

POBOLJSANJA_likert <- likert(POBOLJSANJA[6:12])

STAVST_likert <- likert(STAVST[6:15])
STAVPR_likert <- likert(STAVPR[6:15])

COVID_likert  <- likert(COVID[6:10])

likert_list <- list(PROCIJENA_likert, SMATRAM_likert, IZAZOVI_likert, POBOLJSANJA_likert, STAVST_likert, STAVPR_likert, COVID_likert)

### pripremi i spremi xlsx tablicu sa svim summary tablicama po setovima podataka
dfs <- list("PROCIJENA"=summary(PROCIJENA_likert),
            "ALATI"=summary(ALATI_likert),
            "POBOLJSANJA"=summary(POBOLJSANJA_likert),
            "STAVST"=rbind(summary(STAVST_likert), summary(SMATRAM_likert_s), summary(IZAZOVI_likert_s)),
            "STAVPR"=rbind(summary(STAVPR_likert), summary(SMATRAM_likert_t), summary(IZAZOVI_likert_t)),
            "COVID"=summary(COVID_likert))

dfs2xlsx(dfs, "summary/summary.xlsx")
### KRAJ

### spremanje likert grafova u zasebne dokumente
titlenames <- c("Selfassesment of digital readiness",
                "Opinion on digital technologies for distant learning and teaching",
                "Key challenges of distant learning and teaching",
                "How to improve distant learning and teaching",
                "Students opinion on distant learning",
                "Teachers opinions on distant learning",
                "Impact of COVID-19 pandemic on distant learning and teaching")
i=0
for (i in seq(titlenames)) {
plot_tmp <- plot(likert_list[[i]],
       ordered=F,
       legend.position="right",
       centered = T,
       colors = brewer.pal(5,"RdYlBu"),
       bar.color = "#ced4da", 
       missing.bar.color = "#d62828", 
       label.missing = T,
       wrap = 50) + # 100 za grupirane, a 30 za negrupirane
    ggtitle(titlenames[i]) + 
    theme(plot.title = element_text(size=font_size,lineheight=line_height))
  
cowplot::save_plot(here("images", paste(i, titlenames[i], "likert.png")), # where to save the plot
                     plot_tmp,        # object to plot
                     base_asp = 1.5,  # ratio of space for questions vs space for plot
                     base_height = 6) # size! higher for smaller font size
}

### posebno za alate skeciju
plot_tmp_alati <- plot(likert(items=ALATI[6:14]), #, grouping=ALATI$SSHAvsSTEM),
                 ordered=F,
                 legend.position="right",
                 centered = T,
                 colors = brewer.pal(4,"RdYlBu"),
                 bar.color = "#ced4da", 
                 missing.bar.color = "#d62828", 
                 label.missing = T,
                 wrap = 100) + # 100 za grupirane, a 30 za negrupirane
  ggtitle("Which digital tools do you use for teacher-student communication") + 
  theme(plot.title = element_text(size=font_size,lineheight=line_height))

cowplot::save_plot(here("images", "ALATI_stem_likert.png"), # where to save the plot
                   plot_tmp_alati,        # object to plot
                   base_asp = 1.4,  # ratio of space for questions vs space for plot
                   base_height = 8) # size! higher for smaller font size
### KRAJ

### pripremi grupirane likert objekte
df_list <- list(PROCIJENA, SMATRAM, IZAZOVI, POBOLJSANJA, COVID, STAVST, STAVPR)

titlenames2 <- c("Selfassesment of digital readiness",
                "Opinion on digital technologies for distant learning and teaching",
                "Key challenges of distant learning and teaching",
                "How to improve distant learning and teaching",
                "Impact of COVID-19 pandemic on distant learning and teaching",
                "Students opinion on distant learning",
                "Teachers opinions on distant learning"
                )

for (groupX in c(1,2,4,5,3)) { #redosljed da izbjegnem sto Target group za STAVPR i STAVST ima samo jednu varijablu
for (i in seq(titlenames2)) {
  plot_tmp <- plot(likert(items=df_list[[i]][,6:ncol(df_list[[i]]),drop=F], grouping=df_list[[i]][,groupX]),
                       ordered=F,
                       legend.position="right",
                       centered = T,
                       colors = brewer.pal(5,"RdYlBu"),
                       bar.color = "#ced4da", 
                       missing.bar.color = "#d62828", 
                       label.missing = T,
                       wrap = 150) + # 100 za grupirane, a 30 za negrupirane
                        ggtitle(paste(titlenames2[i], "- grouped")) + 
                        theme(plot.title = element_text(size=font_size,lineheight=line_height))
  
  assign(paste("plot grouped by -", colnames(df_list[[i]][groupX]), "-", titlenames2[i]), plot_tmp) # kreiraj entitete u memoriji za kasnije koristenje
  
  cowplot::save_plot(here("images", paste("grouped_by_", colnames(df_list[[i]][groupX]), i, titlenames2[i], "likert.png")), # where to save the plot
                     plot_tmp,        # object to plot
                     base_asp = 1.4,  # ratio of space for questions vs space for plot
                     base_height = 8) # size! higher for smaller font size
  print(colnames(df_list[[i]][groupX]))
}
}

### test RUCNO
plot_tmp <- plot(likert(items=PROCIJENA[c(7,9:13)], grouping=PROCIJENA[,3]),
                 ordered=F,
                 legend.position="right",
                 centered = T,
                 colors = brewer.pal(5,"RdYlBu"),
                 bar.color = "#ced4da", 
                 missing.bar.color = "#d62828", 
                 label.missing = T,
                 wrap = 150) + # 100 za grupirane, a 30 za negrupirane
  ggtitle("Self-assessment of digital readiness - grouped") + 
  theme(plot.title = element_text(size=font_size,lineheight=line_height))

cowplot::save_plot(here("images", paste("grouped_by_", "target group", "PROCIJENA" , "likert.png")), # where to save the plot
                   plot_tmp,        # object to plot
                   base_asp = 1.4,  # ratio of space for questions vs space for plot
                   base_height = 8) # size! higher for smaller font size

# create pie charts
pie_age <- PROCIJENA %>%
  group_by(`Age group`) %>%
  summarise(freq = n()) %>%
  ggplot(aes(x = 2, y = freq, fill = reorder(`Age group`, freq))) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    geom_label(aes(label = freq), size=5, color = "white", position = position_stack(vjust = 0.5), show.legend = FALSE)+
    scale_fill_brewer(palette = "Paired") +
    theme_void()+
    xlim(0.5, 2.5) +
    guides(fill = guide_legend(title = "Age group"))
              
pie_sex <- PROCIJENA %>%
  group_by(Sex) %>%
  summarise(freq = n()) %>%
  ggplot(aes(x = 2, y = freq, fill = reorder(Sex, freq))) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    geom_label(aes(label = freq), size=5, color = "white", position = position_stack(vjust = 0.5), show.legend = FALSE)+
    scale_fill_brewer(palette = "Paired") +
    theme_void()+
    xlim(0.5, 2.5) +
    guides(fill = guide_legend(title = "Sex"))

pie_target <- PROCIJENA %>%
  group_by(`Target group`) %>%
  summarise(freq = n()) %>%
  ggplot(aes(x = 2, y = freq, fill = reorder(`Target group`, freq))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_label(aes(label = freq), size=5, color = "white", position = position_stack(vjust = 0.5), show.legend = FALSE)+
  scale_fill_brewer(palette = "Paired") +
  theme_void()+
  xlim(0.5, 2.5) +
  guides(fill = guide_legend(title = "Targer grooup"))

pie_field <- PROCIJENA %>%
  group_by(`Field of science`) %>%
  summarise(freq = n()) %>%
  ggplot(aes(x = 2, y = freq, fill = reorder(`Field of science`, freq))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_label(aes(label = freq), size=5, color = "white", position = position_stack(vjust = 0.5), show.legend = FALSE)+
  scale_fill_brewer(palette = "Paired") +
  theme_void()+
  xlim(0.5, 2.5) +
  guides(fill = guide_legend(title = "Field of science"))

pie_ss <- PROCIJENA %>%
  group_by(SSHAvsSTEM) %>%
  summarise(freq = n()) %>%
  ggplot(aes(x = 2, y = freq, fill = reorder(SSHAvsSTEM, freq))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_label(aes(label = freq), size=5, color = "white", position = position_stack(vjust = 0.5), show.legend = FALSE)+
  scale_fill_brewer(palette = "Paired") +
  theme_void()+
  xlim(0.5, 2.5) +
  guides(fill = guide_legend(title = "SSHA vs STEM"))

cowplot::save_plot(here("images", paste("pie_target.png")), # where to save the plot
                   pie_target,        # object to plot
                   base_asp = 1.5,  # ratio of space for questions vs space for plot
                   base_height = 5) # size! higher for smaller font size


# wilcox.test(as.numeric(IZAZOVI$`Technical issues`) ~ IZAZOVI$Sex, data = IZAZOVI)
# # W = 1511.5, p-value = 2.146e-05
# 
# wilcox.test(as.numeric(POBOLJSANJA$`Better adaptation of teaching materials by lecturers for new media`) ~ POBOLJSANJA$Sex, data = POBOLJSANJA)
# # W = 1987.5, p-value = 0.02173

stupci <- c(c(18:26), c(43:96))
statistika <- matrix(ncol = 7, nrow = 142)

for (i in stupci) {
    wilcox_sex <- wilcox.test(data.recoded[,i] ~ data.recoded$Rod, data = data.recoded, exact = FALSE)
    wilcox_field <- wilcox.test(data.recoded[,i] ~ data.recoded$STEMvsSSHA, data = data.recoded, exact = FALSE)
    statistika[i,1] <- colnames(data.recoded)[i]
    statistika[i,2] <- wilcox_sex$p.value
    statistika[i,3] <- wilcox_sex$statistic
    statistika[i,6] <- wilcox_field$p.value
    statistika[i,7] <- wilcox_field$statistic
}

stupci <- c(c(18:26), c(43:69), c(90:96))
for (i in stupci) {
  wilcox_group <- wilcox.test(data.recoded[,i] ~ data.recoded$`Skupina kojoj pripadate`, data = data.recoded, exact = FALSE)
  statistika[i,4] <- wilcox_group$p.value
  statistika[i,5] <- wilcox_group$statistic
}

statistika = as.data.frame(statistika)
names(statistika) <- c("Question", "Sex p-value", "Sex Mann-Whitney U", "Group p-value", "Group Mann-Whitney U", "Field p-value", "Field Mann-Whitney U")

dfs2xlsx(withNames("sheet1",statistika), "summary/statistika.xlsx")


### pregledni udjeli skupina
n=142
stats_overview <- data.frame()

lista_skupina <- c('Rod', 'Skupina kojoj pripadate', 'STEMvsSSHA')
for (i in seq(lista_skupina)) {
  stat_tmp <- data.recoded %>%
    group_by(data.recoded[lista_skupina[i]]) %>%
    summarise(f = n(), fp = n()/n)
  
  names(stat_tmp) <- c("var","f","fp")
  stats_overview <- rbind(stats_overview, stat_tmp)
}

stats_overview2 <- data.recoded %>% 
  group_by(across(all_of(c('Skupina kojoj pripadate', 'STEMvsSSHA')))) %>%
  summarise(f = n(), fp = n()/n)

### END OF pregledni udjeli skupina




### Means and SD table
sd <- sapply(data.recoded[sapply(data.recoded, is.numeric)], sd, na.rm = TRUE)
means <- colMeans(data.recoded[sapply(data.recoded, is.numeric)], na.rm = TRUE)

meansd <- data.frame(means, sd)

meansd <- meansd[-c(1:2),] # izbrisi prva dva retka koja nisu pitanja

translation <- c(PROCIJENA_translate, ALATI_translate, SMATRAM_translate, IZAZOVI_translate, POBOLJSANJA_translate, STAVST_translate, STAVPR_translate, COVID_translate, "", "")

meansd$translation <- translation

dfs2xlsx(withNames("sheet1",meansd), "summary/meansd.xlsx")

### polychoric correlation

tmp_corrs_data <- data.recoded %>% 
  #filter(`Skupina kojoj pripadate` == "Student") %>%
  dplyr::select(c(19,21:25))
  #18:26, 43:51, 52:56, 57:62, 63:69, 70:79, 80:89, 90:96

corrs = psych::polychoric(tmp_corrs_data, na.rm=TRUE)

#correlation viz
GGally::ggcorr(data = NULL, 
               cor_matrix = corrs[["rho"]], 
               size = 2,
               hjust = .75,
               nbreaks = 7,
               palette = "RdYlBu",
               label = TRUE, 
               label_color = "black",
               digits = 2,
               #label_alpha = .3, 
               label_round = 2, 
               label_size = 4, 
               layout.exp = 0.2) + 
  theme(legend.position = "none")

fa.parallel(tmp_corrs_data, cor="poly")

### perform Bartlett's Test of Sphericity
cor_matrix <- cor(tmp_corrs_data)
cortest.bartlett(cor_matrix, n = nrow(tmp_corrs_data))  
### END

### KMO
KMO(data.recoded[c(90:96)])  #c(19,21:26), 43:51, 52:56, 57:62, 63:69, 70:79, 80:89, 90:94 
# c(70:75,77,79), c(80:85,87,89)

data.recoded %>% 
  # filter(Rod == "Woman") %>%
  dplyr::select(90:96) %>%
  na.omit() %>%
  KMO

### END KMO



# Aufgaben Data Frame Tag 3

# 1 Laden Daten File

data_path <- "/Users/tstump/Dropbox/CAS/Kursmaterial/Tag_2/Material/Umfragedaten.txt"
Umfrage <- read.delim(data_path,header=T)

# 2 Struktur > 3471 Obs, 17 Vars

str(Umfrage)

# 3 Names

names(Umfrage)

# 4 Top 10 Rows

head(Umfrage,10)
Umfrage[1:10,]

# 5 Skalenniveau

str(Umfrage)

# 6 Missing Gewicht > none

is.null(Umfrage$GEW)
Umfrage[which(Umfrage$GEW==0),]
sort(Umfrage$GEW,decreasing=T)

# 7 Check Median/Average Arbeitstunden > 40.00/40.24

summary(Umfrage$ARBEITSSTD)
hist(Umfrage$ARBEITSSTD)


# 8 Fachhochschule > 257

summary(Umfrage$HOE_ABSCHLUSS)
length(Umfrage[which(Umfrage$HOE_ABSCHLUSS=="FACHHOCHSCHULABSCHL."),13])

# 9 Smokers > 1018 Smokers vs. 2450 non Smokers

summary(Umfrage$RAUCH)
raucher<-length(Umfrage[which(Umfrage$RAUCH=="JA"),1])
nicht_raucher<-length(Umfrage[which(Umfrage$RAUCH=="NEIN"),1])

# 10 Max Einkommen > 60000 / 2 Times

summary(Umfrage$NETTO)
max(Umfrage$NETTO, na.rm=T)

length(Umfrage[which(Umfrage$NETTO==60000),1])

# 11 men/woman ? > 2 times men

Umfrage[which(Umfrage$NETTO==60000),2]

# 12 Every 4th row to display

Umfrage[seq(4,by=4,length(Umfrage$ID)),]

# 13 Abeitsstunden für Person 6 > 13

Umfrage[6,5]

# 14 Zuweisen Zufriedenheit

Umfrage[2,"ZUFR"] <-8
Umfrage[3,"ZUFR"] <- 9
Umfrage[4,"ZUFR"] <- 10
Umfrage[5,"ZUFR"] <- 6

# 15 Rename Col-Name

names(Umfrage)
colnames(Umfrage)[colnames(Umfrage)=="GEBJAHR"] <- "JAHRGANG"
names(Umfrage)

# 16 Zufriedenheit nach Geschlecht > Chisq = 9.364, df = 10, p-value = 0.4979

zufr <-table(Umfrage$GESCHL,Umfrage$ZUFR)
zufr
barplot(zufr)
summary(zufr)
barplot(zufr,col=c("darkblue","red"))
legend("topleft", legend = c("MAENLICH", "WEIBLICH"), fill = c("darkblue", "red"))

# 18 SubSet raucher

raucher <- subset(Umfrage,RAUCH=="JA")
nicht_raucher <- subset(Umfrage,RAUCH=="NEIN")

summary(raucher)
summary(nicht_raucher)

# 19 Zufriedene Frauen

frauen <- subset(Umfrage,GESCHL=="WEIBLICH" & ZUFR > 8)

# 20 Add rows to Umfrage

Umfrage.neu <- rbind(Umfrage,Umfrage[c(2,4,6),])

# 21 Find Duplicated Rows

double<-Umfrage.neu[duplicated(Umfrage.neu)==T,]

# 22 Remove Duplicates

Umfrage.unique<-unique(Umfrage.neu)

# 23 Check for Duplicates

sum(duplicated(Umfrage.unique)==T)

# 24/25 Check NAs in data-frame

Umfrage.na<-colSums(is.na(Umfrage))

# 26 Complete data-frame

Umfrage.complete <- Umfrage[which(complete.cases(Umfrage)),]

# 27 NonComplete data-frame

Umfrage.na <- Umfrage[which(complete.cases(Umfrage)==F),]

# 28 Split data-frame for Raucher

Umfrage.split_rauch <- split(Umfrage,Umfrage$RAUCH)

# 29 Split data-frama for Raucher and Geschlecht

Umfrage.split_rauch_geschl <- split(Umfrage,list(Umfrage$RAUCH,Umfrage$GESCHL))

# 30 Mean for Zufriedenheit by Geschlecht

tapply(Umfrage.complete$ZUFR,Umfrage.complete$GESCHL,mean)








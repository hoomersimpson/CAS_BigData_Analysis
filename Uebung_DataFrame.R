# (1) Establish data frame (table)

students <- data.frame(Semesterzahl=c(4,7,5,4,9,8,2,3),
                       Alter=c(22,25,24,21,25,26,23,21),
                       Studiengang=c("VWL","BWL","VWL","Soz","VWL","VWL","BWL","Soz"),
                       Geschlecht=c("m","w","m","m","w","m","w","m"))

# (1) show data and structure of data.frame "students"
students
str(students)
class(students)
mode(students)

# (2) evaluate stats > Mean "Alter" = 23.38 (summary)
mean(students$Alter)

# (3) Establish data.frames for m and w (subset)
# students_m<-subset(students,Geschlecht == "m")
# students_w<-subset(students,Geschlecht == "w")

students$Geschlecht <- as.factor(students$Geschlecht)
str(students)

# (4) Draw barplot for Studiengang
barplot(table(students$Studiengang))

# (5) Establish Matrix

# as.matrix > values as chr

students_as_matrix <- as.matrix(students)
students_as_matrix
str(students_as_matrix)
summary(students_as_matrix)

# data.matrix > values as num

students_data_matrix <- data.matrix(students)
students_data_matrix
str(students_data_matrix)
summary(students_data_matrix)

        

# Uebung Hypothesen

# Aufgabe 1: Biertest

# H1: Alkoholfreies Bier kann am Geschmack erkannt werden
# H0: Alkoholfreies Bier kann am Geschmack nicht erkannt werden

Anzahl_Beobachtungen <- 150
Anzahl_Erkannt <- 98

prop.test(x=Anzahl_Erkannt, n=Anzahl_Beobachtungen,p=0.5, alt="greater")
prop.test(x=Anzahl_Erkannt, n=Anzahl_Beobachtungen,p=0.5)


# Mit p=0.00012 kann H0 verworfen werden: Das alkoholfreie Bier kann am Geschmack erkannt werden

# Aufgabe 2

# H1: 

prop.test(c(490,400),c(500,500))
          
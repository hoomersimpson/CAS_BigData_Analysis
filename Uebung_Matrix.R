# matrix

# (1) Estabish matrix

matrix_values <- c(9,5,8,1,13,17,5,3,1,5,7,4)
X1 <-matrix(matrix_values,nrow=4, ncol=3)

# Transpose

X1_t <- t(matrix_1)
X1_t

# Access SubSet

X2 <- matrix_1[2:3,]
X2

# Add row to matrix

X3 <- rbind(X2,c(10,10,10))

# Namings

Cols <- c("BE","ZH","GE")
Rows <- c("Ranking","LastYear","Forecast")
dimnames(X3)<- list(Rows,Cols)
dimnames(X3)
X3

# Corelation, variance, covariance

cor(X3)
var(X3)
cov(X3)

# Replace Values
X3[X3 >= 8]<-NA
X3




library(readxl)
data<- read_excel("C:/kuliah/semester 6/Multivariate statistic/AOL/coba/VAR BANYAK/datafix4varcoba.xlsx")
View(data)
data$t = seq(1:nrow(data))
head(data)

# model regresi
model = lm(t~ x1+x2+x3+x4+y1+y2+y3+y4, data=data)
summary(model)

# multikolinearitas
#install.packages("car")
library(car)
vif(model)

# normalitas
#install.packages("mvnormtest")
library(mvnormtest)
datanorm = data[2:9]
datanorm
shapiro.test(as.matrix(datanorm))



# -----------------
# transformasi 1X
# -----------------
x = data[, c("x1", "x2", "x3", "x4")]
y = data[, c("y1", "y2", "y3", "y4")]

# Shift data to make all values positive
x_shifted <- x + abs(min(x)) + 1
y_shifted <- y + abs(min(y)) + 1

# Power Transformation untuk x
transform_pt_x <- powerTransform(as.matrix(x_shifted))
x_transformed <- as.data.frame(sapply(1:ncol(x_shifted), function(i) x_shifted[, i]^transform_pt_x$lambda[i]))
colnames(x_transformed) <- colnames(x)

# Power Transformation untuk y
transform_pt_y <- powerTransform(as.matrix(y_shifted))
y_transformed <- as.data.frame(sapply(1:ncol(y_shifted), function(i) y_shifted[, i]^transform_pt_y$lambda[i]))
colnames(y_transformed) <- colnames(y)

# Menggabungkan data yang ditransformasi
transformed <- cbind(x_transformed, y_transformed)
transformed <- as.data.frame(scale(transformed))
transformed

# Uji normalitas menggunakan Shapiro-Wilk
shapiro.test(as.matrix(transformed))
# masih belum normal 

# -----------------
# transformasi 2X
# -----------------
xt2 = transformed[, c("x1", "x2", "x3", "x4")]
yt2 = transformed[, c("y1", "y2", "y3", "y4")]

# Shift data to make all values positive
x_shifted2 <- xt2 + abs(min(xt2)) + 1
y_shifted2 <- yt2 + abs(min(yt2)) + 1

# Power Transformation untuk x
transform_pt_x2 <- powerTransform(as.matrix(x_shifted2))
x_transformed2 <- as.data.frame(sapply(1:ncol(x_shifted2), function(i) x_shifted2[, i]^transform_pt_x2$lambda[i]))
colnames(x_transformed2) <- colnames(xt2)

# Power Transformation untuk y
transform_pt_y2 <- powerTransform(as.matrix(y_shifted2))
y_transformed2 <- as.data.frame(sapply(1:ncol(y_shifted2), function(i) y_shifted2[, i]^transform_pt_y2$lambda[i]))
colnames(y_transformed2) <- colnames(yt2)

# Menggabungkan data yang ditransformasi
transformed2 <- cbind(x_transformed2, y_transformed2)
transformed2 <- as.data.frame(scale(transformed2))

shapiro.test(as.matrix(transformed2))
transformed2

#-----------
# TRANS 3X 
#-----------
xt3 = transformed2[, c("x1", "x2", "x3", "x4")]
yt3 = transformed2[, c("y1", "y2", "y3", "y4")]

# Shift data to make all values positive
x_shifted3 <- xt3 + abs(min(xt3)) + 1
y_shifted3 <- yt3 + abs(min(yt3)) + 1

# Power Transformation untuk x
transform_pt_x3 <- powerTransform(as.matrix(x_shifted3))
x_transformed3 <- as.data.frame(sapply(1:ncol(x_shifted3), function(i) x_shifted3[, i]^transform_pt_x3$lambda[i]))
colnames(x_transformed3) <- colnames(xt3)

# Power Transformation untuk y
transform_pt_y3 <- powerTransform(as.matrix(y_shifted3))
y_transformed3 <- as.data.frame(sapply(1:ncol(y_shifted3), function(i) y_shifted3[, i]^transform_pt_y3$lambda[i]))
colnames(y_transformed3) <- colnames(yt3)

# Menggabungkan data yang ditransformasi
transformed3 <- cbind(x_transformed3, y_transformed3)
transformed3 <- as.data.frame(scale(transformed3))

shapiro.test(as.matrix(transformed3))
transformed3

#-----------
# TRANS 4X 
#-----------
xt4 = transformed3[, c("x1", "x2", "x3", "x4")]
yt4 = transformed3[, c("y1", "y2", "y3", "y4")]

# Shift data to make all values positive
x_shifted4 <- xt4 + abs(min(xt4)) + 1
y_shifted4 <- yt4 + abs(min(yt4)) + 1

# Power Transformation untuk x
transform_pt_x4 <- powerTransform(as.matrix(x_shifted4))
x_transformed4 <- as.data.frame(sapply(1:ncol(x_shifted4), function(i) x_shifted4[, i]^transform_pt_x4$lambda[i]))
colnames(x_transformed4) <- colnames(xt4)

# Power Transformation untuk y
transform_pt_y4 <- powerTransform(as.matrix(y_shifted4))
y_transformed4 <- as.data.frame(sapply(1:ncol(y_shifted4), function(i) y_shifted4[, i]^transform_pt_y4$lambda[i]))
colnames(y_transformed4) <- colnames(yt4)

# Menggabungkan data yang ditransformasi
transformed4 <- cbind(x_transformed4, y_transformed4)
transformed4 <- as.data.frame(scale(transformed4))

shapiro.test(as.matrix(transformed4))
transformed4

#-----------
# TRANS 5X 
#-----------
xt5 = transformed4[, c("x1", "x2", "x3", "x4")]
yt5 = transformed4[, c("y1", "y2", "y3", "y4")]

# Shift data to make all values positive
x_shifted5 <- xt5 + abs(min(xt5)) + 1
y_shifted5 <- yt5 + abs(min(yt5)) + 1

# Power Transformation untuk x
transform_pt_x5 <- powerTransform(as.matrix(x_shifted5))
x_transformed5 <- as.data.frame(sapply(1:ncol(x_shifted5), function(i) x_shifted5[, i]^transform_pt_x5$lambda[i]))
colnames(x_transformed5) <- colnames(xt5)

# Power Transformation untuk y
transform_pt_y5 <- powerTransform(as.matrix(y_shifted5))
y_transformed5 <- as.data.frame(sapply(1:ncol(y_shifted5), function(i) y_shifted5[, i]^transform_pt_y5$lambda[i]))
colnames(y_transformed5) <- colnames(yt5)

# Menggabungkan data yang ditransformasi
transformed5 <- cbind(x_transformed5, y_transformed5)
transformed5 <- as.data.frame(scale(transformed5))

shapiro.test(as.matrix(transformed5))
transformed5

#-----------
# TRANS 6X 
#-----------
xt6 = transformed5[, c("x1", "x2", "x3", "x4")]
yt6 = transformed5[, c("y1", "y2", "y3", "y4")]

# Shift data to make all values positive
x_shifted6 <- xt6 + abs(min(xt6)) + 1
y_shifted6 <- yt6 + abs(min(yt6)) + 1

# Power Transformation untuk x
transform_pt_x6 <- powerTransform(as.matrix(x_shifted6))
x_transformed6 <- as.data.frame(sapply(1:ncol(x_shifted6), function(i) x_shifted6[, i]^transform_pt_x6$lambda[i]))
colnames(x_transformed6) <- colnames(xt6)

# Power Transformation untuk y
transform_pt_y6 <- powerTransform(as.matrix(y_shifted6))
y_transformed6 <- as.data.frame(sapply(1:ncol(y_shifted6), function(i) y_shifted6[, i]^transform_pt_y6$lambda[i]))
colnames(y_transformed6) <- colnames(yt6)

# Menggabungkan data yang ditransformasi
transformed6 <- cbind(x_transformed6, y_transformed6)
transformed6 <- as.data.frame(scale(transformed6))

shapiro.test(as.matrix(transformed6))
transformed6


#-----------
# TRANS 7X 
#-----------
xt7 = transformed6[, c("x1", "x2", "x3", "x4")]
yt7 = transformed6[, c("y1", "y2", "y3", "y4")]

# Shift data to make all values positive
x_shifted7 <- xt7 + abs(min(xt7)) + 1
y_shifted7 <- yt7 + abs(min(yt7)) + 1

# Power Transformation untuk x
transform_pt_x7 <- powerTransform(as.matrix(x_shifted7))
x_transformed7 <- as.data.frame(sapply(1:ncol(x_shifted7), function(i) x_shifted7[, i]^transform_pt_x7$lambda[i]))
colnames(x_transformed7) <- colnames(xt7)

# Power Transformation untuk y
transform_pt_y7 <- powerTransform(as.matrix(y_shifted7))
y_transformed7 <- as.data.frame(sapply(1:ncol(y_shifted7), function(i) y_shifted7[, i]^transform_pt_y7$lambda[i]))
colnames(y_transformed7) <- colnames(yt7)

# Menggabungkan data yang ditransformasi
transformed7 <- cbind(x_transformed7, y_transformed7)
transformed7 <- as.data.frame(scale(transformed7))

shapiro.test(as.matrix(transformed7))
transformed7

#-----------
# TRANS 8X 
#-----------
xt8 = transformed7[, c("x1", "x2", "x3", "x4")]
yt8 = transformed7[, c("y1", "y2", "y3", "y4")]

# Shift data to make all values positive
x_shifted8 <- xt8 + abs(min(xt8)) + 1
y_shifted8 <- yt8 + abs(min(yt8)) + 1

# Power Transformation untuk x
transform_pt_x8 <- powerTransform(as.matrix(x_shifted8))
x_transformed8 <- as.data.frame(sapply(1:ncol(x_shifted8), function(i) x_shifted8[, i]^transform_pt_x8$lambda[i]))
colnames(x_transformed8) <- colnames(xt8)

# Power Transformation untuk y
transform_pt_y8 <- powerTransform(as.matrix(y_shifted8))
y_transformed8 <- as.data.frame(sapply(1:ncol(y_shifted8), function(i) y_shifted8[, i]^transform_pt_y8$lambda[i]))
colnames(y_transformed8) <- colnames(yt8)

# Menggabungkan data yang ditransformasi
transformed8 <- cbind(x_transformed8, y_transformed8)
transformed8 <- as.data.frame(scale(transformed8))

shapiro.test(as.matrix(transformed8))
transformed8

#-----------
# TRANS 9X 
#-----------
xt9 = transformed8[, c("x1", "x2", "x3", "x4")]
yt9 = transformed8[, c("y1", "y2", "y3", "y4")]

# Shift data to make all values positive
x_shifted9 <- xt9 + abs(min(xt9)) + 1
y_shifted9 <- yt9 + abs(min(yt9)) + 1

# Power Transformation untuk x
transform_pt_x9 <- powerTransform(as.matrix(x_shifted9))
x_transformed9 <- as.data.frame(sapply(1:ncol(x_shifted9), function(i) x_shifted9[, i]^transform_pt_x9$lambda[i]))
colnames(x_transformed9) <- colnames(xt9)

# Power Transformation untuk y
transform_pt_y9 <- powerTransform(as.matrix(y_shifted9))
y_transformed9 <- as.data.frame(sapply(1:ncol(y_shifted9), function(i) y_shifted9[, i]^transform_pt_y9$lambda[i]))
colnames(y_transformed9) <- colnames(yt9)

# Menggabungkan data yang ditransformasi
transformed9 <- cbind(x_transformed9, y_transformed9)
transformed9 <- as.data.frame(scale(transformed9))

shapiro.test(as.matrix(transformed9))
transformed9

#-----------
# TRANS 10X 
#-----------
xt10 = transformed9[, c("x1", "x2", "x3", "x4")]
yt10 = transformed9[, c("y1", "y2", "y3", "y4")]

# Shift data to make all values positive
x_shifted10 <- xt10 + abs(min(xt10)) + 1
y_shifted10 <- yt10 + abs(min(yt10)) + 1

# Power Transformation untuk x
transform_pt_x10 <- powerTransform(as.matrix(x_shifted10))
x_transformed10 <- as.data.frame(sapply(1:ncol(x_shifted10), function(i) x_shifted10[, i]^transform_pt_x10$lambda[i]))
colnames(x_transformed10) <- colnames(xt10)

# Power Transformation untuk y
transform_pt_y10 <- powerTransform(as.matrix(y_shifted10))
y_transformed10<- as.data.frame(sapply(1:ncol(y_shifted10), function(i) y_shifted10[, i]^transform_pt_y10$lambda[i]))
colnames(y_transformed10) <- colnames(yt10)

# Menggabungkan data yang ditransformasi
transformed10 <- cbind(x_transformed10, y_transformed10)
transformed10 <- as.data.frame(scale(transformed10))

shapiro.test(as.matrix(transformed10))
transformed10

#-----------
# TRANS 11X 
#-----------
xt11 = transformed10[, c("x1", "x2", "x3", "x4")]
yt11 = transformed10[, c("y1", "y2", "y3", "y4")]

# Shift data to make all values positive
x_shifted11 <- xt11 + abs(min(xt11)) + 1
y_shifted11 <- yt11 + abs(min(yt11)) + 1

# Power Transformation untuk x
transform_pt_x11 <- powerTransform(as.matrix(x_shifted11))
x_transformed11 <- as.data.frame(sapply(1:ncol(x_shifted11), function(i) x_shifted11[, i]^transform_pt_x11$lambda[i]))
colnames(x_transformed11) <- colnames(xt11)

# Power Transformation untuk y
transform_pt_y11 <- powerTransform(as.matrix(y_shifted11))
y_transformed11<- as.data.frame(sapply(1:ncol(y_shifted11), function(i) y_shifted11[, i]^transform_pt_y11$lambda[i]))
colnames(y_transformed11) <- colnames(yt11)

# Menggabungkan data yang ditransformasi
transformed11 <- cbind(x_transformed11, y_transformed11)
transformed11 <- as.data.frame(scale(transformed11))

shapiro.test(as.matrix(transformed11))
transformed11

#-----------
# TRANS 12X 
#-----------
xt12 = transformed11[, c("x1", "x2", "x3", "x4")]
yt12 = transformed11[, c("y1", "y2", "y3", "y4")]

# Shift data to make all values positive
x_shifted12 <- xt12 + abs(min(xt12)) + 1
y_shifted12 <- yt12 + abs(min(yt12)) + 1

# Power Transformation untuk x
transform_pt_x12 <- powerTransform(as.matrix(x_shifted12))
x_transformed12 <- as.data.frame(sapply(1:ncol(x_shifted12), function(i) x_shifted12[, i]^transform_pt_x12$lambda[i]))
colnames(x_transformed12) <- colnames(xt12)

# Power Transformation untuk y
transform_pt_y12 <- powerTransform(as.matrix(y_shifted12))
y_transformed12<- as.data.frame(sapply(1:ncol(y_shifted12), function(i) y_shifted12[, i]^transform_pt_y12$lambda[i]))
colnames(y_transformed12) <- colnames(yt12)

# Menggabungkan data yang ditransformasi
transformed12 <- cbind(x_transformed12, y_transformed12)
transformed12 <- as.data.frame(scale(transformed12))

shapiro.test(as.matrix(transformed12))
transformed12

# UJI LINEAR 
cor.test(transformed12$x1, transformed12$x2)
cor.test(transformed12$x1, transformed12$x3)
cor.test(transformed12$x1, transformed12$x4)
cor.test(transformed12$x2, transformed12$x3)
cor.test(transformed12$x2, transformed12$x4)
cor.test(transformed12$x3, transformed12$x4) 

cor.test(transformed12$y1, transformed12$y2)
cor.test(transformed12$y1, transformed12$y3)
cor.test(transformed12$y1, transformed12$y4) 
cor.test(transformed12$y2, transformed12$y3)
cor.test(transformed12$y2, transformed12$y4) 
cor.test(transformed12$y3, transformed12$y4) 


# Melanjutkan analisis kanonikal
data_fix <- transformed12

# ----------------
# canonical
# ----------------
head(data_fix)
S = cov(data_fix)
S
library(expm)

rho22 = S[1:4 , 1:4]
rho11 = S[5:8, 5:8]
rho21 = S[1:4, 5:8] 
rho12 = S[5:8, 1:4]
rho11_inverssqrt = solve(sqrtm(rho11))
rho11_invers = solve(rho11)
rho11_inverssqrt
rho22_invers = solve(rho22)
rho22_inverssqrt = solve(sqrtm(rho22))
rho22_invers
rho11

A = rho11_inverssqrt%*%rho12%*%rho22_invers%*%rho21%*%rho11_inverssqrt
A

eigen = eigen(A)
e = eigen$vectors
(e1 = eigen$vectors[,1])
(u1 = e1 %*% rho11_inverssqrt)

loading_can = rho11 %*% t(u1)
loading_can

B = rho22_inverssqrt%*%rho21%*%rho11_invers%*%rho12%*%rho22_inverssqrt
B

eigenB = eigen(B)
eb = eigenB$vectors
eb
(eb1 = eigenB$vectors[,1])
(v1 = eb1 %*% rho22_inverssqrt)


r2 = (values<-eigen$values)
r2
r = sqrt(r2)
r

# uji serentak
det(S)
det(rho11)
det(rho22)
rho11

(det(S)/(det(rho11)*det(rho22)))


# uji parsial
library(CCP)
n <- 34
p <- 4
q <- 4
wilks_result <- p.asym(r, n, p, q, tstat = "Wilks")

# Print the results
wilks_result

## F-test
F_1 = wilks_result$approx[1]
F_2 = wilks_result$approx[2]
F_3 = wilks_result$approx[3]
F_4 = wilks_result$approx[4]

## F-Table
f_table_1 = qf(p=0.05, wilks_result$df1[1], wilks_result$df2[1], lower.tail = FALSE)
f_table_2 = qf(p=0.05, wilks_result$df1[2], wilks_result$df2[2], lower.tail = FALSE)
f_table_3 = qf(p=0.05, wilks_result$df1[3], wilks_result$df2[3], lower.tail = FALSE)
f_table_4 = qf(p=0.05, wilks_result$df1[4], wilks_result$df2[4], lower .tail = FALSE)

F_1
F_2
F_3
F_4


f_table_1
f_table_2
f_table_3
f_table_4

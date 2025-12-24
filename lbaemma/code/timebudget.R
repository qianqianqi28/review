rm(list=ls())
library('crosstalk')
library('rgl')
library('lba')
library(Ternary)
library(xtable)
library(readxl)
library(R.matlab)
library(Matrix)
library("openxlsx")
library("FactoMineR")
source("code//emma//emma//c_err.R")
source("code//emma//emma//norm1.R")
source("code//emma//emma//r2.R")
source("code//emma//emma//tcalc.R")
source("code//emma//modify emma//RECAauto.R")

set.seed(123)

### ------------------------------------------------------------
### Build full matrix from the two tables (Male + Female)
### ------------------------------------------------------------

activity_cols <- c("paidwork","dom.work","caring","shopping","per.need","eating","sleeping","educat.","particip","soc.cont","goingout","sports","gardening","outside","tv-radio","reading","relaxing","other")

row_info <- c(
  "M1275","M1280","M1285",
  "M2575","M2580","M2585",
  "M3575","M3580","M3585",
  "M5075","M5080","M5085",
  "M6575","M6580","M6585",
  "F1275","F1280","F1285",
  "F2575","F2580","F2585",
  "F3575","F3580","F3585",
  "F5075","F5080","F5085",
  "F6575","F6580","F6585"
)

# ---------------------- MALE ----------------------

M1275 <- c(901,87,33,120,289,508,3737,1447,128,515,490,419,111,48,752,272,78,146)
M1280 <- c(769,157,28,138,294,528,3765,1455,101,505,396,436,102,41,815,256,56,240)
M1285 <- c(707,155,15,127,316,527,3744,1537,92,449,441,485,100,64,860,188,73,200)

M2575 <- c(2180,250,194,152,293,623,3380,124,129,609,382,269,173,69,700,366,64,124)
M2580 <- c(1992,269,206,157,316,649,3403,245,126,649,321,279,213,35,671,318,58,172)
M2585 <- c(1899,341,184,183,302,605,3397,208,143,599,391,271,231,67,812,243,57,148)

M3575 <- c(1901,249,99,173,351,660,3463,56,195,671,360,206,259,88,785,316,59,188)
M3580 <- c(2008,289,128,157,339,709,3445,90,156,593,240,280,238,45,804,343,44,170)
M3585 <- c(2093,331,136,185,332,650,3347,85,148,479,336,291,268,64,812,319,58,146)

M5075 <- c(1708,244,51,227,350,709,3560,18,122,603,237,209,256,116,921,468,79,203)
M5080 <- c(1357,337,54,221,364,744,3569,58,207,704,279,299,288,76,862,413,57,190)
M5085 <- c(1206,450,25,230,352,686,3533,46,272,554,264,316,309,112,1012,467,68,174)

M6575 <- c(176,617,124,273,365,763,3801,10,159,811,213,297,366,86,1161,477,157,223)
M6580 <- c(71,563,27,251,392,767,3871,43,192,671,220,403,312,117,1198,660,92,230)
M6585 <- c(95,636,38,264,383,707,3694,54,214,619,274,476,308,178,1233,578,104,225)

# ---------------------- FEMALE ----------------------

F1275 <- c(723,494,135,208,359,536,3744,1163,125,592,364,348,90,32,594,292,73,208)
F1280 <- c(665,460,99,200,377,513,3777,1321,88,557,400,370,76,32,581,257,74,234)
F1285 <- c(564,397,86,223,387,495,3821,1436,80,527,396,352,86,41,702,207,63,214)

F2575 <- c(439,1342,635,347,311,593,3526,77,85,780,316,306,149,41,547,300,88,199)
F2580 <- c(471,1338,673,336,339,607,3532,115,115,776,270,352,131,32,497,275,54,167)
F2585 <- c(704,1147,651,336,337,572,3447,120,115,736,303,368,145,44,565,265,63,164)

F3575 <- c(299,1567,296,372,325,664,3567,104,133,694,225,335,198,37,622,356,76,207)
F3580 <- c(375,1605,309,347,346,633,3554,98,143,689,229,440,154,34,576,311,63,174)
F3585 <- c(412,1529,308,373,351,656,3444,68,196,699,277,453,170,45,582,307,59,153)

F5075 <- c(151,1600,83,376,367,601,3673,27,195,758,255,323,197,53,710,478,78,154)
F5080 <- c(153,1558,84,335,368,613,3701,30,179,810,212,504,190,41,644,390,53,212)
F5085 <- c(233,1487,82,352,385,595,3566,40,195,721,268,545,217,76,708,377,64,170)

F6575 <- c(11,1319,78,384,372,635,3849,6,108,929,219,297,169,37,888,485,63,230)
F6580 <- c(6,1409,154,292,453,665,3713,21,124,796,187,482,191,39,860,404,67,216)
F6585 <- c(19,1318,44,320,366,615,3675,23,139,749,202,579,169,52,1076,460,69,204)

# ---------------------- Assemble matrix ----------------------

all_rows <- list(
  M1275,M1280,M1285,
  M2575,M2580,M2585,
  M3575,M3580,M3585,
  M5075,M5080,M5085,
  M6575,M6580,M6585,
  F1275,F1280,F1285,
  F2575,F2580,F2585,
  F3575,F3580,F3585,
  F5075,F5080,F5085,
  F6575,F6580,F6585
)

X <- do.call(rbind, all_rows)
colnames(X) <- activity_cols
rownames(X) <- row_info
row_sums <- rowSums(X)

### ------------------------------------------------------------
### LBA
### ------------------------------------------------------------

bmilba <- lba(X, K = 3, row.weights = 1, col.weights = 1, method = "ls", what = 'inner',trace.lba = FALSE)
det(t(bmilba$B) %*% bmilba$B)
det(t(bmilba$Boi) %*% bmilba$Boi)

### ------------------------------------------------------------
### EMA
### ------------------------------------------------------------

emma <- RECAauto(X, q = 3, c1 = -6, i = 100, c5 = 0.5)
emma_A <- emma$m_mod
emma_B <- t(emma$B_mod)
rownames(emma_B) <- colnames(X)

### ------------------------------------------------------------
### NMF Before running the following code, timebudgetH1.mat and timebudgetW1.mat need to be created by matlab code nmf/timebudget.m
### ------------------------------------------------------------
nmf_data_A <- readMat("created data\\timebudgetH1.mat")
nmf_A <- t(nmf_data_A$H1)
rownames(nmf_A) <- rownames(X)
dim(nmf_A)
nmf_data_B <- readMat("created data\\timebudgetW1.mat")
nmf_B <- nmf_data_B$W1
nmf_B <- nmf_B  %*% diag(1/apply(nmf_B, 2, sum))
apply(nmf_B, 2, sum)
rownames(nmf_B) <- colnames(X)

# dimension proportion z
lba_pk <- t(as.matrix(row_sums/sum(X))) %*% bmilba$Aoi
emma_pk <- t(as.matrix(row_sums/sum(X))) %*% emma_A
nmf_pk <- t(as.matrix(row_sums/sum(X))) %*% nmf_A

all_pk <- rbind(lba_pk, emma_pk, nmf_pk)

# Rescaled basis matrix G^{res}
bmilba_Boi_reverse <- t(apply(bmilba$Boi, 1, function(row) {
  (lba_pk[1, ] * row) / sum(lba_pk[1, ] * row)
}))
emma_B_reverse <- t(apply(emma_B, 1, function(row) {
  (emma_pk[1, ] * row) / sum(emma_pk[1, ] * row)
}))
nmf_B_reverse <- t(apply(nmf_B, 1, function(row) {
  (nmf_pk[1, ] * row) / sum(nmf_pk[1, ] * row)
}))

vertice_label <- c("lba1", "lba2", "lba3", "ema1", "ema2", "ema3", "nmf1", "nmf2", "nmf3") 

# Basis matrix
basis <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, byrow = TRUE)
rownames(basis) <- c("e1", "e2", "e3")

# Transform them into simplex coordinates
ternary_to_xy <- function(coord) {
  return(t(apply(coord, 
          1, 
          function(x) {
            x1 <- 0.5 * (2*x[2] + x[3])
            x2 <- (sqrt(3)/2) * x[3]
            return(c(x1, x2))
          })))
}

# Baisis matrix
ternary_to_xy_basis <- ternary_to_xy(basis)

# plots for rows
ternary_to_xy_lba_Ainn <- ternary_to_xy(bmilba$Aoi)
ternary_to_xy_emma_A <- ternary_to_xy(emma_A)
ternary_to_xy_nmf_A <- ternary_to_xy(nmf_A)

# plots for columns
ternary_to_xy_lba_Binn_reverse <- ternary_to_xy(bmilba_Boi_reverse)
ternary_to_xy_emma_B_reverse <- ternary_to_xy(emma_B_reverse)
ternary_to_xy_nmf_B_reverse <- ternary_to_xy(nmf_B_reverse)

# plots for dimension proportion
ternary_to_xy_all_pk <- ternary_to_xy(all_pk)

# plots for parallel lines
int_d1 <- cbind(all_pk[,1], all_pk[,2]+all_pk[,3], c(0,0,0)) 
int_d2 <- cbind(c(0,0,0), all_pk[,2], all_pk[,1] + all_pk[,3]) 
int_d3<-  cbind(all_pk[,1] + all_pk[,2], c(0,0,0), all_pk[,3]) 
ternary_to_xy_int_d1 <- ternary_to_xy(int_d1)
ternary_to_xy_int_d2 <- ternary_to_xy(int_d2)
ternary_to_xy_int_d3 <- ternary_to_xy(int_d3)

# coefficient for LBA not used in paper
png(file=paste0("plots//lbatimebudgetdim3simplexlbadata.png"),
    width=800, height=800)
par(mar = c(0, 0, 0, 0),
    oma = c(0, 0, 0, 0)) 
plot(ternary_to_xy_basis, asp = 1, type="n", axes = F, xlim=c(-0.1,1.1),ylim=c(-0.1,1.1),xlab="",ylab="", cex.lab = 2.75, cex = 2.75)
text(ternary_to_xy_basis[1, 1]-0.08, ternary_to_xy_basis[1, 2], vertice_label[1], cex = 2.75, lwd = 3)
text(ternary_to_xy_basis[2, 1]+0.08, ternary_to_xy_basis[2, 2], vertice_label[2], cex = 2.75, lwd = 3)
text(ternary_to_xy_basis[3, 1], ternary_to_xy_basis[3, 2]+0.08, vertice_label[3], cex = 2.75, lwd = 3)

lines(rbind(ternary_to_xy_basis, ternary_to_xy_basis[1, ]), cex = 2.75, lwd = 3, col = "black")
points(ternary_to_xy_lba_Ainn, cex = 1.5, lwd = 1.5, pch = 20)
autoLab(ternary_to_xy_lba_Ainn[, 1], ternary_to_xy_lba_Ainn[, 2], labels = row_info, cex.lab=1.5, cex = 1.5, lwd = 3)

points(ternary_to_xy_all_pk[1,1], ternary_to_xy_all_pk[1,2], cex = 1.5, lwd = 1.5, pch = 15)
text(ternary_to_xy_all_pk[1,1]+0.08, ternary_to_xy_all_pk[1,2], "average", cex = 1.5, lwd = 1.5)

lines(rbind(ternary_to_xy_all_pk[1,], ternary_to_xy_int_d1[1,]), col = "black", lwd = 3)
lines(rbind(ternary_to_xy_all_pk[1,], ternary_to_xy_int_d2[1,]), col = "black", lwd = 3) 
lines(rbind(ternary_to_xy_all_pk[1,], ternary_to_xy_int_d3[1,]), col = "black", lwd = 3)
text(ternary_to_xy_int_d1[1, 1], ternary_to_xy_int_d1[1, 2]-0.02, round(all_pk[1,1], 3), cex = 1.5, lwd = 1.5)
text(ternary_to_xy_int_d2[1, 1]+0.04, ternary_to_xy_int_d2[1, 2]+0.02, round(all_pk[1,2], 3), cex = 1.5, lwd = 1.5)
text(ternary_to_xy_int_d3[1, 1]-0.04, ternary_to_xy_int_d3[1, 2]+0.02, round(all_pk[1,3], 3), cex = 1.5, lwd = 1.5)
dev.off()

# coefficient for EMA not used in paper
png(file=paste0("plots//emmatimebudgetdim3simplexlbadata.png"),
    width=800, height=800)
par(mar = c(0, 0, 0, 0), 
    oma = c(0, 0, 0, 0)) 
plot(ternary_to_xy_basis, asp = 1, type="n", axes = F, xlim=c(-0.1,1.1),ylim=c(-0.1,1.1),xlab="",ylab="", cex.lab = 2.75, cex = 2.75)
text(ternary_to_xy_basis[1, 1]-0.08, ternary_to_xy_basis[1, 2], vertice_label[4], cex = 2.75, lwd = 3)
text(ternary_to_xy_basis[2, 1]+0.08, ternary_to_xy_basis[2, 2], vertice_label[5], cex = 2.75, lwd = 3)
text(ternary_to_xy_basis[3, 1], ternary_to_xy_basis[3, 2]+0.08, vertice_label[6], cex = 2.75, lwd = 3)

lines(rbind(ternary_to_xy_basis, ternary_to_xy_basis[1, ]), cex = 2.75, lwd = 3, col = "black")

points(ternary_to_xy_emma_A, cex = 1.5, lwd = 3, pch = 20)
autoLab(ternary_to_xy_emma_A[, 1], ternary_to_xy_emma_A[, 2], labels = row_info, cex.lab=1.5, cex = 1.5, lwd = 3)
points(ternary_to_xy_all_pk[2,1], ternary_to_xy_all_pk[2,2], cex = 1.5, lwd = 1.5, pch = 15)
text(ternary_to_xy_all_pk[2,1]+0.08, ternary_to_xy_all_pk[2,2], "average", cex = 1.5, lwd = 1.5)

lines(rbind(ternary_to_xy_all_pk[2,], ternary_to_xy_int_d1[2,]), col = "black", lwd = 3)
lines(rbind(ternary_to_xy_all_pk[2,], ternary_to_xy_int_d2[2,]), col = "black", lwd = 3) 
lines(rbind(ternary_to_xy_all_pk[2,], ternary_to_xy_int_d3[2,]), col = "black", lwd = 3)
text(ternary_to_xy_int_d1[2, 1], ternary_to_xy_int_d1[2, 2]-0.02, round(all_pk[2,1], 3), cex = 1.5, lwd = 1.5)
text(ternary_to_xy_int_d2[2, 1]+0.04, ternary_to_xy_int_d2[2, 2]+0.02, round(all_pk[2,2], 3), cex = 1.5, lwd = 1.5)
text(ternary_to_xy_int_d3[2, 1]-0.04, ternary_to_xy_int_d3[2, 2]+0.02, round(all_pk[2,3], 3), cex = 1.5, lwd = 1.5)
dev.off()

# coefficient for NMF used in paper
png(file=paste0("plots//nmftimebudgetdim3simplexlbadata.png"),
    width=800, height=800)
par(mar = c(0, 0, 0, 0), 
    oma = c(0, 0, 0, 0))
plot(ternary_to_xy_basis, asp = 1, type="n", axes = F, xlim=c(-0.1,1.1),ylim=c(-0.1,1.1),xlab="",ylab="", cex.lab = 2.75, cex = 2.75)
text(ternary_to_xy_basis[1, 1]-0.08, ternary_to_xy_basis[1, 2], vertice_label[7], cex = 2.75, lwd = 3)
text(ternary_to_xy_basis[2, 1]+0.08, ternary_to_xy_basis[2, 2], vertice_label[8], cex = 2.75, lwd = 3)
text(ternary_to_xy_basis[3, 1], ternary_to_xy_basis[3, 2]+0.08, vertice_label[9], cex = 2.75, lwd = 3)
lines(rbind(ternary_to_xy_basis, ternary_to_xy_basis[1, ]), cex = 2.75, lwd = 3, col = "black")
points(ternary_to_xy_nmf_A, cex = 1.5, lwd = 3, pch = 20)
autoLab(ternary_to_xy_nmf_A[, 1], ternary_to_xy_nmf_A[, 2], labels = row_info, cex.lab=1.5, cex = 1.5, lwd = 3)
points(ternary_to_xy_all_pk[3,1], ternary_to_xy_all_pk[3,2], cex = 1.5, lwd = 1.5, pch = 15)
text(ternary_to_xy_all_pk[3,1]+0.08, ternary_to_xy_all_pk[3,2], "average", cex = 1.5, lwd = 1.5)

lines(rbind(ternary_to_xy_all_pk[3,], ternary_to_xy_int_d1[3,]), col = "black", lwd = 3)
lines(rbind(ternary_to_xy_all_pk[3,], ternary_to_xy_int_d2[3,]), col = "black", lwd = 3) 
lines(rbind(ternary_to_xy_all_pk[3,], ternary_to_xy_int_d3[3,]), col = "black", lwd = 3)
text(ternary_to_xy_int_d1[3, 1], ternary_to_xy_int_d1[3, 2]-0.02, round(all_pk[3,1], 3), cex = 1.5, lwd = 1.5)
text(ternary_to_xy_int_d2[3, 1]+0.04, ternary_to_xy_int_d2[3, 2]+0.02, format(round(all_pk[3,2], 3), nsmall = 3), cex = 1.5, lwd = 1.5)
text(ternary_to_xy_int_d3[3, 1]-0.04, ternary_to_xy_int_d3[3, 2]+0.02, round(all_pk[3,3], 3), cex = 1.5, lwd = 1.5)
dev.off()

# Basis for LBA not used in paper
png(file=paste0("plots//lbatimebudgetdim3simplexlbadatabasis.png"),
    width=800, height=800)
par(mar = c(0, 0, 0, 0), 
    oma = c(0, 0, 0, 0)) 
plot(ternary_to_xy_basis, asp = 1, type="n", axes = F, xlim=c(-0.1,1.1),ylim=c(-0.1,1.1),xlab="",ylab="", cex.lab = 2.75, cex = 2.75)
text(ternary_to_xy_basis[1, 1]-0.08, ternary_to_xy_basis[1, 2], vertice_label[1], cex =  2.75, lwd = 3)
text(ternary_to_xy_basis[2, 1]+0.08, ternary_to_xy_basis[2, 2], vertice_label[2], cex =  2.75, lwd = 3)
text(ternary_to_xy_basis[3, 1], ternary_to_xy_basis[3, 2]+0.08, vertice_label[3], cex =  2.75, lwd = 3)

lines(rbind(ternary_to_xy_basis, ternary_to_xy_basis[1, ]), cex = 2.75, lwd = 3, col = "black")
points(ternary_to_xy_lba_Binn_reverse, cex = 1.5, lwd = 1.5, pch = 20)
points(ternary_to_xy_all_pk[1,1], ternary_to_xy_all_pk[1,2], cex = 1.5, lwd = 1.5, pch = 15)
autoLab(rbind(ternary_to_xy_lba_Binn_reverse, ternary_to_xy_all_pk[1,])[, 1], rbind(ternary_to_xy_lba_Binn_reverse, ternary_to_xy_all_pk[1,])[, 2], labels = c(activity_cols, "average"), cex.lab=1.5, cex = 1.5, lwd = 3)
lines(rbind(ternary_to_xy_all_pk[1,], ternary_to_xy_int_d1[1,]), col = "black", lwd = 3)
lines(rbind(ternary_to_xy_all_pk[1,], ternary_to_xy_int_d2[1,]), col = "black", lwd = 3) 
lines(rbind(ternary_to_xy_all_pk[1,], ternary_to_xy_int_d3[1,]), col = "black", lwd = 3)
text(ternary_to_xy_int_d1[1, 1], ternary_to_xy_int_d1[1, 2]-0.02, round(all_pk[1,1], 3), cex = 1.5, lwd = 1.5)
text(ternary_to_xy_int_d2[1, 1]+0.04, ternary_to_xy_int_d2[1, 2]+0.02, round(all_pk[1,2], 3), cex = 1.5, lwd = 1.5)
text(ternary_to_xy_int_d3[1, 1]-0.04, ternary_to_xy_int_d3[1, 2]+0.02, round(all_pk[1,3], 3), cex = 1.5, lwd = 1.5)
dev.off()

# Basis for EMA not used in paper
png(file=paste0("plots//emmatimebudgetdim3simplexlbadatabasis.png"),
    width=800, height=800)
par(mar = c(0, 0, 0, 0), 
    oma = c(0, 0, 0, 0)) 
plot(ternary_to_xy_basis, asp = 1, type="n", axes = F, xlim=c(-0.1,1.1),ylim=c(-0.1,1.1),xlab="",ylab="", cex.lab = 2.75, cex = 2.75)
text(ternary_to_xy_basis[1, 1]-0.08, ternary_to_xy_basis[1, 2], vertice_label[4], cex = 2.75, lwd = 3)
text(ternary_to_xy_basis[2, 1]+0.08, ternary_to_xy_basis[2, 2], vertice_label[5], cex = 2.75, lwd = 3)
text(ternary_to_xy_basis[3, 1], ternary_to_xy_basis[3, 2]+0.08, vertice_label[6], cex = 2.75, lwd = 3)

lines(rbind(ternary_to_xy_basis, ternary_to_xy_basis[1, ]), cex = 2.75, lwd = 3, col = "black")

points(ternary_to_xy_emma_B_reverse, cex = 1.5, lwd = 3, pch = 20)
points(ternary_to_xy_all_pk[2,1], ternary_to_xy_all_pk[2,2], cex = 1.5, lwd = 1.5, pch = 15)
autoLab(rbind(ternary_to_xy_emma_B_reverse, ternary_to_xy_all_pk[2,])[, 1], rbind(ternary_to_xy_emma_B_reverse, ternary_to_xy_all_pk[2,])[, 2], labels = c(activity_cols, "average"), cex.lab=1.5, cex = 1.5, lwd = 3)
lines(rbind(ternary_to_xy_all_pk[2,], ternary_to_xy_int_d1[2,]), col = "black", lwd = 3)
lines(rbind(ternary_to_xy_all_pk[2,], ternary_to_xy_int_d2[2,]), col = "black", lwd = 3) 
lines(rbind(ternary_to_xy_all_pk[2,], ternary_to_xy_int_d3[2,]), col = "black", lwd = 3)
text(ternary_to_xy_int_d1[2, 1], ternary_to_xy_int_d1[2, 2]-0.02, round(all_pk[2,1], 3), cex = 1.5, lwd = 1.5)
text(ternary_to_xy_int_d2[2, 1]+0.04, ternary_to_xy_int_d2[2, 2]+0.02, round(all_pk[2,2], 3), cex = 1.5, lwd = 1.5)
text(ternary_to_xy_int_d3[2, 1]-0.04, ternary_to_xy_int_d3[2, 2]+0.02, round(all_pk[2,3], 3), cex = 1.5, lwd = 1.5)
dev.off()

# Basis for NMF used in paper
png(file=paste0("plots//nmftimebudgetdim3simplexlbadatabasis.png"),
    width=800, height=800)
par(mar = c(0, 0, 0, 0), 
    oma = c(0, 0, 0, 0)) 
plot(ternary_to_xy_basis, asp = 1, type="n", axes = F, xlim=c(-0.1,1.1),ylim=c(-0.1,1.1),xlab="",ylab="", cex.lab = 2.75, cex = 2.75)
text(ternary_to_xy_basis[1, 1]-0.08, ternary_to_xy_basis[1, 2], vertice_label[7], cex = 2.75, lwd = 3)
text(ternary_to_xy_basis[2, 1]+0.08, ternary_to_xy_basis[2, 2], vertice_label[8], cex = 2.75, lwd = 3)
text(ternary_to_xy_basis[3, 1], ternary_to_xy_basis[3, 2]+0.08, vertice_label[9], cex = 2.75, lwd = 3)

lines(rbind(ternary_to_xy_basis, ternary_to_xy_basis[1, ]), cex = 2.75, lwd = 3, col = "black")

points(ternary_to_xy_nmf_B_reverse, cex = 1.5, lwd = 3, pch = 20)
points(ternary_to_xy_all_pk[3,1], ternary_to_xy_all_pk[3,2], cex = 1.5, lwd = 1.5, pch = 15)
autoLab(rbind(ternary_to_xy_nmf_B_reverse, ternary_to_xy_all_pk[3,])[, 1], rbind(ternary_to_xy_nmf_B_reverse, ternary_to_xy_all_pk[3,])[, 2], labels = c(activity_cols, "average"), cex.lab=1.5, cex = 1.5, lwd = 3)

lines(rbind(ternary_to_xy_all_pk[3,], ternary_to_xy_int_d1[3,]), col = "black", lwd = 3)
lines(rbind(ternary_to_xy_all_pk[3,], ternary_to_xy_int_d2[3,]), col = "black", lwd = 3) 
lines(rbind(ternary_to_xy_all_pk[3,], ternary_to_xy_int_d3[3,]), col = "black", lwd = 3)
text(ternary_to_xy_int_d1[3, 1], ternary_to_xy_int_d1[3, 2]-0.02, round(all_pk[3,1], 3), cex = 1.5, lwd = 1.5)
text(ternary_to_xy_int_d2[3, 1]+0.04, ternary_to_xy_int_d2[3, 2]+0.02, format(round(all_pk[3,2], 3), nsmall = 3), cex = 1.5, lwd = 1.5)
text(ternary_to_xy_int_d3[3, 1]-0.04, ternary_to_xy_int_d3[3, 2]+0.02, round(all_pk[3,3], 3), cex = 1.5, lwd = 1.5)
dev.off()

#### ---------------Latex--------------###
print(xtable(X, type = "latex",digits=c(rep(c(0),times=dim(X)[2]+1))), floating=FALSE)
combined_A <- cbind(bmilba$Aoi, emma_A, nmf_A)
combined_B <- cbind(bmilba$Boi, emma_B, nmf_B)


bmilbadim1 <- lba(X, K = 1, row.weights = 1, col.weights = 1, method = "ls", what = 'inner',trace.lba = FALSE)

combined_A <- cbind(bmilbadim1$A, combined_A)
combined_B <- cbind(bmilbadim1$B, combined_B)
print(xtable(combined_A, type = "latex",digits=c(rep(c(3),times=3*3+1+1))), floating=FALSE)
print(xtable(combined_B, type = "latex",digits=c(rep(c(3),times=3*3+1+1))), floating=FALSE)

all_pk
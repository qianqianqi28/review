rm(list=ls())
library('crosstalk')
library('rgl')
library('lba')
library(Ternary)
library(xtable)
library(R.matlab)
library(Matrix)

# Data is from Exhibit 16.1 Greenacre (2017) on Page 122
A <- matrix(c(
  448, 369,
  1789, 1753,
  636, 859,
  177, 237,
  39, 64
), ncol = 2, byrow = TRUE)
rankMatrix(A)

colnames(A) <- c("male", "female")
rownames(A) <- c("very good", "good", "regular", "bad", "very bad")

print(xtable(A, type = "latex",digits=c(rep(c(0),times=3))), floating=FALSE)


# Normalized data such that each row sums to 1
A_norm <- diag(1/apply(A, 1, sum)) %*% A
apply(A_norm, 1, sum)
rownames(A_norm) <- rownames(A)
print(xtable(A_norm, type = "latex",digits=c(rep(c(3),times=3))), floating=FALSE)

# Basis matrix
supp <- round(matrix(c(0.25, 0.85, 0.85, 0.15, 0.15, 0.75, 0.75, 0.25), nrow = 4, byrow = TRUE)*sum(A)/4)

# Normalized basis matrix
supp_norm <- diag(1/apply(supp, 1, sum)) %*% supp
apply(supp_norm, 1, sum)

png(file=paste0("plots//greenacrep122dim2.png"),
    width=600, height=600)

plot(A[,1], A[,2], asp = 1,
     xlab = "", ylab = "",
     pch = 1, col = "blue",
     axes = FALSE, frame.plot = FALSE,
     xlim = c(-200, 1800), ylim = c(-200, 1800), cex = 1.75, cex.lab=1.75, lwd = 2.75)
for (i in 1:nrow(A)) {
  segments(0, 0, A[i,1], A[i,2], col="blue", lwd=1, lty = 2)
}

points(supp[1,], supp[2,], pch = 0, cex = 1.75, col = "red", lwd = 2.75)
segments(0, 0, supp[1,], supp[2,], col="red", lwd=1, lty = 2)
points(supp[3,], supp[4,], pch = 2, cex = 1.75, col = "green", lwd = 2.75)
segments(0, 0, supp[3,], supp[4,], col="green", lwd=1, lty = 2)

# Draw x-axis
segments(-200, 0, 1800, 0, col = "black", lwd = 1.75)

# Draw y-axis
segments(0, -200, 0, 1800, col = "black", lwd = 1.75)

# Add x-axis ticks
axis(1, at = seq(200, 1800, by = 200), pos = 0, cex.axis = 1.75)

# Add y-axis ticks
axis(2, at = seq(200, 1800, by = 200), pos = 0, cex.axis = 1.75)
text(-80,-80,"O", cex = 1.75)
dev.off()

png(file=paste0("plots//greenacrep122dim2l1.png"),
    width=600, height=600)
plot(A_norm[,1], A_norm[,2], asp = 1,
     xlab = "", ylab = "",
     pch = 1, col = "blue",
     axes = FALSE, frame.plot = FALSE,
     xlim = c(-0.1, 1), ylim = c(-0.1, 1), cex = 1.75, cex.lab=1.75, lwd = 2.75)

for (i in 1:nrow(A_norm)) {
  segments(0, 0, A_norm[i,1], A_norm[i,2], col="blue", lwd=1, lty = 2)
}


points(supp_norm[1:2,], pch = 0, cex = 1.75, col = "red", lwd = 2.75)
segments(0, 0, supp_norm[1,1], supp_norm[1,2], col="red", lwd=1, lty = 2)
segments(0, 0, supp_norm[2,1], supp_norm[2,2], col="red", lwd=1, lty = 2)

points(supp_norm[3:4,], pch = 2, cex = 1.75, col = "green", lwd = 2.75)
segments(0, 0, supp_norm[3,1], supp_norm[3,2], col="green", lwd=1, lty = 2)
segments(0, 0, supp_norm[4,1], supp_norm[4,2], col="green", lwd=1, lty = 2)

segments(0, 1, 1, 0, col = "black", lwd = 1.75)
# Draw x-axis
segments(-0.1, 0, 1.1, 0, col = "black", lwd = 1.75)

# Draw y-axis
segments(0, -0.1, 0, 1.1, col = "black", lwd = 1.75)

# Add x-axis ticks
axis(1, at = seq(0.2, 1, by = 0.2), pos = 0, cex.axis = 1.75)

# Add y-axis ticks
axis(2, at = seq(0.2, 1, by = 0.2), pos = 0, cex.axis = 1.75)

text(-0.05,-0.05,"O", cex = 1.75)
dev.off()


png(file=paste0("plots//greenacrep122dim2simplex.png"),
    width=800, height=800)

plot(1,type="n", axes = F, xlim=c(-0.2,1.2),ylim=c(-1,1),xlab="",ylab="", cex.lab = 2.75, cex = 2.75)

basis <- matrix(c(0, 0, 1, 0), nrow = 2, byrow = TRUE)

lines(x = c(basis[1,1], basis[2,1]), y = c(basis[1,2], basis[2,2]), cex = 2.75, lwd = 3)


for (i in c(1:nrow(A_norm))) {
  points(A_norm[i, 1], 0,col = "blue", cex = 2.75, lwd = 3)
}

points(basis[1,1], basis[1,2],  pch = 16, cex = 2.75, lwd = 3)
text(basis[1,1], -0.2, expression(bold(e)[2]), cex = 2.75)
points(basis[2,1], basis[2,2],  pch = 16, cex = 2.75, lwd = 3)
text(basis[2,1], -0.2, expression(bold(e)[1]), cex = 2.75)
points(supp_norm[1, 1], 0,  pch = 0, col = "red", cex = 2.75, lwd = 3)
points(supp_norm[2, 1], 0,  pch = 0, col = "red", cex = 2.75, lwd = 3)
points(supp_norm[3, 1], 0,  pch = 2, col = "green", cex = 2.75, lwd = 3)
points(supp_norm[4, 1], 0,  pch = 2, col = "green", cex = 2.75, lwd = 3)
dev.off()




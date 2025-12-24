rm(list=ls())
library('crosstalk')
library('rgl')
library('lba')
library(Ternary)
library(xtable)
library(R.matlab)
library(Matrix)

# Data is from Exhibit 3.1 Greenacre (2017) on Page 18
A <- matrix(c(
  5, 7, 2,
  18, 46, 20,
  19, 29, 39,
  12, 40, 49,
  3, 7, 16
), ncol = 3, byrow = TRUE)
rankMatrix(A)

colnames(A) <- c("C1", "C2", "C3")
rownames(A) <- c("E1", "E2", "E3", "E4", "E5")

print(xtable(A, type = "latex",digits=c(rep(c(0),times=4))), floating=FALSE)

# Normalized data such that each row sums to 1
A_norm <- diag(1/apply(A, 1, sum)) %*% A
apply(A_norm, 1, sum)
rownames(A_norm) <- rownames(A)
print(xtable(A_norm, type = "latex",digits=c(rep(c(3),times=4))), floating=FALSE)

# Basis matrix
supp <- matrix(c(0.2, 0.1, 0.02, 0.05, 0.5, 0.05, 0.05, 0.1, 0.65, 0.5, 0.1, 0.1, 0.04, 0.2, 0.01, 0.05, 0.2, 0.75), nrow = 6, byrow = TRUE)*100

# Normalized basis matrix
supp_norm <- diag(1/apply(supp, 1, sum)) %*% supp
apply(supp_norm, 1, sum)


png(file=paste0("plots//greenacrep18dim3simplex.png"),
    width=800, height=800)


ternary_to_xy_A <- t(apply(A_norm, 
                           1, 
                           function(x) {
                             x1 <- 0.5 * (2*x[2] + x[3])
                             x2 <- (sqrt(3)/2) * x[3]
                             return(c(x1, x2))
                           }))

basis <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, byrow = TRUE)

ternary_to_xy_basis <- t(apply(basis, 
                               1, 
                               function(x) {
                                 x1 <- 0.5 * (2*x[2] + x[3])
                                 x2 <- (sqrt(3)/2) * x[3]
                                 return(c(x1, x2))
                               }))
ternary_to_xy_supp <- t(apply(supp_norm, 
                              1, 
                              function(x) {
                                x1 <- 0.5 * (2*x[2] + x[3])
                                x2 <- (sqrt(3)/2) * x[3]
                                return(c(x1, x2))
                              }))
plot(ternary_to_xy_basis, asp = 1, type="n", axes = F, xlim=c(-0.1,1.1),ylim=c(-0.1,1.1),xlab="",ylab="", cex.lab = 2.75, cex = 2.75)
text(ternary_to_xy_basis[1,1]-0.08, ternary_to_xy_basis[1,2], expression(bold(e)[1]), rownames(basis), cex = 2.75, lwd = 3)
text(ternary_to_xy_basis[2,1]+0.1, ternary_to_xy_basis[2,2], expression(bold(e)[2]), rownames(basis), cex = 2.75, lwd = 3)
text(ternary_to_xy_basis[3,1], ternary_to_xy_basis[3,2]+0.08, expression(bold(e)[3]), rownames(basis), cex = 2.75, lwd = 3)

lines(rbind(ternary_to_xy_basis, ternary_to_xy_basis[1, ]), cex = 2.75, lwd = 3)

points(ternary_to_xy_A, cex = 2.75, lwd = 3, pch = 16, col = "blue")
points(ternary_to_xy_supp[1:3,], col = "red",  pch = 4, cex = 2.75, lwd = 3)
lines(rbind(ternary_to_xy_supp[1:3, ], ternary_to_xy_supp[1, ]), col = "red", cex = 2.75, lwd = 3)
points(ternary_to_xy_supp[4:6,], col = "green",  pch = 3, cex = 2.75, lwd = 3)
lines(rbind(ternary_to_xy_supp[4:6, ], ternary_to_xy_supp[4, ]), col = "green", cex = 2.75, lwd = 3)
dev.off()

# After the figure is drawn, one needs to adjust the figure to get the good visualization
plot3d(A,type = "s",
       col = "blue",
       asp = 1,
       xlab = "x", ylab = "y", zlab = "z",
       size = 1,
       xlim = c(0, 70),
       ylim = c(0, 70),
       zlim = c(0, 70),
       box = FALSE, axes = TRUE, cex = 1.75, cex.lab=1.75, lwd = 2.75)

for (i in 1:nrow(A)) {
  segments3d(rbind(c(0,0,0), A[i, ]), 
             col = "blue", 
             lwd = 0.8)
}

text3d(supp[1:3, ], texts = "X", cex = 1.2, col = "red")
text3d(supp[4:6, ], texts = "+", cex = 2, col = "green")
for (i in 1:3) {
  segments3d(rbind(c(0,0,0), supp[i, ]), 
             col = "red", 
             lwd = 0.8)
  segments3d(rbind(c(0,0,0), supp[i+3, ]), 
             col = "green", 
             lwd = 0.8)
}

rgl.snapshot("plots//greenacrep18dim3.png")

# Before plot the following figure, please close the previous figure. Again, after the figure is drawn, one needs to adjust the figure to get the good visualization
plot3d(A_norm,type = "s", 
       col = "blue", 
       asp = 1,
       xlab = "x", ylab = "y", zlab = "z",
       size = 1, 
       xlim = c(0, 1.1),
       ylim = c(0, 1.1),
       zlim = c(0, 1.1),
       box = FALSE, axes = TRUE, cex = 1.75, cex.lab=1.75, lwd = 2.75)

for (i in 1:nrow(A_norm)) {
  segments3d(rbind(c(0,0,0), A_norm[i, ]), 
             col = "blue", 
             lwd = 0.8)
}

basis <- matrix(c(0, 0, 1, 0, 1, 0, 1, 0, 0), nrow = 3, byrow = TRUE)

triangles3d(basis, color = "black", alpha = 0.5)

triangles3d(supp_norm[1:3, ], color = "red", alpha = 0.5)
triangles3d(supp_norm[1:3, ], color = "red", alpha = 0.5)
text3d(supp_norm[1:3, ], texts = "X", cex = 1.2, col = "red")
triangles3d(supp_norm[4:6, ], color = "green", alpha = 0.5)
text3d(supp_norm[4:6, ], texts = "+", cex = 2, col = "green")
for (i in 1:3) {
  segments3d(rbind(c(0,0,0), supp_norm[i, ]), 
             col = "red", 
             lwd = 0.8)
  segments3d(rbind(c(0,0,0), supp_norm[i+3, ]), 
             col = "green", 
             lwd = 0.8)
}

rgl.snapshot("plots//greenacrep18dim3l1.png")




rm(list=ls())
library(Matrix)

# Number of points and dimensions
I <- 25
J <- 3

# Create a random 25x3 matrix
A <- matrix(runif(I * J, min = 0.3, max = 0.7), nrow = I, ncol = J)

# Compute the rank
rankMatrix(A)

A_norm <- diag(1/apply(A, 1, sum)) %*% A


ternary_to_xy_A <- t(apply(A_norm, 
                           1, 
                           function(x) {
                             x1 <- 0.5 * (2*x[2] + x[3])
                             x2 <- (sqrt(3)/2) * x[3]
                             return(c(x1, x2))
                           }))

basis <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, byrow = TRUE)
rownames(basis) <- c("e1", "e2", "e3")

ternary_to_xy_basis <- t(apply(basis, 
                               1, 
                               function(x) {
                                 x1 <- 0.5 * (2*x[2] + x[3])
                                 x2 <- (sqrt(3)/2) * x[3]
                                 return(c(x1, x2))
                               }))

# draw the inscribed circle
v1 <- ternary_to_xy_basis[1, ]
v2 <- ternary_to_xy_basis[2, ]
v3 <- ternary_to_xy_basis[3, ]
a <- sqrt(sum((v2 - v3)^2))
b <- sqrt(sum((v1 - v3)^2))
c <- sqrt(sum((v1 - v2)^2))
px <- (a * v1[1] + b * v2[1] + c * v3[1]) / (a + b + c)
py <- (a * v1[2] + b * v2[2] + c * v3[2]) / (a + b + c)
s <- (a + b + c) / 2
area <- sqrt(s * (s - a) * (s - b) * (s - c))
r <- area / s

incenter <- (a*v1 + b*v2 + c*v3) / (a + b + c)
theta <- seq(0, 2*pi, length.out = 300)
incircle_x <- incenter[1] + r * cos(theta)
incircle_y <- incenter[2] + r * sin(theta)

png(file=paste0("plots//separability.png"),
    width=500, height=500)
par(mar = rep(0.2, 4))
plot(ternary_to_xy_basis, asp = 1, type="n", axes = F, xlim=c(-0.3,1.2),ylim=c(-0.3,1.2),xlab="",ylab="", cex.lab = 2.75, cex = 2.75)

points(ternary_to_xy_basis, cex = 2.75, lwd = 3, col = "red", pch = 4)
lines(rbind(ternary_to_xy_basis, ternary_to_xy_basis[1, ]), cex = 2.75, lwd = 1.5, col = "red", lty = 2)

# Plot the extra points
points(ternary_to_xy_basis, cex = 2.75, lwd = 3, col = "blue")
lines(rbind(ternary_to_xy_basis, ternary_to_xy_basis[1, ]), cex = 2.75, lwd = 1)

points(ternary_to_xy_A, cex = 2.75, lwd = 3, col = "blue")

text(ternary_to_xy_basis[1,1]-0.1, ternary_to_xy_basis[1,2], expression(bold(e)[1]), cex = 2.75, lwd = 3)
text(ternary_to_xy_basis[2,1]+0.12, ternary_to_xy_basis[2,2], expression(bold(e)[2]), cex = 2.75, lwd = 3)

text(ternary_to_xy_basis[3,1], ternary_to_xy_basis[3,2]+0.1, expression(bold(e)[3]), cex = 2.75, lwd = 3)
dev.off()

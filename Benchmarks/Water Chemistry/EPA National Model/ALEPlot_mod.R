## standard ALE plot but commented out automatic plots

ALEPlot_mod<- function (X, X.model, pred.fun, J, K = 40, NA.plot = TRUE, ...)
{
    N = dim(X)[1]
    d = dim(X)[2]

    if (length(J) == 1) {
        if (class(X[, J]) == "factor") {
            X[, J] <- droplevels(X[, J])
            x.count <- as.numeric(table(X[, J]))
            x.prob <- x.count/sum(x.count)
            K <- nlevels(X[, J])
            D.cum <- matrix(0, K, K)
            D <- matrix(0, K, K)
            for (j in setdiff(1:d, J)) {
                if (class(X[, j]) == "factor") {
                  A = table(X[, J], X[, j])
                  A = A/x.count
                  for (i in 1:(K - 1)) {
                    for (k in (i + 1):K) {
                      D[i, k] = sum(abs(A[i, ] - A[k, ]))/2
                      D[k, i] = D[i, k]
                    }
                  }
                  D.cum <- D.cum + D
                }
                else {
                  q.x.all <- quantile(X[, j], probs = seq(0,
                    1, length.out = 100), na.rm = TRUE, names = FALSE)
                  x.ecdf = tapply(X[, j], X[, J], ecdf)
                  for (i in 1:(K - 1)) {
                    for (k in (i + 1):K) {
                      D[i, k] = max(abs(x.ecdf[[i]](q.x.all) -
                        x.ecdf[[k]](q.x.all)))
                      D[k, i] = D[i, k]
                    }
                  }
                  D.cum <- D.cum + D
                }
            }
            D1D <- cmdscale(D.cum, k = 1)
            ind.ord <- sort(D1D, index.return = T)$ix
            ord.ind <- sort(ind.ord, index.return = T)$ix
            levs.orig <- levels(X[, J])
            levs.ord <- levs.orig[ind.ord]
            x.ord <- ord.ind[as.numeric(X[, J])]
            row.ind.plus <- (1:N)[x.ord < K]
            row.ind.neg <- (1:N)[x.ord > 1]
            X.plus <- X
            X.neg <- X
            X.plus[row.ind.plus, J] <- levs.ord[x.ord[row.ind.plus] +
                1]
            X.neg[row.ind.neg, J] <- levs.ord[x.ord[row.ind.neg] -
                1]
            y.hat <- pred.fun(X.model = X.model, newdata = X, taxon = taxon)
            y.hat.plus <- pred.fun(X.model = X.model, newdata = X.plus[row.ind.plus,
                ], taxon = taxon)
            y.hat.neg <- pred.fun(X.model = X.model, newdata = X.neg[row.ind.neg,
                ], taxon = taxon)
            Delta.plus <- y.hat.plus - y.hat[row.ind.plus]
            Delta.neg <- y.hat[row.ind.neg] - y.hat.neg
            Delta <- as.numeric(tapply(c(Delta.plus, Delta.neg),
                c(x.ord[row.ind.plus], x.ord[row.ind.neg] - 1),
                mean))
            fJ <- c(0, cumsum(Delta))
            fJ = fJ - sum(fJ * x.prob[ind.ord])
            x <- levs.ord
            barplot(fJ, names = x, xlab = paste("x_", J, " (",
                names(X)[J], ")", sep = ""), ylab = paste("f_",
                J, "(x_", J, ")", sep = ""), las = 3)
        }
        else if (class(X[, J]) == "numeric" | class(X[, J]) ==
            "integer") {
            z = c(min(X[, J]), as.numeric(quantile(X[, J], seq(1/K,
                1, length.out = K), type = 1)))
            z = unique(z)
            K = length(z) - 1
            fJ = numeric(K)
            a1 = as.numeric(cut(X[, J], breaks = z, include.lowest = TRUE))
            X1 = X
            X2 = X
            X1[, J] = z[a1]  # assign value of var of interest to bin boundary
            X2[, J] = z[a1 + 1] # assign value of var of interest to upper bin boundary

            y.hat1 = pred.fun(X.model = X.model, newdata = X1)#, dfcorrect = dfcorrect)
            y.hat2 = pred.fun(X.model = X.model, newdata = X2)#, dfcorrect = dfcorrect)

            Delta = y.hat2 - y.hat1
#            Delta = apply(Delta, 2, function(x) as.numeric(tapply(x, a1, mean)))
            Delta = as.numeric(tapply(Delta, a1, mean))

#            fJ = rbind(rep(0, times = ncol(Delta)), apply(Delta, 2, cumsum))
            fJ = c(0, cumsum(Delta))

            b1 <- as.numeric(table(a1))
#            f0 <- apply(fJ, 2, function(x) sum((x[1:K] + x[2:(K + 1)])/2 * b1)/sum(b1))
#            for (kk in 1:ncol(fJ)) fJ[,kk] <- fJ[,kk] - f0[kk]
            fJ = fJ - sum((fJ[1:K] + fJ[2:(K + 1)])/2 * b1)/sum(b1)
            x <- z
#            plot(x, fJ, type = "l", xlab = paste("x_", J, " (",
#                names(X)[J], ")", sep = ""), ylab = paste("f_",
#                J, "(x_", J, ")", sep = ""))
        }
        else print("error:  class(X[,J]) must be either factor or numeric or integer")
    }
    else if (length(J) == 2) {
        if (class(X[, J[2]]) != "numeric" & class(X[, J[2]]) !=
            "integer") {
            print("error: X[,J[2]] must be numeric or integer. Only X[,J[1]] can be a factor")
        }
        if (class(X[, J[1]]) == "factor") {
            X[, J[1]] <- droplevels(X[, J[1]])
            x.count <- as.numeric(table(X[, J[1]]))
            x.prob <- x.count/sum(x.count)
            K1 <- nlevels(X[, J[1]])
            D.cum <- matrix(0, K1, K1)
            D <- matrix(0, K1, K1)
            for (j in setdiff(1:d, J[1])) {
                if (class(X[, j]) == "factor") {
                  A = table(X[, J[1]], X[, j])
                  A = A/x.count
                  for (i in 1:(K1 - 1)) {
                    for (k in (i + 1):K1) {
                      D[i, k] = sum(abs(A[i, ] - A[k, ]))/2
                      D[k, i] = D[i, k]
                    }
                  }
                  D.cum <- D.cum + D
                }
                else {
                  q.x.all <- quantile(X[, j], probs = seq(0,
                    1, length.out = 100), na.rm = TRUE, names = FALSE)
                  x.ecdf = tapply(X[, j], X[, J[1]], ecdf)
                  for (i in 1:(K1 - 1)) {
                    for (k in (i + 1):K1) {
                      D[i, k] = max(abs(x.ecdf[[i]](q.x.all) -
                        x.ecdf[[k]](q.x.all)))
                      D[k, i] = D[i, k]
                    }
                  }
                  D.cum <- D.cum + D
                }
            }
            D1D <- cmdscale(D.cum, k = 1)
            ind.ord <- sort(D1D, index.return = T)$ix
            ord.ind <- sort(ind.ord, index.return = T)$ix
            levs.orig <- levels(X[, J[1]])
            levs.ord <- levs.orig[ind.ord]
            x.ord <- ord.ind[as.numeric(X[, J[1]])]
            z2 = c(min(X[, J[2]]), as.numeric(quantile(X[, J[2]],
                seq(1/K, 1, length.out = K), type = 1)))
            z2 = unique(z2)
            K2 = length(z2) - 1
            a2 = as.numeric(cut(X[, J[2]], breaks = z2, include.lowest = TRUE))
            row.ind.plus <- (1:N)[x.ord < K1]
            X11 = X
            X12 = X
            X21 = X
            X22 = X
            X11[row.ind.plus, J[2]] = z2[a2][row.ind.plus]
            X12[row.ind.plus, J[2]] = z2[a2 + 1][row.ind.plus]
            X21[row.ind.plus, J[1]] = levs.ord[x.ord[row.ind.plus] +
                1]
            X22[row.ind.plus, J[1]] = levs.ord[x.ord[row.ind.plus] +
                1]
            X21[row.ind.plus, J[2]] = z2[a2][row.ind.plus]
            X22[row.ind.plus, J[2]] = z2[a2 + 1][row.ind.plus]
            y.hat11 = pred.fun(X.model = X.model, newdata = X11[row.ind.plus,
                ], taxon = taxon)
            y.hat12 = pred.fun(X.model = X.model, newdata = X12[row.ind.plus,
                ], taxon = taxon)
            y.hat21 = pred.fun(X.model = X.model, newdata = X21[row.ind.plus,
                ], taxon = taxon)
            y.hat22 = pred.fun(X.model = X.model, newdata = X22[row.ind.plus,
                ], taxon = taxon)
            Delta.plus = (y.hat22 - y.hat21) - (y.hat12 - y.hat11)
            row.ind.neg <- (1:N)[x.ord > 1]
            X11 = X
            X12 = X
            X21 = X
            X22 = X
            X11[row.ind.neg, J[1]] = levs.ord[x.ord[row.ind.neg] -
                1]
            X12[row.ind.neg, J[1]] = levs.ord[x.ord[row.ind.neg] -
                1]
            X11[row.ind.neg, J[2]] = z2[a2][row.ind.neg]
            X12[row.ind.neg, J[2]] = z2[a2 + 1][row.ind.neg]
            X21[row.ind.neg, J[2]] = z2[a2][row.ind.neg]
            X22[row.ind.neg, J[2]] = z2[a2 + 1][row.ind.neg]
            y.hat11 = pred.fun(X.model = X.model, newdata = X11[row.ind.neg,
                ], taxon = taxon)
            y.hat12 = pred.fun(X.model = X.model, newdata = X12[row.ind.neg,
                ], taxon = taxon)
            y.hat21 = pred.fun(X.model = X.model, newdata = X21[row.ind.neg,
                ], taxon = taxon)
            y.hat22 = pred.fun(X.model = X.model, newdata = X22[row.ind.neg,
                ], taxon = taxon)
            Delta.neg = (y.hat22 - y.hat21) - (y.hat12 - y.hat11)
            Delta = as.matrix(tapply(c(Delta.plus, Delta.neg),
                list(c(x.ord[row.ind.plus], x.ord[row.ind.neg] -
                  1), a2[c(row.ind.plus, row.ind.neg)]), mean))
            NA.Delta = is.na(Delta)
            NA.ind = which(NA.Delta, arr.ind = T, useNames = F)
            if (nrow(NA.ind) > 0) {
                notNA.ind = which(!NA.Delta, arr.ind = T, useNames = F)
                range1 = K1 - 1
                range2 = max(z2) - min(z2)
                Z.NA = cbind(NA.ind[, 1]/range1, (z2[NA.ind[,
                  2]] + z2[NA.ind[, 2] + 1])/2/range2)
                Z.notNA = cbind(notNA.ind[, 1]/range1, (z2[notNA.ind[,
                  2]] + z2[notNA.ind[, 2] + 1])/2/range2)
                nbrs <- ann(Z.notNA, Z.NA, k = 1, verbose = F)$knnIndexDist[,
                  1]
                Delta[NA.ind] = Delta[matrix(notNA.ind[nbrs,
                  ], ncol = 2)]
            }
            fJ = matrix(0, K1 - 1, K2)
            fJ = apply(t(apply(Delta, 1, cumsum)), 2, cumsum)
            fJ = rbind(rep(0, K2), fJ)
            fJ = cbind(rep(0, K1), fJ)
            b = as.matrix(table(x.ord, a2))
            b2 = apply(b, 2, sum)
            Delta = fJ[, 2:(K2 + 1)] - fJ[, 1:K2]
            b.Delta = b * Delta
            Delta.Ave = apply(b.Delta, 2, sum)/b2
            fJ2 = c(0, cumsum(Delta.Ave))
            b.ave = matrix((b[1:(K1 - 1), ] + b[2:K1, ])/2, K1 -
                1, K2)
            b1 = apply(b.ave, 1, sum)
            Delta = matrix(fJ[2:K1, ] - fJ[1:(K1 - 1), ], K1 -
                1, K2 + 1)
            b.Delta = matrix(b.ave * (Delta[, 1:K2] + Delta[,
                2:(K2 + 1)])/2, K1 - 1, K2)
            Delta.Ave = apply(b.Delta, 1, sum)/b1
            fJ1 = c(0, cumsum(Delta.Ave))
            fJ = fJ - outer(fJ1, rep(1, K2 + 1)) - outer(rep(1,
                K1), fJ2)
            fJ0 = sum(b * (fJ[, 1:K2] + fJ[, 2:(K2 + 1)])/2)/sum(b)
            fJ = fJ - fJ0
            x <- list(levs.ord, z2)
            K <- c(K1, K2)
            image(1:K1, x[[2]], fJ, xlab = paste("x_", J[1],
                " (", names(X)[J[1]], ")", sep = ""), ylab = paste("x_",
                J[2], " (", names(X)[J[2]], ")", sep = ""), ylim = range(z2),
                yaxs = "i")
            contour(1:K1, x[[2]], fJ, add = TRUE, drawlabels = TRUE)
            axis(side = 1, labels = x[[1]], at = 1:K1, las = 3,
                padj = 1.2)
            if (NA.plot == FALSE) {
                if (nrow(NA.ind) > 0) {
                  NA.ind = which(b == 0, arr.ind = T, useNames = F)
                  rect(xleft = NA.ind[, 1] - 0.5, ybottom = z2[NA.ind[,
                    2]], xright = NA.ind[, 1] + 0.5, ytop = z2[NA.ind[,
                    2] + 1], col = "black")
                }
            }
        }
        else if (class(X[, J[1]]) == "numeric" | class(X[, J[1]]) ==
            "integer") {
            z1 = c(min(X[, J[1]]), as.numeric(quantile(X[, J[1]],
                seq(1/K, 1, length.out = K), type = 1)))
            z1 = unique(z1)
            K1 = length(z1) - 1
            a1 = as.numeric(cut(X[, J[1]], breaks = z1, include.lowest = TRUE))
            z2 = c(min(X[, J[2]]), as.numeric(quantile(X[, J[2]],
                seq(1/K, 1, length.out = K), type = 1)))
            z2 = unique(z2)
            K2 = length(z2) - 1
            fJ = matrix(0, K1, K2)
            a2 = as.numeric(cut(X[, J[2]], breaks = z2, include.lowest = TRUE))
            X11 = X
            X12 = X
            X21 = X
            X22 = X
            X11[, J] = cbind(z1[a1], z2[a2])
            X12[, J] = cbind(z1[a1], z2[a2 + 1])
            X21[, J] = cbind(z1[a1 + 1], z2[a2])
            X22[, J] = cbind(z1[a1 + 1], z2[a2 + 1])
            y.hat11 = pred.fun(X.model = X.model, newdata = X11, taxon = taxon)
            y.hat12 = pred.fun(X.model = X.model, newdata = X12, taxon = taxon)
            y.hat21 = pred.fun(X.model = X.model, newdata = X21, taxon = taxon)
            y.hat22 = pred.fun(X.model = X.model, newdata = X22, taxon = taxon)
            Delta = (y.hat22 - y.hat21) - (y.hat12 - y.hat11)
            Delta = as.matrix(tapply(Delta, list(a1, a2), mean))
            NA.Delta = is.na(Delta)
            NA.ind = which(NA.Delta, arr.ind = T, useNames = F)
            if (nrow(NA.ind) > 0) {
                notNA.ind = which(!NA.Delta, arr.ind = T, useNames = F)
                range1 = max(z1) - min(z1)
                range2 = max(z2) - min(z2)
                Z.NA = cbind((z1[NA.ind[, 1]] + z1[NA.ind[, 1] +
                  1])/2/range1, (z2[NA.ind[, 2]] + z2[NA.ind[,
                  2] + 1])/2/range2)
                Z.notNA = cbind((z1[notNA.ind[, 1]] + z1[notNA.ind[,
                  1] + 1])/2/range1, (z2[notNA.ind[, 2]] + z2[notNA.ind[,
                  2] + 1])/2/range2)
                nbrs <- ann(Z.notNA, Z.NA, k = 1, verbose = F)$knnIndexDist[,
                  1]
                Delta[NA.ind] = Delta[matrix(notNA.ind[nbrs,
                  ], ncol = 2)]
            }
            fJ = apply(t(apply(Delta, 1, cumsum)), 2, cumsum)
            fJ = rbind(rep(0, K2), fJ)
            fJ = cbind(rep(0, K1 + 1), fJ)
            b = as.matrix(table(a1, a2))
            b1 = apply(b, 1, sum)
            b2 = apply(b, 2, sum)
            Delta = fJ[2:(K1 + 1), ] - fJ[1:K1, ]
            b.Delta = b * (Delta[, 1:K2] + Delta[, 2:(K2 + 1)])/2
            Delta.Ave = apply(b.Delta, 1, sum)/b1
            fJ1 = c(0, cumsum(Delta.Ave))
            Delta = fJ[, 2:(K2 + 1)] - fJ[, 1:K2]
            b.Delta = b * (Delta[1:K1, ] + Delta[2:(K1 + 1),
                ])/2
            Delta.Ave = apply(b.Delta, 2, sum)/b2
            fJ2 = c(0, cumsum(Delta.Ave))
            fJ = fJ - outer(fJ1, rep(1, K2 + 1)) - outer(rep(1,
                K1 + 1), fJ2)
            fJ0 = sum(b * (fJ[1:K1, 1:K2] + fJ[1:K1, 2:(K2 +
                1)] + fJ[2:(K1 + 1), 1:K2] + fJ[2:(K1 + 1), 2:(K2 +
                1)])/4)/sum(b)
            fJ = fJ - fJ0
            x <- list(z1, z2)
            K <- c(K1, K2)
            image(x[[1]], x[[2]], fJ, xlab = paste("x_", J[1],
                " (", names(X)[J[1]], ")", sep = ""), ylab = paste("x_",
                J[2], " (", names(X)[J[2]], ")", sep = ""), xlim = range(z1),
                ylim = range(z2), xaxs = "i", yaxs = "i")
            contour(x[[1]], x[[2]], fJ, add = TRUE, drawlabels = TRUE)
            if (NA.plot == FALSE) {
                if (nrow(NA.ind) > 0) {
                  rect(xleft = z1[NA.ind[, 1]], ybottom = z2[NA.ind[,
                    2]], xright = z1[NA.ind[, 1] + 1], ytop = z2[NA.ind[,
                    2] + 1], col = "black")
                }
            }
        }
        else print("error:  class(X[,J[1]]) must be either factor or numeric/integer")
    }
    else print("error:  J must be a vector of length one or two")
    list(K = K, x.values = x, f.values = fJ)
}

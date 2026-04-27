## model mutable environmental varibles as functions of streamcat variables
## with random forest

## envmod4: rework to run with rfsrc

envmod <- function(ss0, runmod = F, doimport = F, doenvplots = F) {

    library(randomForestSRC)
    library(parallel)
    options(rf.cores = detectCores(), mc.cores = detectCores())

    ## envdata is the calibration data
    envdata <- rbind(ss0[[1]], ss0[[2]])

    ## predict to all test sites, here the same as calibration
    envdata.pred <- rbind(ss0[[1]], ss0[[2]])

    print(table(envdata$survey))

    varall <- attr(ss0[[1]], "varall")
    ## predictor variables are any variables that I'm not trying to predict
    ## (listed here in dlist)
    dlist <- c("anc.result.1", "chloride.result", "doc", "no3", "pct.safn",
               "ptl", "turb.result", "sulfate.result",
               "pct.fn", "ptl.diss")
    pvar <- varall[! varall %in% dlist]
    print(pvar)

    ## logtransforms
    varlist <- c("ptl.diss",  "turb.result", "chloride.result",
                 "sulfate.result", "anc.result.1")
    for (i in varlist) {
        incvec <- envdata[,i] <= 0
        incvec[is.na(incvec)] <- F
        minval <- min(envdata[!incvec,i], na.rm = T)
        envdata[incvec,i] <- minval*0.5
        envdata[, i] <- log(envdata[,i])
    }

    ## drop missing variables
    isna <- apply(envdata[, pvar], 1, function(x) any(is.na(x)))
    envdata <- envdata[! isna,]

    isna <- apply(envdata.pred[, pvar], 1, function(x) any(is.na(x)))
    envdata.pred <- envdata.pred[! isna,]

    ## select variables here to predict
    svar <- c("turb.result", "anc.result")
    svarlab <- c("Turbidity", "ANC")

    ## random forest model for environment
    envmod <- function(df1,y,predvar, dfout.ref = NULL) {
        formstr <- paste(y, "~", predvar[1])
        formstr <- paste(c(formstr, predvar[2:length(predvar)]), collapse = "+")

        ## drop samples missing env var of interest
        df1 <- df1[! is.na(df1[, y]),]
        if (y != "turb.result") {
            ## standard RF for all variables except for turb
            mod <- rfsrc(as.formula(formstr), data = df1,
                         ntree = 1000, importance = "permute")
            predout <- mod$predicted.oob
            pred.ref <- predict(mod, dfout.ref)$predicted
        }
        else {
            ## quantile regression for turb because I'm assuming that
            ## most observations are collected at higher than base flow
            ## and therefore contain some increases in turb due to flood
            mod <- quantreg(as.formula(formstr), data = df1,
                            newdata = dfout.ref,
                            ntree = 1000, importance = "permute", prob = 0.1)
            pred.ref <- as.vector(mod$quantreg$quantiles[1,])
        }

        imp <- mod$importance
        print(rev(sort(mod$importance))[1:20])

        ## partial dependence plots with land use
        ## can't do these with quantile regression, so not for turb
        if (y!= "turb.result") {
            varplot <- c("pctforest", "pctcrop2019ws", "pctimp2019ws")
            dev.new()
            par(mar = c(4,4,1,1), mfrow = c(1,3), mgp = c(2.3,1,0), bty = "l")
            for (ip in 1:length(varplot)) {
                pout <- ALEPlot_mod(df1, mod, yhat,
                                    J = which(varplot[ip] == names(df1)))
                plot(pout$x.values, pout$f.values, type = "l", xlab = varplot[ip])
            }
        }
        return(pred.ref)
    }

    ## envdata.ref: dial down human variables to zero
    var.to.zero <- c("pctcrop2019ws", "pctimp2019ws", "rdcrsws", "manurews",
                     "pcturbop2019ws", "coalminedensws", "inorgnwetdep.2008ws")
    envdata.ref <- envdata.pred
    for (i in var.to.zero) envdata.ref[, i] <- 0

    var.to.100 <- c("pctforest")
    for (i in var.to.100) envdata.ref[,i] <- 100

    envdata.ref.sav <- envdata.ref

    ## generate environmental predictions
    set.seed(10)
    for (i in 1:length(svar)) {
        print(svar[i])
        xout <- envmod(envdata, svar[i], pvar,
                       dfout.ref = envdata.ref[, c("uid", pvar)])
        envdata.ref.sav[, svar[i]] <- as.vector(xout)
    }

    return(envdata.ref.sav[, c("uid", svar)])

}

envdata.ref.sav <- envmod(ss.bugs)



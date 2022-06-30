data("OECDunemp")
u <- log(OECDunemp[,-8]/(100 - OECDunemp[,-8]))
R> colnames(u) <- colnames(OECDunemp)[-8]

R> my.u <- as.matrix(window(u, start = c(1979,4), end = c(2010,1)))
R> LLC <- purtest(my.u, test = "levinlin", exo = "intercept",
                  + lags = "AIC", pmax = 5)
R> summary(LLC)


data("OECDgdp")
X.GDP <- diff(log(OECDgdp[,-8]))
pCADF.X <- pCADFtest(Y=u, X=X.GDP, covariates=1:ncol(u),
                     type="drift", max.lag.y = 5, max.lag.X = 5, criterion = "AIC")

R> summary(pCADF.X)
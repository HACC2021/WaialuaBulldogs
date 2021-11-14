


estimated.model <- lm(lm(as.formula(paste(lside," ~ ",paste(row.names(lset$coefficients)[-1][order(tscrs)][-1],collapse=" + "),sep="")),testset))
lset <- summary(estimated.model) 

# Global Model Assessment
global.assessment <- summary(gvmodel(estimated.model))

# Multicollinearity
mc.vif <- vif(estimated.model)

# Residual Analysis
bp.test <-  bptest(estimated.model) # Breusch-Pagan test for homoskedasticity
bg.test <-  bgtest(estimated.model) # Breusch-Godfrey Test for higher order serial correlation

# Assessing Outliers
outlier.test <- outlierTest(estimated.model) # Bonferonni p-value for most extreme obs
leverage.plots <- leveragePlots(estimated.model) # leverage plots


# Assesing Normality of Residuals

# statstical tests
sw.test <- shapiro.test(estimated.model) # Shapiro-Wilk test for normality

# qq plot for studentized resid
qqPlot(estimated.model, main="QQ Plot")


# combine additional statistics
diagnostics      <- c(global.assessment, 
                      mc.vif, 
                      bp.test, 
                      bg.test, 
                      outlier.test, 
                      leverage.plots, 
                      sw.test, 
                      qqplot)

lset <- c(lset,diagnostics)
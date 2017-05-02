#load libraries
library(lavaan)
library(semPlot)
library(readr)
library(corrplot)
library(ggplot2)
library(haven)
library("qgraph", lib.loc="~/R/x86_64-redhat-linux-gnu-library/3.3")
library("semTools", lib.loc="~/R/x86_64-redhat-linux-gnu-library/3.3")
#sem library below seems to be returning an error when loading models into cfa()
#library("sem", lib.loc="~/R/x86_64-redhat-linux-gnu-library/3.3")


#import SPSS .sav file from homework 3
personality_for_analysis <- read_sav("~/Documents/classes/adv stats/Final project/Data/personality_for_analysis.sav")
View(personality_for_analysis)


##model for factors pulled from EFA in homework (the theory driving) (some syntax borrowed from Geoffrey Hubona, Ph.D. https://www.youtube.com/watch?v=1b7anpycZ_k)
myModel <- '
    focus =~ pers03 + pers18r + pers23r + pers28 + pers08r + pers13 + pers33 + pers38 + pers43r
    extroversion =~ pers21r + pers06r + pers31r + pers01 + pers36
    anxieties =~ pers09r + pers14 + pers24r + pers19 + pers34r + pers04 + pers39
    empathic =~ pers32 + pers22 + pers07 + pers42
'

##runnnig model through cfa, saving output to fit1
fit1 <- cfa(myModel, data = personality_for_analysis)

##reports model 1, no fit measures
summary(fit1, standardized = TRUE, rsquare = TRUE, fit.measures = TRUE)

##residual correlations
correl = residuals(fit1, type="cor")
correl
View(correl$cor)
zcorrel = residuals(fit1, type = "standardized")
View(zcorrel$cov)

##model2 loads variables onto factors, standardizing the factors to covariance
myModel2 <- '

    focus =~ pers03 + pers18r + pers23r + pers28 + pers08r + pers13 + pers33 + pers38 + pers43r
    extroversion =~ pers21r + pers06r + pers31r + pers01 + pers36
    anxieties =~ pers09r + pers14 + pers24r + pers19 + pers34r + pers04 + pers39
    empathic =~ pers32 + pers22 + pers07 + pers42
    focus ~~ 1*focus
    extroversion ~~ 1*extroversion
    anxieties ~~ 1*anxieties
    empathic ~~ 1*empathic
'

##this fit model standardizes the first variable to 1
fit2 <- cfa(myModel2, 
            data = personality_for_analysis,
            std.all = TRUE
            )
summary(fit2, fit.measures = TRUE) 

##this fit model does not standardize the 1st variable, resulting in higher comparative fit index
fit3 <- cfa(myModel2, 
            data = personality_for_analysis,
            std.lvl = TRUE
)
summary(fit3, fit.measures = TRUE)

##Moving to lavaan/SEM analysis, using raw data from homework3
##some syntax borrow from http://web.stanford.edu/class/psych253/section/section_8/section8.html

semPaths(fit1, what='std', 
         edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE,
         layout="circle", residuals=FALSE, nCharNodes= 6) 
title("CFA measurement model showing four factors (empathy, focus, anxieties, and \nextroversion, and related variables (model1-fit1)")

semPaths(fit1, what='std', 
         edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE,
         layout="circle", residuals=TRUE, nCharNodes= 6) 
title("CFA measurement model showing four factors (empathy, focus, anxieties, and \nextroversion, and related variables with residuals (model1-fit1)")

##skipping model2.fit2, since the comparative fit index was lower

#with residuals, not as easy to read
semPaths(fit3, what='std', 
         edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE,
         layout="circle", residuals=TRUE, nCharNodes= 6) 
title("CFA measurement model2 showing four factors (empathy, focus, anxieties, and \nextroversion, and related variables with residuals (model2-fit3)")


#no residuals; this will be used for the SEM visualization
semPaths(fit3, what='std', 
         edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE,
         layout="circle", residuals=FALSE, nCharNodes= 6) 
title("CFA measurement model2 showing four factors (empathy, focus, anxieties, and \nextroversion, and related variables (model2-fit3)")

##End of syntax for final project




##DISREGARD - saved syntax below for testing purposes

##import Factor score covariance matrix from SPSS (lower half only)
data.cor = lav_matrix_lower2full(c(.895, .000,	.753, -.016,	-.005,	.754, .019,	-.017,	.032,	.792, -.006,	.075,	.058,	.011,	.641))

rownames(data.cor) = colnames(data.cor) = c("deficiency","integration","rigor","ed-aspirations","confidence")

##
GMS_00_analysis <- read_sav("~/Documents/classes/adv stats/GMS_00_analysis.sav")
View(GMS_00_analysis)

##use this table to start factor loading
reproduced_correlations_table <- read_csv("~/Dropbox/reproduced_correlations_table.csv")
d_stan = as.data.frame(scale(reproduced_correlations_table))



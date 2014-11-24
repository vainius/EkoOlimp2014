library(data.table)
library(plyr)
library(gdata)
library(plm)
library(Hmisc)
library(reshape2)

MakeQuarter <- function(x){
  x <- paste(substr(x, 1, 4), "Q", (as.numeric(substr(x, 6, 7)) - 1) %/% 3 + 1, sep = "")
  return(x)
}

MakeShortName <- function(x){
  substr(gsub("\\.| |\\,|-", "", x), 1, 15)
}


EU.countries <- c("Belgium", "Bulgaria", "Czech Republic", "Denmark",
                    "Germany (until 1990 former territory of the FRG)",
                    "Estonia", "Ireland", "Greece", "France", "Croatia",
                    "Italy", "Latvia", "Lithuania", "Luxembourg", "Hungary",
                    "Malta", "Netherlands", "Austria", "Portugal", "Romania",
                    "Slovenia", "Slovakia", "Finland", "Sweden", "United Kingdom",
                    "Spain", "Cyprus", "Poland")


PanelDiff <- function(data, lag, vars, name = "gr", divide = TRUE) {
  vars.gr <- paste(vars, name, lag, sep = ".")
  nas <- matrix(NA, ncol = length(vars), nrow = lag)
  colnames(nas) <- vars
  
  ddply(as.data.frame(data), .(Country), .fun = function(x) {
    lagged <- rbind(nas, x[1:I( nrow(x) - lag ), vars, drop = F])
    y <- x[, vars, drop = F] - lagged
    if(divide == TRUE) y <- y / lagged
    colnames(y) <- vars.gr
    return(cbind(x,y))
  })
}


RunCountryRegression <- function(data = dt.cast){

  Co <- unique(data$Country)
a <- matrix(ncol = 2, nrow = length(Co))
Co[11] <- "Germany"
for(i in 1:length(Co)){
  test <- subset(data, Country == Co[i])
  result <- try(lm(y_AllitemsHICP.gr.4 ~ Lag(y_AllitemsHICP.gr.4, 1) + Eurodummy +
                     Lag(Unemploymentrat, 1) + Lag(log(Grossdomesticpr), 1) + 
                     Lag(Grossdomesticpr.gr.4, 1), data = test, na.action = na.omit), silent = T)
  
  if (class(result) != "try-error") {
    m <- lm(y_AllitemsHICP.gr.4 ~ Lag(y_AllitemsHICP.gr.4, 1) + Eurodummy +
              Lag(Unemploymentrat, 1) + Lag(log(Grossdomesticpr), 1) + 
              Lag(Grossdomesticpr.gr.4, 1), data = test, na.action = na.omit)
    result1 <- try(summary(m)$coefficients["Eurodummy",c(1,4)], silent = T)
    if (class(result1) != "try-error") {
      a[i,] <- summary(m)$coefficients["Eurodummy",c(1,4)]
    }
    
  }
}
rownames(a) = Co
a <- a[which(is.na(a[,1]) == FALSE),]

for(i in 1:length(a[,1])){
  if (a[i,2] <= 0.01) rownames(a)[i] <- paste(rownames(a)[i], "***", sep = "") else {
    if (a[i,2] <= 0.05) rownames(a)[i] <- paste(rownames(a)[i], "**", sep = "") else {
      if (a[i,2] <= 0.1) rownames(a)[i] <- paste(rownames(a)[i], "*", sep = "")
    }
  }
}

return(a)
}


CalcEuroEffects <- function(mod = mod1){
  mod.sum <- summary(mod)
  
  coef <- 100 * mod.sum$coefficients[2, 1]
  sd <- 100 * mod.sum$coefficients[2, 2]
  lr.multip <- 1/(1 - mod.sum$coefficients[1, 1])
  
  data.frame(Variable = substr(rownames(summary(mod)$coefficients)[1], 7, 22), 
             EuroEffectShort = round(coef, 3), 
             EuroEffectLong = round(coef * lr.multip, 3), 
             ShortUp = round(coef + sd, 3), 
             ShortDown = round(coef - sd, 3),
             LongUp = round((coef + sd) * lr.multip, 3),
             LongDown = round((coef - sd) * lr.multip, 3))
}
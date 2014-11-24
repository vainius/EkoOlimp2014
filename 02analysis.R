# source("01prepdata.R") 
source("00functions.R")

dt.cast <- readRDS(file = "processed_data/dt.cast.Rda")

#Run analysis for individual countries
# 

country.resuls <- RunCountryRegression(data = dt.cast)

#plot output 
par(las=3, mar=c(6,4.1,4.1,2.1))
plot(country.resuls[,1], xaxt="n", xlab = "", ylab= "Koeficientas", main = "Momentinis euro efektas")
abline(h = 0, col = 2)
axis(1, c(1:length(country.resuls[,1])), rownames(country.resuls))


# Run panel model for all countries
#

# Remove outliers
plot(dt.cast$y_AllitemsHICP.gr.4)
dt.cast$Country[which(dt.cast$y_AllitemsHICP.gr.4 > 0.2)]
dt.cast[which(dt.cast$y_AllitemsHICP.gr.4 > 0.2), ]$y_AllitemsHICP.gr.4 <- NA #Romania, Bulgaria
plot(dt.cast$y_AllitemsHICP.gr.4)

#Add avg inflation variable 
avg.inflation <- ddply(dt.cast, .(Time), function(x){
 mean(x$y_AllitemsHICP.gr.4, na.rm = T) 
})
dt.cast$AvgInflation <- avg.inflation$V1
# 

dt.panel.sub <- pdata.frame(dt.cast)
# 
#Run Arellano-Bond first model (full price index)

y.var <- "y_AllitemsHICP.gr.4"
formula.str <- paste0(y.var, " ~ lag(", y.var, ", 1) + Eurodummy + AvgInflation + lag(Unemploymentrat, 1) + lag(log(Grossdomesticpr), 1) + ",
                      "lag(Grossdomesticpr.gr.4, 1) | lag(", y.var, ", 2:8)")

mod1 <- pgmm(as.formula(formula.str),
             data = dt.panel.sub, effect = "individual", model = "onestep", collapse = F)

summary(mod1, robust=TRUE)

#Run full model for food and non-alc beverage prices

y.var <- "y_Foodandnonalc.gr.4"
formula.str <- paste0(y.var, " ~ lag(", y.var, ", 1) + Eurodummy + AvgInflation + lag(Unemploymentrat, 1) + lag(log(Grossdomesticpr), 1) + ",
                      "lag(Grossdomesticpr.gr.4, 1) | lag(", y.var, ", 2:8)")

mod2 <- pgmm(as.formula(formula.str),
             data = dt.panel.sub, effect = "individual", model = "onestep", collapse = F)

summary(mod2, robust=TRUE)


#Run for restaurants and hotels 

y.var <- "y_Restaurantsan.gr.4"
formula.str <- paste0(y.var, " ~ lag(", y.var, ", 1) + Eurodummy + AvgInflation + lag(Unemploymentrat, 1) + lag(log(Grossdomesticpr), 1) + ",
                      "lag(Grossdomesticpr.gr.4, 1) | lag(", y.var, ", 2:8)")

mod3 <- pgmm(as.formula(formula.str),
             data = dt.panel.sub, effect = "individual", model = "onestep", collapse = F)

summary(mod3, robust=TRUE)

#Present long-run and short-run effects
# 

final.effects <- rbind(CalcEuroEffects(mod1),
      CalcEuroEffects(mod2),
      CalcEuroEffects(mod3))

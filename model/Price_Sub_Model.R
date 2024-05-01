# Load required libraries
library(plyr)
library(dplyr)
library(tidyr)
library(nnfor)


# Prepare data for MODEL TS tools package
SUPER <- cbind(DATA$EEZID, DATA$Year, DATA$logPrice, DATA$Tonnes, DATA$Population_SSP1, DATA$GDP_SSP1, DATA$SeafoodConsumption_SSP1, DATA$Global_Tonnes)

# Remove outliers using running median
SUPER[, 3:8] <- apply(SUPER[, 3:8], 2, runmed, k = 3)

# Prepare training and testing data
end <- nrow(SUPER) - 2
year <- SUPER[1:end, 2]
Y <- SUPER[1:end, 3]
X <- SUPER[1:end, 4:8]
Y_new <- SUPER[end:nrow(SUPER), 3]
X_new <- SUPER[end:nrow(SUPER), 4:8]


##### ELM : Extreme learning machine
# Reference:
#For combination operators see: Kourentzes N., Barrow B.K., Crone S.F. (2015) Neural network ensemble operators for time series forecasting. Expert Systems with Applications, 41(9),
#4235-4244.
# For ELMs see: Huang G.B., Zhou H., Ding X. (2006) Extreme learning machine: theory and
#applications. Neurocomputing, 70(1), 489-501.

# Run Artificial Neural Network MODEL
fit <- elm.fast(Y, X, reps = 100, comb = "median")

# Prediction
PRED <- predict(fit, X_new)

# Prepare data for plotting
PR <- data.frame(Year = 1990:2010, Predicted_Price = PRED$Y.hat)
VAL <- data.frame(Year = 1990:2010, Observed_Price = Y_new)

# Plot observed and predicted
plot(PR$Year, PR$Predicted_Price, type = "l", col = "red", xlab = "Year", ylab = "Price", main = "Observed vs Predicted Price")
lines(VAL$Year, VAL$Observed_Price, col = "blue")
legend("topright", legend = c("Predicted", "Observed"), col = c("red", "blue"), lty = 1)

# Rename variables
names(SUPER) <- c("EEZID", "Year", "logPrice", "Tonnes", "Population_SSP1", "GDP_SSP1", "SeafoodConsumption_SSP1", "Global_Tonnes")

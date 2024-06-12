For visualization purpose of the data, I have tried to plot each of the time series data, I could infer that the is a non stationary time series data


## Plot Original Data

```r
# Load necessary libraries
library(HDGCvar)
library(igraph)

# Load your data
data <- read.csv("C:/Users/muham/Desktop/Time series data.csv")

# Extract the year column for the x-axis
years <- data$year

# Remove the date, year, and quarter columns for simplicity
data <- data[ , !names(data) %in% c("date", "year", "quarter")]

# Convert data to matrix for plotting
data_matrix <- as.matrix(data)

# Plot the time series with the year on the x-axis
matplot(years, data_matrix, type = "l", lty = 1, col = 1:ncol(data_matrix), xlab = "Year", ylab = "Values", main = "Time Series Data")

# Add legend
legend("topright", legend = colnames(data), col = 1:ncol(data_matrix), lty = 1, cex = 0.8)

```

## Normalize and Plot Data

```r
# Load necessary libraries
library(HDGCvar)
library(igraph)

# Load your data
data <- read.csv("C:/Users/muham/Desktop/Time series data.csv")

# Extract the year column for the x-axis
years <- data$year

# Remove the date, year, and quarter columns for simplicity
data <- data[ , !names(data) %in% c("date", "year", "quarter")]

# Normalize the data to make it more comparable
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
data_normalized <- as.data.frame(lapply(data, normalize))

# Convert normalized data to matrix for plotting
data_matrix <- as.matrix(data_normalized)

# Plot the normalized time series with the year on the x-axis
matplot(years, data_matrix, type = "l", lty = 1, col = 1:ncol(data_matrix), xlab = "Year", ylab = "Normalized Values", main = "Normalized Time Series Data")

# Add legend
legend("topright", legend = colnames(data), col = 1:ncol(data_matrix), lty = 1, cex = 0.8)

```

## Plot Separated Normalized Data

```r
# Load necessary libraries
library(HDGCvar)
library(igraph)

# Load your data
data <- read.csv("C:/Users/muham/Desktop/Time series data.csv")

# Extract the year column for the x-axis
years <- data$year

# Remove the date, year, and quarter columns for simplicity
data <- data[ , !names(data) %in% c("date", "year", "quarter")]

# Normalize the data to make it more comparable
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
data_normalized <- as.data.frame(lapply(data, normalize))

# Convert normalized data to matrix for plotting
data_matrix <- as.matrix(data_normalized)

# Split the data for plotting
PE_index <- data_matrix[, "PE_index"]
rest1 <- data_matrix[, c("VC_index", "Bond10", "SP500", "GSCI", "HFRI")]
rest2 <- data_matrix[, c("NFCI", "PMI", "PE_r", "VC_r", "Bond10_r")]
rest3 <- data_matrix[, c("SP500_r", "GSCI_r", "HFRI_r", "NFCI_r", "PMI_r")]

# Set up the plotting area to have 4 plots in a 2x2 grid
par(mfrow=c(2, 2))

# Plot PE_index
plot(years, PE_index, type = "l", col = "blue", xlab = "Year", ylab = "Normalized Values", main = "PE_index")

# Plot rest1
matplot(years, rest1, type = "l", lty = 1, col = 1:ncol(rest1), xlab = "Year", ylab = "Normalized Values", main = "VC_index, Bond10, SP500, GSCI, HFRI")
legend("topright", legend = colnames(rest1), col = 1:ncol(rest1), lty = 1, cex = 0.8)

# Plot rest2
matplot(years, rest2, type = "l", lty = 1, col = 1:ncol(rest2), xlab = "Year", ylab = "Normalized Values", main = "NFCI, PMI, PE_r, VC_r, Bond10_r")
legend("topright", legend = colnames(rest2), col = 1:ncol(rest2), lty = 1, cex = 0.8)

# Plot rest3
matplot(years, rest3, type = "l", lty = 1, col = 1:ncol(rest3), xlab = "Year", ylab = "Normalized Values", main = "SP500_r, GSCI_r, HFRI_r, NFCI_r, PMI_r")
legend("topright", legend = colnames(rest3), col = 1:ncol(rest3), lty = 1, cex = 0.8)

```

## Conclusion

The plots above clearly visualize the time series data, both in their original and normalized forms. Each variable is also displayed in separate graphs to avoid scaling issues and improve readability.

## Example Plots

### Original Data Plot

![Original Data Plot](https://github.com/kgmikhdad/HDGCvar/blob/kgmikhdad-files/Rplot01.png)

### Normalized Data Plot

![Normalized Data Plot](https://github.com/kgmikhdad/HDGCvar/blob/kgmikhdad-files/Rplot02.png)

### Separated Normalized Data Plots

![Separated Normalized Data Plot 3](https://github.com/kgmikhdad/HDGCvar/blob/kgmikhdad-files/Rplot03.png)







```r
# Load necessary libraries
library(HDGCvar)
library(igraph)

# Load your data (assuming your data is saved as 'time_series_data.csv')
data <- read.csv("C:/Users/muham/Desktop/Time series data.csv")

# Set the dependent variable and the dataset
dependent_variable <- 'PE_index'
independent_variables <- c('VC_index', 'Bond10', 'SP500', 'GSCI', 'HFRI', 'NFCI', 'PMI', 'PE_r', 'VC_r', 'Bond10_r', 'SP500_r', 'GSCI_r', 'HFRI_r', 'NFCI_r', 'PMI_r')

# Select the lag length
selected_lag <- lags_upbound_BIC(data[, c(dependent_variable, independent_variables)], p_max = 10)
print(selected_lag)

# Prepare the list of interest variables
interest_variables <- lapply(independent_variables, function(var) {
  list(GCto = dependent_variable, GCfrom = var)
})

# Test for Granger causality for each variable
results <- lapply(interest_variables, function(pair) {
  HDGC_VAR(GCpair = pair, data = data[, c(dependent_variable, pair$GCfrom)], p = selected_lag, d = 2, bound = 0.5 * nrow(data), parallel = TRUE)
})

# Print results
print(results)

# Optional: Estimate the full network of causality and plot the estimated network
network <- HDGC_VAR_all(data[, c(dependent_variable, independent_variables)], p = selected_lag, d = 2, bound = 0.5 * nrow(data), parallel = TRUE)
Plot_GC_all(network, Stat_type = "FS_cor", alpha = 0.01, multip_corr = list(FALSE), directed = TRUE, layout = layout.circle, main = "Network", edge.arrow.size = .2, vertex.size = 5, vertex.color = c("lightblue"), vertex.frame.color = "blue", vertex.label.size = 2, vertex.label.color = "black", vertex.label.cex = 0.6, vertex.label.dist = 1, edge.curved = 0, cluster = list(TRUE, 5, "black", 0.8, 1, 0))


```



"We can alter the significance level from 1% to a different level. We can manually choose the lag. I am attaching plots of different networks that I have generated based on the following changes, such as different values of alpha and different p values."

![Original Data Plot](https://github.com/kgmikhdad/HDGCvar/blob/kgmikhdad-files/Rplot04.png)
![Original Data Plot](https://github.com/kgmikhdad/HDGCvar/blob/kgmikhdad-files/Rplot05.png)
![Original Data Plot](https://github.com/kgmikhdad/HDGCvar/blob/kgmikhdad-files/Rplot06.png)
![Original Data Plot](https://github.com/kgmikhdad/HDGCvar/blob/kgmikhdad-files/Rplot07.png)
![Original Data Plot](https://github.com/kgmikhdad/HDGCvar/blob/kgmikhdad-files/Rplot08.png)
![Original Data Plot](https://github.com/kgmikhdad/HDGCvar/blob/kgmikhdad-files/Rplot09.png)
![Original Data Plot](https://github.com/kgmikhdad/HDGCvar/blob/kgmikhdad-files/Rplot10.png)
![Original Data Plot](https://github.com/kgmikhdad/HDGCvar/blob/kgmikhdad-files/Rplot11.png)
![Original Data Plot](https://github.com/kgmikhdad/HDGCvar/blob/kgmikhdad-files/Rplot12.png)


For visualisation purpose of the data lets draw the time series plot of the data
```markdown
# Time Series Data Analysis

This repository contains the analysis of time series data, including plotting the original data, normalizing the data, and creating separate plots for better visualization.

## Load Necessary Libraries

```r
# Load necessary libraries
library(HDGCvar)
library(igraph)
```

## Load Data

```r
# Load your data
data <- read.csv("C:/Users/muham/Desktop/Time series data.csv")

# Extract the year column for the x-axis
years <- data$year

# Remove the date, year, and quarter columns for simplicity
data <- data[ , !names(data) %in% c("date", "year", "quarter")]
```

## Plot Original Data

```r
# Convert data to matrix for plotting
data_matrix <- as.matrix(data)

# Plot the original time series with the year on the x-axis
matplot(years, data_matrix, type = "l", lty = 1, col = 1:ncol(data_matrix), xlab = "Year", ylab = "Values", main = "Time Series Data")

# Add legend
legend("topright", legend = colnames(data), col = 1:ncol(data_matrix), lty = 1, cex = 0.8)
```

## Normalize and Plot Data

```r
# Normalize the data to make it more comparable
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
data_normalized <- as.data.frame(lapply(data, normalize))

# Convert normalized data to matrix for plotting
data_matrix_normalized <- as.matrix(data_normalized)

# Plot the normalized time series with the year on the x-axis
matplot(years, data_matrix_normalized, type = "l", lty = 1, col = 1:ncol(data_matrix_normalized), xlab = "Year", ylab = "Normalized Values", main = "Normalized Time Series Data")

# Add legend
legend("topright", legend = colnames(data), col = 1:ncol(data_matrix_normalized), lty = 1, cex = 0.8)
```

## Plot Separated Normalized Data

```r
# Split the normalized data for plotting
PE_index <- data_matrix_normalized[, "PE_index"]
rest1 <- data_matrix_normalized[, c("VC_index", "Bond10", "SP500", "GSCI", "HFRI")]
rest2 <- data_matrix_normalized[, c("NFCI", "PMI", "PE_r", "VC_r", "Bond10_r")]
rest3 <- data_matrix_normalized[, c("SP500_r", "GSCI_r", "HFRI_r", "NFCI_r", "PMI_r")]

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

The plots above provide a clear visualization of the time series data, both in their original and normalized forms. Each variable is also displayed in separate graphs to avoid scaling issues and improve readability.

## Example Plots

### Original Data Plot

![Original Data Plot](https://github.com/kgmikhdad/HDGCvar/blob/kgmikhdad-files/Rplot01.png)

### Normalized Data Plot

![Normalized Data Plot](https://github.com/kgmikhdad/HDGCvar/blob/kgmikhdad-files/Rplot02.png)

### Separated Normalized Data Plots

![Separated Normalized Data Plot 3](https://github.com/kgmikhdad/HDGCvar/blob/kgmikhdad-files/Rplot03.png)

Replace `path_to_original_plot.png`, `path_to_normalized_plot.png`, `path_to_separated_plot1.png`, `path_to_separated_plot2.png`, and `path_to_separated_plot3.png` with the actual paths to your plot images in the repository.

### How to Run

1. **Clone the repository**:
   ```sh
   git clone https://github.com/yourusername/yourrepository.git
   ```

2. **Run the R script**:
   Open the R script file in RStudio and run the code to generate the plots.

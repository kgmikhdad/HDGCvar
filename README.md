Sure, I will provide you with the structured R code along with an explanation and the corresponding GitHub Markdown writeup. You can directly copy and paste this into your GitHub repository.

### R Code

Here's the R code to generate the three plots:

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

### GitHub Markdown Writeup

Here is the structured writeup for your GitHub repository:

```markdown
# Time Series Data Analysis

This repository contains the analysis of time series data, including normalization and plotting of different variables. The data is divided into multiple plots for better visualization.

## Load Necessary Libraries

```r
# Load necessary libraries
library(HDGCvar)
library(igraph)
```

## Load and Normalize Data

```r
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
```

## Split Data and Plot

```r
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

The plots above provide a clear visualization of the normalized time series data. Each variable is displayed in a separate graph to avoid scaling issues and improve readability.

## Example Plots

![Plot1](path_to_plot1.png)
![Plot2](path_to_plot2.png)
![Plot3](path_to_plot3.png)

Replace `path_to_plot1.png`, `path_to_plot2.png`, and `path_to_plot3.png` with the actual paths to your plot images in the repository.

### How to Run

1. **Clone the repository**:
   ```sh
   git clone https://github.com/yourusername/yourrepository.git
   ```

2. **Run the R script**:
   Open the R script file in RStudio and run the code to generate the plots.

Feel free to update the paths to the images and the repository information accordingly. This writeup provides a structured overview of the code and the analysis for your professor.

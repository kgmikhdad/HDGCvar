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






Here is a more visually appealing representation of the output, with clear distinctions between the different test results and selections:

---

### Test Results

#### Test 1
- **Asymp**: 
  - LM_stat: 1.8140219 
  - p_value: 0.4037292
- **FS_cor**: 
  - LM_stat: 0.8769226 
  - p_value: 0.4186671
- **Selections**: 
  - PE_index l1: TRUE 
  - PE_index l2: TRUE

---

#### Test 2
- **Asymp**: 
  - LM_stat: 4.4698064 
  - p_value: 0.1070025
- **FS_cor**: 
  - LM_stat: 2.2072190 
  - p_value: 0.1143806
- **Selections**: 
  - PE_index l1: TRUE 
  - PE_index l2: TRUE

---

#### Test 3
- **Asymp**: 
  - LM_stat: 1.0942569 
  - p_value: 0.5786089
- **FS_cor**: 
  - LM_stat: 0.5259783 
  - p_value: 0.5923109
- **Selections**: 
  - PE_index l1: TRUE 
  - PE_index l2: TRUE

---

#### Test 4
- **Asymp**: 
  - LM_stat: 4.1442320 
  - p_value: 0.1259191
- **FS_cor**: 
  - LM_stat: 2.0410689 
  - p_value: 0.1343019
- **Selections**: 
  - PE_index l1: TRUE 
  - PE_index l2: TRUE

---

#### Test 5
- **Asymp**: 
  - LM_stat: 0.9944839 
  - p_value: 0.6082058
- **FS_cor**: 
  - LM_stat: 0.4776448 
  - p_value: 0.6213974
- **Selections**: 
  - PE_index l1: TRUE 
  - PE_index l2: TRUE

---

#### Test 6
- **Asymp**: 
  - LM_stat: 0.1920133 
  - p_value: 0.9084580
- **FS_cor**: 
  - LM_stat: 0.09164383 
  - p_value: 0.91249282
- **Selections**: 
  - PE_index l1: TRUE 
  - PE_index l2: TRUE

---

#### Test 7
- **Asymp**: 
  - LM_stat: 0.4494550 
  - p_value: 0.7987338
- **FS_cor**: 
  - LM_stat: 0.2149482 
  - p_value: 0.8068881
- **Selections**: 
  - PE_index l1: TRUE 
  - PE_index l2: TRUE

---

#### Test 8
- **Asymp**: 
  - LM_stat: 0.4082490 
  - p_value: 0.8153609
- **FS_cor**: 
  - LM_stat: 0.1951787 
  - p_value: 0.8229440
- **Selections**: 
  - PE_index l1: TRUE 
  - PE_index l2: TRUE

---

#### Test 9
- **Asymp**: 
  - LM_stat: 0.1961634 
  - p_value: 0.9065748
- **FS_cor**: 
  - LM_stat: 0.0936276 
  - p_value: 0.9106872
- **Selections**: 
  - PE_index l1: TRUE 
  - PE_index l2: TRUE

---

#### Test 10
- **Asymp**: 
  - LM_stat: 2.1276638 
  - p_value: 0.3451308
- **FS_cor**: 
  - LM_stat: 1.0311042 
  - p_value: 0.3596994
- **Selections**: 
  - PE_index l1: TRUE 
  - PE_index l2: TRUE

---

#### Test 11
- **Asymp**: 
  - LM_stat: 0.1007208 
  - p_value: 0.9508866
- **FS_cor**: 
  - LM_stat: 0.04803757 
  - p_value: 0.95311599
- **Selections**: 
  - PE_index l1: TRUE 
  - PE_index l2: TRUE

---

#### Test 12
- **Asymp**: 
  - LM_stat: 6.78940185 
  - p_value: 0.03355059
- **FS_cor**: 
  - LM_stat: 3.41680941 
  - p_value: 0.03598886
- **Selections**: 
  - PE_index l1: TRUE 
  - PE_index l2: TRUE

---

#### Test 13
- **Asymp**: 
  - LM_stat: 1.2621274 
  - p_value: 0.5320256
- **FS_cor**: 
  - LM_stat: 0.6074725 
  - p_value: 0.5463652
- **Selections**: 
  - PE_index l1: TRUE 
  - PE_index l2: TRUE

---

#### Test 14
- **Asymp**: 
  - LM_stat: 0.1094028 
  - p_value: 0.9467678
- **FS_cor**: 
  - LM_stat: 0.05218186 
  - p_value: 0.94917741
- **Selections**: 
  - PE_index l1: TRUE 
  - PE_index l2: TRUE

---

#### Test 15
- **Asymp**: 
  - LM_stat: 0.3831062 
  - p_value: 0.8256758
- **FS_cor**: 
  - LM_stat: 0.1831221 
  - p_value: 0.8328949
- **Selections**: 
  - PE_index l1: TRUE 
  - PE_index l2: TRUE

---

Let me know if you need any additional formatting or information!


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
![Original Data Plot](https://github.com/kgmikhdad/HDGCvar/blob/kgmikhdad-files/Rplot13.png)


### Potential Modifications and Their Implications

#### 1. Changing the Lag Length (p)

**Description:** The lag length (p) determines how many past values of the variables are included in the model. The `lags_upbound_BIC` function is used to select an optimal lag length based on the Bayesian Information Criterion (BIC).

**Implications:**
- **Increasing p:** Including more lags can capture more historical dependencies, potentially improving model accuracy but increasing complexity and computational cost. This may help in better modeling the dynamics but could lead to overfitting, especially with limited data.
- **Decreasing p:** Reducing the number of lags simplifies the model and decreases computational burden, but it may miss important historical dependencies, leading to poorer model performance.

**Example Adjustment:**
```r
selected_lag <- 5  # Manually setting p to 5
```

#### 2. Changing the Augmentation Parameter (d)

**Description:** The parameter d accounts for the potential non-stationarity in the data. It is the maximum order of integration suspected in the time series.

**Implications:**
- **Increasing d:** Handling higher levels of integration (e.g., I(2) processes) reduces the risk of spurious regression but increases the model complexity and computational load. This is useful if there are strong indications of higher-order integration.
- **Decreasing d:** Assumes lower levels of integration (e.g., I(1) processes), simplifying the model but risking inaccurate results if higher-order integration is present.

**Example Adjustment:**
```r
results <- lapply(interest_variables, function(pair) {
  HDGC_VAR(GCpair = pair, data = data[, c(dependent_variable, pair$GCfrom)], p = selected_lag, d = 1, bound = 0.5 * nrow(data), parallel = TRUE)
})
```

#### 3. Changing the Bound Parameter

**Description:** The `bound` parameter controls the lower bound on the penalty parameter of the lasso, affecting the number of variables selected.

**Implications:**
- **Increasing bound (> 0.5 * nrow(data)):** Makes the lasso more restrictive, selecting fewer variables. This reduces overfitting but may miss important predictors, increasing type II error.
- **Decreasing bound (< 0.5 * nrow(data)):** Makes the lasso less restrictive, selecting more variables. This can capture more potential relationships but increases the risk of overfitting and type I error.

**Example Adjustment:**
```r
results <- lapply(interest_variables, function(pair) {
  HDGC_VAR(GCpair = pair, data = data[, c(dependent_variable, pair$GCfrom)], p = selected_lag, d = 2, bound = 0.3 * nrow(data), parallel = TRUE)
})
```

#### 4. Changing the Alpha Value

**Description:** The `alpha` parameter in the `Plot_GC_all` function sets the significance level for the Granger causality tests.

**Implications:**
- **Increasing alpha (> 0.01):** Allows for a higher type I error rate, which means more false positives. This is more lenient and can detect more causal relationships but increases the risk of detecting spurious causality.
- **Decreasing alpha (< 0.01):** Stricter significance level reduces type I error, leading to fewer false positives but may increase type II error, potentially missing true causal relationships.

**Example Adjustment:**
```r
Plot_GC_all(network, Stat_type = "FS_cor", alpha = 0.05, multip_corr = list(FALSE), directed = TRUE, layout = layout.circle, main = "Network", edge.arrow.size = .2, vertex.size = 5, vertex.color = c("lightblue"), vertex.frame.color = "blue", vertex.label.size = 2, vertex.label.color = "black", vertex.label.cex = 0.6, vertex.label.dist = 1, edge.curved = 0, cluster = list(TRUE, 5, "black", 0.8, 1, 0))
```

### Interpretation of Changes

#### Lag Length (p)
- **Higher p:** Captures more historical data, potentially improving model performance if the additional lags contain valuable information. However, it increases model complexity, risk of overfitting, and computational cost.
- **Lower p:** Simplifies the model, reducing computational cost and overfitting risk, but may fail to capture important temporal dependencies, leading to poorer performance.

#### Augmentation Parameter (d)
- **Higher d:** Ensures robustness against higher-order integration and avoids spurious regression. This is important if the data is suspected to be non-stationary at higher levels but comes at the cost of increased complexity.
- **Lower d:** Assumes simpler dynamics (e.g., I(0) or I(1)), reducing model complexity. This is suitable if there is confidence that the series are stationary or first-order integrated but risks inaccurate results if higher-order integration is present.

#### Bound Parameter
- **Higher bound:** Reduces overfitting by selecting fewer variables. This is beneficial for high-dimensional data but may exclude relevant predictors, increasing type II error.
- **Lower bound:** Includes more variables, capturing a wider range of potential causal relationships. This is useful if there are many relevant predictors but increases the risk of overfitting and type I error.

#### Alpha Value
- **Higher alpha:** More lenient, allowing more causal relationships to be detected. This can be useful in exploratory analyses but increases the risk of false positives (type I error).
- **Lower alpha:** Stricter, reducing the likelihood of false positives. This is important for confirmatory analyses but may miss true causal relationships (type II error).

### Conclusion

Optimizing the Granger causality analysis involves balancing model complexity, computational efficiency, and statistical robustness. By adjusting parameters such as lag length, augmentation, bound, and alpha, you can tailor the model to better fit the characteristics of your data and research objectives. It is essential to experiment with these parameters and validate the model's performance through robust statistical testing and domain-specific knowledge.














































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
  HDGC_VAR(GCpair = pair, data = data[, c(dependent_variable, pair$GCfrom)], p = selected_lag, d = 2, bound = 0.5 * nrow(data), parallel = TRUE, n_cores = 4)
})

# Print results
print(results)

# Optional: Estimate the full network of causality and plot the estimated network with multiple testing correction
network <- HDGC_VAR_all(data[, c(dependent_variable, independent_variables)], p = selected_lag, d = 2, bound = 0.5 * nrow(data), parallel = TRUE, n_cores = 4)
Plot_GC_all(network, Stat_type = "FS_cor", alpha = 0.01, multip_corr = list(TRUE, method = "BH"), directed = TRUE, layout = layout.circle, main = "Network", edge.arrow.size = .2, vertex.size = 5, vertex.color = c("lightblue"), vertex.frame.color = "blue", vertex.label.size = 2, vertex.label.color = "black", vertex.label.cex = 0.6, vertex.label.dist = 1, edge.curved = 0, cluster = list(TRUE, 5, "black", 0.8, 1, 0))
```
![Original Data Plot](https://github.com/kgmikhdad/HDGCvar/blob/kgmikhdad-files/Rplot14.png)
![Original Data Plot](https://github.com/kgmikhdad/HDGCvar/blob/kgmikhdad-files/Rplot15.png)

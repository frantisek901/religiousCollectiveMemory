### Loading data
library(qmethod)
data(lipset)
lipset[[1]] # Shows the dataset, a matrix of 33x9
lipset[[2]] # Shows the text of the 33 statements

### Doing and reporting FA (rotated) of Q-sorts
results <- qmethod(lipset[[1]], nfactors = 3, rotation = "varimax")
summary(results)

### Merge the statements with their actual text:
scores <- cbind(results$zsc_n, lipset[[2]])
scores

### Order the results by the scores of each factor:
for (i in 1:length(results$loa)) {
  View(scores[order(scores[i], decreasing = TRUE), ],
       title = paste0("Order for f", i))
}

### Plotting results
par(lwd = 1.5, mar = c(4, 4, 0, 0) + 0.1)
plot(results)
abline(h = seq(from = 2, to = 32, by = 3), col = grey(0.2), lty = 2)

### Data frame of distinguishing and consensus statements:
format(results$qdc, digits = 1, nsmall = 2)

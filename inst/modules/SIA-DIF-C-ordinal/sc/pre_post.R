# load libraries
library(ShinyItemAnalysis)
library(difNLR)
library(ggplot2)

# prepare data
data <- AttitudesExpulsion[, c(paste0("PostMacro_0", 1:7))]
group <- as.numeric(AttitudesExpulsion[, "Group"] == "E")
score <- AttitudesExpulsion$PreMacro # DIF matching score

# DIF-C with cumulative logit regression model
(fit <- difORD(
  Data = data, group = group, focal.name = 1, model = "cumulative",
  type = "both", match = score, p.adjust.method = "BH", purify = FALSE,
  parametrization = "classic"
))

# plot cumulative probabilities for item X2003
plot(fit, item = "PostMacro_04", plot.type = "cumulative")

# plot category probabilities for item X2003
plot(fit, item = "PostMacro_04", plot.type = "category")

# estimate coefficients for all items with SE
coef(fit, SE = TRUE)

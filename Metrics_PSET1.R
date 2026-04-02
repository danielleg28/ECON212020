# ECON 21020 PSET 1
# Danielle Guo
# 4/1/2026

library(ggplot2)
library(ggthemes)
rm(list = ls())

# QUESTION 10:
  # For the discrete joint distribution of (X, Y) given by 
  # X ∈ {−5, −4.9, . − 4.8, ..., 4.8, 4.9, 5}, Y ∈ {−2, −1.9, . − 1.8, ..., 1.8, 1.9, 2}. 
  # There are 101 possible values for X, and 41 for Y. The probability of point (x_i, y_j ) 
  # (corresponding to the i and jth elements of X and Y respectively)
  # p(x_i, y_j) = (i + j)/(sum(outer(1:101, 1:41, "+")))

### (A) Plot the marginal distributions of X and Y ###

#Assigning x and y R.V.s & creating lists to hold the values of the marginal distributions of X and Y.
xvalues = seq(-5, 5, length.out = 101)
yvalues = seq(-2, 2, length.out = 41)
ymargdist <- list()
xmargdist <- list()

#For each i, summing over all j to get the marginal distribution of x.
for (i in 1:101) {
  temp <- 0
  for (j in 1:41) {
    temp <- temp + ((i + j)/(sum(outer(1:101, 1:41, "+"))))
  }
  xmargdist[[i]] <- temp
}

#For each j, summing over all i to get the marginal distribution of Y.
for (j in 1:41) {
  temp <- 0
  for (i in 1:101) {
    temp <- temp + ((i + j)/(sum(outer(1:101, 1:41, "+"))))
  }
  ymargdist[[j]] <- temp
}

#Verifying that the probabilities sum to 1 for the marginal distributions. 
sum_x = 0
for (i in 1:101){
  sum_x <- sum_x + xmargdist[[i]]
}

sum_y = 0
for (j in 1:41){
  sum_y <- sum_y + ymargdist[[j]]
}

#Plotting marginal distribution of X.
df_x <- data.frame(
  x_x = xvalues,
  y_x = unlist(xmargdist)
)
ggplot(df_x, aes(x = x_x, y = y_x)) +
  geom_bar(stat = "identity", fill = "slateblue2") +
  labs(title = "Marginal Distribution of X", x = "X", y = "Probability") +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey90"),
    text = element_text(family = "mono"),
    plot.title = element_text(family = "mono", size = 14, hjust = 0.5),
    axis.title = element_text(family = "mono", size = 12),
    axis.text = element_text(family = "mono", size = 10))

#Plotting marginal distribution of Y.
df_y <- data.frame(
  x_y = yvalues,
  y_y = unlist(ymargdist)
)
ggplot(df_y, aes(x = x_y, y = y_y)) +
  geom_bar(stat = "identity", fill = "plum2") +
  labs(title = "Marginal Distribution of Y", x = "Y", y = "Probability") +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey90"),
    text = element_text(family = "mono"),
    plot.title = element_text(family = "mono", size = 14, hjust = 0.5),
    axis.title = element_text(family = "mono", size = 12),
    axis.text = element_text(family = "mono", size = 10))

###(B) On the same figure, plot two conditional distributions when Y = −2 and Y = 2.###

#For each i, determining conditional probability given that Y = -2.
condist_yneg2 <- list()
for (i in 1:101) {
  temp_c1 = 0
  j = 1
  temp_c1 <- temp_c1 + ((i + j)/(sum(outer(1:101, 1:41, "+"))))/ymargdist[[j]]
  condist_yneg2[[i]] <- temp_c1
}

#For each i, determining conditional probability given that Y = 2
condist_y2 <- list()
for (i in 1:101) {
  temp_c2 = 0
  j = 41
  temp_c2 <- temp_c2 + ((i + j)/(sum(outer(1:101, 1:41, "+"))))/ymargdist[[j]]
  condist_y2[[i]] <- temp_c2
}

#Verifying that the probabilities sum to 1 for the conditional distributions. 
sum_cx = 0
for (i in 1:101){
  sum_cx <- sum_cx + condist_y2[[i]]
}

sum_cy = 0
for (i in 1:101){
  sum_cy <- sum_cy + condist_yneg2[[i]]
}

#Plotting conditional distributions of X|Y = -2 and X|Y = 2.
df_yneg2 <- data.frame(
  x_neg2 = xvalues,
  y_neg2 = unlist(condist_yneg2)
)

df_y2 <- data.frame(
  x_2 = xvalues,
  y_2 = unlist(condist_y2)
)

ggplot() +
  geom_bar(data = df_yneg2, aes(x = x_neg2, y = y_neg2, fill = "(X|Y = -2)"), stat = "identity", alpha = 0.7) +
  geom_bar(data = df_y2, aes(x = x_2, y = y_2, fill = "(X|Y = 2)"), stat = "identity", alpha = 0.7) +
  scale_fill_manual(values = c("(X|Y = -2)" = "deepskyblue", "(X|Y = 2)" = "lightgreen")) +
  labs(title = "Conditional Distributions of X|Y = -2 and X|Y = 2", x = "X", y = "Pr(X | Y = y)", fill = "Distribution") +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey90"),
    legend.background = element_rect(color = "black"),
    legend.position = c(0.2, 0.85),
    text = element_text(family = "mono"), 
    plot.title = element_text(family = "mono", size = 14),
    axis.title = element_text(family = "mono", size = 12),
    axis.text = element_text(family = "mono", size = 10))

library(ggplot2)

set.seed(12345)
population_a <- rnorm(1000, mean = 50, sd = 10)
population_b <- rnorm(1000, mean = 50, sd = 10)

initial_n <- 10
sample_a <- sample(population_a, size = initial_n, replace = FALSE)
sample_b <- sample(population_b, size = initial_n, replace = FALSE)

df <- data.frame(
  group = rep(c("A", "B"), each = initial_n),
  value = c(sample_a, sample_b)
)

ggplot(df, aes(x = group, y = value)) +
  geom_boxplot() +
  labs(x = "Group", y = "Value") +
  theme_minimal()

res <- t.test(sample_a, sample_b)
pvals <- res$p.value

# simulating adding 1 sample at a time and checking the p-value

for (i in 1:200) {
  new_sample_a <- sample(population_a, size = 1, replace = FALSE)
  new_sample_b <- sample(population_b, size = 1, replace = FALSE)
  
  sample_a <- c(sample_a, new_sample_a)
  sample_b <- c(sample_b, new_sample_b)
  
  res <- t.test(sample_a, sample_b)
  pvals <- c(pvals, res$p.value)

  if (res$p.value < 0.05) {
    break
  }
}

plot(initial_n:(initial_n + i), pvals, t="b", pch=20, ylim = c(0, 1))
abline(h = 0.05, col = "red", lty = 2)

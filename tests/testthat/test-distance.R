set.seed(123)
## Standard scenario
n = 200
x = rnorm(n)
y = x + rnorm(n)
plot(x, y)
abline(a = 0, b = 1, col = "red")
CPOP::identityDist(x, y)

## Bias scenario
z = y - 1
plot(x, z)
abline(a = 0, b = 1, col = "red")
CPOP::identityDist(x, z)

mse = function(x, y){
  ## Assuming x is the truth
  var(y) + mean(y - x)^2
}
mse(x, y)
mse(x, z)

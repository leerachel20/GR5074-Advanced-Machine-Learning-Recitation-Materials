aggregate(cbind(n.cases = Sepal.Length) ~ Species, iris, length)

set.seed(1)
iris.setosa <- iris[iris$Species == 'setosa',]
iris.versicolor <- iris[iris$Species == 'versicolor',]
iris.virginica <- iris[iris$Species == 'virginica',]

split_data <- function(N, p) {
  stopifnot(p > 0 & p < 1)
  n <- N * p
  trn.index <- sample.int(N, n, replace = FALSE)
  test.index <- setdiff(1:N, trn.index)
  list(train = trn.index, test = test.index)
}

setosa.split <- split_data(nrow(iris.setosa), 0.2)
setosa.train <- iris.setosa[setosa.split[["train"]],]
setosa.test  <- iris.setosa[setosa.split[["test"]],]

versicolor.split <- split_data(nrow(iris.versicolor), 0.2)
versicolor.train <- iris.versicolor[versicolor.split[["train"]],]
versicolor.test  <- iris.versicolor[versicolor.split[["test"]],]

virginica.split <- split_data(nrow(iris.virginica), 0.2)
virginica.train <- iris.virginica[virginica.split[["train"]],]
virginica.test  <- iris.virginica[virginica.split[["test"]],]

train.data <- rbind(setosa.train, versicolor.train, virginica.train)
test.data  <- rbind(setosa.test, versicolor.test, virginica.test)

rm(
  setosa.split,
  versicolor.split,
  virginica.split,
  iris.setosa,
  iris.versicolor,
  iris.virginica
)

rm(setosa.train, versicolor.train, virginica.train)
rm(setosa.test, versicolor.test, virginica.test)


library(nnet)
# Training the network
nn.1 <-
  nnet(
    Species ~ .,
    data = train.data,
    size = 2,
    decay = 1e-5,
    maxit = 50
  )
summary(nn.1)

# Testing the network
predicted.species <- predict(nn.1, test.data, type = "class")
comparison <- data.frame(actual = test.data$Species, predicted = predicted.species)

# How did we do?
table(comparison)


nn.2 <-
  nnet(
    Species ~ .,
    data = train.data,
    size = 5,
    decay = 1e-5,
    maxit = 100
  )
predicted.species <- predict(nn.2, test.data, type = "class")
comparison <- data.frame(actual = test.data$Species, predicted = predicted.species)

# How did we do?
table(comparison)


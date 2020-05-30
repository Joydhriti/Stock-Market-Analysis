library(rpart)       # performing regression trees
#install.packages("rpart.plot")
library(dplyr)  
library(rpart.plot)  # plotting regression trees
#installing Libraries

#inserting dataset
dataset = read.csv("Stock market Bank NYSE.csv", header = TRUE)

#converting variable into 'Date' format
dataset$Date = as.Date(dataset$Date, "%m/%d/%Y")

#creating tree without tuning
m1 <- rpart(
  formula = Volume ~ .,
  data    = dataset[-2],
  method  = "anova"
)

#ploting tree
rpart.plot(m1)

#finding optimal Node number for the tree
plotcp(m1,upper = "none")




hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(8, 15, 1)
)

models <- list()

for (i in 1:nrow(hyper_grid)) {
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  models[[i]] <- rpart(
    formula = Volume ~ .,
    data    = dataset[-2],
    method  = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}


hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-10, wt = error)


optimal_tree <- rpart(
  formula = Volume ~ .,
  data    = dataset[-2],
  method  = "anova",
  control = list(minsplit = 18, maxdepth = 10, cp = 0.01)
)
rpart.plot(optimal_tree, type = 1)

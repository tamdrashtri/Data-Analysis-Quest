## get the active dataset and show the first few observations
.get_data() %>%
  head()

## access a dataset
diamonds %>%
  select(price, clarity) %>%
  head()

## add a variable to the diamonds data
diamonds <- mutate(diamonds, log_price = log(price))

## show the first observations in the price and log_price columns
diamonds %>%
  select(price, log_price) %>%
  head()

## create a histogram of prices
diamonds %>%
  ggplot(aes(x = price)) +
    geom_histogram()

## and a histogram of log-prices using radiant.data::visualize
visualize(diamonds, xvar = "log_price", custom = TRUE)

## open help in the R-studio viewer from Radiant
# help(package = "radiant.data")

## If you are familiar with Shiny you can call reactives when the code
## is evaluated inside a Shiny app. For example, if you transformed
## some variables in Data > Transform you can call the transform_main
## reacive to see the latest result. Very useful for debugging
# transform_main() %>% head()
visualize(
  fruitflies, 
  xvar = "treatment", 
  yvar = "longevity_days", 
  type = "box", 
  custom = FALSE
)

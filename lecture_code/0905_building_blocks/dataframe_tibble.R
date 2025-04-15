library(tidyverse)

# tibble - data frame that has the tbl_df class
example_df=
  tibble(
    vec_numeric = 5:8, # from 5 to 8
    vec_char = c('My','name','is','Ze'),
    vec_logical = c(TRUE,TRUE,FALSE,TRUE),
  )
example_df
example_df$vec_numeric

plot_df = 
  tibble(
    x = rnorm(1000,sd=0.5),
    y = 1 + 2*x + rnorm(1000),
    y2 = 1 + 2*x + 4*x^2 + rnorm(1000)
  )
plot_df

ggplot(plot_df, aes(x=x)) + geom_histogram()
ggplot(plot_df, aes(x=x,y=y2)) + geom_point()
ggsave('scatterplot.pdf')

ls()
# rm(list = ls()) 
# remove all in the environment
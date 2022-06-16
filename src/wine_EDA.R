library(tidyverse)
library(skimr)
library(janitor)
library(here)
library(plotly)
library(GGally)
library(kableExtra)

# Import winequlity-red dataset.
wine <- read_csv(here("data", "winequality-red.csv"))

# Clean attributes' names.
wine %>% 
  clean_names() -> wine

# View data summary.
glimpse(wine)
skim(wine)

# Display random 10 rows of the dataset.
set.seed(1234)
kable(sample_n(wine, 10), "html") %>% 
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# Plot a bar graph of the quality attribute.
plot_ly(data = wine, 
        x = ~quality,
        type = "histogram", 
        color = ~quality) %>%
  layout(xaxis = list(range=c(min(wine$quality),max(wine$quality))), 
         title = 'Distribution of Red Wine Quality Ratings')

# Plot a bar graph of the quality attribute.
# ggplot(wine, aes(quality, fill = quality)) +
#   geom_bar(fill = seq(1, 6, 1)) + 
#   geom_text(position = "stack", stat='count', aes(label=..count..), vjust = -0.5) + 
#   theme(plot.title=element_text(hjust=0.5)) + 
#   labs(x="Wine Quality", 
#        y="Observations",
#        title = "Red Wine Quality Ratings Distribution")



# Plot fixed_acidity with respect to quality.
ggplot(wine, aes(fixed_acidity, fill = quality, color = seq(1, 1599, 1))) +
  geom_density(alpha=0.5)




# Since residual_sugar, free_sulfur_dioxide, total_sulfur_dioxide and chlorides 
# do not change significantly across different qualities they will not be included.  
wine %>% 
  mutate(quality = as.factor(quality)) %>% 
  select(-c(residual_sugar, free_sulfur_dioxide, total_sulfur_dioxide, chlorides)) %>%
  ggpairs(aes(color = quality, alpha=0.4),
          columns=1:7,
          lower=list(continuous="points"),
          upper=list(continuous="blank"),
          axisLabels="none", switch="both")


here("data", "winequality-red.csv")
# rm() # Function to delete environment variables.

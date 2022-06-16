library(tidyverse)
library(skimr)
library(janitor)
library(here)
library(plotly)
library(GGally)
library(kableExtra)
library(corrplot)
library(viridis)

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

# Plot number of good wine vs bad wine.
wine %>% 
  select(quality) %>% 
  mutate(goodQuality = quality >= 6) %>% 
  ggplot(aes(goodQuality, fill = goodQuality)) + 
  geom_text(position = "stack", stat='count',aes(label=..count..), vjust = -0.5) +
  geom_bar(stat="count")

# Plot the distribution of all variables.
wine %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value,fill=key)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins=sqrt(nrow(wine))) +
  theme(legend.position="none") 


# Plot fixed_acidity V.S. quality.
ggplot(wine, aes(fixed_acidity, quality, fill = quality)) +
  geom_jitter()

# Plot alcohol V.S. quality. 
ggplot(wine, aes(alcohol, quality,  fill = quality)) +
  geom_jitter() 

# Plot relationship between each attribute with every other attribute.
wine %>% 
  mutate(quality = as.factor(quality)) %>% 
  ggpairs(aes(color = quality, alpha=0.4),
          columns=1:7,
          lower=list(continuous="points"),
          upper=list(continuous="blank"),
          axisLabels="none", switch="both")

# Plot a heat-map to show the relationships between attributes.
nc=ncol(wine)
winedf <- wine[,1:12]
winedf$quality <- as.integer(wine[,12])
correlations <- cor(winedf,method="pearson")
corrplot(correlations, number.cex = .9, method = "square", 
         hclust.method = "ward", order = "FPC",
         type = "full", tl.cex=0.8,tl.col = "black")

wine %>% 
  plot_ly(x=~alcohol, y=~volatile_acidity, z= ~sulphates, color=~quality, hoverinfo = 'text', colors = viridis(3),
          text = ~paste('Quality:', quality,
                        '<br>Alcohol:', alcohol,
                        '<br>Volatile Acidity:', volatile_acidity,
                        '<br>sulphates:', sulphates)) %>% 
  add_markers(opacity = 0.8) %>%
  layout(title = "3D Wine Quality",
         annotations=list(yref='paper',xref="paper",y=1.05,x=1.1, text="quality",showarrow=F),
         scene = list(xaxis = list(title = 'Alcohol'),
                      yaxis = list(title = 'Volatile Acidity'),
                      zaxis = list(title = 'sulphates')))

# rm() # Function to delete environment variables.

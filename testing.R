#Converting rows to Columns

gap <- gapminder
View(gap)

selectedGap <- gap %>%
  select(country, year, lifeExp)

widerData <- selectedGap %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

View(widerData)

#Converting Columns back to Rows

gap <- gapminder

selectedGap <- gap %>%
  select(country, year, lifeExp)

widerData <- selectedGap %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

original <- widerData %>% 
  # transforming 13 to 2
  pivot_longer(2:13, names_to = "year", values_to = "lifeExp")

View(original)

# Range / Spread
sleep <- msleep

min(sleep$awake)
max(sleep$awake)
range(sleep$awake)
IQR(sleep$awake)

# Centrality
sleep <- msleep

mean(sleep$awake)
median(sleep$awake)

# Centrality
sleep <- msleep
var(sleep$awake)

# Summary
sleep <- msleep

summary(sleep$awake)


# Meaningful information from Data
sleep <- msleep

sleep %>% 
  drop_na(vore) %>% 
  group_by(vore) %>%
  summarise(
    Lower = min(sleep_total),
    Average = mean(sleep_total),
    Upper = max(sleep_total),
    Difference = min(sleep_total) - max(sleep_total)
  ) %>% 
  arrange(Average) %>% 
  View()


# TABLE
sleep <- msleep
sleep %>% 
  select(vore, order) %>% 
  filter(order %in% c("Rodentia")) %>% 
  table()

#The grammer of graphics
# ! Data
# 2 Mapping
# 3 Geometry

# Bar Plots
ggplot(data = starwars, mapping = aes(x = gender))+geom_bar()

#what if we have perform some operations before showing the data
# on the Geometry so we need pipeline as usual

starwars %>% 
  drop_na(height) %>% 
  ggplot(mapping = aes(x = height)) + geom_histogram()

# Boxplot with further stuff
starwars %>% 
  drop_na(height) %>% 
  ggplot(mapping = aes(x = height)) + geom_boxplot(fill = 'steelblue') + theme_bw() + labs(title = 'Histogram for displaying height', x = 'Height of Humans')

#  scatter plots alpha=0.2 or whatever upto you and size is size of dots
starwars %>% 
  filter(mass < 200) %>% 
  ggplot(mapping = aes(x = height, y = mass, color = sex)) + geom_point(size = 5) + theme_bw() + labs(title = 'Height and Mass by Sex')

# Smooth Model faced_wrap for 2D
starwars %>% 
  filter(mass < 200) %>% 
  ggplot(mapping = aes(x = height, y = mass, color = sex)) + geom_point(size = 5) + geom_smooth() + facet_wrap(~sex) + theme_bw() + labs(title = 'Height and Mass by Sex')


# T-Test checking density and then applying p-test to test the hypothesis
gap <- gapminder

gap %>% 
  filter(continent %in% c('Europe', 'Africa')) %>% 
  t.test(lifeExp ~ continent, data = ., alternative = 'two.sided', paired = FALSE)


# T-Test checking density and then applying p-test to test the hypothesis
gap <- gapminder

# Summary will give brief overview
gap %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c('Americas', 'Europe', 'Asia')) %>%
  aov(lifeExp ~ continent, data = .) %>% 
  summary()

gap %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c('Americas', 'Europe', 'Asia')) %>%
  aov(lifeExp ~ continent, data = .) %>% 
  TukeyHSD() %>% 
  plot()

# Cut method and chi squared g
ir <- iris
flowers <- ir %>% 
  mutate(size = cut(
    Sepal.Length,
    breaks = 3,
    labels = c('Small', 'Medium', 'Large')
  )) %>% 
  select(Species, size)

flowers %>% 
  select(size) %>% 
  table %>% 
  chisq.test()










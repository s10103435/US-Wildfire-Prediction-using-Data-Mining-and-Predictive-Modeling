#load the required packages

install.packages("RSQLite")
library(RSQLite)
install.packages("dbplyr")
library(dbplyr)
install.packages("dplyr")
library(dplyr)
install.packages("purrr")
library(purrr)
library(ggplot2)
install.packages("xts")
library(xts)
install.packages("ggfortify")
library(ggfortify)
install.packages("ggthemes")
library(ggthemes)
install.packages("maps")
library(maps)
install.packages("mapdata")
library(mapdata)
install.packages("leaflet")
library(leaflet)

#create database connection

connect <- dbConnect(SQLite(), '/Users/nikhilmirchandani/Downloads/FPA_FOD_20170508.sqlite')
#pull the table into RAM
wildfires <- tbl(connect, "Fires") %>% collect()
#size check
print(object.size(wildfires), units = 'Gb')
# disconnect from the database
dbDisconnect(connect)
#quick overview
glimpse(wildfires)

#number of wildfires over time
wildfires %>%
group_by(FIRE_YEAR) %>%
summarise(n_fires = n()) %>%
ggplot(aes(x=FIRE_YEAR, y=n_fires/1000)) +
geom_bar(stat = 'identity', fill = 'lightblue') +
geom_smooth(method = 'auto', se = FALSE, linetype = 'solid', size = 0.3, colour = 'darkred') +
labs(x='Year', y = 'Number of wildfires across US (thousands)', title = 'US Wildfires by YEAR')


#Day of the year
wildfires %>%
group_by(DISCOVERY_DOY) %>%
summarise(NumberOfFires = n()) %>%
ggplot(aes(x=DISCOVERY_DOY, y=NumberOfFires)) + geom_line(color = "Blue")
geom_smooth(method = 'lm', se = FALSE, linetype = 'dashed', size = 0.4, color = 'red') +
labs(x='', y = 'Number of wildfires across US', title = 'US Wildfires by Day of the Year')

#Wildfire Size
SizeCategory <- c('A' = '0-0.25', 'B' = '0.26-9.9', 'C' = '10.0-99.9', 'D' = '100-299', 'E' = '300-999',
'F' = '1000-4999', 'G' = '5000+')
wildfires %>%
group_by(FIRE_SIZE_CLASS) %>%
summarize(n = n()) %>%
mutate(FIRE_SIZE_CLASS = SizeCategory[FIRE_SIZE_CLASS]) %>%
ggplot(aes(x = FIRE_SIZE_CLASS, y= n)) +
geom_bar(stat = 'identity', fill = 'lightblue') +
labs(x = 'Fire size (acres)', y = 'Number of fires', title = 'Number of Wildfires by Size Category')

#Cause of wildfire
wildfires %>%
group_by(STAT_CAUSE_DESCR) %>%
summarize(cause = n()/1000) %>%
ggplot(aes(x = reorder(STAT_CAUSE_DESCR, cause), y = cause)) +
geom_bar(stat = 'identity', fill = 'darkblue') + coord_flip() +
labs(x = '', y = 'Number of fires (thousands)', title = 'US Wildfires by Cause 1992 to 2015')

# Relationship between the cause and the size of fires
fires %>%
group_by(STAT_CAUSE_DESCR) %>%
summarize(MeanSize = mean(FIRE_SIZE, na.rm = TRUE)) %>%
ggplot(aes(x = reorder(STAT_CAUSE_DESCR, MeanSize), y = MeanSize)) +
geom_bar(stat = 'identity', fill = 'darkblue') +
coord_flip() +
labs(x = '', y = 'Number of fires (thousands)', title = 'US Wildfires by Cause 1992 to 2015')

#Representation of individual fire
wildfire %>%
filter(STATE == "NY", FIRE_YEAR == "2010", STAT_CAUSE_DESCR == "Campfire") %>%
leaflet() %>%

#setView(lat = -0.900653, lng = -78.467834, zoom = 7) %>%
addTiles() %>%
addMarkers(
~LONGITUDE,
~LATITUDE,
label = ~paste("Name:", FIRE_NAME, "Size:", FIRE_SIZE, "Acres")
)


# DATA MINING – Decision Tree
features <- c('FIRE_SIZE', 'FIRE_YEAR', 'LATITUDE', 'LONGITUDE')
wildfire$STAT_CAUSE_DESCR <- as.factor(wildfire$STAT_CAUSE_DESCR)

# index for train/test split
set.seed(123)
train_index <- sample(c(TRUE, FALSE), nrow(wildfire), replace = TRUE, prob = c(0.85, 0.15))
test_index <- !train_index

# Create x/y, train/test data
x_train <- as.data.frame(wildfire[train_index, features])
y_train <- wildfire$STAT_CAUSE_DESCR[train_index]
x_test <- as.data.frame(wildfire[test_index, features])
y_test <- wildfire$STAT_CAUSE_DESCR[test_index]
predict <- rep('Debris Burning', length(y_test))
TestAccuracy <- round(sum(y_test == predict)/length(predict), 4)
print(paste(c("Accuracy:" , TestAccuracy)))
tr_control <- trainControl(method = 'cv', number = 3)

# Train the decision tree model
set.seed(123)
dtree <- train(x = x_train,
y = y_train,
method = 'rpart',
trControl = tr_control)

# make predictions using test set
preds <- predict(dtree, newdata = x_test)
# calculate accuracy on test set
test_set_acc <- round(sum(y_test == preds)/length(preds), 4)
print(paste(c("Accuracy:" , test_set_acc)))
print(dtree$resample)
rpart.plot(dtree$finalModel)

# Performance Evaluation – Part 1
confusionMatrix(y_test, preds)$table %>%
prop.table(margin = 1) %>%
as.data.frame.matrix() %>%
rownames_to_column(var = 'ACTUAL') %>%
gather(key = 'PREDICTED', value = 'freq',-ACTUAL) %>%
ggplot(aes(x = ACTUAL, y = PREDICTED, fill = freq)) +
geom_tile() +
geom_text(aes(label = round(freq, 2)), size = 5, color = 'black') +
scale_fill_gradient(high = 'blue', low = 'white', limits = c(0,1), name = 'Relative Frequency') +
theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "darkblue"),
axis.text.y = element_text(angle = 0, hjust = 1, colour = "darkblue")) +
ggtitle('Confusion Matrix - Simple Decision Tree')

# Performance Evaluation – Part 2
features <- c('FIRE_YEAR', 'FIRE_SIZE', 'LATITUDE', 'LONGITUDE')
wildfire$STAT_CAUSE_DESCR <- as.factor(wildfire$STAT_CAUSE_DESCR)

# index for train/test split
set.seed(123)
train_index <- sample(c(TRUE, FALSE), nrow(wildfire), replace = TRUE, prob = c(0.85, 0.15))
test_index <- !train_index
x_train <- as.data.frame(fires[train_index, features])
y_train <- fires$STAT_CAUSE_DESCR[train_index]
x_test <- as.data.frame(fires[test_index, features])
y_test <- fires$STAT_CAUSE_DESCR[test_index]
predict <- rep('Debris Burning', length(y_test))
TestAccuracy <- round(sum(y_test == predict)/length(predict), 4)
print(paste(c("Accuracy:" , TestAccuracy)))
tr_control <- trainControl(method = 'cv', number = 3)

# Train the decision tree model
set.seed(123)
dtree <- train(x = x_train,
y = y_train,
method = 'rpart',
tuneLength = 8,
trControl = tr_control)

# make predictions using test set
preds <- predict(dtree, newdata = x_test)

# calculate accuracy on test set
test_set_acc <- sum(y_test == preds)/length(preds)
print(paste(c("Accuracy:" , round(test_set_acc, 4))))
confusionMatrix(y_test, preds)$table %>%
prop.table(margin = 1) %>%
as.data.frame.matrix() %>%
rownames_to_column(var = 'actual') %>%
gather(key = 'prediction', value = 'freq',-actual) %>%
ggplot(aes(x = actual, y = prediction, fill = freq)) +
geom_tile() +
geom_text(aes(label = round(freq, 2)), size = 3, color = 'black') +
scale_fill_gradient(low = 'white', high = 'blue', limits = c(0,1), name = 'Relative Frequency') +
theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "darkblue"),
axis.text.y = element_text(angle = 0, hjust = 1, colour = "darkblue")) +
ggtitle('Confusion Matrix - Simple Decision Tree')

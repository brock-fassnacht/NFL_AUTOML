adv_df_full <- read.csv("C:\\Users\\bfass\\OneDrive\\Desktop\\CS\\Misc\\NFL Data Bowl\\adv_cleaned.csv")

adv_df_full$Udog_cov <- 1-adv_df_full$Fav_cov

adv_df <- adv_df_full[c('Fav_cov', 'Favored_by', 'Avg_pass_rating_x', 'Avg_pass_rating_y', 'Avg_ints_x', 'Avg_ints_y', 'avg_aggr_x', 'avg_aggr_y', 'Fav_pth_wins', 'Udog_pth_wins')]

#sample <- Data %>% sample_frac(size=.2, replace = FALSE)

adv_df$Fav_cov <- as.factor(adv_df$Fav_cov)


test_index <- createDataPartition(adv_df$Fav_cov, times = 1, p = 0.25, list = FALSE)

train <- adv_df[-test_index,]
test <- adv_df[test_index,]

write.csv(train, "C:\\Users\\bfass\\OneDrive\\Desktop\\CS\\Misc\\NFL Data Bowl\\DataNew\\adv_train.csv")
write.csv(test, "C:\\Users\\bfass\\OneDrive\\Desktop\\CS\\Misc\\NFL Data Bowl\\DataNew\\adv_test.csv")

# Using H20 AI

library(h2o)


h2o.init()

train <- h2o.importFile("C:\\Users\\bfass\\OneDrive\\Desktop\\CS\\Misc\\NFL Data Bowl\\DataNew\\adv_train.csv")
test <- h2o.importFile("C:\\Users\\bfass\\OneDrive\\Desktop\\CS\\Misc\\NFL Data Bowl\\DataNew\\adv_test.csv")

y <- "Fav_cov"
x <- setdiff(names(train), y)

train[, y] <- as.factor(train[, y])
test[, y] <- as.factor(test[, y])

#running for 20 base models

aml <- h2o.automl(x = x, y = y,
                  training_frame = train,
                  max_models = 20,
                  seed = 1)


lb <- aml@leaderboard
View(print(lb, n = nrow(lb)))

# Retrieving the best model

m <- aml@leader
# this is equivalent to
m <- h2o.get_best_model(aml)


# Get the best model using a non-default metric
#m <- h2o.get_best_model(aml, criterion = "logloss")

# Get the best XGBoost model using default sort metric
#xgb <- h2o.get_best_model(aml, algorithm = "xgboost")

# Get the best XGBoost model, ranked by logloss
#xgb <- h2o.get_best_model(aml, algorithm = "xgboost", criterion = "logloss")

m@params$actual



## h2o.varimp(m)  dpes not have any for the model selected

# examining accuracy
pred <- h2o.predict(m, test)

mean(pred$predict == test$Fav_cov)

head(pred, 50)

# Examining Performance

perf <- h2o.performance(m, test)
perf

# We want to look at overall performance of the model and where we can be profitable
# profitability is when we can predict games with over 52.5% accuracy with a decent sample size. (Hopefully above 5)


# looking at when favorites cover spread
f1 <- data.frame(h2o.F1(perf))

predicted_1s <- sapply(f1$threshold, function(thresh){
  sum(pred$p1 > thresh)
})

f1$Npredi1 <- predicted_1s

precision <- h2o.precision(perf)

f1$precision <- precision$precision

plot(f1$Npredi1, f1$precision)

max(f1$precision)

f1[which.max(f2$precision),]








# Recreate model where 1s are underdogs covering spread 


library(h2o)


h2o.init()

adv_df2 <- adv_df_full[c('Udog_cov', 'Favored_by', 'Avg_pass_rating_x', 'Avg_pass_rating_y', 'Avg_ints_x', 'Avg_ints_y', 'avg_aggr_x', 'avg_aggr_y', 'Fav_pth_wins', 'Udog_pth_wins')]

train2 <- adv_df2[test_index,]
test2 <- adv_df2[-test_index,]

train2 <- as.h2o(train2)
test2 <- as.h2o(test2)

y2 <- "Udog_cov"
x2 <- setdiff(names(train2), y2)

train2[, y2] <- as.factor(train2[, y2])
test2[, y2] <- as.factor(test2[, y2])

#running for 20 base models

aml <- h2o.automl(x = x2, y = y2,
                  training_frame = train2,
                  max_models = 20,
                  seed = 1)

# Leaderboard that ranks 20 models
lb <- aml@leaderboard
View(print(lb, n = nrow(lb)))

#best model
m2 <- h2o.get_best_model(aml)

pred2 <- h2o.predict(m2, test2)
perf2 <- h2o.performance(m2, test2)

f12 <- data.frame(h2o.F1(perf2))

### From past analysis, in general it is slightly easier to predict underdogs covering than 


predicted_1s <- sapply(f12$threshold, function(thresh){
  sum(pred2$p1 > thresh)
})

f12$Npredi1 <- predicted_1s

precision2 <- h2o.precision(perf2)

f12$precision <- precision2$precision

plot(f12$Npredi1, f12$precision)

max(f12$precision)

f12[which.max(f12$precision),]

f12

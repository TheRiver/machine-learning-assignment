# Show correlations between numeric values
ggcorr(weights.training[,numeric.columns], angle=-45, hjust=1.05, size=2)


# remove columns with too many missing values.
tmp <- weights.training %>% select(which(sapply(1:159, function(i) sum(is.na(weights.training[,i]))) == 0))

# See that data is highly fitted to time
table(tmp$cvtd_timestamp, tmp$classe)
tmp2 <- tmp %>% select(-user_name, -raw_timestamp_part_1, -raw_timestamp_part_2, -cvtd_timestamp)


# ---------------------------------
# Using trees
# 0.61 accuracy
modFit <- train(classe ~ ., method="rpart", data=tmp)

# 0.55
modFit <- train(classe ~ ., method="rpart", data=tmp2)

# ---------------------------------
# Bagging 

# 0.99
modFitBag <- train(classe ~ ., method="treebag", data=tmp2)
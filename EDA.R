
sales <- read.csv('catalog sales data.csv', header=T)

head(sales)

train <- sales[sales$train == 1,]
test <- sales[sales$train == 0,]

head(train)

summary(train$targdol)
table(train$targdol == 0)

summary(train)
table(is.na(train$lpuryear))

summary(train$datelp6)

train$targ_pop <- ifelse(train$targdol > 0, 1, 0)
table(train$targ_pop)

#train_pop_lg <- glm(targ_pop ~.-targdol-lpuryear, data=train, family=binomial)
#summary(train_pop_log)

summary(train)
str(train)

train_noNA <- na.omit(train)
train_pop_lg <- glm(targ_pop ~.-targdol-lpuryear, data=train_noNA, family=binomial)

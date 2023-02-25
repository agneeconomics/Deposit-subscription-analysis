---
title: "SEB task"
output: html_document
date: "`r Sys.Date()`"
---

```{r setup, include=FALSE}
library(tidyr)
library(dplyr)
bank_full<-read.csv("~/AG/SEB/bank-full.csv")
```


The data analysed in this document is related with direct marketing campaigns of a Portuguese banking institution. The marketing campaigns were based on phone calls. Often, more than one contact to the same client was required, in order to access if the product (bank term deposit) would be (or not) subscribed.
\
The full dataset was described and analyzed in:
S. Moro, R. Laureano and P. Cortez. Using Data Mining for Bank Direct Marketing: An Application of the CRISP-DM Methodology. In P. Novais et al. (Eds.), Proceedings of the European Simulation and Modelling Conference - ESM'2011, pp. 117-121, Guimarães, Portugal, October, 2011. EUROSIS. Available here: <http://hdl.handle.net/1822/14838>.



### 1. DATA MANIPULATION TASK

Demonstrate ability to: \
* select random subsample of data set;\
* filter desired rows using simple and more complex conditions;\
* drop unnecessary variables, rename some variables;\
* calculate summarizing statistics (for full sample and by categorical variables as well);\
* create new variables using simple transformation and custom functions;\
* order data set by several variables.\



**1.1. Random subsample selection:**

```{r sample}
set.seed(567)
index_train <- sample(1:nrow(bank_full), 2/3 * nrow(bank_full))
training_set <-bank_full[index_train,]
test_set <- bank_full[-index_train, ]
head(training_set)
```

Here 66.67% of sample has been randomly split into training subset and test subset. This split will be used in task No. 3.


**1.2. Filtering desired rows using simple and more complex conditions:**


```{r filtering}
head(bank_full[bank_full$age>65 & bank_full$job != "retired",])
```
The code above filters data rows of clients that can be retired (due to state pension age), but are marked as not retired.

```{r filtering1}
head(bank_full %>% 
  select(age, job) %>% 
  filter(between(age, 18, 64)))
```
The code above selects only `age` and `job` columns and filters `age` from 18 to 64 years.


```{r filtering2}
head(bank_full %>% 
  filter(job %in% c("retired", "student") & y != 0 & housing == "yes" & poutcome == "success"))
```

The code above is used to filter retired and student job types, customers that subscribed for a term deposit, have a housing loan and had the outcome of the previous marketing campaign equal to success.



**1.3. Dropping unnecessary variables and renaming some of the variables:**


```{r drop}
colnames(select(bank_full, -c("pdays", "previous")))
```

During visualization part it was noticed that number of days that passed by after the client was last contacted from a previous campaign and number of contacts performed before this campaign and for this client do not seem to have a clear / material effect on a term deposit subscription.


```{r rename}
colnames(rename(bank_full, c(prev_out = poutcome, deposit = y)))
```

or:

```{r rename1}
ndat<-bank_full
colnames(ndat)[16:17] <- c("prev_out", "deposit")
colnames(ndat)[16:17]
```

The code above is used to change the name of `previous` and `y` variables to `prev_out` and `deposit` respectively.


**1.4. Calculation of summarizing statistics (for full sample and categorical variables):**
```{r lib0, include=FALSE} 
library(gmodels)
```


```{r summary} 
summary(bank_full)
```
It can be seen that data set mainly consist of categorical variables. Clients age varies between 18 and 95 years (median and mean close to 40 years). Yearly average balance varies from negative values to hundreds of thousands with a mean equal to 1362 EUR. Average duration of the last contact was ~ 4 minutes. Small mean value of the variable `previous` indicate that not many contacts were made on average before this campaign and the maximum number of contacts (275) might be an outlier.

\

```{r lib, include=FALSE} 
library(table1)
```

```{r catsummary}
table1(~factor(job) + factor(marital) + factor(education) + factor(default) + factor(housing) + factor(loan) + factor(contact) + factor(poutcome) + factor(month)| factor(y), data = bank_full, na.rm = TRUE, digits = 1, format.number = TRUE)
```

Summary table for categorical variables show proportions (column overall) and split by clients who subscribed a term deposit (column *yes*) and not subscribed ones (column *no*). A couple of noticeable cases to mention:\
* Clients with tertiary education have higher share among the all clients that subscribed for a term deposit vs. share of such clients in a group of non-subscribed clients.\
* Out of total subscribed clients, the large share of clients (63.4%) did not have a housing loan, while for non-subscribed clients the larger part consist of clients with housing loan (58.1%).

**1.5. New variable creation by using simple transformation and custom functions:**
```{r transform}
head(bank_full %>% transmute(y = ifelse(bank_full$y=="yes",1,0),
                   default = ifelse(bank_full$default=="yes",1,0),
                   housing = ifelse(bank_full$housing=="yes",1,0),
                   loan = ifelse(bank_full$loan=="yes",1,0)))

head(bank_full %>% mutate(bal_month = ifelse(bank_full$balance > mean(bank_full$balance), 1, 0)))[,18]

head(bank_full$balance / 12)

```
The code above shows the following transformations: values of binary variables transformed into 0 and 1, also a new binary variable where value is equal to 1 if its greater than mean (1362 EUR) and equal to 0 if its value is bellow the mean and annual balance transformed to monthly balance.


**1.6. Data ordering by several variables:**
```{r order}
head(bank_full[order(bank_full$job, bank_full$y),])

head(bank_full %>% arrange(job, desc(y), desc(marital)))
```

\
\

### 2. DATA VISUALISATION TASK

In order to understand the data please visualize it. You are free to select the scope, types of plots, etc.

```{r lib1, include=FALSE}
library(ggplot2)

```



```{r plot1, message=FALSE}
education_y <- bank_full %>%
  group_by(education) %>%
  summarise(Count = n())


education_y_ratio <- bank_full %>%
  group_by(education, y) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))


bank_full %>%
  ggplot() +
  geom_bar(aes(x = education, fill = y)) +
  geom_text(data = education_y, 
            aes(x = education, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = education_y_ratio, 
             aes(x = education, y = Count, label = paste0(Percentage, "%"), group = y), 
             position = position_stack(vjust = 0.5)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  scale_x_discrete(name= "Education") +
  scale_y_continuous(name = "Count") +
  scale_fill_brewer(name = "Deposit", labels = c("Not subscribed", "Subscribed"), palette = "Paired")

```

From the bar plot above it is seen that clients that have tertiary education have a bit higher subscription share compared to other types of education (in particular, primary education).


```{r plot2, message=FALSE}
housing_y <- bank_full %>%
  group_by(housing) %>%
  summarise(Count = n())


housing_y_ratio <- bank_full %>%
  group_by(housing, y) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))


bank_full %>%
  ggplot() +
  geom_bar(aes(x = housing, fill = y)) +
  geom_text(data = housing_y, 
            aes(x = housing, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = housing_y_ratio, 
             aes(x = housing, y = Count, label = paste0(Percentage, "%"), group = y), 
             position = position_stack(vjust = 0.5)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  scale_x_discrete(name= "Has housing loan?") +
  scale_y_continuous(name = "Count") +
  scale_fill_brewer(name = "Deposit", labels = c("Not subscribed", "Subscribed"), palette = "Paired")

```

It is seen that clients who do not have a housing loan are more keen to subscribe for a term deposit (17% vs. 8%). This might be because they have more money (less expenses) left to save compared to the ones that have a mortgage loan.


```{r plot3, message=FALSE}
poutcome_y <- bank_full %>%
  group_by(poutcome) %>%
  summarise(Count = n())


poutcome_y_ratio <- bank_full %>%
  group_by(poutcome, y) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))


bank_full %>%
  ggplot() +
  geom_bar(aes(x = poutcome, fill = y)) +
  geom_text(data = poutcome_y, 
            aes(x = poutcome, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = poutcome_y_ratio, 
             aes(x = poutcome, y = Count, label = paste0(Percentage, "%"), group = y), 
             position = position_stack(vjust = 0.5)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  scale_x_discrete(name= "Outcome of the previous marketing campaign") +
  scale_y_continuous(name = "Count") +
  scale_fill_brewer(name = "Deposit", labels = c("Not subscribed", "Subscribed"), palette = "Paired")

```

High share (65%) of clients that subscribed in previous marketing campaign also subscribed in this campaign.


```{r plot4, fig.width=8.4, message=FALSE}
job_y <- bank_full %>%
  group_by(job) %>%
  summarise(Count = n())


job_y_ratio <- bank_full %>%
  group_by(job, y) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))


bank_full %>%
  ggplot() +
  geom_bar(aes(x = job, fill = y)) +
  geom_text(data = job_y, 
            aes(x = job, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = job_y_ratio, 
             aes(x = job, y = Count, label = paste0(Percentage, "%"), group = y), 
             position = position_stack(vjust = 0.5)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354"), axis.text.x=element_text(size=6.8)) +
  scale_x_discrete(name= "Type of job ") +
  scale_y_continuous(name = "Count") +
  scale_fill_brewer(name = "Deposit", labels = c("Not subscribed", "Subscribed"), palette = "Paired")

```

Students and retired clients are among the top ones that put a term deposit.



```{r plot5, fig.width=8.4, message=FALSE}
month_y <- bank_full %>%
  group_by(month) %>%
  summarise(Count = n())


month_y_ratio <- bank_full %>%
  group_by(month, y) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))


bank_full %>%
  ggplot() +
  geom_bar(aes(x = month, fill = y)) +
  geom_text(data = month_y, 
            aes(x = month, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = month_y_ratio, 
             aes(x = month, y = Count, label = paste0(Percentage, "%"), group = y), 
             position = position_stack(vjust = 0.5)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  scale_x_discrete(name= "Last contact month of year") +
  scale_y_continuous(name = "Count") +
  scale_fill_brewer(name = "Deposit", labels = c("Not subscribed", "Subscribed"), palette = "Paired")

```

A higher share of subscriptions occurred in cases where the last contact was made on March, September, October and December.



```{r plot6, fig.width=8,fig.height=4.5}
ggplot(bank_full, aes(x=age, y=balance, color=y)) + 
  geom_point(size=2)+
  labs(y = "Average yearly balance, in euros", x = "Client age", colour = "Has the client subscribed a deposit?")+
  ggtitle("Client age vs. balance")
```

The most of positive subscriptions concentrate bellow 2500 EUR yearly balance. Relation of positive subscriptions and `age` is not transparent.


```{r plot7, fig.width=8,fig.height=4.5}
ggplot(bank_full, aes(x=duration, y=campaign, color=y)) + 
  geom_point(size=2)+
  labs(y = "Number of contacts performed", x = "Last contact duration, in seconds", colour = "Has the client subscribed a deposit?")+
  ggtitle("Last contact duration vs. number of contacts")
```


Here it is seen that very short duration of the contact can be a good indicator of non-subscribtion. Also, high number of contracts performed does not seem to work in terms of successfully signed term deposits.


```{r plot8, fig.width=8,fig.height=4.5}
ggplot(bank_full, aes(x=pdays, y=previous, color=y)) + 
  geom_point(size=2)+
  labs(y = "Number of contacts performed before", x = "Number of days that passed by after the client was last contacted", colour = "Has the client subscribed a deposit?")+
  ggtitle("Number of days that passed by vs. number of contacts")
```

No explicit relations are seen with `pdays`, `previous` and positive term deposit subscriptions.

\
\

### 3.  MODELLING TASK
Perform a logistic regression to obtain the predicted probability that a customer has subscribed for a term deposit.\
Use continuous variables and dummy variables created for categorical columns. Not necessarily all variables provided in data sample should be used.\
Evaluate model goodness of fit and predictive ability. If needed, data set could be split into training and test sets.
\
The model was chosen considering several goodness of fit measurements, such as AIC criterion, deviance and Pseudo R².

```{r modelling}
model<-glm(as.factor(y)~duration+balance+education+housing+poutcome+month, data = training_set, family = binomial)
summary(model)
```

The fitted model summary showed the lowest AIC criteria value (14929) and deviance (14887).

```{r good, include=FALSE}
library(rcompanion)
```

```{r good1}
nagelkerke(model)
```

The model also had a higher Pseudo R² values compared with other models (these models are not included here).
\
From fitting the model it was noticed that dropping `duration`, `balance`, `housing` variables reduces the goodness of fit of the model quite significantly.


```{r lib5, include=FALSE}
library(pROC)
library(caret)
```


```{r acc}
test_prob = predict(model, newdata = test_set, type = "response")
range(test_prob)
```

It seen that the range of predicted probabilities is large therefore the created model seem to discriminate subscribed and non-subscribed clients quite well.

To measure predictive model performance, ROC and AUC metrics are used.

```{r acc2, message=FALSE}
test_roc = roc(test_set$y ~ test_prob, plot = TRUE, print.auc = TRUE)
as.numeric(test_roc$auc)
```

ROC curve shows relatively high sensitivity and specificity and AUC value (~0.9) indicates quite good model’s ability to distinguish between the subscribed and non-subscribed clients.





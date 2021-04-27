library(fpp3)

#import the raw data set
raw_df <- read.csv("px-x-0603020000_101_20210420-140452.csv", header = FALSE)
# keep variable 16 which is automotive fuel
raw_df <- raw_df[1:16,-1]
raw_df[1,1] <- "Date"
raw_df <- t(raw_df) #change the disposition of the data

library(janitor)
a <- raw_df %>%
  row_to_names(row_number = 1) #row 1 -> title

a <- as_tibble(a) #from dataframe to tibble

#column cleaning
a$`47: Total retail sector` <- as.numeric(a$`47: Total retail sector`)
#change the chr column as numeric
# have to change the column title

mut <- a %>% 
  mutate(Date = yearmonth(
    as.Date(
      paste0(Date, "D01"),
      format = "%YM%mD%d"))) #date bug manipulation
ts <- mut %>% as_tsibble(index = Date) #tsibble import

ts %>% autoplot(`47: Total retail sector`) #TS plot

# Classical decomposition

# See the extended name of the total sales variable:

mut[,2]

# Classical decomposition additive
mut %>% 
  as_tsibble() %>%
  model(classical_decomposition(`47: Total retail sector`, type = "additive")) %>%
  components() %>%
  autoplot() +
  xlab("year")



# Classical decomposition multiplicative
mut %>% 
  as_tsibble() %>%
  model(classical_decomposition(`47: Total retail sector`, type = "multiplicative")) %>%
  components() %>%
  autoplot() +
  xlab("year")


# Let's check the seasonality:

mut %>% as_tsibble() %>% gg_season(`47: Total retail sector`)


# We can notice from the season plot that December is the month with the highest index of sales,
# On the other hand, every year in February sales sink compared to January for almost every year of the time series.
# Moreover, in March, May and July the sales increase after declining for the months of April and June. Interesting
# also to notice that from August until the end of the year the sales increase following something similar to a linear trend.


# Exponential smoothing forecasting

retaildata <- mut %>% as_tsibble()

fit_ETS <- retaildata %>%
  model(ETS(`47: Total retail sector`))

fit_ETS %>%
  report()

# The default function gives an ETS with multiplicative errors, additive trend with dampening and a multiplicative seasonality.

fit_ETS %>% forecast( h= 10) %>% autoplot(retaildata)

# Let's apply an additive model
fit_ETS2 <- retaildata %>%
  model(ETS(`47: Total retail sector`~ error("A") + trend("Ad")  + season("A")))

fit_ETS2 %>%
  report()

fit_ETS2 %>% forecast( h= 10) %>% autoplot(retaildata)

# Compare the accuracy between the 2 models:

accuracy(fit_ETS)
accuracy(fit_ETS2)

# The multiplicative model seems to be better.

# Regression model with trend and season

fit_reg <- retaildata %>%
  model(TSLM(`47: Total retail sector` ~ trend() + season()))

report(fit_reg)

# Just to have a picture of the predictions and the actual data

augment(fit_reg) %>% ggplot(aes(x = Date)) +
  geom_line(aes(y = `47: Total retail sector`, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted"))

# Check for the accuracy:

accuracy(fit_reg)

#It is not improved in relation with the previous ETS model.

# As we know that the seasonality is probably slightly multiplicative log transfor the model

fit_regl <- retaildata %>%
  model(TSLM = TSLM(log(`47: Total retail sector`)~ trend() + season()))

augment(fit_regl)

tidy(fit_regl) %>% select(term, estimate)

expcoef <- (exp(coef(fit_regl)$estimate) - 1) * 100
options( scipen = 999)
expcoef

# Every month the sales increase by 7,2 %

fit_regl %>% forecast (h = 12) %>% autoplot(retaildata)

# compare reg model and log reg model:

accuracy(fit_regl)
accuracy(fit_reg)

# The reg model seems better.




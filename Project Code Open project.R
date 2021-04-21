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

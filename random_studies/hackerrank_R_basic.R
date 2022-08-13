# hackerrank basic R exercises -------------------------------------------

# Complete the 'calculate_average_temperature' function below.

#' Calculate average temperature from different data sources
#' @param df_data File with data about temperature observations
#' @return df_result Data table with average temperature sorted by years and months


df_data <- data.frame(source = c(1,2,2,2,2,2,2,3,1,3), 
                      date = c("2010-01-13 11:35:57 CET", "11-01-2010","22-01-2010",
                               "03-01-2010","18-01-2010","06-02-2010","29-01-2010",
                               "2010 10-02","2010-01-19 11:35:57 CET","2010 06-01"), 
                      temperature = c(4,-7,30,7,-4,18,14,17,5,19))


calculate_average_temperature <- function(df_data) {
  
  # data positions
  first_string_year_rows <- as.numeric(row.names(df_data[nchar(stringr::str_extract(df_data$date, "(\\w+)")) < 4,]))
  last_string_year_rows <- as.numeric(row.names(df_data[nchar(stringr::str_extract(df_data$date, "\\w+$")) < 4,]))
  
  # extracting month from date column
  df_data$month <- NA
  df_data$month[first_string_year_rows] <- substr(df_data[rownames(df_data) %in% first_string_year_rows, 2], start = 4, stop = 5)
  df_data$month[last_string_year_rows] <- substr(df_data[rownames(df_data) %in% last_string_year_rows, 2], start = 6, stop = 7)
  
  # extracting year from date column
  df_data$year <- NA
  df_data$year[first_string_year_rows] <- substr(df_data[rownames(df_data) %in% first_string_year_rows, 2], start = 7, stop = 10)
  df_data$year[last_string_year_rows] <- substr(df_data[rownames(df_data) %in% last_string_year_rows, 2], start = 1, stop = 4)
  
  # casting variables:
  lapply(df_data, class)
  managed_data <- as.data.frame(sapply(df_data[,c(1,3,4,5)], as.numeric))
  
  # Return result
  # average temperature by year, month and source: 
  aggregate(temperature ~ source + year + month, data = managed_data, FUN = mean)
  
}

calculate_average_temperature(df_data)

# output
# "source","year","month","temperature_mean"
# 1,2010,1,4.5
# 2,2010,1,8
# 2,2010,2,18
# 3,2010,1,19
# 3,2010,2,17


df_data <- data.frame(datetime = c("2017-10-27 00:00:00","2017-10-27 01:00:00",
                                   "2017-10-27 02:00:00","2017-10-27 03:00:00",
                                   "2017-10-27 04:00:00","2017-10-27 05:00:00",
                                   "2017-10-27 06:00:00","2017-10-27 07:00:00",
                                   "2017-10-27 08:00:00","2017-10-27 09:00:00",
                                   "2017-10-27 10:00:00","2017-10-27 11:00:00",
                                   "2017-10-27 12:00:00","2017-10-27 13:00:00",
                                   "2017-10-27 14:00:00","2017-10-27 15:00:00",
                                   "2017-10-27 16:00:00","2017-10-27 17:00:00",
                                   "2017-10-27 18:00:00","2017-10-27 19:00:00",
                                   "2017-10-27 20:00:00","2017-10-27 21:00:00",
                                   "2017-10-27 22:00:00","2017-10-27 23:00:00",
                                   "2017-10-28 00:00:00"), 
                      Vancouver = c(287.21,286.42,284.4,282.56,281.94,281.52,281.66,281.37,
                                    281.83,281.17,280.13,281.37,280.89,279.57,279.7,280.08,279.07,
                                    281.04,284.56,286.18,286.44,287.87,288.42,288.65,288.01),
                      
                      Portland = c(293.31,291.14,289.39,288.52,287,286.14,285.5,285.01,284.49,283.87,
                                   283.62,283.48,283,282.77,282.89,281.62,283.48,286.06,289.69,
                                   292.27,293.84,294.85,295.57,294.99,294.52))


#' Calculate the average temperature in cities by dates
#' @param df_data Data frame from 'csv' file
#' @return Data frame with average temperatures
calculate_average_temperature_in_cities_by_dates <- function(df_data) {
  
  # Extracting Date from datetime column:  
  df_data$Date <- substr(df_data$datetime, start = 1, stop = 10)
  
  # casting variables
  lapply(df_data, class)
  df_data$Date <- as.Date(df_data$Date)
  
  # Return result
  aggregate(cbind(Portland, Vancouver) ~ Date, data = df_data, FUN = mean)

}

calculate_average_temperature_in_cities_by_dates(df_data)

# # output
# "Date",    "Portland","Vancouver"
# 2017-10-27, 287.6,     283.09
# 2017-10-28, 294.52,    288.01



twdf <- data.frame(tweet_id = c("40b149d273","e61d64740e",
                                "431ed00e5d","fe530042f7",
                                "2ca5660e66","b59a33cbaf",
                                "7c843939bb","9fb39d3c8c",
                                "c481433b33","1a34426d09",
                                "93a9346111","4d6da31db3",
                                "b442a0a793","969185c4e9",
                                "b3fa6e62e2"), 
                   tweet_test = c("poor girl",
                                  "Yeah but it doesn`t sound indie enough i need2learn some other tunes and then pick up mo style =] 1hour! I`ll c u then ;)",
                                  "hey beautiful",
                                  "I shudder at the thought of what she was thinking she`d do with it if she`d managed to reclaim it...",
                                  "May 10 is Human Kindness Day.",
                                  "needs 1000 words for 502 part 2 .... then freedom ..... until another few assignments and exams",
                                  "ned to go to beathroom, don`t know if i can reach/find clothes. need to walk outsie to get to bathroom. ok at night, not ok in daylight.",
                                  "Only has under 200 words left to write on her assignment",
                                  "hm... i don`t I can recommend any white chocolates though.. you have to move to the `dark` side first..",
                                  "my grandpa was telling me how they used to cut up human bodies in med school",
                                  "I believe a man died in a car wreck today just right down the road. It happened at 12 and at 2 he was still in the car.",
                                  "Superman",
                                  "I`m stuck wearing a sun dress",
                                  "omg!! i have so many finals to study for !!! i so freaked out that im gona fail",
                                  "is looking forward to tonight`s dinner date with Mom; is gymming this afternoon; started 'bawas kanin' (less rice) movement; is celibate."),
                   tweet_sentiment = c("negative", "neutral", "positive", "neutral", rep("neutral", 6), "negative", "positive", "negative", "negative", "neutral")
)

tt_words <- function(twdf){
  # dropping all punctuantion:
  twdf$tweet_test <- gsub('[[:punct:] ]+',' ',twdf$tweet_test)
  
  if (twdf[c(grep("dress", twdf$tweet_test)), 3] == "neutral") {
    dress <- data.frame(positive = 0, neutral = 1, negative = 0)
  } else if (twdf[c(grep("dress", twdf$tweet_test)), 3] == "positive") {
    dress <- data.frame(positive = 1, neutral = 0, negative = 0)
  } else {
    dress <- data.frame(positive = 0, neutral = 0, negative = 1)
  }
  
  if (twdf[c(grep("clothes", twdf$tweet_test)), 3] == "neutral") {
    clothes <- data.frame(positive = 0, neutral = 1, negative = 0)
  } else if (twdf[c(grep("clothes", twdf$tweet_test)), 3] == "positive") {
    clothes <- data.frame(positive = 1, neutral = 0, negative = 0)
  } else {
    clothes <- data.frame(positive = 0, neutral = 0, negative = 1)
  }
  
  df1 <- rbind(clothes, dress)
  df2 <- data.frame(word = c("clothes", "dress"))
  
  cbind(df2, df1)
}

tt_words(twdf)

# # output
# "word",   "positive","neutral","negative"
# "clothes",   0,        1,           0
# "dress",     0,        0,           1

# input
# 
# Region","Representative","Item","Units","Price","Total_sales"


data_sales <- data.frame(Region = c("West",
           "Central",
           "West",
           "Central",
           "Central",
           "East",
           "East",
           "East",
           "East",
           "Central",
           "Central",
           "East",
           "West",
           "East",
           "Central"),
           Representative = c("Paul", "Peter", "Connor", "David", "Thomas", "Richard",
           "John","Jared", "Adam", "Mark", "Jimmy", "Stuart", "Aishley", "Rafel", "Jaminee"),
           Item = c("Juicer",
                    "Ceiling fan",
                    "Juicer",
                    "Ceiling fan",
                    "Dish washer",
                    "Juicer",
                    "Toaster",
                    "Ceiling fan",
                    "Washing machine",
                    "Ceiling fan",
                    "Washing machine",
                    "Washing machine",
                    "Dish washer",
                    "Toaster",
                    "Washing machine"),
           Units = c(19,
           2,
           17,
           8,
           12,
           20,
           5,
           16,
           6,
           7,
           13,
           15,
           10,
           18,
           4), Price = c(80,
           100,
           80,
           100,
           300,
           80,
           50,
           100,
           600,
           100,
           600,
           600,
           300,
           50,
           600), Total_sales = c(1520,
           200,
           1360,
           800,
           3600,
           1600,
           250,
           1600,
           3600,
           700,
           7800,
           9000,
           3000,
           900,
           2400))

# best sales in each region 
x <- merge(aggregate(Total_sales ~ Region, data = data_sales, FUN = max), data_sales)
x[,c(1,3,2)]

# output

# "Region","Representative","Total_sales"
# "Central","Jimmy",7800
# "East","Stuart",9000
# "West","Aishley",3000


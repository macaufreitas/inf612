#
# INF-612 Handling Data
# Atividade a Distancia
# Aluno: Eliseu Massing Junior
# Aluno: Marcos Aurelio Freitas de Almeida Costa
#


#
# Constants
#
MIN_DATE <- strptime("2014-07-01", "%Y-%m-%d")
MAX_DATE <- strptime("2015-06-30", "%Y-%m-%d")
TIME_IDX <- "Horario"
TEMP_IDX <- "Temperatura"
WIND_IDX <- "Vento"
HUMIDITY_IDX <- "Umidade"
SENSATION_IDX <- "Sensacao"
ERROR_MGS <- " [ERRO]"


#
# Functions
#
get_daily_accumulated <- function(dataset) {
    k <- 1
    count_measures <- 0
    daily_temp <- 0
    daily_wind <- 0
    daily_humidity <- 0
    daily_sensation <- 0
    measures_daily <- list()
    for(i in 1:nrow(dataset)) {
        current_day <- dates[k]
        day_i <- substring(dataset[[TIME_IDX]][i], 1, 10)
        if(current_day == day_i) {
            daily_temp <- daily_temp + as.numeric(dataset[[TEMP_IDX]][i])
            daily_wind <- daily_wind + as.numeric(dataset[[WIND_IDX]][i])
            daily_humidity <- daily_humidity + as.numeric(dataset[[HUMIDITY_IDX]][i])
            daily_sensation <- daily_sensation + as.numeric(dataset[[SENSATION_IDX]][i])
            count_measures <- count_measures + 1
        }
        else {
            measures_daily[[current_day]] <- list(temp = daily_temp, wind = daily_wind,
                                                  humidity = daily_humidity, n_measures = count_measures,
                                                  sensation = daily_sensation)
            count_measures <- 0
            daily_temp <- 0
            daily_wind <- 0
            daily_humidity <- 0
            daily_sensation <- 0
            k <- k + 1 # go to the next day
            i <- i - 1 # make the loop go back one iteraction
        }
    }

    return(measures_daily)
}

# TODO: finish this function trying to use apply functions for lists
get_daily_avg <- function(acc_values) {
    daily_avg_result <- list()
    dates_list <- names(acc_values)
    temp_list <- sapply(acc_values, "[[", "temp")
    wind_list <- sapply(acc_values, "[[", "wind")
    humidity_list <- sapply(acc_values, "[[", "humidity")
    nmeasures_list <- sapply(acc_values, "[[", "n_measures")
    sensation_list <- sapply(acc_values, "[[", "sensation")

    for(i in 1:length(acc_values)) {
        temp_avg <- temp_list[i] / nmeasures_list[i]
        wind_avg <- wind_list[i] / nmeasures_list[i]
        humidity_avg <- humidity_list[i] / nmeasures_list[i]
        sensation_avg <- sensation_list[i] / nmeasures_list[i]
        daily_avg_result[[dates_list[i]]] <- list(Temperatura = temp_avg, Vento = wind_avg,
                                                Humidade = humidity_avg, Sensacao = sensation_avg)
    }

    return(daily_avg_result)
}



#
# Plot graph of Temperature X Sensation
#
graph_temp_VS_sensat <- function(dataset, query_day = "2014-07-01") {
  day_data <- cepagri_data[substring(cepagri_data[[1]], 1, 10) == query_day, ]

  graph_title <- paste("Temperature X Sensation ->", query_day)
  plot(day_data$Sensacao ~ day_data$Temperatura, col = "red", main = graph_title, xlab = "Temperature", ylab = "Sensation")
}

#
# Plot graph of Temperature X Humidity
#
graph_temp_VS_humid <- function(dataset, query_day = "2014-07-01") {
  day_data <- cepagri_data[substring(cepagri_data[[1]], 1, 10) == query_day, ]

  graph_title <- paste("Temperature X Humidity ->", query_day)
  plot(day_data$Umidade ~ day_data$Temperatura, col = "red", main = graph_title, xlab = "Temperature", ylab = "Humidity")
}

#
# Plot graph of Humidity X Sensation
#
graph_humid_VS_sensat <- function(dataset, query_day = "2014-07-01") {
  day_data <- cepagri_data[substring(cepagri_data[[1]], 1, 10) == query_day, ]

  graph_title <- paste("Humidity X Sensation ->", query_day)
  plot(day_data$Sensacao ~ day_data$Umidade, col = "red", main = graph_title, xlab = "Humidity", ylab = "Sensation")
}

#
# Plot graph of monthly average temperature
#
graph_monthly_avg_temp <- function(dataset) {
  # TODO
}


#
# Main Execution
#

# Read data from cepagri URL
connection <- url("http://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")

# Parse cepagri data to a dataframe
cepagri_data <- read.csv(connection, header = FALSE, sep = ";", stringsAsFactors = F,
                     col.names = c("Horario","Temperatura","Vento","Umidade","Sensacao"))

# Update date to the format YYYY-MM-DD
cepagri_data[[1]] <- strptime(cepagri_data[[1]], "%d/%m/%Y-%H:%M")
# Apply filter to get only measurements between MIN and MAX dates
cepagri_data <- cepagri_data[(cepagri_data[[1]] >= MIN_DATE),]
cepagri_data <- cepagri_data[(cepagri_data[[1]] <= MAX_DATE),]

# Delete all entries that have missing data
cepagri_data <- cepagri_data[cepagri_data[[TEMP_IDX]] != " [ERRO]",]

# List with all days and months between defined dates
dates <- unique(substring(cepagri_data[[TIME_IDX]], 1, 10))
months <- unique(substring(cepagri_data[[TIME_IDX]], 1, 7))

mytest <- get_daily_accumulated(cepagri_data)
mytest2 <- get_daily_avg(mytest)
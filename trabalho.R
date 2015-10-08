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
# Return a data.frame with monthly data
#
get_monthly_data <- function(dataframe) {
  avg_temp = c() ; avg_wind = c() ; avg_humid = c()
  avg_sensat = c() ; max_temp = c() ; min_temp = c() 
  
  for(month in months) {
    avg_temp <- c(avg_temp, mean(as.numeric(cepagri_data[cepagri_data$Mes == month, ]$Temperatura)))
    avg_wind <- c(avg_wind, mean(as.numeric(cepagri_data[cepagri_data$Mes == month, ]$Vento)))
    avg_humid <- c(avg_humid, mean(as.numeric(cepagri_data[cepagri_data$Mes == month, ]$Umidade)))
    avg_sensat <- c(avg_sensat, mean(as.numeric(cepagri_data[cepagri_data$Mes == month, ]$Sensacao)))
    max_temp <- c(max_temp, max(as.numeric(cepagri_data[cepagri_data$Mes == month, ]$Temperatura)))
    min_temp <- c(min_temp, min(as.numeric(cepagri_data[cepagri_data$Mes == month, ]$Temperatura)))
  }
  
  monthly_avg_data <- data.frame(months, avg_temp, avg_wind, avg_humid, avg_sensat, max_temp, min_temp)
  
  return(monthly_avg_data)
}


# Plot graph of monthly average temperatures
graph_monthly_avg_temp <- function(df) {
  g_range <- c(10, 30)
  plot(df$avg_temp, type="o", col="blue", ylim=g_range, axes=FALSE, ann=FALSE) ; box()
  axis(1, at=1:12, lab=as.character(df$months), las=2)
  axis(2, las=1, at=2*0:g_range[2])
  title(main="Temperatura Media Mensal", col.main="blue", font.main=4)
  title(ylab="Temperaturas [°C]", col.lab=rgb(0,0.5,0))
}


# Plot graph of monthly average wind speed
graph_monthly_avg_wind <- function(df) {
  g_range <- c(10, 40)
  plot(df$avg_wind, type="o", col="green", ylim=g_range, axes=FALSE, ann=FALSE) ; box()
  axis(1, at=1:12, lab=as.character(df$months), las=2)
  axis(2, las=1, at=2*0:g_range[2])
  title(main="Velocidade do Vento Media Mensal", col.main="Blue", font.main=4)
  title(ylab="Velocidade do Vento [Km/h]", col.lab=rgb(0,0.5,0))
}


# Plot graph of monthly average humidity
graph_monthly_avg_humid <- function(df) {
  g_range <- c(40, 100)
  plot(df$avg_humid, type="o", col="blue", ylim=g_range, axes=FALSE, ann=FALSE) ; box()
  axis(1, at=1:12, lab=as.character(df$months), las=2)
  axis(2, las=1, at=2*0:g_range[2])
  title(main="Umidade Media Mensal", col.main="Blue", font.main=4)
  title(ylab="Umidade", col.lab=rgb(0,0.5,0))
}


# Plot graph of monthly average termal sensation
graph_monthly_avg_sensat <- function(df) {
  g_range <- c(10, 40)
  plot(df$avg_sensat, type="o", col="blue", ylim=g_range, axes=FALSE, ann=FALSE) ; box()
  axis(1, at=1:12, lab=as.character(df$months), las=2)
  axis(2, las=1, at=2*0:g_range[2])
  title(main="Sensacao Termica Media Mensal", col.main="Blue", font.main=4)
  title(ylab="Sensacao Termica [°C]", col.lab=rgb(0,0.5,0))
}

#
# Plot graph of monthly average , max and min temperatures
#
graph_monthly_avg_max_min_temp <- function(df) {
  g_range <- c(0, 40)
  plot(df$avg_temp, type="o", col="green", ylim=g_range, axes=FALSE, ann=FALSE) ; box()
  lines(df$max_temp, type="o", col="red", pch=22)
  lines(df$min_temp, type="o", col="blue", pch=23)
  axis(1, at=1:12, lab=as.character(df$months), las=2)
  axis(2, at=2*0:g_range[2], las=1)
  title(main="Temperaturas Mensais", col.main="blue", font.main=4)
  title(ylab="Temperaturas [°C]", col.lab=rgb(0,0.5,0))
  legend("topright", g_range[2], c("Maxima","Media", "Minima"), col=c("red","green", "blue"), pch=c(22, 21, 23), cex=0.8)
}




#
# Plot graph of Humidity X Temperature X Sensation
#
graph_temp_VS_sensat <- function(df, query_day = "2014-07-01") {
  day_data <- df[substring(df[[1]], 1, 10) == query_day, ]

  green <- rgb(0,0.5,0)
  
  g_range <- c(0, 30)
  plot(day_data$Temperatura, type="l", col="red", ylim=g_range, axes=FALSE, ann=FALSE) ; box()
  lines(day_data$Sensacao, type="l", col="blue")
  #axis(1, at=1:12, lab=as.character(day_data), las=2)
  axis(2, at=2*0:g_range[2], las=1)
  
  par(new=TRUE)
  
  plot(day_data$Umidade, type="l", col=green, xlab="", ylab="", ylim=c(0,100), axes=FALSE)
  mtext("Humidity",side=4,col=green,line=10) 
  axis(4, ylim=c(0,100), col=green,col.axis=green,las=1)
  
  title(main="Umidade x Temperatura x Sensacao", col.main="blue", font.main=4)
  title(ylab="Temperaturas [°C]", col.lab=green)
  legend("topleft", g_range[2], c("Umidade","Temperatura", "Sensacao"), cex=0.8, col=c(green, "red", "blue"), lty=1:1)  

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

# Adding month column on the data.frame
cepagri_data$Mes <- substring(cepagri_data[[1]], 1, 7)

#mytest <- get_daily_accumulated(cepagri_data)
#mytest2 <- get_daily_avg(mytest)

monthly_data <- get_monthly_data(cepagri_data)

# Plotting the graphs
graph_monthly_avg_temp(monthly_data)
graph_monthly_avg_wind(monthly_data)
graph_monthly_avg_humid(monthly_data)
graph_monthly_avg_sensat(monthly_data)
graph_monthly_avg_max_min_temp(monthly_data)
graph_temp_VS_sensat(cepagri_data, "2014-07-01")
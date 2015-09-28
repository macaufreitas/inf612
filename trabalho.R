#
# INF-612 Handling Data
# Atividade a Distancia
# Aluno: Eliseu Massing Junior
# Aluno: Marcos Aurelio Freitas de Almeida Costa
#

# Constants
MIN_DATE <- strptime("2014-07-01", "%Y-%m-%d")
MAX_DATE <- strptime("2015-06-30", "%Y-%m-%d")

#
# Main Execution
#

# Read data from cepagri URL
connection <- url("http://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv")

# Parse cepagri data to a dataframe
cepagri_data <- read.csv(connection, header = FALSE, sep = ";", stringsAsFactors = F,
                     col.names = c("Horario","Temperatura","Vento","Umidade","Sensacao"))

# Update date to the format YYYY-MM-DD
cepagri_data[[1]] <- strptime(cepagri_data[[1]], "%d/%m/%Y-%H:%M")
# Apply filter to get only measurements between MIN and MAX dates
cepagri_data <- cepagri_data[(cepagri_data[[1]] >= MIN_DATE),]
cepagri_data <- cepagri_data[(cepagri_data[[1]] <= MAX_DATE),]


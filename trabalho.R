#
# INF-612 Handling Data
# Atividade a Distancia
# Aluno: Eliseu Massing Junior
# Aluno: Marcos Aurelio Freitas de Almeida Costa
#

# Constants
MIN_DATE <- "2014-07-01"
MAX_DATE <- "2015-06-30"

#
# Main Execution
#

# Read data from cepagri URL
connection <- url("http://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv")

# Parse cepagri data to a dataframe
cepagri_data <- read.csv(connection, header = FALSE, sep = ";", stringsAsFactors = F,
                     col.names = c("Horario","Temperatura","Vento","Umidade","Sensacao"))

# Update date to the format YYYY-MM-DD
cepagri_data[[1]] <- strptime(cepagri_data[[1]], "%d/%m/%Y")
# Apply filter to get only measurements between MIN and MAX dates
cepagri_data <- cepagri_data[((cepagri_data[[1]] >= MIN_DATE) && (cepagri_data[[1]] <= MAX_DATE)),]


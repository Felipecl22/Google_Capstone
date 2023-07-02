
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Paso 1: 
# Instalar paquetes de librerías necesarias para el análisis.                 #
# tidyverse para importar data y wrangling (convertir data en data usable)    #
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  

#Instalar paquetes en caso de estar instalados en R:

install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

#Llamar librerías para poder utilizar sus funciones:

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data

#Mostrar el directorio actual:

getwd() 

setwd("C:/Users/Felipe/Documents/Archivos Google")

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here

May_2023 <- read.csv("202305-divvy-tripdata.csv")
Abr_2023 <- read.csv("202304-divvy-tripdata.csv")
Mar_2023 <- read.csv("202303-divvy-tripdata.csv")
Feb_2023 <- read.csv("202302-divvy-tripdata.csv")
Ene_2023 <- read.csv("202301-divvy-tripdata.csv")
Dic_2022 <- read.csv("202212-divvy-tripdata.csv")
Nov_2022 <- read.csv("202211-divvy-tripdata.csv")
Oct_2022 <- read.csv("202210-divvy-tripdata.csv")
Sep_2022 <- read.csv("202209-divvy-publictripdata.csv")
Ago_2022 <- read.csv("202208-divvy-tripdata.csv")
Jul_2022 <- read.csv("202207-divvy-tripdata.csv")
Jun_2022 <- read.csv("202206-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Verificar que todos los archivos tengan las mismas columnas para
# posteriormente consolidar en un único archivo.

colnames(May_2023)
colnames(Abr_2023)
colnames(Mar_2023)
colnames(Feb_2023)
colnames(Ene_2023)
colnames(Dic_2022)
colnames(Nov_2022)
colnames(Oct_2022)
colnames(Sep_2022)
colnames(Ago_2022)
colnames(Jul_2022)
colnames(Jun_2022)

# Combinar todos los archivos en una gran DATAFRAME, considera los últimos 12 
# periodos.

viajes <- bind_rows(May_2023
                    ,Abr_2023
                    ,Mar_2023
                    ,Feb_2023
                    ,Ene_2023
                    ,Dic_2022
                    ,Nov_2022
                    ,Oct_2022
                    ,Sep_2022
                    ,Ago_2022
                    ,Jul_2022
                    ,Jun_2022)
View(viajes)
class(viajes)
# Remover lat, long,  dado a que esta información fue dada de baja en 2020.

viajes <- viajes %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================

# Inspect the new table that has been created

colnames(viajes)  #List of column names
nrow(viajes)  #How many rows are in data frame?
dim(viajes)  #Dimensions of the data frame?
head(viajes)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(viajes)  #See list of columns and data types (numeric, character, etc)
summary(viajes)  #Statistical summary of data. Mainly for numerics

# Begin by seeing how many observations fall under each usertype
table(viajes$member_casual)

# Reassign to the desired values (we will go with the current 2020 labels)
viajes <-  viajes %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# Check to make sure the proper number of observations were reassigned
table(viajes$member_casual)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
#viajes$date <- as.Date(viajes$started_at) #The default format is yyyy-mm-dd, no me sirve por que no da cuenta del formato.
viajes$date <- as.Date(viajes$started_at,format='%Y-%m-%d %H:%M:%S')
viajes$month <- month(ymd(viajes$date))
viajes$day <- day(viajes$date)
viajes$year <- year(viajes$date)
viajes$day_of_week <- wday(viajes$date)

str(viajes)

######################################################################

viajes_2 <- viajes # creando copia de nuestra data para no perder lo avanzado.



viajes_2$started_at <- viajes_2$started_at <- as.POSIXct(x = as.character(viajes_2$started_at), 
                                                                 format='%Y-%m-%d %H:%M:%S')

str(viajes_2)


viajes_2$ended_at <- viajes_2$ended_at <- as.POSIXct(x = as.character(viajes_2$ended_at), 
                                                             format='%Y-%m-%d %H:%M:%S')

str(viajes_2)

viajes_2 <- viajes_2 %>% 
  mutate(ride_length = ended_at - started_at)

str(viajes_2)
#################################################################################

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(viajes_2$ride_length)
viajes_2$ride_length <- as.numeric(as.character(viajes_2$ride_length))
is.numeric(viajes_2$ride_length)

viajes_2 <- viajes_2 %>% 
  mutate(ride_length_seconds = (ended_at - started_at)/60)

is.factor(viajes_2$ride_length_seconds)
viajes_2$ride_length_seconds <- as.numeric(as.character(viajes_2$ride_length_seconds))
is.numeric(viajes_2$ride_length_seconds)

str(viajes_2)

# Remove "bad" data
viajes_2_v3 <- viajes_2[!(viajes_2$start_station_name == "HQ QR" | viajes_2$ride_length<0),]

View(viajes_2_v3)
str(viajes_2)

# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)

#Comprobando si la data contiene NA, ya que sino no podremos realizar calculos.

#La data contiene 1026 NA
sum(is.na(viajes_2_v3$ride_length_seconds)) 
 
#La data contiene 5827892 datos buenos
sum(!is.na(viajes_2_v3$ride_length_seconds)) 

(sum(is.na(viajes_2_v3$ride_length_seconds)) / sum(!is.na(viajes_2_v3$ride_length_seconds))) *100

# El porcentaje de NAs presentes en la columna ride_length_Seconds corresponde a un 0.017%, por
# ello no impacta fuertemente en el resultado de los agregados.

# Dado que algunos datos son NA es necesario utilizar na.rm = T, para omitirlos.

mean(viajes_2_v3$ride_length_seconds,na.rm = T) #straight average (total ride length / rides)
median(viajes_2_v3$ride_length_seconds,na.rm = T) #midpoint number in the ascending array of ride lengths
max(viajes_2_v3$ride_length_seconds,na.rm = T) #longest ride
min(viajes_2_v3$ride_length_seconds,na.rm = T) #shortest ride

mean(viajes_2_v3$ride_length,na.rm = T) #straight average (total ride length / rides)
median(viajes_2_v3$ride_length,na.rm = T) #midpoint number in the ascending array of ride lengths
max(viajes_2_v3$ride_length,na.rm = T) #longest ride
min(viajes_2_v3$ride_length,na.rm = T) #shortest ride


# You can condense the four lines above to one line using summary() on the specific attribute
summary(viajes_2_v3$ride_length_seconds)
summary(viajes_2_v3$ride_length)

# Comparando las variables ride_length y member_casual
aggregate(viajes_2_v3$ride_length ~ viajes_2_v3$member_casual, FUN = mean) # Esto muestra la duración media de un viaje.
aggregate(viajes_2_v3$ride_length ~ viajes_2_v3$member_casual, FUN = median) # Esto muestra la duración media de un viaje, usando la mediana.
aggregate(viajes_2_v3$ride_length ~ viajes_2_v3$member_casual, FUN = max) # Esto muestra la duración máxima de un viaje.
aggregate(viajes_2_v3$ride_length ~ viajes_2_v3$member_casual, FUN = min) # Esto muestra la duración mínima de un viaje.

# See the average ride time by each day for members vs casual users

aggregate(viajes_2_v3$ride_length ~ viajes_2_v3$member_casual + viajes_2_v3$day_of_week, FUN = mean)


# Notice that the days of the week are out of order. Let's fix that.
viajes_2_v3$day_of_week <- ordered(viajes_2_v3$day_of_week, levels=c("Sunday", 
                                                                     "Monday", 
                                                                     "Tuesday", 
                                                                     "Wednesday",
                                                                     "Thursday", 
                                                                     "Friday", 
                                                                     "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users

aggregate(viajes_2_v3$ride_length ~ viajes_2_v3$member_casual + viajes_2_v3$day_of_week, FUN = mean)

viajes_2_v3 %>% 
  group_by(viajes_2_v3$ride_length,viajes_2_v3$member_casual) %>% 
  summarise(mean_2 = mean(day_of_week)) %>% 
  arrange(day_of_week)


# analyze ridership data by type and weekday
viajes_2_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)		

# Let's visualize the number of rides by rider type
viajes_2_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Listo!

# Create a dataframe to export
counts <- aggregate(viajes_2_v3$ride_length ~ viajes_2_v3$member_casual + viajes_2_v3$day_of_week, FUN = mean)
View(counts)
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
write.csv(counts, file = 'C:/Users/Felipe/Documents/Archivos Google/avg_ride_length.csv')








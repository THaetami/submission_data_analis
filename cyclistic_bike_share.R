### Library yang digunakan
library(tidyverse)
library(skimr)
library(ggplot2)

##### mengimpor data dan memeriksa struktur table (kolom)
trip_juli_2022 <- read.csv("202207-divvy-tripdata.csv")
colnames(trip_juli_2022)

trip_agustus_2022 <- read.csv("202208-divvy-tripdata.csv")
colnames(trip_agustus_2022)

trip_september_2022 <- read.csv("202209-divvy-tripdata.csv")
colnames(trip_september_2022)

trip_oktober_2022 <- read.csv("202210-divvy-tripdata.csv")
colnames(trip_oktober_2022)

trip_november_2022 <- read.csv("202211-divvy-tripdata.csv")
colnames(trip_november_2022)

trip_desember_2022 <-read.csv("202212-divvy-tripdata.csv")
colnames(trip_desember_2022)

trip_januari_2023 <- read.csv("202301-divvy-tripdata.csv")
colnames(trip_januari_2023)

trip_febuari_2023 <- read.csv("202302-divvy-tripdata.csv")
colnames(trip_febuari_2023)

trip_maret_2023 <- read.csv("202303-divvy-tripdata.csv")
colnames(trip_maret_2023)

trip_april_2023 <- read.csv("202304-divvy-tripdata.csv")
colnames(trip_april_2023)

trip_mei_2023 <- read.csv("202305-divvy-tripdata.csv")
colnames(trip_mei_2023)

trip_juni_2023 <- read.csv("202306-divvy-tripdata.csv")
colnames(trip_juni_2023)

##### menggabungkan seluruh data 
trip_one_year <- bind_rows(trip_juli_2022, trip_agustus_2022, trip_september_2022, 
                           trip_oktober_2022, trip_november_2022, trip_desember_2022,
                           trip_januari_2023, trip_febuari_2023, trip_maret_2023, 
                           trip_april_2023, trip_mei_2023, trip_juni_2023)

# ====================

##### memeriksa kebersihan data
skim_without_charts(trip_one_year)

##### Menghapus baris dengan nilai hilang/kosong pada variabel "end_lat" dan "end_lng"
clean_data_trip <- trip_one_year[complete.cases(trip_one_year$end_lat, trip_one_year$end_lng), ]

##### memeriksa kebersihan data
skim_without_charts(clean_data_trip)

##### menghapus kolom start_station_name, start_station_id, end_station_name dan end_station_id
data_trip_whitout_station_identify <- subset(clean_data_trip, select = -c(start_station_name, start_station_id, end_station_name, end_station_id))


# ====================

##### function menghitung jarak tempuh
calculate_distance <- function(lat1, lon1, lat2, lon2) {
  # Konversi latitude dan longitude dari derajat ke radian
  lat1 <- lat1 * (pi/180)
  lon1 <- lon1 * (pi/180)
  lat2 <- lat2 * (pi/180)
  lon2 <- lon2 * (pi/180)
  
  # Jari-jari bumi dalam kilometer
  radius <- 6371
  
  # Selisih latitude dan longitude
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  # Rumus Haversine
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance <- radius * c
  
  return(round(distance, 2))
}

##### function menghitung durasi penggunaan sepeda
calculate_duration <- function(started_at, ended_at) {
  # Mengubah string menjadi tipe data waktu
  started_at <- as.POSIXct(started_at)
  ended_at <- as.POSIXct(ended_at)
  
  # Menghitung selisih waktu dalam satuan detik
  duration_sec <- difftime(ended_at, started_at, units = "secs")
  
  # Mengonversi selisih waktu dalam satuan menit
  duration_min <- duration_sec / 60
  
  return(round(duration_min, 2))
}

##### menambahkan kolom milaege dan duration menggunakan function calculate_duratioin() dan calculate_distance()
data_trip_with_mileage_and_duration <- data_trip_whitout_station_identify %>% 
  mutate(mileage_in_km = calculate_distance(start_lat, start_lng, end_lat, end_lng), 
         duration_in_minute = calculate_duration(started_at, ended_at))

##### mengubah tipe data kolom duraion_in_minute menjadi numeric
data_trip_with_mileage_and_duration <- data_trip_with_mileage_and_duration %>%
  mutate(duration_in_minute = as.numeric(duration_in_minute))

##### memeriksa data bisa yg terdapat di kolom end_lat dan end_lng
data_bias <- data_trip_with_mileage_and_duration %>% 
  filter(end_lat == 0 | end_lng == 0)

##### menghapus data bias (data end_lat atau end_lng yang bernilai 0)
data_clean <- subset(data_trip_with_mileage_and_duration, !(end_lat == 0 | end_lng == 0))

##### menghapus data bias (data duration_in_minute yang bernilai <= 0)
data_trip_clean <-subset(data_clean, !(duration_in_minute <= 0))

##### menghapus data bias (data mileage_in_km == 0.00)
data_trip <-subset(data_trip_clean, !(mileage_in_km == 0.00))


# ====================

##### menambahkan kolom baru yakni, kolom date, month, day, year dan day_of_week
data_trip$date <- as.Date(data_trip$started_at) #The default format is yyyy-mm-dd
data_trip$month <- format(as.Date(data_trip$date), "%m")
data_trip$day <- format(as.Date(data_trip$date), "%d")
data_trip$year <- format(as.Date(data_trip$date), "%Y")
data_trip$day_of_week <- format(as.Date(data_trip$date), "%A")


# ====================

##### merangkum data untuk membuat visualisasi
viz_data <- data_trip %>%
  mutate(started_at = as.Date(started_at)) %>%
  mutate(month_list = format(started_at, "%B %Y")) %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, month_list, weekday, rideable_type) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(duration_in_minute),
            average_mileage = mean(mileage_in_km)) %>%
  arrange(member_casual, month_list, weekday, rideable_type)

##### Konversi kolom month_list menjadi tipe data Date
viz_data$month_list <- as.Date(paste0(viz_data$month_list, "-01"), format = "%B %Y-%d")

##### Mengurutkan data berdasarkan kolom month_list
viz_data <- viz_data[order(viz_data$month_list), ]

##### Mengubah urutan nama hari menjadi urutan hari sebenarnya
weekday_order <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
viz_data$weekday <- factor(viz_data$weekday, levels = weekday_order)


# ====================

##### visualisasi pertama menggunakan chart bar
ggplot(viz_data, aes(x = month_list, y = number_of_rides, fill = rideable_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ member_casual, ncol = 1, scales = "free_y") +
  labs(title = "Perbedaan jumlah pengguna Cyclistic setiap bulan (Member vs Casual)",
       x = "Periode",
       y = "Jumlah Pengguna",
       caption = "Data from: Juli 2022 to Juni 2023") +
  theme_minimal()


# ====================

##### Membuat dataset baru yang Menghitung rata-rata pengguna berdasarkan weekday dan member_casual
avg_by_weekday <- viz_data %>% 
  group_by(weekday, member_casual, rideable_type) %>% 
  summarise(average_rides = mean(number_of_rides))

##### visualisasi kedua menggunakan chart kolom
ggplot(avg_by_weekday, aes(x = weekday, y = average_rides, fill = rideable_type)) +
  geom_col() +
  facet_wrap(~ member_casual, scales = "free_y") +
  labs(title = "Jumlah rata-rata pengguna Cyclistic setiap hari (Member vs Casual)",
       x = "Weekday",
       y = "Jumlah Pengguna",
       caption = "Data from: Juli 2022 to Juni 2023") +
  theme_minimal()








#Aktivasi Package rtweet
library(rtweet)

#Membuat Kredensial Akses Twitter dengan API
create_token(app = "JokoAde", consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
             consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
             access_token = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
             access_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")


#Memantau Tweets harga cabain dan cabe
pedas <- search_tweets(q = "harga cabai", n = 3200, lang = "id", retryonratelimit = T)
pedas

#Visualisasi Tweets menurut waktu jam
pedas %>%
  ts_plot(by ="hours")

#Visualisasi Tweets yang diretweet dan tidak
library(dplyr)
pedas %>%
  group_by(is_retweet) %>%
  ts_plot("hours")

#Visualisasi Tweets menurut waktu menit dengan Custome
pedas %>%
  ts_plot(by ="hours") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  labs(x = "Waktu dalam jam",
       y = "Frekuensi",
       title = "Frekuensi Tweet dengan Kata Kunci 'harga cabai'",
       subtitle = "visualisasi: jokoding.com",
       caption = paste0("Sumber: Twitter, tanggal: ", Sys.Date()))


#Visualisasi Wordcloud teks Tweets
pedas %>%
  select(text) %>%
  mutate(text = gsub(pattern = "http\\S+", replacement = "", x = text)) %>%
  mutate(text = gsub(pattern = "#", replacement = "", x = text)) %>%
  mutate(text = gsub(pattern = "@", replacement = "", x = text)) %>%
  mutate(text = gsub(pattern = "[[:punct:]]", replacement = "", x = text)) %>%
  mutate(text = tolower(text)) %>%
  plain_tweets() -> text_cleaned

#Tokenize teks
library(tidytext)
#Mengoreksi bahasa Indonesia
stop_indo <- readLines("~/stopwords-indonesia.txt") %>% as.data.frame()
text_cleaned %>%
  unnest_tokens(input = text, output = token) %>%
  anti_join(stop_indo, by = c("token" = "."))

#Visualisasi
library(wordcloud2)
text_cleaned %>%
  unnest_tokens(input = text, output = token) %>%
  anti_join(stop_indo, by = c("token" = ".")) %>%
  count(token, sort = T) %>%
  wordcloud2(size = 3)

cobacabai <- data.frame(pedas, text_cleaned, stringsAsFactors = F)

#Memfilter Tweets yang mengandung kata harga dari cabai "Ribu" dan "rp" serta "Rp"
library(stringr)
cobacabai %>%
  filter(str_detect(text.1, c("Ribu | rp | Rp"))) %>%
  ts_plot(by = "hours")

#Mendapatkan Tweets yang di dalamnya disertai informasi harga cabai
library(strex)
cobacabai %>%
  filter(str_detect(text.1, c("ribu | rp | Rp"))) -> harga_cabai

#filter yang ada nama wilayahnya
harga_cabai %>%
  filter(location != "") -> hcku

#Filter kolom Teks Tweet
hcku[,c(91)] -> harga

#Fungsi ekstraksi harga pada kalimat Tweet yang mengandung awalan Rp
ekstrak <- function(x){
  gsub('(?i)\\b(rp\\S+)|.\\b(ribu\\S+)(?i)', '\\1', x, perl = TRUE)
}

#Menerapkan Fungsi Ekstraksi harga
sapply(harga, FUN = ekstrak) %>%
  str_replace_all("\n","") %>%
  str_replace_all("/kg","") %>%
  str_replace_all("kg","") %>%
  str_replace_all("kilogram","") %>%
  str_replace_all("!","") %>%
  str_replace_all("rp","-") %>%
  str_replace_all("[[.]]","") %>%
  str_replace_all("[[-]]"," ") %>%
  str_replace_all("per","") %>%
  str_extract_numbers() -> hargaku

str(hargaku)

#Mengubah list dari list ke karakter vektor dengan pemisah koma
sapply(hargaku, paste0, collapse = ";") -> hargaoke

#Membuat Data Frame dari hasil ubahan list dari list ke vektor karakter
library(splitstackshape)
dataharga <- cSplit(data.frame(hargaoke), 'hargaoke', ";")

#Validasi Harga dengan pengecekan teks Tweets sebenarnya
dataharga[dataharga == "2162022"] <- "216000"

#Mengubah tipe dataharga menjadi numerik
df <- as.data.frame(sapply(dataharga, as.numeric))

#Mengganti nilai NA dengan 0
df[is.na(df)] <- 0
str(df)

df[,"oke"] <- pmax(df[1], df[2], df[3], df[4], df[5], df[6], df[7], df[8],na.rm = F)
max(df$oke)

#Menggabungkan hargaku dengang hcku
cabaipedas <- data.frame(hcku, df$oke, stringsAsFactors = F)

#Mengubah nama kolom dataharga.max dengan hrgmax
colnames(cabaipedas)[colnames(cabaipedas) == "df.oke"] = "hrgmax"

#Mengubah tipe hrgmax
cabaipedas$hrgmax <- as.character(cabaipedas$hrgmax)
cabaipedas <- datacabaifix
#Wordcloud tweet harga cabai di Indonesia
cabaipedas %>%
  unnest_tokens(input = text.1, output = token) %>%
  count(token, sort = T) %>%
  wordcloud2(size = 3)

#Wordcloud tweet harga cabai di Jawa Timur
cabaipedas %>%
  filter(location == "Surabaya" | location == "Malang, Jawa Timur" | location == "Jember Jawa Timur Indonesia") %>%
  unnest_tokens(input = hrgmax, output = token) %>%
  count(token, sort = T) %>%
  wordcloud2(size = 1)

#WordCloud Menurut Identitas Media
cabaipedas %>%
  unnest_tokens(input = screen_name, output = token) %>%
  count(token, sort = T) %>%
  wordcloud2(size = 1)

#Mengubah tipe hrgmax
cabaipedas$hrgmax <- as.numeric(cabaipedas$hrgmax)

#Visualisasi
plot(cabaipedas$hrgmax, type = "l",ylim = c(20000, 210000), xlim = c(-20, 169), ylab = c("harga cabai"), col = "blue")
abline(h = mean(cabaipedas$hrgmax), col = "red")
text(-10,113700, c("Rp. 111.619,- per Kg"), cex = 0.7, col = "black")
barplot(cabaipedas$hrgmax, names.arg = cabaipedas$location)

library(ggplot2)
ggplot(data = datacabaifix,
       aes(x=lokasi, y = hrgmax, fill = as.factor(lokasi))) +
  geom_boxplot() +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "Lokasi", y = "Sebaran harga cabai", caption = paste0("Sumber: diolah dari Twitter, ",Sys.Date())) +
  ggtitle("Rentang harga cabai menurut lokasi Tweets 18 - 28 Juni 2022")

ggplot(data = datacabaifix,
       aes(x=reorder(lokasi, hrgmax), y = hrgmax, fill = as.factor(lokasi))) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "Lokasi", y = "Sebaran harga cabai", caption = paste0("Sumber: diolah dari Twitter, ",Sys.Date())) +
  ggtitle("Rentang harga cabai menurut lokasi Tweets 18 - 28 Juni 2022")

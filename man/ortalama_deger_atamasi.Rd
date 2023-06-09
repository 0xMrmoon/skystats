\name{ortalama_deger_atamasi}
\alias{ortalama_deger_atamasi}
\title{Eksik Değerlere Ortalama Değer Atama}
\description{
  Bu fonksiyon, verilen bir veri çerçevesindeki eksik değerlere (NA) ortalama değeri atar ve güncellenmiş veri çerçevesini döndürür.
}
\usage{
  ortalama_deger_atamasi(df)
}
\arguments{
  \item{df}{Veri çerçevesini temsil eden bir nesne.}
}
\details{
  Fonksiyon, veri çerçevesindeki her bir sütun için eksik değerleri (NA) kontrol eder. Eğer bir sütunda eksik değerler varsa, ortalama değeri hesaplayarak eksik değerlere ortalama değeri atar. \code{na.rm = TRUE} parametresi, ortalama hesaplarken eksik değerleri dikkate almaz.

  Fonksiyon, güncellenmiş veri çerçevesini döndürür.
}
\examples{
  veri <- data.frame(a = c(1, 2, NA, 4, 5),
                     b = c(NA, 2, 3, NA, 5)) # Örnek veri çerçevesi
  temiz_veri <- ortalama_deger_atamasi(veri) # Eksik değerlere ortalama değer atar
  temiz_veri
}

\name{medyan_atamasi}
\alias{medyan_atamasi}
\title{Eksik Değerlere Medyan Değeri Atama}
\description{
  Bu fonksiyon, verilen bir veri çerçevesindeki eksik değerlere (NA) medyan değerini atar ve güncellenmiş veri çerçevesini döndürür.
}
\usage{
  medyan_atamasi(df)
}
\arguments{
  \item{df}{Veri çerçevesini temsil eden bir nesne.}
}
\details{
  Fonksiyon, veri çerçevesindeki her bir sütun için eksik değerleri (NA) kontrol eder. Eğer bir sütunda eksik değerler varsa, medyan değerini hesaplayarak eksik değerlere medyan değerini atar. \code{na.rm = TRUE} parametresi, medyan hesaplarken eksik değerleri dikkate almaz.

  Fonksiyon, güncellenmiş veri çerçevesini döndürür.
}
\examples{
  veri <- data.frame(a = c(1, 2, NA, 4, 5),
                     b = c(NA, 2, 3, NA, 5)) # Örnek veri çerçevesi
  temiz_veri <- medyan_atamasi(veri) # Eksik değerlere medyan değeri atar
  temiz_veri
}

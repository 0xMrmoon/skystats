\name{rastgele_deger_atama}
\alias{rastgele_deger_atama}
\title{Eksik Değerlere Rastgele Değer Atama}
\description{
  Bu fonksiyon, verilen bir veri çerçevesindeki eksik değerlere (NA) rastgele bir değer atar ve güncellenmiş veri çerçevesini döndürür.
}
\usage{
  rastgele_deger_atama(df)
}
\arguments{
  \item{df}{Veri çerçevesini temsil eden bir nesne.}
}
\details{
  Fonksiyon, veri çerçevesindeki her bir sütun için eksik değerleri (NA) kontrol eder. Eğer bir sütunda eksik değerler varsa, eksik değerlere rastgele bir değer atar. Bu işlem, eksik değerleri veri çerçevesindeki diğer değerlerden rastgele seçerek gerçekleştirir.

  Fonksiyon, güncellenmiş veri çerçevesini döndürür.
}
\examples{
  veri <- data.frame(a = c(1, 2, NA, 4, 5),
                     b = c(NA, 2, 3, NA, 5)) # Örnek veri çerçevesi
  temiz_veri <- rastgele_deger_atama(veri) # Eksik değerlere rastgele değer atar
  temiz_veri
}

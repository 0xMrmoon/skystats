\name{not_na}
\alias{not_na}
\title{Eksik Değerleri Çıkarma}
\description{
  Bu fonksiyon, verilen bir veri kümesindeki eksik değerleri (NA) çıkarır ve temizlenmiş bir veri kümesini döndürür.
}
\usage{
  not_na(data)
}
\arguments{
  \item{data}{Veri kümesini temsil eden bir nesne.}
}
\details{
  Fonksiyon, veri kümesindeki eksik değerleri çıkarmak için \code{na.omit()} fonksiyonunu kullanır. Eksik değerler (NA) olan gözlemler çıkarılarak temizlenmiş bir veri kümesi döndürülür.
}
\examples{
  veri <- c(1, 2, NA, 4, 5) # Örnek veri kümesi
  temiz_veri <- not_na(veri) # Eksik değerleri çıkar
  temiz_veri
}

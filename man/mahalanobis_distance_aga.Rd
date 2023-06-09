\name{mahalanobis_distance_aga}
\alias{mahalanobis_distance_aga}
\title{Mahalanobis Uzaklığına Göre Aykırı Değerleri Kaldırma}
\description{
  Bu fonksiyon, Mahalanobis uzaklığı kullanarak verilen bir veri kümesindeki aykırı değerleri tespit eder ve kaldırır.
}
\usage{
  mahalanobis_distance_aga(x, p = 0.95)
}
\arguments{
  \item{x}{Veri kümesini temsil eden matris veya veri çerçevesi.}
  \item{p}{Kesim noktasını belirlemek için kullanılan güven seviyesi. Varsayılan değer: 0.95.}
}
\details{
  Fonksiyon, veri kümesindeki aykırı değerleri tespit etmek için Mahalanobis uzaklığını kullanır. Veri kümesindeki her bir gözlemi, veri kümesinin merkezi (ortalaması) ve kovaryans matrisi ile karşılaştırır. Mahalanobis uzaklığı, bir gözlemi diğerlerinden ne kadar uzakta olduğunu ölçer ve aykırı değerleri belirlemek için kullanılır.

  Fonksiyon, Mahalanobis uzaklığı cutoff değerini hesaplar ve belirtilen güven seviyesine (p değeri) göre aykırı değerleri tespit eder. Aykırı değerler veri kümesinden çıkarılarak temizlenmiş bir veri kümesi döndürülür.
}
\examples{
  veri <- matrix(c(1, 2, 3, 4, 100, 1, 2, 3, 4, 5), nrow = 5, ncol = 2) # Örnek veri kümesi
  temiz_veri <- mahalanobis_distance_aga(veri, p = 0.95) # Aykırı değerleri kaldır
  temiz_veri
}

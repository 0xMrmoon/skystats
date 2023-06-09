\name{z_skor_aga}
\alias{z_skor_aga}
\title{Aykırı Değerleri Bulma ve Kaldırma}
\description{
  Bu fonksiyon, verilen bir veri kümesindeki aykırı değerleri tespit eder ve kaldırır.
}
\usage{
  z_skor_aga(x)
}
\arguments{
  \item{x}{Veri kümesini temsil eden sayısal bir vektör.}
}
\details{
  Fonksiyon, veri kümesinin normal dağılıma uygun olup olmadığını kontrol eder. Eğer uygunsa, z-skorunu hesaplar ve ortalamanın 3 standart sapma üzerinde veya altında olan değerleri aykırı değer olarak tanımlar. Aykırı değerleri çıkartılmış bir veri kümesini döndürür.

  Eğer veri kümesi normal dağılıma uygun değilse, fonksiyon bir hata mesajı verir ve parametrik olmayan aykırı değer analiz testlerini uygulamanızı önerir.
}
\examples{
  data <- c(1, 2, 3, 4, 100) # Örnek veri kümesi
  temiz_veri <- z_skor_aga(data) # Aykırı değerleri kaldır
  temiz_veri
}
\seealso{
  \code{\link{shapiro.test}}, \code{\link{sd}}, \code{\link{mean}}
}

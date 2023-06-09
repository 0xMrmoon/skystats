\name{poisson}
\alias{poisson}
\title{Poisson Dağılım Fonksiyonu}
\description{
  Bu fonksiyon, Poisson dağılımı için bir hesaplama yapar.
}
\usage{
  poisson(lambda, x)
}
\arguments{
  \item{lambda}{Poisson dağılımının ortalama parametresi (lambda)}
  \item{x}{Hesaplanacak Poisson dağılımı değeri}
}
\details{
  Bu fonksiyon, verilen ortalama parametre (lambda) ve Poisson dağılımı değeri (x) kullanılarak, Poisson dağılımının olasılık fonksiyonunu hesaplar. Hesaplama sonucu olarak, Poisson dağılımının herhangi bir değeri için olasılığı döndürür.
}
\value{
  Fonksiyon, hesaplanan Poisson dağılımının olasılığını döndürür.
}
\examples{
  poisson(3, 2)
}
\seealso{
  \code{\link{dpois}}
}

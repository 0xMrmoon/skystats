\name{normal_dagılım}
\alias{normal_dagılım}
\title{Normal Dağılım Fonksiyonu}
\description{
  Bu fonksiyon, normal (Gauss) dağılımı için bir hesaplama yapar.
}
\usage{
  normal_dagılım(x, mean = 0, sd = 1)
}
\arguments{
  \item{x}{Hesaplanacak normal dağılım değeri}
  \item{mean}{Normal dağılımın ortalaması (varsayılan değer: 0)}
  \item{sd}{Normal dağılımın standart sapması (varsayılan değer: 1)}
}
\details{
  Bu fonksiyon, verilen normal dağılım değeri (x), ortalaması (mean) ve standart sapması (sd) kullanarak normal dağılımın yoğunluk fonksiyonunu hesaplar. Hesaplama sonucu olarak, normal dağılımın herhangi bir değeri için olasılık yoğunluğunu döndürür.
}
\value{
  Fonksiyon, hesaplanan normal dağılımın olasılık yoğunluğunu döndürür.
}
\examples{
  normal_dagılım(2, mean = 1, sd = 0.5)
}
\seealso{
  \code{\link{dnorm}}
}

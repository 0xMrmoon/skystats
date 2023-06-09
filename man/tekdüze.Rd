\name{tekdüze}
\alias{tekdüze}
\title{Uniform Dağılım Fonksiyonu}
\description{
  Bu fonksiyon, uniform (tekdüze) dağılım için bir hesaplama yapar.
}
\usage{
  tekdüze(b, a)
}
\arguments{
  \item{b}{Üst sınır}
  \item{a}{Alt sınır}
}
\details{
  Bu fonksiyon, uniform dağılımı için verilen üst sınır (b) ve alt sınır (a) değerlerini kullanarak, uniform dağılımın yoğunluk fonksiyonunu hesaplar. Hesaplama sonucu olarak, uniform dağılımın herhangi bir değeri için olasılık yoğunluğunu döndürür.
}
\value{
  Fonksiyon, hesaplanan uniform dağılımın olasılık yoğunluğunu döndürür.
}
\examples{
  tekdüze(10, 0)
}
\seealso{
  \code{\link{runif}}
}

\name{minMax}
\alias{minMax}
\title{Min-Max Normalizasyon Fonksiyonu}
\description{
  Bu fonksiyon, verilen vektörün değerlerini 0 ile 1 arasına normalize eder.
}
\usage{
  minMax(x)
}
\arguments{
  \item{x}{Normalize edilecek vektör}
}
\details{
  Bu fonksiyon, verilen vektörün değerlerini minimum ve maksimum değerlerine göre normalize eder. Normalize edilmiş değerler, 0 ve 1 arasında yer alır. Hesaplama sonucu olarak, normalize edilmiş vektörü döndürür.
}
\value{
  Fonksiyon, normalize edilmiş vektörü döndürür.
}
\examples{
  x <- c(1, 2, 3, 4, 5)
  minMax(x)
}

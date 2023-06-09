\name{aga}
\alias{aga}
\title{aga: Aykırı Değerleri Giderme ve Kutu Grafiği}
\description{
  Bu fonksiyon, bir veri vektöründeki aykırı değerleri giderir ve kutu grafiği çizer.
}
\usage{
  aga(x)
}
\arguments{
  \item{x}{bir sayısal vektör.}
}
\details{
  Fonksiyon, veri vektöründeki aykırı değerleri gidererek \code{minimum} ve \code{maximum} sınırlar arasındaki değerleri içeren yeni bir veri vektörü döndürür. Ayrıca, orijinal ve giderilmiş veri için kutu grafiği çizer.
}
\value{
  Fonksiyon, aykırı değerleri giderilmiş veri vektörünü döndürür.
}
\examples{
  x <- c(1, 2, 3, 4, 5, 10, 15, 20, 100)
  aga(x)
}

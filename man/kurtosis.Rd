\name{kurtosis}
\alias{kurtosis}
\title{Kurtosis Hesaplama Fonksiyonu}
\description{
  Bu fonksiyon, verilen bir sayısal vektörün kurtosis (basıklık) değerini hesaplar.
}
\usage{
  kurtosis(x, pop = TRUE)
}
\arguments{
  \item{x}{Kurtosis hesaplanacak sayısal vektör}
  \item{pop}{Popülasyon kurtosis mi hesaplansın? (varsayılan değer: TRUE)}
}
\details{
  Bu fonksiyon, verilen sayısal vektörün kurtosis değerini hesaplar. Eğer "pop" parametresi TRUE olarak belirtilirse, popülasyon kurtosis hesaplanır. Aksi halde, örneklem kurtosis hesaplanır. Kurtosis değeri, verilen vektörün dördüncü momentini standart sapmanın dördüncü kuvvetine böler ve belli bir düzeltme yapar. Hesaplama sonucu olarak, kurtosis değerini döndürür.
}
\value{
  Fonksiyon, hesaplanan kurtosis değerini döndürür.
}
\examples{
  x <- c(1, 2, 3, 4, 5)
  kurtosis(x)
}

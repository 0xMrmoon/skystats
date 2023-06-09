\name{kantil}
\alias{kantil}
\title{Kantil Hesaplama Fonksiyonu}
\description{
  Bu fonksiyon, verilen bir sayısal vektörün belirli bir yüzdelik dilimdeki kantil değerini hesaplar.
}
\usage{
  kantil(x, q)
}
\arguments{
  \item{x}{Kantil hesaplanacak sayısal vektör}
  \item{q}{Hesaplanacak yüzdelik dilim (0-1 aralığında)}
}
\details{
  Bu fonksiyon, verilen sayısal vektörün belirli bir yüzdelik dilimdeki kantil değerini hesaplar. Yüzdelik dilim, 0 ile 1 arasında bir değer olmalıdır. Örneğin, q=0.5, medyan değerini hesaplar. Hesaplama sonucu olarak, kantil değerini döndürür.
}
\value{
  Fonksiyon, hesaplanan kantil değerini döndürür.
}
\examples{
  x <- c(1, 2, 3, 4, 5)
  kantil(x, 0.25)
}

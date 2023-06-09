\name{jarqueBera}
\alias{jarqueBera}
\title{Jarque-Bera Testi ve Görselleştirme Fonksiyonu}
\description{
  Bu fonksiyon, verilen sayısal bir vektörün Jarque-Bera test istatistiğini hesaplar ve görselleştirme yapar.
}
\usage{
  jarqueBera(z)
}
\arguments{
  \item{z}{Jarque-Bera testi yapılacak sayısal vektör}
}
\details{
  Bu fonksiyon, verilen sayısal bir vektörün Jarque-Bera test istatistiğini hesaplar ve sonucu ekrana basar. Ayrıca, verilen vektör için histogram ve yoğunluk eğrisini görselleştirir. Jarque-Bera testi, verilerin normal dağılıma uygunluğunu test etmek için kullanılır. Hesaplama sonucu olarak, Jarque-Bera test istatistiği değerini döndürür.
}
\value{
  Fonksiyon, Jarque-Bera test istatistiği değerini ekrana basar.
}
\examples{
  z <- c(1, 2, 3, 4, 5)
  jarqueBera(z)
}

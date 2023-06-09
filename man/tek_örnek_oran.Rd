\name{tek_örnek_oran}
\alias{tek_örnek_oran}
\title{Tek Örnek Oran Testi}
\description{
  Bu fonksiyon, tek örnek oran testi yapmak için kullanılır.
}
\usage{
  tek_örnek_oran(n, a, alfa = c(0.01, 0.05), hedef, alternatif = c("Büyüktür", "Küçüktür"))
}
\arguments{
  \item{n}{Örneklem büyüklüğü.}
  \item{a}{Başarı sayısı.}
  \item{alfa}{Kabul düzeyi vektörü. Varsayılan olarak c(0.01, 0.05) kullanılır.}
  \item{hedef}{Test edilen oranın hedef değeri.}
  \item{alternatif}{Testin alternatif hipotezini belirten karakter vektörü. Varsayılan olarak "Büyüktür" seçeneği kullanılır.}
}
\details{
  Bu fonksiyon, verilen bir örneklem için tek örnek oran testi yapar. İlk olarak, oran hesaplanır ve standart hata hesaplanır. Ardından, test istatistiği hesaplanır ve belirtilen kabul düzeyine göre alternatif hipoteze karar verilir.

  Fonksiyon, sonuçları ekrana basar.
}
\examples{
  tek_örnek_oran(100, 50, alfa = c(0.01, 0.05), hedef = 0.5, alternatif = "Büyüktür")
}

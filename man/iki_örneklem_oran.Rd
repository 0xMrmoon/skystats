\name{iki_örneklem_oran}
\alias{iki_örneklem_oran}
\title{İki Örneklem Oran Testi}
\description{
  Bu fonksiyon, iki örneklem oran testi yapmak için kullanılır.
}
\usage{
  iki_örneklem_oran(a1, a2, n1, n2, alternatif = c("Büyüktür", "Küçüktür"))
}
\arguments{
  \item{a1}{Grup 1'deki başarı sayısı.}
  \item{a2}{Grup 2'deki başarı sayısı.}
  \item{n1}{Grup 1'nin örneklem büyüklüğü.}
  \item{n2}{Grup 2'nin örneklem büyüklüğü.}
  \item{alternatif}{Testin alternatif hipotezini belirten karakter vektörü. Varsayılan olarak "Büyüktür" seçeneği kullanılır.}
}
\details{
  Bu fonksiyon, verilen iki grup için iki örneklem oran testi yapar. İlk olarak, her iki grup için oranlar hesaplanır ve standart hata hesaplanır. Ardından, test değeri hesaplanır ve alternatif hipoteze göre karar verilir.

  Varsayılan olarak, alternatif hipotez "Büyüktür" olarak kabul edilir. Bu durumda, test değeri 1.64'ten küçükse, null hipotezi reddedilmez. Alternatif olarak, "Küçüktür" seçeneği seçildiğinde, test değeri 1.64'ten büyükse, null hipotezi reddedilmez.

  Fonksiyon, sonuçları ekrana basar.
}
\examples{
  iki_örneklem_oran(100, 150, 200, 250, "Büyüktür")
}

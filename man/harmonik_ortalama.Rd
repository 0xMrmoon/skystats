\name{harmonik_ortalama}
\alias{harmonik_ortalama}
\title{Harmonik Ortalama}
\description{
  Bu fonksiyon, verilen sayısal vektörün harmonik ortalamasını hesaplar.
}
\usage{
  harmonik_ortalama(x)
}
\arguments{
  \item{x}{Sayısal vektör.}
}
\details{
  Bu fonksiyon, verilen sayısal vektörün (x) harmonik ortalamasını hesaplar. Vektörün her bir elemanının sayısal olup olmadığı kontrol edilir. Eğer sayısal olmayan bir değer bulunursa, bir hata mesajı döndürülür.
}
\examples{
  veri <- c(2, 4, 6) # Örnek veri vektörü
  harmonik_ort <- harmonik_ortalama(veri) # Harmonik ortalamayı hesaplar
  harmonik_ort
}

\name{kareli_ortalama}
\alias{kareli_ortalama}
\title{Kareli Ortalama}
\description{
  Bu fonksiyon, verilen sayısal vektörün kareli ortalamasını hesaplar.
}
\usage{
  kareli_ortalama(x)
}
\arguments{
  \item{x}{Sayısal vektör.}
}
\details{
  Bu fonksiyon, verilen sayısal vektörün (x) kareli ortalamasını hesaplar. Vektörün her bir elemanının sayısal olup olmadığı kontrol edilir. Eğer sayısal olmayan bir değer bulunursa, bir hata mesajı döndürülür.
}
\examples{
  veri <- c(1, 2, 3, 4, 5) # Örnek veri vektörü
  kareli_ort <- kareli_ortalama(veri) # Kareli ortalamayı hesaplar
  kareli_ort
}

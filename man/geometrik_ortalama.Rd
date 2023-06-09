\name{geometrik_ortalama}
\alias{geometrik_ortalama}
\title{Geometrik Ortalama}
\description{
  Bu fonksiyon, verilen sayısal vektörün geometrik ortalamasını hesaplar.
}
\usage{
  geometrik_ortalama(x)
}
\arguments{
  \item{x}{Sayısal vektör.}
}
\details{
  Bu fonksiyon, verilen sayısal vektörün (x) geometrik ortalamasını hesaplar. Vektörün her bir elemanının sayısal olup olmadığı kontrol edilir. Eğer sayısal olmayan bir değer bulunursa, bir hata mesajı döndürülür. Ayrıca, vektörde negatif değerlerin bulunmaması gerektiği kontrol edilir. Negatif değer varsa, bir hata mesajı döndürülür.
}
\examples{
  veri <- c(2, 4, 6) # Örnek veri vektörü
  geom_ort <- geometrik_ortalama(veri) # Geometrik ortalamayı hesaplar
  geom_ort
}

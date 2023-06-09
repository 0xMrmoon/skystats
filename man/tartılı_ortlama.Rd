\name{tartılı_ortlama}
\alias{tartılı_ortlama}
\title{Tartılı Ortalama}
\description{
  Bu fonksiyon, verilen sayısal vektörlerin ve ağırlıkların kullanılarak tartılı ortalamayı hesaplar.
}
\usage{
  tartılı_ortlama(x, w)
}
\arguments{
  \item{x}{Sayısal vektör.}
  \item{w}{Ağırlık vektörü. \code{x} ile aynı uzunlukta olmalıdır.}
}
\details{
  Bu fonksiyon, verilen sayısal vektörlerin (x) ve ağırlıkların (w) kullanılarak tartılı ortalamayı hesaplar. Vektörlerin uzunluklarının eşit olduğu kontrol edilir. Eğer uzunluklar farklı ise, bir hata mesajı döndürülür.

  Fonksiyon, vektörlerin her bir elemanının sayısal olup olmadığını kontrol eder. Eğer sayısal olmayan bir değer bulunursa, bir hata mesajı döndürülür.
}
\examples{
  veri <- c(1, 2, 3, 4, 5) # Örnek veri vektörü
  agirlik <- c(0.2, 0.3, 0.1, 0.4, 0.5) # Örnek ağırlık vektörü
  tartili_ortalama <- tartılı_ortlama(veri, agirlik) # Tartılı ortalamayı hesaplar
  tartili_ortalama
}

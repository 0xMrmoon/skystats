\name{range}
\alias{range}
\title{Değer Aralığı}
\description{
  Bu fonksiyon, verilen bir sayısal vektörün değer aralığını hesaplar.
}
\usage{
  range(x)
}
\arguments{
  \item{x}{Değer aralığı hesaplanacak sayısal vektör.}
}
\details{
  Bu fonksiyon, verilen sayısal vektörün en büyük ve en küçük değerlerini kullanarak değer aralığını hesaplar. Değer aralığı, verinin minimum ve maksimum değerleri arasındaki farkı temsil eder.

  Fonksiyon, verinin her bir elemanının sayısal olup olmadığını kontrol eder. Eğer veride sayısal olmayan bir değer bulunursa, bir hata mesajı döndürülür.
}
\examples{
  veri <- c(10, 20, 30, 40, 50) # Örnek veri vektörü
  aralik <- range(veri) # Değer aralığını hesaplar
  aralik
}

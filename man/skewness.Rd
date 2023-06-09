\name{skewness}
\alias{skewness}
\title{Çarpıklık Hesaplama}
\description{
  Bu fonksiyon, verilen bir sayısal vektörün çarpıklık değerini hesaplar. Çarpıklık, bir dağılımın simetrisini ve kuyruklarının simetrisizliğini ölçer.
}
\usage{
  skewness(x, pop = TRUE)
}
\arguments{
  \item{x}{Çarpıklık hesaplanacak sayısal vektör.}
  \item{pop}{Eğer \code{TRUE} ise, popülasyon çarpıklığı hesaplanır. Eğer \code{FALSE} ise, örneklem çarpıklığı hesaplanır. Varsayılan değer \code{TRUE} olarak ayarlanmıştır.}
}
\details{
  Bu fonksiyon, verilen sayısal vektörün çarpıklık değerini hesaplar. Vektördeki değerlerin sayısal olması gerekmektedir. Fonksiyon, çarpıklığı hesaplarken örneklem ya da popülasyon bazında hesaplama yapabilir.

  Çarpıklık, bir dağılımın simetrisini ve kuyruklarının simetrisizliğini ölçer. Pozitif çarpıklık değerleri, sağa çekik bir dağılımı, negatif çarpıklık değerleri ise sola çekik bir dağılımı gösterir. Sıfır çarpıklık değeri, simetrik bir dağılımı ifade eder.
}
\examples{
  veri <- c(10, 15, 15, 20, 25, 25, 25, 30) # Örnek veri vektörü
  sk <- skewness(veri) # Örneklem çarpıklığını hesaplar
  sk
}

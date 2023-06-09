\name{scaling}
\alias{scaling}
\title{Veri Ölçeklendirme}
\description{
  Bu fonksiyon, verilen bir sayısal vektörü ölçeklendirir. Ölçeklendirme işlemi, veriyi ortalama değere göre merkezlemeyi ve standart sapmaya göre ölçeklendirmeyi sağlar.
}
\usage{
  scaling(x)
}
\arguments{
  \item{x}{Ölçeklendirilecek sayısal vektör.}
}
\details{
  Bu fonksiyon, verilen sayısal vektörü ölçeklendirir. Veri ölçeklendirme, veriyi ortalama değere göre merkezlemeyi ve standart sapmaya göre ölçeklendirmeyi sağlar. Ölçeklendirilmiş veri, daha karşılaştırılabilir ve daha dengeli bir dağılıma sahip olur.

  Fonksiyon, verinin her bir elemanını ortalama değere göre merkezler ve standart sapmaya böler. Böylece, elde edilen sonuç verisi, ortalama değeri 0 ve standart sapması 1 olan bir dağılım sergiler.
}
\examples{
  veri <- c(10, 20, 30, 40, 50) # Örnek veri vektörü
  scaled_veri <- scaling(veri) # Veriyi ölçeklendirir
  scaled_veri
}

\name{kumulatif_seri}
\alias{kumulatif_seri}
\title{Kümülatif Seri Oluşturma}
\description{
  Bu fonksiyon, verilen bir veri vektörünü kümülatif bir veri çerçevesi olarak döndürür. Verilerin frekanslarına dayalı olarak kümülatif frekansları hesaplar.
}
\usage{
  kumulatif_seri(x)
}
\arguments{
  \item{x}{Kümülatif serisi oluşturulacak verileri temsil eden bir vektör.}
}
\details{
  Fonksiyon, verilen veri vektörünü kümülatif bir veri çerçevesine dönüştürür. Verilerin benzersiz değerlerini, frekanslarını ve kümülatif frekanslarını içeren bir veri çerçevesi döndürür.

  Kümülatif frekanslar, verilerin frekanslarına dayalı olarak hesaplanır. Her bir benzersiz değer için frekanslar toplanarak kümülatif frekanslar oluşturulur.
}
\examples{
  veri <- c(10, 15, 15, 20, 25, 25, 25, 30) # Örnek veri vektörü
  kumulatif_seri <- kumulatif_seri(veri) # Veriyi kümülatif bir veri çerçevesine dönüştürür
  kumulatif_seri
}

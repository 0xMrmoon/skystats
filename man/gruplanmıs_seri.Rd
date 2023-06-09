\name{gruplanmıs_seri}
\alias{gruplanmıs_seri}
\title{Gruplanmış Seri Oluşturma}
\description{
  Bu fonksiyon, verilen bir veri vektörünü gruplanmış bir veri çerçevesi olarak döndürür. Veriler grup aralıklarına göre sınıflandırılır ve her grup için frekans ve grup ortalaması hesaplanır.
}
\usage{
  gruplanmıs_seri(x)
}
\arguments{
  \item{x}{Gruplanacak verileri temsil eden bir vektör.}
}
\details{
  Fonksiyon, verilen veri vektörünü gruplanmış bir veri çerçevesine dönüştürür. Veriler grup aralıklarına göre sınıflandırılır ve her grup için frekans ve grup ortalaması hesaplanır.

  Fonksiyon, gruplanmış veri çerçevesini döndürür. Çıktı, grup aralıklarını, frekansları ve grup ortalamalarını içeren bir veri çerçevesidir.
}
\examples{
  veri <- c(10, 15, 18, 22, 25, 30, 32, 36, 40, 45, 50) # Örnek veri vektörü
  gruplanmış_seri <- gruplanmıs_seri(veri) # Veriyi gruplanmış bir veri çerçevesine dönüştürür
  gruplanmış_seri
}

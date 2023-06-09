\name{sınıflanmıs_seri}
\alias{sınıflanmıs_seri}
\title{Sınıflandırılmış Seri Oluşturma}
\description{
  Bu fonksiyon, verilen bir veri vektörünü sınıflandırılmış bir veri çerçevesi olarak döndürür. Sınıflandırma, verilerin benzersiz değerlerini ve frekanslarını içerir.
}
\usage{
  sınıflanmıs_seri(x, p = FALSE)
}
\arguments{
  \item{x}{Sınıflandırılacak verileri temsil eden bir vektör.}
  \item{p}{Oranları hesaplamak için bir mantıksal değer. Varsayılan değeri \code{FALSE}'dir.}
}
\details{
  Fonksiyon, verilen veri vektörünü sınıflandırılmış bir veri çerçevesine dönüştürür. Verilerin benzersiz değerleri ve frekansları çerçevesinde oluşan bir tabloyu döndürür.

  Eğer \code{p = FALSE} olarak belirtilirse, çıktı veri çerçevesi sadece frekansları içerir. Eğer \code{p = TRUE} olarak belirtilirse, çıktı veri çerçevesi frekansların yanı sıra oranları da içerir.

  Fonksiyon, oluşturulan sınıflandırılmış veri çerçevesini ekrana yazdırır.
}
\examples{
  veri <- c("A", "B", "A", "C", "B", "B") # Örnek veri vektörü
  sınıflanmış_veri <- sınıflanmıs_seri(veri, p = FALSE) # Veriyi sınıflandırılmış bir veri çerçevesine dönüştürür
  sınıflanmış_veri

  sınıflanmış_oranlar <- sınıflanmıs_seri(veri, p = TRUE) # Veriyi sınıflandırılmış bir veri çerçevesine dönüştürür ve oranları hesaplar
  sınıflanmış_oranlar
}

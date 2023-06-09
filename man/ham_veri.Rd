\name{ham_veri}
\alias{ham_veri}
\title{Ham Verileri Sıralama}
\description{
  Bu fonksiyon, verilen bir veri vektörünü sıralanmış bir veri çerçevesi olarak döndürür.
}
\usage{
  ham_veri(x)
}
\arguments{
  \item{x}{Sıralanacak verileri temsil eden bir vektör.}
}
\details{
  Fonksiyon, verilen veri vektörünü sıralanmış bir veri çerçevesine dönüştürür. Veriler, "veriler" adlı bir sütunda sıralanır.

  Fonksiyon, sıralanmış veri çerçevesini döndürür.
}
\examples{
  veri <- c(5, 2, 8, 1, 3) # Örnek veri vektörü
  sıralı_veri <- ham_veri(veri) # Veriyi sıralanmış bir veri çerçevesine dönüştürür
  sıralı_veri
}

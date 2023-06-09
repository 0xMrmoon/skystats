\name{plot_missing_values}
\alias{plot_missing_values}
\title{Eksik Veri Dağılımını Görselleştirme}
\description{
  Bu fonksiyon, verilen bir veri çerçevesindeki eksik verilerin dağılımını görselleştirir. Her bir değişkenin eksik veri oranını bar grafiğiyle gösterir.
}
\usage{
  plot_missing_values(dataframe)
}
\arguments{
  \item{dataframe}{Veri çerçevesini temsil eden bir nesne.}
}
\details{
  Fonksiyon, veri çerçevesindeki her bir değişken için eksik verilerin sayısını hesaplar ve eksik veri oranını yüzde olarak hesaplar. Ardından, bu bilgileri kullanarak değişkenlerin eksik veri oranını bar grafiğiyle görselleştirir.

  Fonksiyon, ggplot2 kütüphanesini kullanır ve eksik veri dağılımını gösteren bir grafik çıktısı üretir.
}
\examples{
  veri <- data.frame(a = c(1, 2, NA, 4, 5),
                     b = c(NA, 2, 3, NA, 5)) # Örnek veri çerçevesi
  plot_missing_values(veri) # Eksik veri dağılımını görselleştirir
}

\name{sturges}
\alias{sturges}
\title{Sturges Formülüyle Sınıf Sayısını Hesaplama}
\description{
  Bu fonksiyon, verilen bir örneklem hacmi için Sturges formülünü kullanarak uygun sınıf sayısını hesaplar.
}
\usage{
  sturges(n)
}
\arguments{
  \item{n}{Örneklem hacmini temsil eden bir sayı.}
}
\details{
  Fonksiyon, verilen örneklem hacmi için Sturges formülünü kullanarak uygun sınıf sayısını hesaplar. Sturges formülü, örneklem hacmi ile uyumlu bir sınıf sayısı önermektedir.

  Fonksiyon, hesaplanan sınıf sayısını döndürür.
}
\examples{
  n <- 100 # Örneklem hacmi
  sınıf_sayısı <- sturges(n) # Sturges formülüyle sınıf sayısını hesaplar
  sınıf_sayısı
}

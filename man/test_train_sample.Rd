\name{test_train_sample}
\alias{test_train_sample}
\title{Veri Çerçevesini Eğitim ve Test Kümelerine Ayırma}
\description{
  Bu fonksiyon, verilen bir veri çerçevesini eğitim ve test kümelerine ayırır. Veri çerçevesinin belirtilen yüzdesi kadarını eğitim kümesi olarak kullanırken, geri kalan kısmını test kümesi olarak ayırır.
}
\usage{
  test_train_sample(df, p = 0.80, setseed = 150)
}
\arguments{
  \item{df}{Veri çerçevesini temsil eden bir nesne.}
  \item{p}{Eğitim kümesinin oranı. Varsayılan değer 0.80'dir.}
  \item{setseed}{Rastgele örnekleme için kullanılan tohum değeri. Varsayılan değer 150'dir.}
}
\details{
  Fonksiyon, verilen veri çerçevesini eğitim ve test kümelerine ayırmak için rastgele örnekleme yapar. Belirtilen oranda veri çerçevesinin bir kısmını eğitim kümesi olarak seçerken, geri kalanını test kümesi olarak belirler.

  Fonksiyon, \code{set.seed()} işlevini kullanarak rastgele örnekleme için bir tohum değeri belirler. Bu, rastgele seçilen örneklerin tekrarlanabilir olmasını sağlar.

  Fonksiyon, ayrılan eğitim ve test kümelerini \code{trainSet_} ve \code{testSet_} adında global değişkenlere atar. Kullanıcıya bu değişkenlerin oluşturulduğu mesajını verir.
}
\examples{
  veri <- data.frame(a = c(1, 2, 3, 4, 5),
                     b = c("A", "B", "C", "D", "E")) # Örnek veri çerçevesi
  test_train_sample(veri, p = 0.70, setseed = 123) # Veriyi 0.70 oranında eğitim ve test kümelerine ayırır
  trainSet_ # Eğitim kümesini kontrol eder
  testSet_  # Test kümesini kontrol eder
}

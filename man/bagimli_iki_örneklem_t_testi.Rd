\name{bagimli_iki_örneklem_t_testi}
\alias{bagimli_iki_örneklem_t_testi}
\title{Bağımlı İki Örneklem T Testi}
\description{
  Bu fonksiyon, bağımlı iki örneklem T testi yapmak için kullanılır.
}
\usage{
  bagimli_iki_örneklem_t_testi(grup1, grup2, a)
}
\arguments{
  \item{grup1}{İlk grup verileri.}
  \item{grup2}{İkinci grup verileri.}
  \item{a}{Anlamlılık düzeyi.}
}
\details{
  Bu fonksiyon, verilen iki grup verisi (grup1 ve grup2) için bağımlı iki örneklem T testi yapar. İlk olarak, her iki grup için normal dağılım varsayımını kontrol eder ve sonuçları ekrana basar. Ardından, grupların varyans homojenliğini Levene testiyle kontrol eder ve sonucunu ekrana basar.

  Daha sonra, grup1 ve grup2 arasındaki farkın istatistiksel olarak anlamlı olup olmadığını tespit etmek için T istatistiği hesaplanır. P değeri hesaplanır ve anlamlılık düzeyiyle karşılaştırılır. Son olarak, hipotez testi sonuçları ve grup istatistikleri ekrana basılır.
}
\examples{
  grup1 <- c(5, 7, 9, 11, 13) # Örnek veri grupları
  grup2 <- c(6, 8, 10, 12, 14)
  bagimli_iki_örneklem_t_testi(grup1, grup2, 0.05)
}

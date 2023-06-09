\name{cift_yonlu_anaova_uygula}
\alias{cift_yonlu_anaova_uygula}
\title{Çift Yönlü ANOVA Analizi Uygulama}
\description{
Verilen veri çerçevesinde ve iki grup değişkeninin adlarını kullanarak bir çift yönlü ANOVA analizi uygulama.
}
\usage{
cift_yonlu_anaova_uygula(veri, grup_degiskeni1, grup_degiskeni2, bagimli_degisken)
}
\arguments{
  \item{veri}{Veri çerçevesi}
  \item{grup_degiskeni1}{İlk grup değişkeninin adı (faktör)}
  \item{grup_degiskeni2}{İkinci grup değişkeninin adı (faktör)}
  \item{bagimli_degisken}{Bağımlı değişkenin adı (sayısal)}
}
\value{
Çift yönlü ANOVA analizi sonuçları
}
\examples{
veri <- data.frame(Ders = rep(c("Matematik", "Fizik", "Kimya"), each = 10),
                   Sinif = rep(c("A", "B", "C"), times = 10),
                   Performans = c(78, 85, 80, 92, 88, 90, 82, 87, 84, 81,
                                  76, 79, 82, 86, 89, 84, 88, 90, 83, 85,
                                  81, 77, 75, 80, 85, 82, 86,

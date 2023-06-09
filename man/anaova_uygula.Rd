\name{anaova_uygula}
\alias{anaova_uygula}
\title{ANOVA Analizi Uygulama}
\description{
Uygulamak için verilen veri çerçevesinde ve grup ve bağımlı değişkenlerin adlarını kullanarak bir ANOVA analizi.
}
\usage{
anaova_uygula(veri, grup_degiskeni, bagimli_degisken)
}
\arguments{
  \item{veri}{Veri çerçevesi}
  \item{grup_degiskeni}{Grup değişkeninin adı (faktör)}
  \item{bagimli_degisken}{Bağımlı değişkenin adı (sayısal)}
}
\value{
ANOVA analizi sonuçları
}
\examples{
veri <- data.frame(Grup = rep(c("A", "B", "C"), each = 10),
                   Bagimli = rnorm(30))
sonuclar <- anaova_uygula(veri, "Grup", "Bagimli")
print(sonuclar)
}

if (!require(crayon, quietly = TRUE)) {
  install.packages("crayon")
  library(crayon)
} else {
  library(crayon)
}
if (!require(car, quietly = TRUE)) {
  install.packages("car")
  library(car)
} else {
  library(car)
}
sturges <- function(n){
  if (n <= 0){
    stop("Örneklem hacmi pozitif bir değer olmalıdır.")}
  k = 1+3.322*log10(n)
  return(k)}
ham_veri <- function(x){
  df <- data.frame("veriler"=sort(x))
  return(df)}
sınıflanmıs_seri <- function(x,p=FALSE){
  if(p==FALSE){
    df <- data.frame("Veriler" = unique(x),
                     "Frekanslar"=as.numeric(table(x)))
    print(df)
  }
  if(p == TRUE){
    df <- data.frame("Veriler" = unique(x),
                     "Frekanslar"=as.numeric(table(x)),
                     "Oranlar" = as.numeric(table(x)) /
                       sum(as.numeric(table(x))))
    print(df)}}
gruplanmıs_seri <- function(x) {
  # x: gruplanacak vektör

  # Veri setinin boyutunu alınması
  n <- length(x)

  # Sturges kuralına göre grup sayısının hesaplanması
  k <- sturges(n) + 1
  print(k)
  # Minimum ve maksimum değerlerin bulunması
  min_val <- min(x)
  max_val <- max(x)

  # Grup aralıklarının belirlenmesi
  interval_width <- ceiling((max_val - min_val) / k)
  intervals <- seq(min_val, max_val, by = interval_width)
  intervals <- c(intervals, max_val + interval_width) # son aralıkta dahil edilsin

  # Gruplar için etiketlerin oluşturulması
  labels <- paste0(intervals[-length(intervals)], "-", intervals[-1] - 1)

  # Gruplanmış seri hesaplama
  freq_table <- table(cut(x, breaks = intervals, labels = labels, include.lowest = TRUE))

  # Grup ortalama hesaplama
  mean_table <- tapply(x, cut(x, breaks = intervals, labels = labels, include.lowest = TRUE), mean)

  # Gruplanmış seri ve grup ortalamalarının birleştirilmesi
  output <- cbind.data.frame(freq_table, mean_table)

  # Çıktının dönülmesi
  return(output)}
kumulatif_seri <- function(x){
  df <- data.frame("Veriler" = unique(x),
                   "Frekanslar"=as.numeric(table(x)))
  denaz = vector()
  toplam = 0
  for (i in 1:length(df$Frekanslar)){
    toplam = toplam + df$Frekanslar[i]
    denaz[i] = toplam}

  df <- data.frame("Veriler" = unique(x),
                   "Frekanslar"=as.numeric(table(x)),
                   "DENAZ"=denaz)
  return(df)}
varyans <- function(x,pop=TRUE){
  for(i in x ){
    if (class(i) !="numeric"){
      stop("Lütfen sayısal değerler giriniz.")}}
  toplam = 0
  if(pop==TRUE){
    for(i in x){
      toplam = toplam + (i-mean(x))**2}
    return(toplam/length(x))

  }else{
    for(i in x){
      toplam = toplam + (i-mean(x))**2}
    return(toplam/(length(x)-1))}}
standartSapma <- function(x,pop=T){
  toplam = 0
  if (pop == T){
    for(i in x){
      toplam = toplam + (i-mean(x))**2}
    toplam = toplam/length(x)
    toplam = sqrt(toplam)
    return(toplam)}
  else if(pop != T){
    for(i in x){
      toplam = toplam + (i-mean(x))**2}
    toplam = toplam/(length(x)-1)
    toplam = sqrt(toplam)
    return(toplam)}}
skewness <- function(x,pop=T){
  for(i in x ){
    if (class(i) !="numeric"){
      stop("Lütfen sayısal değerler giriniz.")}}
  toplam = 0
  if(pop==FALSE){
    for(i in x){
      toplam = toplam + (i-mean(x))**3}
    toplam = toplam / standartSapma(x,pop=FALSE)**3
    toplam = toplam/(length(x)-1)
    return(toplam)
  }else{
    for(i in x){
      toplam = toplam + (i-mean(x))**3}
    toplam = toplam / standartSapma(x,pop=FALSE)**3
    toplam = toplam/(length(x))
    return(toplam)}}
scaling <- function(x){
  return((x-mean(x))/sd(x))}
range <- function(x){
  for(i in x ){
    if (class(i) !="numeric"){
      stop("Lütfen sayısal değerler giriniz.")}}
  return(max(x)-min(x))}
ortalama <- function(x) {
  for(i in x ){
    if (class(i) !="numeric"){
      stop("Lütfen sayısal değerler giriniz.")}}
  toplam = 0
  for(i in x){
    toplam = toplam + i}
  return(toplam/length(x))}
geometrik_ortalama <- function(x) {
  for(i in x ){
    if (class(i) !="numeric"){
      stop("Lütfen sayısal değerler giriniz.")}}
  if (any(x < 0)) {
    stop("Geometrik ortalama hesaplamak için sadece pozitif değerleri kullanmalısınız.")}

  carpim <- prod(x)
  sayi <- carpim
  kok_derecesi <- length(x)
  kok <- sayi ^ (1/kok_derecesi)
  return(kok)}
harmonik_ortalama <- function(x) {
  for(i in x ){
    if (class(i) !="numeric"){
      stop("Lütfen sayısal değerler giriniz.")}}
  liste <- 1/x
  ho <- length(x) / sum(liste)
  return(ho)}
kareli_ortalama <- function(x) {
  for(i in x ){
    if (class(i) !="numeric"){
      stop("Lütfen sayısal değerler giriniz.")}}
  kareler <- x^2
  kareler_ortalamasi <- mean(kareler)
  kareli_ortalama <- sqrt(kareler_ortalamasi)
  return(kareli_ortalama)}
tartılı_ortlama <- function(x, w) {
  # x ve w listelerinin uzunluklarının aynı olduğundan emin olalım
  if (length(x) != length(w)) {
    stop("girilen vektörlerin uzunlukları eşit olmalıdır.")}
  if (class(x)!="numeric" | class(w)!="numeric"){
    stop("Vektörler sadece sayılardan oluşmalıdır.")
  }

  # sonuç listesini ve toplamları tanımlayalım
  sonuc <- numeric(length(x))
  toplam <- 0
  toplam2 <- 0

  # verileri ağırlıklarıyla çarpıp sonuç listesine ekleyelim
  for (i in 1:length(x)) {
    sonuc[i] <- x[i] * w[i]
  }

  # toplamları hesaplayalım
  toplam <- sum(sonuc)
  toplam2 <- sum(w)

  # ağırlıklı ortalama hesabı
  return(toplam / toplam2)}
minMax <- function(x){
  return((x-min(x))/(max(x)-min(x)))}
kurtosis <- function(x,pop=T){
  for(i in x ){
    if (class(i) !="numeric"){
      stop("Lütfen sayısal değerler giriniz.")}}
  toplam = 0
  if(pop==FALSE){
    for(i in x){
      toplam = toplam + (i-mean(x))**4}
    toplam = toplam / standartSapma(x,pop=FALSE)**4
    toplam = toplam/(length(x)-1)
    return(toplam-3)
  }else{
    for(i in x){
      toplam = toplam + (i-mean(x))**4}
    toplam = toplam / standartSapma(x,pop=FALSE)**4
    toplam = toplam/(length(x))
    return(toplam-3)}}
kantil <- function(x,q){
  return(quantile(x,q))}
jarqueBera <- function(z){
  for(i in z ){
    if (class(i) !="numeric"){
      stop("Lütfen sayısal değerler giriniz.")}}
  sturges <- function(veri){
    round(1+3.322*log10(veri))}
  k=2
  a=(length(z)-k)/6
  b=(skewness(z))**2
  c=(((kurtosis(z))**2)/4)

  sonuc = a*(b+c)

  cat("Jarque-Bera Test İstatistiği : ",sonuc,"\n")

  par(mfrow=c(1,2))

  hist(z,xlab=c("GÖZLEM DEĞERLERİ"),

       ylab=c("FREKANSLAR"),

       col=c("#ffeaa7"),

       main=c("Histogram Grafiği"),

       breaks=sturges(length(z)))

  hist(z,xlab=c("GÖZLEM DEĞERLERİ"),

       ylab=c("ORANLAR"),

       col=c("#ffeaa7"),

       main=c("HİSTOGRAM GRAFİĞİ" ,"\n" ,"VE POLİGON EĞRİSİ"),

       breaks=sturges(length(z)),

       prob=T)

  lines(density(z,adjust=2),col="red",lwd=3,lty="dotted")

  print("Ho:Veriler teorik normal dağılma uygundur")

  print("H1:Veriler teorik normal dağılma uygun değildir.")

  print("Güven Düzeyi : 95")

  cat("Gözlem Sayısı",length(z),"\n")

  cat("Çarpıklık Değeri",skewness(z),"\n")

  cat("Basıklık Değeri",kurtosis(z),"\n")

  if (sonuc > 5.991){

    print("Ho hipotezi reddedilir,veriler teorik normal dağılıma uygun değildir")

  }else{

    print("Ho hipotezi reddedilemez,veriler teorik normal dağılma uygundur.")}}
isr <- function(x) {
  q3 <- quantile(x, 0.75)
  q1 <- quantile(x, 0.25)
  isr <- (q3 - q1) / 2
  return(isr[[1]])}
iqr <- function(x) {
  q3 <- quantile(x, 0.75)
  q1 <- quantile(x, 0.25)
  iqr <- q3 - q1
  return(iqr[[1]])}
ipr <- function(x) {
  p75 <- quantile(x, 0.75)
  p25 <- quantile(x, 0.25)
  ipr <- p75 - p25
  return(ipr[[1]])}
idr <- function(x) {
  p90 <- quantile(x, 0.90)
  p10 <- quantile(x, 0.10)
  idr <- p90 - p10
  return(idr[[1]])}
cv <- function(x) {
  for(i in x ){
    if (class(i) !="numeric"){
      stop("Lütfen sayısal değerler giriniz.")}}
  (standartSapma(x)/mean(x))*100}
binom <- function(n, p, x, cumulative = FALSE) {
  if (p < 0.05) {
    warning("İlgilenilen olayın ortaya çıkma olasılığının %5'ten büyük veya eşit olması gerekmektedir.")
  }

  if (cumulative) {
    # Kumulatif olasılık hesaplaması
    cum_prob <- 0
    for (i in 0:x) {
      cum_prob <- cum_prob + choose(n, i) * p**i * (1-p)**(n-i)
    }
    return(cum_prob)
  } else {
    # Normal olasılık hesaplaması
    return(choose(n, x) * p**x * (1-p)**(n-x))
  }
}
poisson <- function(lambda,x){
  return(2.71828**-lambda*lambda**x)/(factorial(x))}
hipergeometrik <- function(M,N,n,x){
  return(choose(M,x)*choose((N-M),(n-x))/(choose(N,n)))}
tekdüze <- function(b,a){
  return(1/(b-a))}
normal_dagılım <- function(x, mean = 0, sd = 1) {
  exponent <- -((x-mean)^2)/(2*sd^2)
  constant <- 1/(sqrt(2*pi)*sd)
  density <- constant*exp(exponent)
  return(density)}
describe <- function(df){
  if (!require(pander, quietly = TRUE)) {
    install.packages("pander")
    library(pander)
  } else {
    library(pander)}


  if (!is.data.frame(df)) {
    stop("Girdi, bir veri çerçevesi (data frame) olmalıdır.")
  }

  if (ncol(df) == 0) {
    stop("Veri çerçevesinde sütun bulunamadı.")
  }

  df <- na.omit(df)

  mean = vector()
  sd   = vector()
  var  = vector()
  kareli_ortalama = vector()
  harmonik_ortalama = vector()
  q1 = vector()
  median = vector()
  q3 = vector()
  iqr = vector()
  idr = vector()
  ipr = vector()
  min = vector()
  max = vector()
  n = vector()
  shapiro = vector()

  for(i in 1:ncol(df)){
    mean[i] = mean(df[[i]])
    sd[i] = sd(df[[i]])
    var[i] = var(df[[i]])
    kareli_ortalama[i] = kareli_ortalama(df[[i]])
    harmonik_ortalama[i] = harmonik_ortalama(df[[i]])
    q1[i]=quantile(df[[i]])[[2]]
    median[i] = median(df[[i]])
    q3[i] =quantile(df[[i]])[[4]]
    iqr[i] = iqr(df[[i]])
    idr[i] = idr(df[[i]])
    ipr[i] = ipr(df[[i]])
    min[i] = min(df[[i]])
    max[i] = max(df[[i]])
    n[i] = length(df[[i]])
    shapiro[i]= round(shapiro.test(df[[i]])[[2]],4)
  }

  describe <- data.frame("n"=n,"Ortalama" = mean, "StandartSapma" = sd, "Varyans" = var,
                         "KareliOrtalama" = kareli_ortalama,"Harmoik_Ortalama"=harmonik_ortalama,
                         "Min"=min,"Max"=max,"IQR"=iqr,"IDR"=idr,"IPR"=ipr,
                         "q1"=q1, "median"=median, "q3"=q3,"Shapiro"=shapiro)

  rownames(describe) <- colnames(df)

  View(describe)
}
bd_shapiro <- function(x,n=1000){
  if(length(x)<=120){
    return(shapiro.test(x))}
  else{
    pvec <- vector()
    for(i in 1:n){
      sample = sample(x,size=30,replace = TRUE)
      pvec[i]<- shapiro.test(sample)[[2]]}}

  kabulRed <- vector()
  for(i in 1:length(pvec)){
    if(pvec[i]>0.05){
      kabulRed[i] = "H0 REDDEDİLEMEZ"
    }else{
      kabulRed[i] = "HO RED"}}
  return(table(kabulRed))}
bir_örneklem_t <- function(x,a=0.05,ctt =T,hedef){
  #n tane gözlem değerinden oluşan  bir örnek için  skewness
  pay_skewness <- function(x){
    toplam = 0
    for (i in x){
      q = (i - mean(x))**3
      toplam = toplam + q
    }
    sonuc = toplam/length(x)

  }

  payda_skewness <- function(qq){
    toplam = 0
    for(i in qq){
      q = (i - mean(qq))**2
      toplam = toplam + q
    }
    sonuc=(toplam/(length(qq)-1))**(1.5)

  }


  skewness_tutorial <- function(x){
    sonuc = pay_skewness(x)/payda_skewness(x)

  }
  #Kontrol
  test_verisi <- c(10,15,30,40,23,43,23,43,54,76,65,43,12,34,54,65,40)
  skewness_tutorial(test_verisi)
  #Sonuç diğer hesaplayıcılar ile arasında fark yok.


  #n tane gözlem değerinden oluşan bir örnek için kurtosis


  pay_kurtosis <- function(y){
    toplam2=0
    for(i in y){
      z= (i - mean(y))**4
      toplam2= toplam2 + z
    }
    sonuc = toplam2/(length(y))
  }

  payda_kurtosis <- function(y){
    toplam2=0
    for(i in y){
      z=(i-mean(y))**2
      toplam2= toplam2 + z
    }
    sonuc = toplam2/length(y)
    sonuc = sonuc**2
  }


  kurtosis_tutorial <- function(y){
    sonuc = (pay_kurtosis(y)/payda_kurtosis(y))
  }
  #Sonuç vikipedi örneği ile kıyaslanmış ve aynı sonuç elde edilmiştir.
  #Örneklem için jarque bera test istatistiği
  sturges <- function(veri){
    round(1+3.322*log10(veri))
  }
  jarque_bera <- function(z){
    k=2
    a=(length(z)-k)/6
    b=(skewness_tutorial(z))**2
    c=(((kurtosis_tutorial(z)-3)**2)/4)
    sonuc = a*(b+c)
    cat(green("Jarque-Bera Test İstatistiği : ",sonuc,"\n"))
    cat(green("Shapiro P-Value Değeri :",shapiro.test(x)[[2]],"\n"))
    par(mfrow=c(1,2))
    hist(z,xlab=c("GÖZLEM DEĞERLERİ"),
         ylab=c("FREKANSLAR"),
         col=c("#ffeaa7"),
         main=c("Histogram Grafiği"),
         breaks=sturges(length(z)))
    hist(z,xlab=c("GÖZLEM DEĞERLERİ"),
         ylab=c("ORANLAR"),
         col=c("#ffeaa7"),
         main=c("HİSTOGRAM GRAFİĞİ" ,"\n" ,"VE POLİGON EĞRİSİ"),
         breaks=sturges(length(z)),
         prob=T)
    lines(density(z,adjust=2),col="red",lwd=3,lty="dotted")
    print("Ho:Veriler teorik normal dağılma uygundur")
    print("H1:Veriler teorik normal dağılma uygun değildir.")
    print("Güven Düzeyi : 95")
    cat("Gözlem Sayısı",length(z),"\n")
    cat("Çarpıklık Değeri",skewness_tutorial(z),"\n")
    cat("Basıklık Değeri",kurtosis_tutorial(z),"\n")
    if (abs(sonuc) > 5.991){
      print("Ho hipotezi reddedilir,veriler teorik normal dağılıma uygun değildir")
    }else{
      print("Ho hipotezi reddedilemez,veriler teorik normal dağılma uygundur.")}

  }
  kdç = c(12.706,4.303,3.182,2.776,2.571,
          2.447,2.365,2.306,2.262,2.228,
          2.201,2.179,2.160,2.145,2.131,
          2.120,2.110,2.101,2.093,2.086,
          2.080,2.074,2.069,2.064,2.060,
          2.056,2.052,2.048,2.045,2.042,rep(2.021,10),rep(2.009,10),rep(2,10),
          rep(1.99,10),rep(1.984,10),rep(1.98,10),rep(1.96,100000))
  kdt = c(6.314,2.920,2.353,2.132,2.015,
          1.943,1.895,1.860,1.833,1.812,
          1.796,1.782,1.771,1.761,1.753,
          1.746,1.74,1.734,1.729,1.725,
          1.721,1.717,1.714,1.711,1.708,
          1.706,1.703,1.701,1.699,1.697,rep(1.684,10),rep(1.676,10),rep(1.671,10),
          rep(1.664,10),rep(1.660,10),rep(1.658,10),rep(1.645,100000))
  kritik_t <- data.frame("kdçtt"=kdç,"kdttt"=kdt)
  x_ort = mean(x)
  x_sd  = sd(x)
  x_sh  = x_sd/sqrt(length(x))
  v     = length(x)-1
  t     = abs((x_ort-hedef)/x_sh)
  kd    = kritik_t[v,"kdçtt"]
  #cat(blue("Hello", "world!\n"))
  cat(cyan("İstatistikler Hesaplanıyor."),"\n")
  Sys.sleep(0.5)
  df <- (data.frame("İstatistikler"=c(x_ort,x_sd,x_sh,v,t,kd)))
  rownames(df) <- c("Ortalama","Standart Sapma","Standart Hata","Serbestlik Derecesi","T Test Değeri","Kritik Değer")
  print(df)
  cat(cyan("Normallik Varsayımı Kontrol Ediliyor..."),"\n")
  Sys.sleep(0.5)
  jarque_bera(x)
  cat(red("Güven Aralığı Hesaplanıyor. Güvenirlilik Düzeyi % 95 "),"\n")
  Sys.sleep(0.5)
  cat(red("Güven Aralığı"),"\n")
  cat(red(x_ort-((kritik_t[(length(x)-1),"kdttt"])*x_sh),red("<"),red("μ"),red("<"),red(x_ort+((kritik_t[(length(x)-1),"kdttt"])*x_sh))),"\n")
  Sys.sleep(0.5)
  cat(blue("Tek Örneklem t Testine Geçiliyor."),"\n")
  Sys.sleep(0.5)
  cat(blue("Verilerinizin Normallik Varsayımını Sağladına Emin Olun"),"\n")
  Sys.sleep(0.5)
  cat(blue("Hipotezler Oluşturuluyor"),"\n")
  Sys.sleep(0.5)
  cat("Ho:μ =",hedef,"\n" )
  cat("H1:μ ≠",hedef,"\n")
  Sys.sleep(0.5)
  cat(blue("Karar Veriliyor"),"\n")
  if(abs(t)>kd){
    cat(cyan("Ho Hipotezi Reddedilir."),"\n")
    cat("Test Değeri :" ,t, ">", "Kritik Değer : " ,kd)

  }else{
    cat(cyan("Ho Hipotezi Reddedilemez."),"\n")
    cat("Test Değeri :" ,t, "<", "Kritik Değer : " ,kd)}}
iki_örneklem_t <- function(x,alternatif="Eşit"){
  if (any(is.na(df)) == TRUE){
    warning("Lütfen eksik değerlerinizi mice paketi ile doldurmayı deneyin.('Bu sadece bir uyarı mesajıdır!')")
  }
  x <- na.omit(x)
  #n tane gözlem değerinden oluşan  bir örnek için  skewness
  pay_skewness <- function(x){
    toplam = 0
    for (i in x){
      q = (i - mean(x))**3
      toplam = toplam + q
    }
    sonuc = toplam/length(x)

  }

  payda_skewness <- function(qq){
    toplam = 0
    for(i in qq){
      q = (i - mean(qq))**2
      toplam = toplam + q
    }
    sonuc=(toplam/(length(qq)-1))**(1.5)

  }


  skewness_tutorial <- function(x){
    sonuc = pay_skewness(x)/payda_skewness(x)

  }
  #Kontrol
  test_verisi <- c(10,15,30,40,23,43,23,43,54,76,65,43,12,34,54,65,40)
  skewness_tutorial(test_verisi)
  #Sonuç diğer hesaplayıcılar ile arasında fark yok.


  #n tane gözlem değerinden oluşan bir örnek için kurtosis


  pay_kurtosis <- function(y){
    toplam2=0
    for(i in y){
      z= (i - mean(y))**4
      toplam2= toplam2 + z
    }
    sonuc = toplam2/(length(y))
  }

  payda_kurtosis <- function(y){
    toplam2=0
    for(i in y){
      z=(i-mean(y))**2
      toplam2= toplam2 + z
    }
    sonuc = toplam2/length(y)
    sonuc = sonuc**2
  }


  kurtosis_tutorial <- function(y){
    sonuc = (pay_kurtosis(y)/payda_kurtosis(y))
  }

  #Sonuç vikipedi örneği ile kıyaslanmış ve aynı sonuç elde edilmiştir.


  #Örneklem için jarque bera test istatistiği



  sturges <- function(veri){
    round(1+3.322*log10((veri)))
  }
  jarque_bera <- function(z){
    k=2
    a=(length(z)-k)/6
    b=(skewness_tutorial(z))**2
    c=(((kurtosis_tutorial(z)-3)**2)/4)
    sonuc = a*(b+c)
    cat("Jarque-Bera Test İstatistiği : ",sonuc,"\n")
    Sys.sleep(0.9)
    par(mfrow=c(1,2))
    hist(z,xlab=c("GÖZLEM DEĞERLERİ"),
         ylab=c("FREKANSLAR"),
         col=c("#ffeaa7"),
         main=c("Histogram Grafiği"),
         breaks=sturges(length(z)))
    hist(z,xlab=c("GÖZLEM DEĞERLERİ"),
         ylab=c("ORANLAR"),
         col=c("#ffeaa7"),
         main=c("HİSTOGRAM GRAFİĞİ" ,"\n" ,"VE POLİGON EĞRİSİ"),
         breaks=sturges(length(z)),
         prob=T)
    lines(density(z,adjust=2),col="red",lwd=3,lty="dotted")
    cat("Ho:Veriler teorik normal dağılma uygundur","\n")
    cat("H1:Veriler teorik normal dağılma uygun değildir.","\n")
    Sys.sleep(0.9)
    cat("Güven Düzeyi : 95","\n")
    Sys.sleep(0.9)
    cat("Gözlem Sayısı",length(z),"\n","\n")
    Sys.sleep(0.9)
    cat("Çarpıklık Değeri",skewness_tutorial(z),"\n")
    Sys.sleep(0.9)
    cat("Basıklık Değeri",kurtosis_tutorial(z),"\n")
    if (abs(sonuc) > 5.991){
      cat("Ho hipotezi reddedilir,veriler teorik normal dağılıma uygun değildir","\n")
    }else{
      cat("Ho hipotezi reddedilemez,veriler teorik normal dağılma uygundur.","\n")}

  }


  kdç = c(12.706,4.303,3.182,2.776,2.571,
          2.447,2.365,2.306,2.262,2.228,
          2.201,2.179,2.160,2.145,2.131,
          2.120,2.110,2.101,2.093,2.086,
          2.080,2.074,2.069,2.064,2.060,
          2.056,2.052,2.048,2.045,2.042,rep(2.021,10),rep(2.009,10),rep(2,10),
          rep(1.99,10),rep(1.984,10),rep(1.98,10),rep(1.96,100000))
  kdt = c(6.314,2.920,2.353,2.132,2.015,
          1.943,1.895,1.860,1.833,1.812,
          1.796,1.782,1.771,1.761,1.753,
          1.746,1.74,1.734,1.729,1.725,
          1.721,1.717,1.714,1.711,1.708,
          1.706,1.703,1.701,1.699,1.697,rep(1.684,10),rep(1.676,10),rep(1.671,10),
          rep(1.664,10),rep(1.660,10),rep(1.658,10),rep(1.645,100000))
  kritik_t <- data.frame("kdçtt"=kdç,"kdttt"=kdt)
  cat(green("İKİ ANAKÜTLE ORTALAMASINA İLİŞKİN BAĞIMSIZ İKİ ÖRNEKLEM T TESTİ"),"\n")
  Sys.sleep(0.5)
  cat(cyan("Normallik Testi Yapılıyor"),"\n")
  Sys.sleep(0.5)
  cat(cyan("1.Örneklem İçin Normallik Testi Yapılıyor"),"\n")
  Sys.sleep(0.5)
  jarque_bera(x[,1])
  cat(cyan("2.Örneklem İçin Normallik Testi Yapılıyor"),"\n")
  Sys.sleep(0.5)
  jarque_bera(x[,2])
  cat(red("Her iki örnekleminde normal dağılıma yakın olduğuna emin olun."),"\n")
  Sys.sleep(0.5)
  cat(cyan("Varyans Homojenliği Test Ediliyor"),"\n")
  Sys.sleep(0.5)
  cat(cyan("Varyans Homojenliği İçin Hipotezler Oluşturuluyor."),"\n")
  Sys.sleep(0.5)
  cat(cyan("Ho: İki Örneklem Aynı Anakütleden Gelmiştir"),"\n")
  cat(cyan("H1: İki Örneklem Aynı Anakütleden Gelmemiştir."),"\n")
  if(bartlett.test(x)[[3]] > 0.05){
    cat(cyan(bartlett.test(x)[[3]]),">",cyan(0.05),cyan("Ho Reddedilemez"),"\n")
    cat(green("Varyans Homojenliği Varsayımı Sağlandı"),"\n")
  }else{
    cat(cyan(bartlett.test(x)[[3]]),"<",cyan(0.05),cyan("Ho Red"),"\n")
    cat(red("Varyans Homojenliği Varsayımı Sağlanamadı","\n"))}


  ort_1 = mean(x[,1])
  ort_2 = mean(x[,2])
  sd_1  = sd(x[,1])
  sd_2  = sd(x[,2])
  s_hata = sqrt(((nrow(x)*sd_1**2)+(nrow(x)*sd_2**2)) / ((nrow(x)+nrow(x))-2))
  carpan = sqrt((1/length(x[,1]))+(1/length(x[,2])))
  s_hata = s_hata * carpan
  t= abs(((ort_1-ort_2)/(s_hata)))
  v = length(x[,1]) + length(x[,2]) - 2
  Sys.sleep(0.5)
  cat(cyan("İstatistikler"),"\n")
  df <- data.frame("İstatistikler" = c(ort_1,ort_2,sd_1,sd_2,s_hata,t,v))
  rownames(df) <- c("1.örneklem ortalaması","2.örneklem ortalaması","1.örneklem standart sapması","2.örneklem standart sapması",
                    "Standart Hata","T Test İstatistiği","Serbestlik Derecesi")
  print(df)
  Sys.sleep(0.9)
  Sys.sleep(0.5)
  #c("Eşit Değil","Büyüktür","Küçüktür")
  cat(blue("İki Anakütle Ortalamasına İlişkin Bağımsız İki Örneklem T Testi"),"\n")
  if(alternatif == "Eşit"){
    cat(cyan("Hipotezler Oluşturuluyor."),"\n")
    Sys.sleep(0.5)
    cat(cyan("Ho : μ1 = μ2"),"\n")
    cat(cyan("Ho : μ1 ≠ μ2"),"\n")
    kritik_deger1 = kritik_t[v,"kdçtt"]
    kritik_deger2 = -1*kritik_t[v,"kdçtt"]
    alt_aralık = (ort_1-ort_2)-(kritik_t[v,"kdçtt"]*s_hata)
    üst_aralık = (ort_1-ort_2)+(kritik_t[v,"kdçtt"]*s_hata)
    cat(cyan("Güven Aralıkları"),"\n")
    cat(blue(alt_aralık),blue("<"),blue("μ1-μ2"),blue("<"),blue(üst_aralık),"\n")
    if(t<kritik_deger1){
      cat("Ho Hipotezi Reddedilemez","\n")
    }else{
      cat("Ho Hipotezi Reddedilir.","\n")}}}
bir_ornek_z <- function(x,hedef,CTT=T,a=0.05){
  #n tane gözlem değerinden oluşan  bir örnek için  skewness
  pay_skewness <- function(x){
    toplam = 0
    for (i in x){
      q = (i - mean(x))**3
      toplam = toplam + q
    }
    sonuc = toplam/length(x)

  }

  payda_skewness <- function(qq){
    toplam = 0
    for(i in qq){
      q = (i - mean(qq))**2
      toplam = toplam + q
    }
    sonuc=(toplam/(length(qq)-1))**(1.5)

  }


  skewness_tutorial <- function(x){
    sonuc = pay_skewness(x)/payda_skewness(x)

  }
  #Kontrol
  test_verisi <- c(10,15,30,40,23,43,23,43,54,76,65,43,12,34,54,65,40)
  skewness_tutorial(test_verisi)
  #Sonuç diğer hesaplayıcılar ile arasında fark yok.


  #n tane gözlem değerinden oluşan bir örnek için kurtosis


  pay_kurtosis <- function(y){
    toplam2=0
    for(i in y){
      z= (i - mean(y))**4
      toplam2= toplam2 + z
    }
    sonuc = toplam2/(length(y))
  }

  payda_kurtosis <- function(y){
    toplam2=0
    for(i in y){
      z=(i-mean(y))**2
      toplam2= toplam2 + z
    }
    sonuc = toplam2/length(y)
    sonuc = sonuc**2
  }


  kurtosis_tutorial <- function(y){
    sonuc = (pay_kurtosis(y)/payda_kurtosis(y))
  }

  #Sonuç vikipedi örneği ile kıyaslanmış ve aynı sonuç elde edilmiştir.


  #Örneklem için jarque bera test istatistiği



  sturges <- function(veri){
    round(1+3.322*log10(veri))
  }
  jarque_bera <- function(z){
    k=2
    a=(length(z)-k)/6
    b=(skewness_tutorial(z))**2
    c=(((kurtosis_tutorial(z)-3)**2)/4)
    sonuc = a*(b+c)
    cat("Jarque-Bera Test İstatistiği : ",sonuc,"\n")
    par(mfrow=c(1,2))
    hist(z,xlab=c("GÖZLEM DEĞERLERİ"),
         ylab=c("FREKANSLAR"),
         col=c("#ffeaa7"),
         main=c("Histogram Grafiği"),
         breaks=sturges(length(z)))
    hist(z,xlab=c("GÖZLEM DEĞERLERİ"),
         ylab=c("ORANLAR"),
         col=c("#ffeaa7"),
         main=c("HİSTOGRAM GRAFİĞİ" ,"\n" ,"VE POLİGON EĞRİSİ"),
         breaks=sturges(length(z)),
         prob=T)
    lines(density(z,adjust=2),col="red",lwd=3,lty="dotted")
    Sys.sleep(0.5)
    cat(cyan("Hipotezler Oluşturuluyor"),"\n")
    Sys.sleep(0.5)
    print("Ho:Veriler teorik normal dağılma uygundur")
    Sys.sleep(0.5)
    print("H1:Veriler teorik normal dağılma uygun değildir.")
    Sys.sleep(0.5)
    print("Güven Düzeyi : 95")
    Sys.sleep(0.5)
    cat("Gözlem Sayısı",length(z),"\n")
    Sys.sleep(0.5)
    cat("Çarpıklık Değeri",skewness_tutorial(z),"\n")
    Sys.sleep(0.5)
    cat("Basıklık Değeri",kurtosis_tutorial(z),"\n")
    if (sonuc > 5.991){
      print("Ho hipotezi reddedilir,veriler teorik normal dağılıma uygun değildir")
    }else{
      print("Ho hipotezi reddedilemez,veriler teorik normal dağılma uygundur.")}

  }

  ######

  #cat(blue("Hello", "world!\n"))
  cat(blue("İstatistikler oluşturuluyor"),"\n")
  Sys.sleep(0.5)
  x_ort   = mean(x)
  x_sd    = sd(x)
  x_shata = x_sd/sqrt(length(x))
  v_d =   hedef
  z= abs((x_ort-v_d)/x_shata)
  df <- (data.frame("İstatistikler" =c(x_ort,x_sd,x_shata,hedef,z)))
  rownames(df) <- c("Ortalama","Standart Sapma","Standart Hata","Varsayımsal Değer","Z Test İstatistiği")
  print(df)
  cat(cyan("Normallik Varsayımı Kontrol Ediliyor"),"\n")
  Sys.sleep(0.5)
  cat(green("Shapiro P-Value Değeri : ",shapiro.test(x)[[2]]))
  jarque_bera(x)
  cat(red("Güven Aralığı Hesaplanıyor"),"\n")
  Sys.sleep(0.5)
  cat(red("Güven Aralığı"),"\n")
  cat(red(x_ort-(1.96*x_shata),red("<"),red("μ"),red("<"),red(x_ort+(1.96*x_shata))),"\n")
  Sys.sleep(0.5)
  cat(blue("Tek Örneklem z Testine Geçiliyor."),"\n")
  Sys.sleep(0.5)
  cat(blue("Verilerinizin Normallik Varsayımını Sağladına Emin Olun"),"\n")
  Sys.sleep(0.5)
  cat(blue("Hipotezler Oluşturuluyor"),"\n")
  Sys.sleep(0.5)
  cat("Ho:μ =",v_d,"\n" )
  cat("H1:μ ≠",v_d,"\n")
  Sys.sleep(0.5)
  cat(blue("Karar Veriliyor"),"\n")
  if(abs(z) < 1.96){
    cat(green("Ho Hipotezi Reddedilemez."),"\n")
    cat(green(a,"Anlamlılık Düzeyinde","\n","anakütle Ortalaması ile test edilen değer arasında","\n", "istatistiksel olarak anlamlı bir farklılık yoktur."),"\n")
  }else{
    cat(red("Ho Hipotezi Reddedilir"))
    cat(red(a,"Anlamlılık Düzeyinde","\n","anakütle ortalaması ile test edilen değer arasında","\n","istatistiksel olaral anlamlı bir farklılık vardır."),"\n")
  }}
iki_orneklem_z <- function(x,a=0.05){
  x <- na.omit(x)
  ort_1  = mean(x[,1])
  ort_2  = mean(x[,2])
  sd_1   = sd(x[,1])
  sd_2   = sd(x[,2])
  s_hata = sqrt((sd_1**2/length(nrow(x)))+(sd_2**2/nrow(x)))
  z      = abs((ort_1-ort_2)/s_hata)
  dff <- data.frame("İstatistikler"=c(ort_1,ort_2,sd_1,sd_2,s_hata,z))
  rownames(dff) <- c("1.Değişkenin Ortalaması","2.Değişkenin Ortlaması",
                     "1.Değişkenin Standart Sapması","2.Değişkenin Standart Sapması",
                     "Standart Hata","Z Değeri")

  cat(blue("İstatistikler"),"\n")
  print(dff)
  #n tane gözlem değerinden oluşan  bir örnek için  skewness
  pay_skewness <- function(x){
    toplam = 0
    for (i in x){
      q = (i - mean(x))**3
      toplam = toplam + q
    }
    sonuc = toplam/length(x)

  }

  payda_skewness <- function(qq){
    toplam = 0
    for(i in qq){
      q = (i - mean(qq))**2
      toplam = toplam + q
    }
    sonuc=(toplam/(length(qq)-1))**(1.5)

  }


  skewness_tutorial <- function(x){
    sonuc = pay_skewness(x)/payda_skewness(x)

  }
  #Kontrol
  test_verisi <- c(10,15,30,40,23,43,23,43,54,76,65,43,12,34,54,65,40)
  skewness_tutorial(test_verisi)
  #Sonuç diğer hesaplayıcılar ile arasında fark yok.


  #n tane gözlem değerinden oluşan bir örnek için kurtosis


  pay_kurtosis <- function(y){
    toplam2=0
    for(i in y){
      z= (i - mean(y))**4
      toplam2= toplam2 + z
    }
    sonuc = toplam2/(length(y))
  }

  payda_kurtosis <- function(y){
    toplam2=0
    for(i in y){
      z=(i-mean(y))**2
      toplam2= toplam2 + z
    }
    sonuc = toplam2/length(y)
    sonuc = sonuc**2
  }


  kurtosis_tutorial <- function(y){
    sonuc = (pay_kurtosis(y)/payda_kurtosis(y))
  }

  #Sonuç vikipedi örneği ile kıyaslanmış ve aynı sonuç elde edilmiştir.


  #Örneklem için jarque bera test istatistiği



  sturges <- function(veri){
    round(1+3.322*log10(veri))
  }
  jarque_bera <- function(z){
    k=2
    a=(length(z)-k)/6
    b=(skewness_tutorial(z))**2
    c=(((kurtosis_tutorial(z)-3)**2)/4)
    sonuc = a*(b+c)
    cat("Jarque-Bera Test İstatistiği : ",sonuc,"\n")
    Sys.sleep(0.2)
    par(mfrow=c(1,2))
    hist(z,xlab=c("GÖZLEM DEĞERLERİ"),
         ylab=c("FREKANSLAR"),
         col=c("#ffeaa7"),
         main=c("Histogram Grafiği"),
         breaks=sturges(length(z)))
    hist(z,xlab=c("GÖZLEM DEĞERLERİ"),
         ylab=c("ORANLAR"),
         col=c("#ffeaa7"),
         main=c("HİSTOGRAM GRAFİĞİ" ,"\n" ,"VE POLİGON EĞRİSİ"),
         breaks=sturges(length(z)),
         prob=T)
    lines(density(z,adjust=2),col="red",lwd=3,lty="dotted")
    print("Ho:Veriler teorik normal dağılma uygundur")
    print("H1:Veriler teorik normal dağılma uygun değildir.")
    Sys.sleep(0.2)
    print("Güven Düzeyi : 95")
    Sys.sleep(0.2)
    cat("Gözlem Sayısı",length(z),"\n")
    Sys.sleep(0.2)
    cat("Çarpıklık Değeri",skewness_tutorial(z),"\n")
    Sys.sleep(0.2)
    cat("Basıklık Değeri",kurtosis_tutorial(z),"\n")
    if (abs(sonuc) > 5.991){
      print("Ho hipotezi reddedilir,veriler teorik normal dağılıma uygun değildir")
    }else{
      print("Ho hipotezi reddedilemez,veriler teorik normal dağılma uygundur.")}

  }
  cat(cyan(colnames(x)[1],"Değişkeni İçin Normallik Testi"),"\n")
  Sys.sleep(0.2)
  jarque_bera(x[,1])
  cat(cyan(colnames(x)[2],"Değişkeni İçin Normallik Testi"),"\n")
  Sys.sleep(0.2)
  jarque_bera(x[,2])
  cat(cyan("Varyans Homojenliği Test Ediliyor"),"\n")
  Sys.sleep(0.2)
  cat(cyan("Varyan Homojenliği İçin Hipotezler Oluşturuluyor."),"\n")
  Sys.sleep(0.2)
  cat(cyan("Ho: İki Örneklem Aynı Anakütleden Gelmiştir"),"\n")
  cat(cyan("H1: İki Örneklem Aynı Anakütleden Gelmemiştir."),"\n")
  if(bartlett.test(x)[[3]] > 0.05){
    cat(cyan(bartlett.test(x)[[3]]),">",cyan(0.05),cyan("Ho Reddedilemez"),"\n")
    cat(green("Varyans Homojenliği Varsayımı Sağlandı"),"\n")
  }else{
    cat(cyan(bartlett.test(x)[[3]]),"<",cyan(0.05),cyan("Ho Red"),"\n")
    cat(red("Varyans Homojenliği Varsayımı Sağlanamadı"),"\n")}

  if(a==0.01){
    cat(cyan("Güven Aralığı"),"\n")
    alt = (ort_1-ort_2)-(2.58*(s_hata))
    üst = (ort_1-ort_2)+(2.58*(s_hata))
    cat(red(alt,"<","μ1-μ2","<",üst),"\n")

  }else if(a==0.05){
    cat(cyan("Güven Aralığı"),"\n")
    alt = (ort_1-ort_2)-(1.96*(s_hata))
    üst = (ort_1-ort_2)+(1.96*(s_hata))
    cat(red(alt,"<","μ1-μ2","<",üst),"\n")}



  cat(cyan("İki Anakütle Ortalmasına İlişkin Bağımsız İki Örneklem Z Testi Hipotezleri"),"\n")
  Sys.sleep(0.2)
  cat(cyan("Ho:μ1=μ2"),"\n")
  cat(cyan("Ho:μ1≠μ2"),"\n")


  if(a==0.01 & z > 2.58){
    cat(cyan("Ho Hipotezi Reddedilir."),"\n")

  }else if(a==0.01 & z < 2.58){
    cat(cyan("Ho Hipotezi Reddedilemez."),"\n")
  }else if(a==0.05 & z > 1.960){
    cat(cyan("Ho Hipotezi Reddedilir."),"\n")

  }else if(a==0.05 & z < 1.960){
    cat(cyan("Ho Hipotezi Reddedilemez."),"\n")
  }

}
tek_örnek_oran <- function(n,a,alfa=c(0.01,0.05),hedef,alternatif=c("Büyüktür","Küçüktür")){
  p=a/n
  #cat(blue("Hello", "world!\n"))
  sigmap = sqrt((hedef*(1-hedef))/n)
  z= abs((p-hedef)/sigmap)
  x=c(0.05,0.01)
  y=c(1.64485,2.32635)
  tableset = data.frame(alfa=x,value=y)
  if(alfa==0.05 & alternatif=="Küçüktür"){
    cat(cyan("Hipotezler Oluşturuluor"),"\n")
    Sys.sleep(2)
    cat(cyan("Ho:π="),cyan(hedef),"\n")
    cat(cyan("H1:π<"),cyan(hedef),"\n")
    Sys.sleep(2)
    cat(cyan("Test İstatistiği"),cyan(z),"\n")
    if(z<1.64){
      cat(cyan("Ho Hipotezi Reddedilir"),"\n")}
    else{
      cat(cyan("Ho Hipotezi Reddedilemez"),"\n")}}
  if(alfa==0.05 & alternatif=="Büyüktür"){
    cat(cyan("Hipotezler Oluşturuluor"),"\n")
    Sys.sleep(2)
    cat(cyan("Ho:π="),cyan(hedef),"\n")
    cat(cyan("H1:π>"),cyan(hedef),"\n")
    Sys.sleep(2)
    cat(cyan("Test İstatistiği"),cyan(z),"\n")
    if(z>1.64){
      cat(cyan("Ho Hipotezi Reddedilir"),"\n")}
    else{
      cat(cyan("Ho Hipotezi Reddedilemez"),"\n")}}
  if(alfa==0.01 & alternatif=="Küçüktür"){
    cat(cyan("Hipotezler Oluşturuluor"),"\n")
    Sys.sleep(2)
    cat(cyan("Ho:π="),cyan(hedef),"\n")
    cat(cyan("H1:π<"),cyan(hedef),"\n")
    Sys.sleep(2)
    cat(cyan("Test İstatistiği"),cyan(z),"\n")
    if(z<2.33){
      cat(cyan("Ho Hipotezi Reddedilir"),"\n")}
    else{
      cat(cyan("Ho Hipotezi Reddedilemez"),"\n")}}
  if(alfa==0.01 & alternatif=="Büyüktür"){
    cat(cyan("Hipotezler Oluşturuluor"),"\n")
    Sys.sleep(2)
    cat(cyan("Ho:π="),cyan(hedef),"\n")
    cat(cyan("H1:π>"),cyan(hedef),"\n")
    Sys.sleep(2)
    cat(cyan("Test İstatistiği"),cyan(z),"\n")
    if(z>2.33){
      cat(cyan("Ho Hipotezi Reddedilir"),"\n")}
    else{
      cat(cyan("Ho Hipotezi Reddedilemez"),"\n")}}







}
iki_orneklem_oran<-function(a1,a2,n1,n2,alternatif=c("Büyüktür","Küçüktür")){
  p1=a1/n1
  p2=a2/n2
  s_hata = sqrt((p1*(1-p1)/n1)+(p2*(1-p2))/n2)
  cat(cyan("Sp1-Sp2"),"\n")
  cat(red(s_hata),"\n")
  z=abs((p1-p2)/s_hata)
  cat(cyan("Test Değeri"),"\n")
  cat(cyan(z),"\n")
  if(alternatif == "Büyüktür"){
    cat(red("Ho: π1 = π2"),"\n")
    cat(red("Ho: π1 > π2"),"\n")
    if(z < 1.64){
      cat(red("Ho Reddedilemez"),"\n")
    }else{
      cat(red("Ho Red"),"\n")}}

  if(alternatif == "Küçüktür"){
    cat(cyan("Ho: π1 = π2"),"\n")
    cat(cyan("Ho: π1 < π2"),"\n")
    if(z > 1.64){
      cat("Ho Hipotezi Reddedilir.")
    }else{
      cat("Ho Hipotezi Reddedilemez.")
    }

  }


}
bagimli_iki_örneklem_t_testi <- function(grup1,grup2,a){
  g1_normal <- shapiro.test(grup1)[[2]]
  g2_normal <- shapiro.test(grup2)[[2]]
  cat(cyan("Birinci grup için normallik varsayımı sonuçları:",round(g1_normal,5),"\n"))
  cat(cyan("İkinci grup için normallik varsayımı sonuçları:",round(g2_normal,5),"\n"))
  veri <- data.frame("grup"=rep(c("Grup1","Grup2"),each = length(grup1)),"deger"=c(grup1,grup2))
  veri$grup <- as.factor(veri$grup)
  levene_test <- leveneTest(deger ~ grup, data = veri)
  cat(cyan("Levene testi hipotezleri"),"\n")
  cat(red("HO : Grupların varyansları arasında fark yoktur (varyanslar homojendir)."),"\n")
  cat(red("HO : Grupların varyansları arasında fark vardır (varyanslar hetorojendir)."),"\n")
  cat(green("Levene test sonucu : ",round(levene_test$`Pr(>F)`[1],5)),"\n")
  if (round(levene_test$`Pr(>F)`[1],5) > 0.05 ){
    cat("HO Hipotezi reddedilemez.(Homojen Varyans)","\n")
  }else{
    cat("HO Hipotezi reddedilir.(Hetorojen Varyans)","\n")}

  tablo <<- data.frame("Grup1"=grup1,
                       "Grup2"=grup2,
                       "A" = grup1-grup2,
                       "B" = (grup1-grup2)-mean(grup1-grup2),
                       "C" = ((grup1-grup2)-mean(grup1-grup2))**2)
  Dmean = mean(tablo$Grup1-tablo$Grup2)
  toplam = 0
  for (i in tablo$A){
    toplam = toplam + ((i-Dmean))**2}
  toplam = toplam / (length(grup1)-1)
  toplam = sqrt(toplam)



  sd = toplam
  t_value = abs((Dmean*sqrt((length(grup1)))/sd))
  p_value <- pt(t_value, (length(grup1)-1), lower.tail = FALSE)
  p_value <- round(p_value,5)
  cat(blue("Bağımlı iki örneklem testi sonuçları :"), "\n")
  cat(blue("Grup1 Ortalaması:",mean(grup1)), "\n")
  cat(blue("Grup2 Ortalaması:",mean(grup2)), "\n")
  cat(blue("Grup1 ile Grup2 arasında istatistiksel olarak anlamlı bir fark yoktur."), "\n")
  cat(blue("Grup1 ile Grup2 arasında istatistiksel olarak anlamlı bir fark vardır."), "\n")
  cat(blue("P-value:",p_value), "\n")
  cat(blue("t-value:",t_value), "\n")

  if (p_value > a ){
    cat(blue("Ho Hipotezi reddedilemez."), "\n")
  }else{
    cat(blue("Ho Hipotezi reddedilir."), "\n")
  }
  describe(as.data.frame(grup1))
  describe(as.data.frame(grup2))}
anaova_uygula <- function(veri, grup_degiskeni, bagimli_degisken) {
  # Veriyi gruplara ayırma
  gruplar <- unique(veri[[grup_degiskeni]])
  grup_sayisi <- length(gruplar)

  # Grupların ortalamalarını hesaplama
  grup_ortalamalari <- numeric(grup_sayisi)
  for (i in 1:grup_sayisi) {
    grup_ortalamalari[i] <- mean(veri[veri[[grup_degiskeni]] == gruplar[i], bagimli_degisken])
  }

  # Toplam varyansın hesaplanması
  toplam_varyans <- var(veri[[bagimli_degisken]])

  # Grup içi ve gruplar arası varyansların hesaplanması
  grup_ici_varyans <- numeric(grup_sayisi)
  gruplar_arasi_varyans <- numeric(grup_sayisi)
  for (i in 1:grup_sayisi) {
    grup_ici_varyans[i] <- var(veri[veri[[grup_degiskeni]] == gruplar[i], bagimli_degisken])
    gruplar_arasi_varyans[i] <- length(veri[veri[[grup_degiskeni]] == gruplar[i], bagimli_degisken]) * (grup_ortalamalari[i] - mean(veri[[bagimli_degisken]]))^2
  }

  # Gruplar arası varyansın toplamı
  gruplar_arasi_varyans_toplam <- sum(gruplar_arasi_varyans)

  # Hata (grup içi) varyansının hesaplanması
  hata_varyansi <- toplam_varyans - gruplar_arasi_varyans_toplam

  # Serbestlik dereceleri
  toplam_serbestlik <- length(veri[[bagimli_degisken]]) - 1
  gruplar_arasi_serbestlik <- grup_sayisi - 1
  grup_ici_serbestlik <- toplam_serbestlik - gruplar_arasi_serbestlik

  # F istatistiği ve p-değeri
  F_istatistigi <- (gruplar_arasi_varyans_toplam / gruplar_arasi_serbestlik) / (hata_varyansi / grup_ici_serbestlik)
  p_degeri <- 1 - pf(F_istatistigi, gruplar_arasi_serbestlik, grup_ici_serbestlik)

  # Sonuçların döndürülmesi
  sonuclar <- list(Grup_Ortalamalari = grup_ortalamalari,
                   Toplam_Varyans = toplam_varyans,
                   Grup_Ici_Varyans = grup_ici_varyans,
                   Gruplar_Arasi_Varyans = gruplar_arasi_varyans,
                   Gruplar_Arasi_Varyans_Toplam = gruplar_arasi_varyans_toplam,
                   Hata_Varyansi = hata_varyansi,
                   Toplam_Serbestlik = toplam_serbestlik,
                   Gruplar_Arasi_Serbestlik = gruplar_arasi_serbestlik,
                   Grup_Ici_Serbestlik = grup_ici_serbestlik,
                   F_Istatistigi = F_istatistigi,
                   P_Degeri = p_degeri)

  return(sonuclar)
}
cift_yonlu_anaova_uygula <- function(veri, grup_degiskeni1, grup_degiskeni2, bagimli_degisken) {
  # Veriyi gruplara ayırma
  gruplar1 <- unique(veri[[grup_degiskeni1]])
  grup_sayisi1 <- length(gruplar1)

  gruplar2 <- unique(veri[[grup_degiskeni2]])
  grup_sayisi2 <- length(gruplar2)

  # Grupların ortalamalarını hesaplama
  grup_ortalamalari <- matrix(nrow = grup_sayisi1, ncol = grup_sayisi2)
  for (i in 1:grup_sayisi1) {
    for (j in 1:grup_sayisi2) {
      grup_ortalamalari[i, j] <- mean(veri[veri[[grup_degiskeni1]] == gruplar1[i] & veri[[grup_degiskeni2]] == gruplar2[j], bagimli_degisken])
    }
  }

  # Toplam varyansın hesaplanması
  toplam_varyans <- var(veri[[bagimli_degisken]])

  # Grup içi ve gruplar arası varyansların hesaplanması
  grup_ici_varyans <- numeric(grup_sayisi1, grup_sayisi2)
  gruplar_arasi_varyans <- numeric(grup_sayisi1, grup_sayisi2)
  for (i in 1:grup_sayisi1) {
    for (j in 1:grup_sayisi2) {
      grup_ici_varyans[i, j] <- var(veri[veri[[grup_degiskeni1]] == gruplar1[i] & veri[[grup_degiskeni2]] == gruplar2[j], bagimli_degisken])
      gruplar_arasi_varyans[i, j] <- length(veri[veri[[grup_degiskeni1]] == gruplar1[i] & veri[[grup_degiskeni2]] == gruplar2[j], bagimli_degisken]) * (grup_ortalamalari[i, j] - mean(veri[[bagimli_degisken]]))^2
    }
  }

  # Gruplar arası varyansın toplamı
  gruplar_arasi_varyans_toplam <- sum(gruplar_arasi_varyans)

  # Hata (grup içi) varyansının hesaplanması
  hata_varyansi <- toplam_varyans - gruplar_arasi_varyans_toplam

  # Serbestlik dereceleri
  toplam_serbestlik <- length(veri[[bagimli_degisken]]) - 1
  gruplar_arasi_serbestlik <- (grup_sayisi1 - 1) * (grup_sayisi2 - 1)
  grup_ici_serbestlik <- toplam_serbestlik - gruplar_arasi_serbestlik

  # F istatistiği ve p-değeri
  F_istatistigi <- (gruplar_arasi_varyans_toplam / gruplar_arasi_serbestlik) / (hata_varyansi / grup_ici_serbestlik)
  p_degeri <- 1 - pf(F_istatistigi, gruplar_arasi_serbestlik, grup_ici_serbestlik)

  # Sonuçların döndürülmesi
  sonuclar <- list(Grup_Ortalamalari = grup_ortalamalari,
                   Toplam_Varyans = toplam_varyans,
                   Grup_Ici_Varyans = grup_ici_varyans,
                   Gruplar_Arasi_Varyans = gruplar_arasi_varyans,
                   Gruplar_Arasi_Varyans_Toplam = gruplar_arasi_varyans_toplam,
                   Hata_Varyansi = hata_varyansi,
                   Toplam_Serbestlik = toplam_serbestlik,
                   Gruplar_Arasi_Serbestlik = gruplar_arasi_serbestlik,
                   Grup_Ici_Serbestlik = grup_ici_serbestlik,
                   F_Istatistigi = F_istatistigi,
                   P_Degeri = p_degeri)

  return(sonuclar)
}












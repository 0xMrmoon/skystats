if (!require(crayon, quietly = TRUE)) {
  install.packages("crayon")
  library(crayon)
} else {
  library(crayon)
}
aga <- function(x){
  boxplot(x,col="orange")
  iqr = quantile(x)[4]-quantile(x)[2]
  minimum = quantile(x)[2]-1.5*iqr[[1]]
  maximum = quantile(x)[4]+1.5*iqr[[1]]
  veri = subset(x,subset=(x>minimum[[1]] & x<maximum[[1]]))
  boxplot(subset(x,subset=(x>minimum[[1]] & x<maximum[[1]])))
  return(veri)}




z_skor_aga <- function(x){
  n = length(x)
  sample <- vector()
  if (n>30){
    for(i in 1:100){
      sample[i] = shapiro.test(sample(x,size=30,replace = T))[[2]]}
    sonuc = mean(sample)
  }else if(n<30){
    sonuc = shapiro.test(x)[[2]]}


  if (sonuc >= 0.05){
    z_skor_vector = (x-mean(x))/sd(x)
    deger <- subset(z_skor_vector,subset=(abs(z_skor_vector)>=3))
    indis <- which(z_skor_vector==deger)
    return(x[-indis])}
  else if (sonuc < 0.05){
    stop("Verileriniz teorik normal dağılıma uygun olmalıdır.Bu bir varsayımdır bu varsayımın ihlali hatalı sonuçlar üretir.","\n",
    "Aykırı gözlemlerden dolayı verileriniz normal dağılma uymuyor olabilir.
    Böyle bir durumda parametrik olmayan aykırı gözlem analizi testlerini uygulayabilirsiniz.Lütfen kütüphane fonksiyonlarını inceleyiniz.")}}




mahalanobis_distance_aga <- function(x,p=0.95){
  df = ncol(x)
  center <- colMeans(x)
  cov    <- cov(x)
  dist   <- mahalanobis(x,center = center, cov = cov)
  cutoff <-qchisq(p=p,df=df)
  inds   <- which(dist > cutoff)
  data_new <- x[-c(inds),]
  return(data_new)
}

not_na <- function(data){
  return(na.omit(data))
}



ortalama_deger_atamasi <- function(df) {
  for (col in colnames(df)) {
    if (any(is.na(df[[col]]))) {
      mean_value <- mean(df[[col]], na.rm = TRUE)
      df[[col]][is.na(df[[col]])] <- mean_value
    }
  }
  return(df)
}



medyan_atamasi <- function(df) {
  for (col in colnames(df)) {
    if (any(is.na(df[[col]]))) {
      median_value <- median(df[[col]], na.rm = TRUE)
      df[[col]][is.na(df[[col]])] <- median_value
    }
  }
  return(df)
}






rastgele_deger_atama <- function(df){
  for(col in colnames(df)){
    sample_value <- sample(na.omit(df[[col]]),size=1)
    df[[col]][is.na(df[[col]])] <- sample_value
  }
  return(df)
}




plot_missing_values <- function(dataframe) {
  library(ggplot2)
  missing_values <- sapply(dataframe, function(x) sum(is.na(x)))
  missing_values_percent <- round(missing_values / nrow(dataframe) * 100, 2)
  missing_values_df <- data.frame(variable = names(missing_values_percent),
                                  missing_percent = missing_values_percent)

  ggplot(missing_values_df, aes(x = reorder(variable, -missing_percent), y = missing_percent)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    scale_y_continuous(labels = scales::percent_format()) +
    xlab("Değişken") +
    ylab("Eksik Veri Oranı") +
    ggtitle("Eksik Verilerin Dağılımı") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1))
}





test_train_sample <- function(df,p=0.80,setseed=150){
  set.seed(setseed)
  sampleIndex <- sample(1:nrow(df),size=p*nrow(df))
  trainSet_ <<- df[sampleIndex,]
  testSet_  <<- df[-sampleIndex,]
  cat("Train ve test ayrımı tamamlandı,trainSet_ ve testSet_ adında değişkenler oluşturuldu kontrol ediniz.")
  }







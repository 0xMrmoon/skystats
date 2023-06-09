library(shiny)
library(skystats)

# UI (Kullanıcı Arayüzü) tanımlaması
ui <- fluidPage(
  titlePanel("skystats Paketi Uygulaması"),

  sidebarLayout(
    sidebarPanel(
      fileInput("veriDosyasi", "Veri Setini Yükle"),  # Veri seti dosyasını yükleme alanı
      selectInput("fonksiyonSecimi", "Kullanılacak Fonksiyon", choices = c("sturges", "gruplanmıs_seri", "scaling")),  # Kullanılacak fonksiyonları seçme alanı
      actionButton("uygulaButton", "Fonksiyonu Uygula")  # Fonksiyonu uygulama düğmesi
    ),

    mainPanel(
      verbatimTextOutput("sonuc")  # Sonuçları gösterme alanı
    )
  )
)

# Server (Sunucu) fonksiyonu tanımlaması
server <- function(input, output) {

  veri <- reactive({
    req(input$veriDosyasi)  # Veri dosyası yüklendiğinde çalışır
    read.csv(input$veriDosyasi$datapath)
  })

  sonuc <- eventReactive(input$uygulaButton, {
    req(input$veriDosyasi)  # Fonksiyon uygula düğmesine basıldığında çalışır
    fonksiyonAdi <- input$fonksiyonSecimi
    # Seçilen fonksiyonu çağırın ve veri setini argüman olarak geçirin
    sonuc <- switch(fonksiyonAdi,
                    "sturges" = sturges(veri()),
                    "gruplanmıs_seri" = gruplanmıs_seri(veri()),
                    "scaling" = scaling(veri()))
    sonuc
  })

  output$sonuc <- renderPrint({
    sonuc()  # Sonuçları göster
  })
}

# Uygulamayı başlat
shinyApp(ui = ui, server = server)

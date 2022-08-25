library(shiny)
library(shinydashboard)
library(dygraphs)

dashboardPage(
  dashboardHeader(title = "ISPU DKI Jakarta"),
  dashboardSidebar
  (
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("book-open")),
      menuItem("Tren Parameter", tabName = "tren", icon = icon("chart-line")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Tentang Situs", tabName = "about", icon = icon("code"))
    )
  ),
  dashboardBody
  (
    tags$head(
      tags$link(rel = "stylesheet", href = "custom.css")),
    tabItems
    (
      #----------------Overview Page-------------------------------
      tabItem(tabName = "overview",
              h1("Indeks Standar Pencemar Udara DKI Jakarta"),
              fluidRow(
                tabBox(
                  title = "",
                  width = 12,
                  tabPanel("Overview", 
                           p("Pada tahun 2020 Kementerian Lingkungan Hidup & Kehutanan telah mengeluarkan kebijakan terbaru terkait 
                           pemantauan dan pelaporan kualitas udara yang disebut dengan Indeks standar Pencemar Udara. 
                           Sebelumnya pemantauan dan pelaporan kualitas udara mengikuti Keputusan Menteri Lingkungan Hidup No. 45 tahun 1997. 
                           Peraturan tersebut tetap menjadi acuan walaupun WHO beberapakali memperbarui kebijakan terkait Air Quality Guidelines.
                           Dashboard berikut menunjukkan perkembangan kualitas udara di daerah DKI Jakarta selama periode 2016 - 2021 dengan mengacu
                           pada Indeks Standar Pencemar Udara sesuai Permen LHK No. 14 Tahun 2020. DKI Jakarta sendiri merupakan daerah
                           metropolitan padat penduduk yang dinilai memiliki kualitas udara terburuk di Indonesia (versi IQAir 2020)"), br(),
                           p(strong("Catatan: "), "Terdapat penambahan parameter ukur baru (PM", tags$sub(2.5),") yang tersedia mulai Februari 2021. 
                             Pengaruh PM", tags$sub(2.5)," terhadap ISPU jauh lebih tinggi dibandingkan parameter lainnya.")),
                  
                  tabPanel("Tentang ISPU",
                           tags$table(tags$tr(tags$td(img(class = "Ispu", src = "ispu.png", height = 250), style="vertical-align: top;"),
                            tags$td(p("ISPU adalah standar indeks kualitas udara versi Indonesia. Pada umumnya setiap negara mempunyai standarnya
                              masing-masing. ISPU diperbarui pada tahun 2020 melalui Permen LHK No.14 Tahun 2020 menggantikan Kepmen LH No. 45 tahun 1997.
                              Pada peraturan baru terdapat 7 parameter ukur yaitu partikulat dengan ukuran minimal 10 mikron (PM",tags$sub(10),", sulfur dioksida (SO", tags$sub(2),"),
                              karbon dioksida(CO), ozon (O", tags$sub(3),"), nitrogen dioksida (NO", tags$sub(2),"), partikulat dengan ukuran minimal 2,5 mikron (PM", tags$sub(2.5),") dan
                              hidrokarbon (HC). 2 parameter terakhir adalah parameter yang baru ditambahkan pada kebijakan baru"), br(),
                           p("Parameter-parameter tersebut dikonversi menjadi skor ISPU yang kemudian dapat disesuaikan dengan kategori ISPU
                             yang telah ditetapkan. Informasi lebih lanjut terkait ISPU dapat dilihat", tags$a(href="https://ditppu.menlhk.go.id/portal/read/indeks-standar-pencemar-udara-ispu-sebagai-informasi-mutu-udara-ambien-di-indonesia", "disini"), "."),style="vertical-align: top;")))
                           ),
                  tabPanel("Tentang Data", 
                           p("Dataset raw diperoleh melalui situs", tags$a(href="https://data.jakarta.go.id/", "data.jakara.go.id"), ". Dataset berisi Indeks Standar Pencemar Udara (ISPU) 
                             yang diukur dari 5 stasiun pemantau kualitas udara (SPKU) yang ada di Provinsi DKI Jakarta selama periode 2016 - 2021. Data kemudian diolah
                             lebih lanjut untuk kebutuhan pembuatan dashboard ini.",),
                           p("Penjelasan variabel dataset adalah sebagai berikut:"),
                           p("1. tanggal : Tanggal pengukuran kualitas udara", br(), "2. pm10 : Partikulat 10 salah satu parameter yang diukur", br(), "3. pm25 : Partikulat 2.5 salah satu parameter yang diukur (tersedia mulai Februari 2021)", br(),
                             "4. so2 : Sulfida (dalam bentuk SO2) salah satu parameter yang diukur", br(), "5. co : Carbon Monoksida salah satu parameter yand diukur", br(),
                             "6. o3 : Ozon salah satu parameter yang diukur", br(), "7. no2 : NItrogen dioksida salah satu parameter yang diukur", br(),
                             "8. maks : Nilai ukur paling tinggi", br(), "9. crit : parameter dengan nilai ukur tertinggi", br(),
                             "10. ispu_max : nilai parameter dikonversi ke nilai ISPU dan dipilih yang tertinggi", br(), "12. ispu_cat : Kategori ISPU"))
                )
              ),
              fluidRow(
                box(dygraphOutput("plotIspuAll"), width = 9),
                column(width = 3,
                       box( 
                           selectInput(
                             inputId = "input_ispuTahun",
                             label = "Jumlah Hari",
                             choices = unique(ispu_select$tahun),
                             selected = "All"),
                           valueBox(textOutput("baik"), "Hari Baik", icon = icon("face-grin-beam"), width = 12, color = "green"),
                           valueBox(textOutput("sedang"), "Hari Sedang", icon = icon("face-smile"), width = 12),
                           valueBox(textOutput("tidaksehat"), "Hari Tidak Sehat", icon = icon("face-meh"), width = 12, color = "yellow"),
                           valueBox(textOutput("stidaksehat"), "Hari Sangat Tidak Sehat", icon = icon("face-frown"), width = 12, color = "red"),
                           width = 12))
                
              ),
              br(),br(),
              fluidRow(
                tabBox(title = "Annual Parameter Overview",
                       width = 12,
                       tabPanel("Ranking",
                                plotlyOutput("ranking")),
                       tabPanel("Parameter",
                                checkboxGroupInput("annualParameter", 
                                                   label = "Parameter",
                                                   choices = unique(allParam$Parameter),
                                                   selected = unique(allParam$Parameter),
                                                   inline = T),
                                plotlyOutput("plotAnnualParameter"))
                )
              )
        ),
      
      #------------------------------Tren Parameter-----------------------------------------
      
      tabItem(tabName = "tren",
              fluidRow(
                box(width = 12,
                    selectInput(
                      inputId = "input_parameter",
                      label = "Pilih Parameter",
                      choices = unique(colnames(ispu_complete[2:7]))
                    )
                ),
                box(width = 12,
                    dygraphOutput("plotTrenParameter")),
                box(width =12,
                    p(strong("Notice:"), 
                      br(),"1. Pencemar udara cenderung lebih tinggi pada musim kemarau, dan menurun
                      pada musim hujan akibat efek pembilasan hujan.",
                      br(),"2. Pengaruh pandemi/PSBB terhadap pencemar udara dapat terlihat pada tahun 2020"))
              )
      ),
      
      #---------------------------------Data---------------------------------------------------
      
      tabItem(tabName = "data",
              DT::dataTableOutput(outputId = "data")),
      
      #---------------------------------Tentang-------------------------------------------------
      
      tabItem(tabName = "about",
              p(tags$a(href="https://github.com/benoafryan/capstone-shiny-dashboard/", "Dashboard"), " dibuat oleh Mohammad Beno Afryan"))
    )
  )
  
)

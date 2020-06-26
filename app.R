library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyBS)
library(DT)
library(rhandsontable)
library(plotly)
library(ggplot2)


ui <- navbarPage(
  title = "Modul Evaluasi", theme = shinytheme("flatly"), id="evaluationApps",
  tabPanel("Input", value="tabInput",
    selectInput(
      "prov",
      label = "Pilih provinsi",
      choices = c("Nangroe Aceh Darussalam", "Sumatera Utara", "Sumatera Barat", "Riau",
                  "Jambi","Sumatera Selatan", "Bengkulu", "Lampung", "Kepulauan Riau", "Bangka Belitung"),
      selected = "Sumatera Selatan"
    ),
    fileInput("bauEmisi", "Tabel BAU Baseline emisi", buttonLabel="Browse...", placeholder="No file selected"),
    fileInput("targetEmisi", "Tabel target emisi", buttonLabel="Browse...", placeholder="No file selected"),
    fileInput("dataPEP", "Tabel PEP", buttonLabel="Browse...", placeholder="No file selected"),
    actionButton("prosesButton", "Proses")
  ),
  tabPanel("Results", value="tabResults",
    fluidRow(
      column(width=6,
        plotlyOutput("emisiTotal")
      ),
      column(width=6,
        plotlyOutput("kumulatifEmisiTotal")
      )
    ),
    hr(),
    fluidRow(
      column(width=6,
        plotlyOutput("emisiEnergi")
      ),
      column(width=6,
        plotlyOutput("kumulatifEmisiEnergi")
      )
    ),
    hr(),
    fluidRow(
      column(width=6,
        plotlyOutput("emisiLahan")
      ),
      column(width=6,
        plotlyOutput("kumulatifEmisiLahan")
      )
    ),
    hr(),
    fluidRow(
      column(width=6,
        plotlyOutput("emisiLimbah")
      ),
      column(width=6,
        plotlyOutput("kumulatifEmisiLimbah")
      )
    ),
    
    hr(),
    fluidRow(
      column(width=6,
        plotlyOutput("penurunanEmisiTotal")
      ),
      column(width=6,
        plotlyOutput("kumulatifPenurunanEmisiTotal")
      )
    ),
    hr(),
    fluidRow(
      column(width=6,
        plotlyOutput("penurunanEmisiEnergi")
      ),
      column(width=6,
        plotlyOutput("kumulatifPenurunanEmisiEnergi")
      )
    ),
    hr(),
    fluidRow(
      column(width=6,
        plotlyOutput("penurunanEmisiLahan")
      ),
      column(width=6,
        plotlyOutput("kumulatifPenurunanEmisiLahan")
      )
    ),
    hr(),
    fluidRow(
      column(width=6,
        plotlyOutput("penurunanEmisiLimbah")
      ),
      column(width=6,
        plotlyOutput("kumulatifPenurunanEmisiLimbah")
      )
    ),
    hr()
  ),
  tabPanel("Recommendation", value="tabRecommendation",
    h2(textOutput("statementRecommendation")),
    hr(),
    dataTableOutput("kinerjaProvTbl")
  )
  
)

server <- function(input, output, session) {
  allPlot <- reactiveValues(
    plot.emisi.total = NULL,
    plot.emisiEnergi = NULL,
    plot.emisiLahan = NULL,
    plot.emisiLimbah = NULL,
    plot.kumulatif.emisi.total = NULL,
    plot.kumulatif.emisi.energi = NULL,
    plot.kumulatif.emisi.lahan = NULL,
    plot.kumulatif.emisi.limbah = NULL,
    plot.penurunan.emisi.total = NULL,
    plot.penurunan.emisi.energi = NULL,
    plot.penurunan.emisi.lahan = NULL,
    plot.penurunan.emisi.limbah = NULL,
    plot.penurunan.kumulatif.emisi.total = NULL,
    plot.penurunan.kumulatif.emisi.energi = NULL,
    plot.penurunan.kumulatif.emisi.lahan = NULL,
    plot.penurunan.kumulatif.emisi.limbah = NULL
  )
  
  allRecommendation <- reactiveValues(
    kinerjaProvinsiTable = NULL,
    statement = "",
    kinerjaTahunan = NULL
  )
  
  observeEvent(input$prosesButton, { 
    csvBauEmisi <- input$bauEmisi
    if(is.null(csvBauEmisi))
      return(NULL)
    
    csvTargetEmisi <- input$targetEmisi
    if(is.null(csvTargetEmisi))
      return(NULL)
    
    csvDataPEP <- input$dataPEP
    if(is.null(csvDataPEP))
      return(NULL)
    
    bauEmisi <- read.table(csvBauEmisi$datapath, header=T, sep=",")
    targetEmisi <- read.table(csvTargetEmisi$datapath, header=T, sep=",")
    dataPEP <- read.table(csvDataPEP$datapath, header=T, sep=",")
    
    ##### BEGIN: Prepare Emission Data #####

    # susun penurunan emisi per tahun per sektor berdasarkan data PEP
    dataPEP.aggregate<-aggregate(dataPEP$Total.Penurunan.emisi, by = list(dataPEP$tahun_pelaporan,dataPEP$sektor), FUN=sum)
    colnames(dataPEP.aggregate)<-c("tahun", "sektor", "penurunan.emisi")
    aktualPenurunanEmisi<-data.frame(reshape(dataPEP.aggregate, idvar="tahun", v.names="penurunan.emisi", timevar= "sektor", direction="wide" ), stringsAsFactors = FALSE)
    aktualPenurunanEmisi[is.na(aktualPenurunanEmisi)]<-0
    aktualPenurunanEmisi<-aktualPenurunanEmisi[order(aktualPenurunanEmisi$tahun),]
    colnames(aktualPenurunanEmisi)<-c("tahun",
                                "penurunan.emisi.energi",
                                "penurunan.emisi.lahan",
                                "penurunan.emisi.limbah")
    
    
    #### prepare data emisi ####
    
    # susun data aktual emisi dari bau emisi - penurunan emisi
    aktualEmisi<-aktualPenurunanEmisi
    colnames(aktualEmisi)<-c("tahun",
                             "emisiEnergi",
                             "emisiLahan",
                             "emisiLimbah")
    aktualEmisi[,2:4]<-bauEmisi[,2:4]-aktualPenurunanEmisi[,2:4]
    
    # buat tabel perbandingan emisi 
    
    aktualEmisi$emisi.total<-rowSums(aktualEmisi[,2:4])
    bauEmisi$emisi.total<-rowSums(bauEmisi[,2:4])
    targetEmisi$emisi.total<-rowSums(targetEmisi[,2:4])
    
    aktualEmisi$kumulatif.emisi.energi<-cumsum(aktualEmisi$emisiEnergi)
    aktualEmisi$kumulatif.emisi.lahan<-cumsum(aktualEmisi$emisiLahan)
    aktualEmisi$kumulatif.emisi.limbah<-cumsum(aktualEmisi$emisiLimbah)
    aktualEmisi$kumulatif.emisi.total<-cumsum(aktualEmisi$emisi.total)
    
    bauEmisi$kumulatif.emisi.energi<-cumsum(bauEmisi$emisiEnergi)
    bauEmisi$kumulatif.emisi.lahan<-cumsum(bauEmisi$emisiLahan)
    bauEmisi$kumulatif.emisi.limbah<-cumsum(bauEmisi$emisiLimbah)
    bauEmisi$kumulatif.emisi.total<-cumsum(bauEmisi$emisi.total)
    
    targetEmisi$kumulatif.emisi.energi<-cumsum(targetEmisi$emisiEnergi)
    targetEmisi$kumulatif.emisi.lahan<-cumsum(targetEmisi$emisiLahan)
    targetEmisi$kumulatif.emisi.limbah<-cumsum(targetEmisi$emisiLimbah)
    targetEmisi$kumulatif.emisi.total<-cumsum(targetEmisi$emisi.total)
    
    aktualEmisi<-data.frame(aktualEmisi, ID="aktual")
    bauEmisi<-data.frame(bauEmisi,ID="BAU")
    targetEmisi<-data.frame(targetEmisi,ID="target")
    
    compareEmisi<-rbind(aktualEmisi,bauEmisi, targetEmisi)
    
    # plot emisi 
    for (dataName in colnames(compareEmisi)[2:9]){
      eval(parse(text=paste0("plot.",dataName,"<-ggplot(compareEmisi, aes(x=tahun, y=",dataName,", group=ID))+
                                             geom_line(aes(color=ID))+
                                             geom_point(aes(color=ID))+
                                             labs(x='Tahun', y='",dataName,"')+
                                             ggtitle('Grafik ",dataName,"')+
                                             theme(plot.title = element_text(hjust = 0.5))")))
      eval(parse(text=paste0("allPlot$plot.",dataName,"<-plot.",dataName)))
    }
    
    #### prepare data penurunan emisi ####

    # susun data target penurunan emisi dari bau emisi - target emisi
    targetPenurunanEmisi<-aktualPenurunanEmisi
    
    colnames(targetPenurunanEmisi)<-c("tahun",
                                      "penurunan.emisi.energi",
                                      "penurunan.emisi.lahan",
                                      "penurunan.emisi.limbah")
    targetPenurunanEmisi[,2:4]<-bauEmisi[,2:4]-targetEmisi[,2:4]
    
    # buat tabel perbandingan penurunan emisi 
    aktualPenurunanEmisi$penurunan.emisi.total<-rowSums(aktualPenurunanEmisi[,2:4])
    targetPenurunanEmisi$penurunan.emisi.total<-rowSums(targetPenurunanEmisi[,2:4])
    
    aktualPenurunanEmisi$penurunan.kumulatif.emisi.energi<-cumsum(aktualPenurunanEmisi$penurunan.emisi.energi)
    aktualPenurunanEmisi$penurunan.kumulatif.emisi.lahan<-cumsum(aktualPenurunanEmisi$penurunan.emisi.lahan)
    aktualPenurunanEmisi$penurunan.kumulatif.emisi.limbah<-cumsum(aktualPenurunanEmisi$penurunan.emisi.limbah)
    aktualPenurunanEmisi$penurunan.kumulatif.emisi.total<-cumsum(aktualPenurunanEmisi$penurunan.emisi.total)
    
    targetPenurunanEmisi$penurunan.kumulatif.emisi.energi<-cumsum(targetPenurunanEmisi$penurunan.emisi.energi)
    targetPenurunanEmisi$penurunan.kumulatif.emisi.lahan<-cumsum(targetPenurunanEmisi$penurunan.emisi.lahan)
    targetPenurunanEmisi$penurunan.kumulatif.emisi.limbah<-cumsum(targetPenurunanEmisi$penurunan.emisi.limbah)
    targetPenurunanEmisi$penurunan.kumulatif.emisi.total<-cumsum(targetPenurunanEmisi$penurunan.emisi.total)
    
    aktualPenurunanEmisi<-data.frame(aktualPenurunanEmisi, ID="aktual")
    targetPenurunanEmisi<-data.frame(targetPenurunanEmisi, ID="target")
    
    comparePenurunanEmisi<-rbind(aktualPenurunanEmisi,targetPenurunanEmisi)
    
    # plot penurunan emisi 
    for (dataName in colnames(comparePenurunanEmisi)[2:9]){
      eval(parse(text=paste0("plot.",dataName,"<-ggplot(comparePenurunanEmisi, aes(x=tahun, y=",dataName,", group=ID))+
                                             geom_line(aes(color=ID))+
                                             geom_point(aes(color=ID))+
                                             labs(x='Tahun', y='",dataName,"')+
                                             ggtitle('Grafik ",dataName,"')+
                                             theme(plot.title = element_text(hjust = 0.5))")))
      eval(parse(text=paste0("allPlot$plot.",dataName,"<-plot.",dataName)))
    }
    
    ##### END: Prepare Emission Data #####
    
    ##### BEGIN: Data Analysis ####

    kinerjaProvinsi<-list()
    
    functionKinerjaProvinsi<- function(emissionType = NULL, emissionData= compareEmisi){
      result<-list()
      # - (sumEmisiTotalTarget - sumEmisiTotalBAU) / sumEmisiTotalBAU * 100
      # result$emisi.target <- - ( sum(emissionData[emissionData$ID=="target",]$emisi.total) - sum(emissionData[emissionData$ID=="BAU",]$emisi.total) ) / sum(emissionData[emissionData$ID=="BAU",]$emisi.total) *100
      eval(parse(text=paste0(
        'result$emisi.target<- - ( sum(emissionData[emissionData$ID=="target",]$',emissionType,') - sum(emissionData[emissionData$ID=="BAU",]$',emissionType,') ) / sum(emissionData[emissionData$ID=="BAU",]$',emissionType,') *100'
      )))
      # - (sumEmisiTotalTarget - sumEmisiTotalBAU) / sumEmisiTotalBAU * 100
      eval(parse(text=paste0(
        'result$emisi.aktual<- - ( sum(emissionData[emissionData$ID=="aktual",]$',emissionType,') - sum(emissionData[emissionData$ID=="BAU",]$',emissionType,') ) / sum(emissionData[emissionData$ID=="BAU",]$',emissionType,') *100'
      )))
    
      if (result$emisi.target==result$emisi.aktual){
        result$ketercapaian<- "terpenuhi"
      } else if (result$emisi.target <= result$emisi.aktual){
        result$ketercapaian<- "terlewati"
      } else {
        result$ketercapaian<- "tidak terpenuhi"
      }
      
      result$persenKetercapaian<- result$emisi.aktual/result$emisi.target * 100
      
      result
    }
    
    # kinerja provinsi secara keseluruhan: tercapai tidaknya target penurunan emisi kumulatif
    kinerjaProvinsi$keseluruhan<-functionKinerjaProvinsi(emissionType="emisi.total")
    
    
    # kinerja provinsi per sektor PRK: 
    
    # 1. tercapai tidaknya target penurunan emisi kumulatif per sektor PRK
    # pakai kumulatif (bukan rata2 penurunan tiap tahun), karena di dokumen RADGRK hitungnya juga dari kumulatif emisi
    kinerjaProvinsi$energi<-functionKinerjaProvinsi(emissionType="emisiEnergi")
    kinerjaProvinsi$lahan<-functionKinerjaProvinsi(emissionType="emisiLahan")
    kinerjaProvinsi$limbah<-functionKinerjaProvinsi(emissionType="emisiLimbah")
    
    # 2. sektor PRK dengan penurunan emisi (thd target) terburuk 
    # dianggap terburuk jika rasio ketercapaian (penurunan emisi aktual/penrunan emisi target) terkecil
    kinerjaProvinsiTable <- data.frame(matrix(unlist(kinerjaProvinsi), nrow=4, byrow=T),stringsAsFactors=FALSE)
    rownames(kinerjaProvinsiTable)<-names(kinerjaProvinsi)
    colnames(kinerjaProvinsiTable)<-names(kinerjaProvinsi[[1]])
    sektorTerburuk<-rownames(kinerjaProvinsiTable[which.min(kinerjaProvinsiTable$persenKetercapaian),])
    sektorTerbaik<-rownames(kinerjaProvinsiTable[which.max(kinerjaProvinsiTable$persenKetercapaian),])
    
    
    # kinerja provinsi per tahun:
    
    # 1. ketercapaian tahunan per sektor
    
    tahun=c(initialYear:finalYear)
    kinerjaProvinsi$ketercapaianTahunan <- data.frame(tahun= c(initialYear:finalYear), 
                                                      penurunan.emisi.energi=NA, 
                                                      penurunan.emisi.lahan=NA, 
                                                      penurunan.emisi.limbah=NA,
                                                      penurunan.emisi.total=NA,
                                                      stringsAsFactors = FALSE)
    for (type in colnames(aktualPenurunanEmisi[,2:5])){
      for (year in initialYear:finalYear){
        if(aktualPenurunanEmisi[tahun==year, type]<targetPenurunanEmisi[tahun==year, type]){
          kinerjaProvinsi$ketercapaianTahunan[tahun==year,type]<-"tidak terpenuhi"
        } else if(aktualPenurunanEmisi[tahun==year, type]==targetPenurunanEmisi[tahun==year, type]){
          kinerjaProvinsi$ketercapaianTahunan[tahun==year,type]<-"terpenuhi"
        }else if(aktualPenurunanEmisi[tahun==year, type]>targetPenurunanEmisi[tahun==year, type]){
          kinerjaProvinsi$ketercapaianTahunan[tahun==year,type]<-"terlewati"
        }
      }
    }
    
    # kesimpulan ketercapaian tahunan 
    for (type in colnames(aktualPenurunanEmisi[,2:5])){
      
      if (type=='penurunan.emisi.total'){
        index="keseluruhan"
      } else if(type=='penurunan.emisi.energi'){
        index="energi"
      } else if(type=='penurunan.emisi.lahan'){
        index="lahan"
      } else if(type=='penurunan.emisi.limbah'){
        index="limbah"
      } 
      
      if (all(kinerjaProvinsi$ketercapaianTahunan[,type]=="tidak terpenuhi")){
        kinerjaProvinsiTable[index,"ketercapaianTahunan"]= "tidak pernah terpenuhi"
      } else if (all(kinerjaProvinsi$ketercapaianTahunan[,type]=="terlewati")){
        kinerjaProvinsiTable[index,"ketercapaianTahunan"]= "selalu terlewati"
      } else if (all(kinerjaProvinsi$ketercapaianTahunan[,type]=="terpenuhi")){
        kinerjaProvinsiTable[index,"ketercapaianTahunan"]= "selalu terpenuhi"
      } else {
        if(length(which(kinerjaProvinsi$ketercapaianTahunan[,type]=="terpenuhi"))>length(which(kinerjaProvinsi$ketercapaianTahunan[,type]!="terpenuhi"))){
          kinerjaProvinsiTable[index,"ketercapaianTahunan"]= "hampir selalu terpenuhi"
        } else if(length(which(kinerjaProvinsi$ketercapaianTahunan[,type]=="terlewat"))>length(which(kinerjaProvinsi$ketercapaianTahunan[,type]!="terlewati"))){
          kinerjaProvinsiTable[index,"ketercapaianTahunan"]= "hampir selalu terlewati"
        } else if(length(which(kinerjaProvinsi$ketercapaianTahunan[,type]=="tidak terpenuhi"))>length(which(kinerjaProvinsi$ketercapaianTahunan[,type]!="tidak terpenuhi"))){
          kinerjaProvinsiTable[index,"ketercapaianTahunan"]= "hampir tidak pernah terpenuhi"
        } else {
          kinerjaProvinsiTable[index,"ketercapaianTahunan"]= "bervariasi"
        }
      }  
      
    }
    
    # 2. tren tahunan
    
    kinerjaProvinsi$trenTahunan <- data.frame(tahun= NA, 
                                              penurunan.emisi.energi=NA, 
                                              penurunan.emisi.lahan=NA, 
                                              penurunan.emisi.limbah=NA,
                                              penurunan.emisi.total=NA,
                                              stringsAsFactors = FALSE)
    for (type in colnames(aktualPenurunanEmisi[,2:5])){
      for (year in initialYear:(finalYear-1)){
        kinerjaProvinsi$trenTahunan[paste0(year+1,"_",year),"tahun"]<-paste0(year+1,"_",year)
        if(aktualPenurunanEmisi[tahun==year+1, type]>aktualPenurunanEmisi[tahun==year, type]){
          kinerjaProvinsi$trenTahunan[paste0(year+1,"_",year),type]<-"naik"
        } else if(aktualPenurunanEmisi[tahun==year+1, type]<aktualPenurunanEmisi[tahun==year, type]){
          kinerjaProvinsi$trenTahunan[paste0(year+1,"_",year),type]<-"turun"
        } else if(aktualPenurunanEmisi[tahun==year+1, type]==aktualPenurunanEmisi[tahun==year, type]){
          kinerjaProvinsi$trenTahunan[paste0(year+1,"_",year),type]<-"tetap"
        }
      }
    }
    kinerjaProvinsi$trenTahunan<-kinerjaProvinsi$trenTahunan[-is.na(kinerjaProvinsi$trenTahunan),] #remove initial value
    
    # kesimpulan tren tahunan
    for (type in colnames(aktualPenurunanEmisi[,2:5])){
      if (type=='penurunan.emisi.total'){
        index="keseluruhan"
      } else if(type=='penurunan.emisi.energi'){
        index="energi"
      } else if(type=='penurunan.emisi.lahan'){
        index="lahan"
      } else if(type=='penurunan.emisi.limbah'){
        index="limbah"
      } 
      if(all(kinerjaProvinsi$trenTahunan[,type]=="naik")){
        kinerjaProvinsiTable[index,"trenTahunan"] <- "penurunan emisi konsisten naik"
      } else if (all(kinerjaProvinsi$trenTahunan[,type]=="turun")){
        kinerjaProvinsiTable[index,"trenTahunan"]<- "penurunan emisi konsisten turun"
      } else if (all(kinerjaProvinsi$trenTahunan[,type]=="tetap")){
        kinerjaProvinsiTable[index,"trenTahunan"]<- "penurunan emisi konstan"
      } else {
        kinerjaProvinsiTable[index,"trenTahunan"]<- "penurunan emisi fluktuatif"
      }
    }
    
    if(kinerjaProvinsiTable["keseluruhan","ketercapaian"]=="tidak terpenuhi"){
      allRecommendation$statement <- paste0("Hasil evaluasi ketercapaian penurunan emisi provinsi ", input$prov, 
                                            " adalah ", kinerjaProvinsiTable["keseluruhan", "ketercapaian"],
                                            " dengan persentase sebesar ", kinerjaProvinsiTable["keseluruhan", "persenKetercapaian"], 
                                            ". Perfoma sektor ", sektorTerburuk, " sebagai sektor dengan capaian terendah sebesar ", kinerjaProvinsiTable[sektorTerburuk, "persenKetercapaian"],
                                            ". Tren ", kinerjaProvinsiTable["keseluruhan", "trenTahunan"], " terjadi setiap tahunnya.
                                            Oleh karena itu, tindak lanjut terhadap rencara aksi perlu dilakukan.")
    } else {
      allRecommendation$statement <- paste0("Hasil evaluasi ketercapaian penurunan emisi provinsi ", input$prov, 
                                            " adalah ", kinerjaProvinsiTable["keseluruhan", "ketercapaian"],
                                            " dengan persentase sebesar", kinerjaProvinsiTable["keseluruhan", "persenKetercapaian"], 
                                            ". Perfoma sektor ", sektorTerbaik, " sebagai sektor terbaik dengan capaian sebesar ", kinerjaProvinsiTable[sektorTerbaik, "persenKetercapaian"],
                                            ". Tren ", kinerjaProvinsiTable["keseluruhan", "trenTahunan"], " terjadi setiap tahunnya.")
      
    }
    allRecommendation$kinerjaProvinsiTable <- kinerjaProvinsiTable
  })
  
  output$emisiTotal <- renderPlotly({
    allPlot$plot.emisi.total
  })
  
  output$kumulatifEmisiTotal <- renderPlotly({
    allPlot$plot.kumulatif.emisi.total
  })
  
  output$emisiEnergi <- renderPlotly({
    allPlot$plot.emisiEnergi
  })
  
  output$kumulatifEmisiEnergi <- renderPlotly({
    allPlot$plot.kumulatif.emisi.energi
  })
  
  output$emisiLahan <- renderPlotly({
    allPlot$plot.emisiLahan
  })
  
  output$kumulatifEmisiLahan <- renderPlotly({
    allPlot$plot.kumulatif.emisi.lahan
  })
  
  output$emisiLimbah <- renderPlotly({
    allPlot$plot.emisiLimbah
  })
  
  output$kumulatifEmisiLimbah <- renderPlotly({
    allPlot$plot.kumulatif.emisi.limbah
  })
  
  output$penurunanEmisiTotal <- renderPlotly({
    allPlot$plot.penurunan.emisi.total
  })
  
  output$kumulatifPenurunanEmisiTotal <- renderPlotly({
    allPlot$plot.penurunan.kumulatif.emisi.total
  })
  
  output$penurunanEmisiEnergi <- renderPlotly({
    allPlot$plot.penurunan.emisi.energi
  })
  
  output$kumulatifPenurunanEmisiEnergi <- renderPlotly({
    allPlot$plot.penurunan.kumulatif.emisi.energi
  })
  
  output$penurunanEmisiLahan <- renderPlotly({
    allPlot$plot.penurunan.emisi.lahan
  })
  
  output$kumulatifPenurunanEmisiLahan <- renderPlotly({
    allPlot$plot.penurunan.kumulatif.emisi.lahan
  })
  
  output$penurunanEmisiLimbah <- renderPlotly({
    allPlot$plot.penurunan.emisi.limbah
  })
  
  output$kumulatifPenurunanEmisiLimbah <- renderPlotly({
    allPlot$plot.penurunan.kumulatif.emisi.limbah
  })
  
  output$kinerjaProvTbl <- renderDataTable({
    allRecommendation$kinerjaProvinsiTable
  })
  
  output$statementRecommendation <- renderText({
    allRecommendation$statement
  })
}

###*Run the application#### 
shinyApp(ui = ui, server = server)
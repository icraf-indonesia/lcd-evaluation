library(ggplot2)

selectedProv<-"sumsel"
initialYear<-2010
finalYear<-2018

dataPEP<-data.frame(read.csv(paste0("data/",selectedProv,"/dataPEP.csv")))
targetEmisi<-data.frame(read.csv(paste0("data/",selectedProv,"/targetEmisi.csv")))
bauEmisi<-data.frame(read.csv(paste0("data/",selectedProv,"/bauEmisi.csv")))

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
                         "emisi.energi",
                         "emisi.lahan",
                         "emisi.limbah")
aktualEmisi[,2:4]<-bauEmisi[,2:4]-aktualPenurunanEmisi[,2:4]

# buat tabel perbandingan emisi 

aktualEmisi$emisi.total<-rowSums(aktualEmisi[,2:4])
bauEmisi$emisi.total<-rowSums(bauEmisi[,2:4])
targetEmisi$emisi.total<-rowSums(targetEmisi[,2:4])

aktualEmisi$kumulatif.emisi.energi<-cumsum(aktualEmisi$emisi.energi)
aktualEmisi$kumulatif.emisi.lahan<-cumsum(aktualEmisi$emisi.lahan)
aktualEmisi$kumulatif.emisi.limbah<-cumsum(aktualEmisi$emisi.limbah)
aktualEmisi$kumulatif.emisi.total<-cumsum(aktualEmisi$emisi.total)

bauEmisi$kumulatif.emisi.energi<-cumsum(bauEmisi$emisi.energi)
bauEmisi$kumulatif.emisi.lahan<-cumsum(bauEmisi$emisi.lahan)
bauEmisi$kumulatif.emisi.limbah<-cumsum(bauEmisi$emisi.limbah)
bauEmisi$kumulatif.emisi.total<-cumsum(bauEmisi$emisi.total)

targetEmisi$kumulatif.emisi.energi<-cumsum(targetEmisi$emisi.energi)
targetEmisi$kumulatif.emisi.lahan<-cumsum(targetEmisi$emisi.lahan)
targetEmisi$kumulatif.emisi.limbah<-cumsum(targetEmisi$emisi.limbah)
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
}

##### END: Prepare Emission Data #####

##### BEGIN: Data Analysis ####

kinerjaProvinsi<-list()

functionKinerjaProvinsi<- function(emissionType = NULL, emissionData= compareEmisi){
  result<-list()
  eval(parse(text=paste0(
    'result$emisi.target<- - ( sum(emissionData[emissionData$ID=="target",]$',emissionType,') - sum(emissionData[emissionData$ID=="BAU",]$',emissionType,') ) / sum(emissionData[emissionData$ID=="BAU",]$',emissionType,') *100'
  )))
  eval(parse(text=paste0(
    'result$emisi.aktual<- - ( sum(emissionData[emissionData$ID=="aktual",]$',emissionType,') - sum(emissionData[emissionData$ID=="BAU",]$',emissionType,') ) / sum(emissionData[emissionData$ID=="BAU",]$',emissionType,') *100'
  )))

  if (result$emisi.target==result$emisi.aktual){
    result$ketercapaian<- "terpenuhi"
  } else if (result$emisi.target<=result$emisi.aktual){
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
kinerjaProvinsi$energi<-functionKinerjaProvinsi(emissionType="emisi.energi")
kinerjaProvinsi$lahan<-functionKinerjaProvinsi(emissionType="emisi.lahan")
kinerjaProvinsi$limbah<-functionKinerjaProvinsi(emissionType="emisi.limbah")

# 2. sektor PRK dengan penurunan emisi (thd target) terburuk 
# dianggap terburuk jika rasio ketercapaian (penurunan emisi aktual/penrunan emisi target) terkecil
kinerjaProvinsiTable <- data.frame(matrix(unlist(kinerjaProvinsi), nrow=4, byrow=T),stringsAsFactors=FALSE)
rownames(kinerjaProvinsiTable)<-names(kinerjaProvinsi)
colnames(kinerjaProvinsiTable)<-names(kinerjaProvinsi[[1]])
sektorTerburuk<-rownames(kinerjaProvinsiTable[which.min(kinerjaProvinsiTable$persenKetercapaian),])
sektorTerbaik<-rownames(kinerjaProvinsiTable[which.min(kinerjaProvinsiTable$persenKetercapaian),])


# kinerja provinsi per tahun:

# 1. ketercapaian tahunan per sektor

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

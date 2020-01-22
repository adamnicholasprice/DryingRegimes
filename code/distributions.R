## Adam N. Price
#
#
#
#
#
#
#
#
#
#
#
library(data.table)
library(here)
library(viridis)

files = list.files(here("data/reference/"),full.names = TRUE)


ap_pdf <- function(file,metric,threshold){
  dat = fread(file,sep = ",", select = c(metric))
  dat[is.na(dat)]=-999
  nf_period = rle(dat[[metric]])
  nf_period = nf_period$lengths[nf_period$values<=threshold  & nf_period$values>-999.0]
  # plot(density(nf_period))
  return(nf_period)
}


# Plot Reference Gauges

files = list.files(here("data/reference/"),full.names = TRUE,pattern = '*csv')

for (i in 1:length(files)){
  colpal = viridis(length(files))
  temp = ap_pdf(files[i],"Q_cfs",0.5)
  if (i==1){
    png("plot.png",
        width=1440,
        height=1080)
    plot(
      density(temp),
      xlim = c(0,10),
      col = colpal[i],
      main = 'PDF of Reference Gauges',
      xlab=("Number of No Flow Days"))
  }
  else if(i>1 & length(temp)>1){
    lines(
      density(temp),
      col = colpal[i],
      add=T)
    print(i)
  }
}
dev.off()

# Plot Non-reference Gauges

files = list.files(here("data/non_reference/"),full.names = TRUE)

for (i in 1:length(files)){
  colpal = viridis(length(files))
  temp = ap_pdf(files[i],"Q_cfs",0.5)
  if (i==1){
    png("plot.png",
        width=1440,
        height=1080)
    plot(
      density(temp),
      xlim = c(0,5),
      col = colpal[i],
      main = 'PDF of Non-Reference Gauges',
      xlab=("Number of No Flow Days"))
  }
  else if(i>1 & length(temp)>1){
    lines(
      density(temp),
      col = colpal[i])
    print(i)
  }
}
dev.off()


# Plot all gauges

files = list.files(here("data/all/"),full.names = TRUE)

for (i in 1:length(files)){
  colpal = viridis(length(files))
  temp = ap_pdf(files[i],"Q_cfs",0.5)
  if (i==1){
    png("plot.png",
        width=1440,
        height=1080)
    plot(
      density(temp),
      xlim = c(0,5),
      col = colpal[i],
      main = 'PDF of Non-Reference Gauges',
      xlab=("Number of No Flow Days"))
  }
  else if(i>1 & length(temp)>6){
    lines(
      density(temp),
      col = colpal[i])
    print(i)
  }
}
dev.off()





# Get all reference data nf

files = list.files(here("data/reference/"),full.names = TRUE)

ref = list()
for (i in 1:length(files)){
  temp = ap_pdf(files[i],"Q_cfs",0.5)
  ref[[i]] = temp
  print(i)
}



p <- plot_ly()



files = list.files(here("data/reference/"),full.names = TRUE)

for(i in 1:length(files)){
  temp = ap_pdf(files[i],"Q_cfs",0.5)
  if (length(temp)>10){
    p <- add_trace(p, 
                 x = density(temp)$x,
                 y = density(temp)$y,
                 type = 'scatter', 
                 mode = 'lines',
                 opacity=0.5,
                 line = list(width = 1,color = 'rgb(22, 96, 167)')
                 )
  }
  else{
    next()
  }
}


files = list.files(here("data/non_reference/"),full.names = TRUE)


for(i in 1:length(files)){
  temp = ap_pdf(files[i],"Q_cfs",0.5)
  if (length(temp)>10){
    p <- add_trace(p, 
                   x = density(temp)$x,
                   y = density(temp)$y,
                   type = 'scatter', 
                   mode = 'lines',
                   opacity=0.5,
                   line = list(width = 1,color = 'red'),
                   
    )
  }
  else{
    next()
  }
}




p <- layout(showlegend = FALSE)


p


library(raster)
library(ggplot2)

# transfer geotiff image to pdf, tiff, etc figures 
dir = '../'
fn = list.files(dir, '.tif', full.names = T)
fnname = list.files(dir, '.tif', full.names = F)

for(i in 1:length(fn)){
  data = brick(fn[i])
  name = substr(fnname[i], 1, nchar(fnname[i])-4)
  
  tiff(paste0(dir, 'pdf/', name,'.tif'), res = 300, width=5, height=5*nrow(data)/ncol(data), unit='in')
  plotRGB(data, scale = 255, stretch="lin")
  dev.off()
  
}



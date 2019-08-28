#Function 1: with number of components
#function
PCA_1image <- function(image_path,ncom=50,output_dir=getwd())
{
  library(jpeg)
  
  image=readJPEG(image_path)
  r=image[,,1]
  g=image[,,2]
  b=image[,,3]
  image.r.pca=prcomp(r,center=FALSE)
  image.g.pca=prcomp(g,center=FALSE)
  image.b.pca=prcomp(b,center=FALSE)
  rgb.pca=list(image.r.pca,image.g.pca,image.b.pca)
  R=image.r.pca$x[,1:ncomp]%*%t(image.r.pca$rotation[,1:ncomp])
  G=image.g.pca$x[,1:ncomp]%*%t(image.g.pca$rotation[,1:ncomp])
  B=image.b.pca$x[,1:ncomp]%*%t(image.b.pca$rotation[,1:ncomp])
  R=ifelse(R>1,1,R)
  G=ifelse(G>1,1,G)
  B=ifelse(B>1,1,B)
  
  R=ifelse(R<0,0,R)
  G=ifelse(G<0,0,G)
  B=ifelse(B<0,0,B)
  
  img=array(c(R,G,B),dim=dim(image))
  writeJPEG(image,paste(output_dir,"compressed_image",".jpeg",sep=""))
}



#function 2: compressed image based on the clarity


PCA_withclarity <- function(image_path,clarity=99,output_dir=getwd())
{
  library(jpeg)
  
  image=readJPEG(image_path)
  r=image[,,1]
  g=image[,,2]
  b=image[,,3]
  image.r.pca=prcomp(r,center=FALSE)
  image.g.pca=prcomp(g,center=FALSE)
  image.b.pca=prcomp(b,center=FALSE)
  rgb.pca=list(image.r.pca,image.g.pca,image.b.pca)
  a=round(cumsum(image.r.pca$sdev^2)/sum(image.r.pca$sdev^2)*100,2)
  n1=which(clarity>=a)[1]
  b=round(cumsum(image.g.pca$sdev^2)/sum(image.g.pca$sdev^2)*100,2)
  n2=which(clarity>=b)[1]
  c=round(cumsum(image.b.pca$sdev^2)/sum(image.b.pca$sdev^2)*100,2)
  n3=which(clarity>=c)[1]
  ncomp=max(n1,n2,n3)
  R=image.r.pca$x[,1:ncomp]%*%t(image.r.pca$rotation[,1:ncomp])
  G=image.g.pca$x[,1:ncomp]%*%t(image.g.pca$rotation[,1:ncomp])
  B=image.b.pca$x[,1:ncomp]%*%t(image.b.pca$rotation[,1:ncomp])
  R=ifelse(R>1,1,R)
  G=ifelse(G>1,1,G)
  B=ifelse(B>1,1,B)
  
  R=ifelse(R<0,0,R)
  G=ifelse(G<0,0,G)
  B=ifelse(B<0,0,B)
  
  img=array(c(R,G,B),dim=dim(image))
  writeJPEG(image,paste(output_dir,"compressed_image",".jpeg",sep=""))
}



#function 3:reading all images from the folder and delecting original images if requires

PCA_withfolder <- function(folder_path=getwd(),clarity=99,delete_original=F)
{
  library(jpeg)
  folder=setwd(folder_path)
  file_list <- list.files(path=folder, pattern="*.jpg")
  folder1=paste(folder,"/","Compressed Images",sep="")
  dir.create("Compressed Images")
  for (i in file_list)
  {
    image=readJPEG(i)
    r=image[,,1]
    g=image[,,2]
    b=image[,,3]
    image.r.pca=prcomp(r,center=FALSE)
    image.g.pca=prcomp(g,center=FALSE)
    image.b.pca=prcomp(b,center=FALSE)
    rgb.pca=list(image.r.pca,image.g.pca,image.b.pca)
    n1=which(clarity>=a)[1]
    b=round(cumsum(image.g.pca$sdev^2)/sum(image.g.pca$sdev^2)*100,2)
    n2=which(clarity>=b)[1]
    c=round(cumsum(image.b.pca$sdev^2)/sum(image.b.pca$sdev^2)*100,2)
    n3=which(clarity>=c)[1]
    ncomp=max(n1,n2,n3)
    R=image.r.pca$x[,1:ncomp]%*%t(image.r.pca$rotation[,1:ncomp])
    G=image.g.pca$x[,1:ncomp]%*%t(image.g.pca$rotation[,1:ncomp])
    B=image.b.pca$x[,1:ncomp]%*%t(image.b.pca$rotation[,1:ncomp])
    R=ifelse(R>1,1,R)
    G=ifelse(G>1,1,G)
    B=ifelse(B>1,1,B)
    
    R=ifelse(R<0,0,R)
    G=ifelse(G<0,0,G)
    B=ifelse(B<0,0,B)
    
    img=array(c(R,G,B),dim=dim(image))
    
    writeJPEG(image,paste(folder1,"/",i,".jpeg",sep=""))
    if (delete_original=="T")
    {
      file.remove(i)
    }
  }
}        

PCA_withfolder(folder_path = "C:/Data science/R/test/repca/Compressed/repca/test2",clarity=99,delete_original=T)       
#TRANSECT COUNTER, BR 7/13/21
#This pixel counter pulls in the defines transects and evaluates Platy,Dips,Cyph,Por
#there is no sorting into size classes, just the raw size of each polygon

# define the colors you'll be using in the string
R<-c(255/255,0,255/255,127/255)#imager assigns colors 0-1
G<-c(0,255/255,127/255,0)
B<-c(0,0,0,255/255)
#define the output names for the species
Spec<-c("Platy","Dips","Cyph","Por")

#define the input files
d1<-load.image("Filepath/T_2.png")#you need to define your filepath here
d2<-load.image("Filepath/T_3.png")
dnames<-c("d1","d2")

for(j in 1:2){
	im<-get(dnames[j])
	imr<-EBImage::resize(im,w=5000,h=1000)
	for(i in 1:4){
		Cor<-which(imr[,,,1]==R[i] & imr[,,,2]==G[i] & imr[,,,3]==B[i])#color goes 0-1
		Black<-matrix(0,5000,1000) #this is the suitable background
		Black[Cor]<-1 # now slap the corals onto it
		plot(as.Image(Black))
		x=bwlabel(Black)#this counts all non-zero connected sets of pixels
		fts = computeFeatures.shape(as.matrix(x))#the image must be turned into 2-D array
		CSize<-sort(fts[,1],decreasing=F)
		assign(paste("Sizes",Spec[i],dnames[j],sep="_"),CSize)}}

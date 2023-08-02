## Packages installation
install.packages("igraph")
library(igraph)

## Setting the working directory
setwd("/Users/jun/Library/CloudStorage/OneDrive-개인/대장암")

#### Stomach Cancer
## Loading dataset and creating network variables
node <- read.csv("NODE.csv", header=T)
edge <- read.csv("EDGE.csv", header=T)

node$p_size<-node$size/node$pop*100 # prevalence

label<-as.vector(c("Myocardial\ninfarction", 
                   "Congestive\nheart failure", 
                   "Peripheral\nvascular\ndisease", 
                   "Cerebrovascular\ndisease", 
                   "Dementia", 
                   "Chronic pulmonary\ndisease",
                   "Rheumatic\ndisease",
                   "Peptic ulcer\ndisease", 
                   "Mild liver\ndisease", 
                   "DM\nwithout C.C", 
                   "DM\nwith C.C",
                   "Hemiplegia or\nparaplegia",
                   "Renal disease", 
                   "Moderate or\nsevere liver\ndisease"))

# Reducing the weight
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}

lab.locs <- radian.rescale(x=1:14, direction=-1, start=0)
g <- erdos.renyi.game(n=14, p=1/14)
la<- layout.circle(g)

# windowsFonts(Times=windowsFont("Times New Roman"))

#Young
edge_0<-edge[edge$age==0 & edge$Cases==1, c(3,4,5)]
node_0<-node[node$age==0 & node$Cases==1, c(3,7,8)]
#Old
edge_1<-edge[edge$age==1 & edge$Cases==1, c(3,4,5)]
node_1<-node[node$age==1 & node$Cases==1, c(3,7,8)]
#All
edge_2<-edge[edge$age==2 & edge$Cases==1, c(3,4,5)]
node_2<-node[node$age==2 & node$Cases==1, c(3,7,8)]

net_0<- graph_from_data_frame(d=edge_0, vertices=node_0, directed = F)
net_1<- graph_from_data_frame(d=edge_1, vertices=node_1, directed = F)
net_2<- graph_from_data_frame(d=edge_2, vertices=node_2, directed = F)

V(net_0)$size <- V(net_0)$p_size*2; 
E(net_0)$width <- E(net_0)$support*5

V(net_1)$size <- V(net_1)$p_size; 
E(net_1)$width <- E(net_1)$support

V(net_2)$size <- V(net_2)$p_size; 
E(net_2)$width <- E(net_2)$support

#############################Cancer patients

max(E(net_0)$width)
max(V(net_0)$strength)

tiff('/Users/jun/Library/CloudStorage/OneDrive-개인/대장암/Network_b_modif.tiff', units="in", width=10, height=7.5, res=1000, compression = 'lzw')
par(mar = c(3,3,3,3))
plot(net_0, 
     layout=la, 
     edge.color=ifelse(E(net_0)$width > 10/9*2*0.8,"dimgrey",
                       ifelse(E(net_0)$width > 8/9*2*0.8,"dimgrey",
                              ifelse(E(net_0)$width > 6/9*2*0.8,"dimgrey",
                                     ifelse(E(net_0)$width > 4/9*2*0.8,"dimgrey",
                                            ifelse(E(net_0)$width > 2/9*2*0.8,"dimgrey",
                                                   ifelse(E(net_0)$width > 1.5/9*2*0.8,"dimgrey",
                                                          ifelse(E(net_0)$width > 1/9*2*0.8,"dimgrey",
                                                                 ifelse(E(net_0)$width > 0.5/9*2*0.8,"dimgrey",NA)))))))),
     vertex.color=ifelse(V(net_0)$strength > 30/5,"dodgerblue4",
                         ifelse(V(net_0)$strength > 20/5,"dodgerblue3",
                                ifelse(V(net_0)$strength > 10/5,"dodgerblue2",
                                       ifelse(V(net_0)$strength > 5/5,"dodgerblue1",
                                              ifelse(V(net_0)$strength > 1/5,"skyblue2","white"))))),
     vertex.label=label,
     vertex.label.color="black",
     vertex.label.cex=1.2,
     vertex.label.dist=3.5,
     vertex.label.degree=lab.locs
)            
dev.off()


max(E(net_1)$width)
max(V(net_1)$strength)

tiff('/Users/jun/Library/CloudStorage/OneDrive-개인/대장암/Network_e.tiff', units="in", width=10, height=7.5, res=1000, compression = 'lzw')
par(mar = c(3,3,3,3))
plot(net_1, 
     layout=la, 
     edge.color=ifelse(E(net_1)$width > 10/9*12*0.8,"dimgrey",
                       ifelse(E(net_1)$width > 8/9*12*0.8,"dimgrey",
                              ifelse(E(net_1)$width > 6/9*12*0.8,"dimgrey",
                                     ifelse(E(net_1)$width > 4/9*12*0.8,"dimgrey",
                                            ifelse(E(net_1)$width > 2/9*12*0.8,"dimgrey",
                                                   ifelse(E(net_1)$width > 1.5/9*12*0.8,"dimgrey",
                                                          ifelse(E(net_1)$width > 1/9*12*0.8,"dimgrey",
                                                                 ifelse(E(net_1)$width > 0.5/9*12*0.8,"dimgrey",NA)))))))),
     vertex.color=ifelse(V(net_1)$strength > 30*2,"dodgerblue4",
                         ifelse(V(net_1)$strength > 20*2,"dodgerblue3",
                                ifelse(V(net_1)$strength > 10*2,"dodgerblue2",
                                       ifelse(V(net_1)$strength > 5*2,"dodgerblue1",
                                              ifelse(V(net_1)$strength > 1*2,"skyblue2","white"))))),
     vertex.label=label,
     vertex.label.color="black",
     vertex.label.cex=1.2,
     vertex.label.dist=3.5,
     vertex.label.degree=lab.locs
)            
dev.off()


max(E(net_2)$width)
max(V(net_2)$strength)

tiff('/Users/jun/Library/CloudStorage/OneDrive-개인/대장암/Network_a.tiff', units="in", width=10, height=7.5, res=1000, compression = 'lzw')
par(mar = c(3,3,3,3))
plot(net_2, 
     layout=la, 
     edge.color=ifelse(E(net_2)$width > 10/9*6*0.8,"dimgrey",
                       ifelse(E(net_2)$width > 8/9*6*0.8,"dimgrey",
                              ifelse(E(net_2)$width > 6/9*6*0.8,"dimgrey",
                                     ifelse(E(net_2)$width > 4/9*6*0.8,"dimgrey",
                                            ifelse(E(net_2)$width > 2/9*6*0.8,"dimgrey",
                                                   ifelse(E(net_2)$width > 1.5/9*6*0.8,"dimgrey",
                                                          ifelse(E(net_2)$width > 1/9*6*0.8,"dimgrey",
                                                                 ifelse(E(net_2)$width > 0.5/9*6*0.8,"dimgrey",NA)))))))),
     vertex.color=ifelse(V(net_2)$strength > 30,"dodgerblue4",
                         ifelse(V(net_2)$strength > 20,"dodgerblue3",
                                ifelse(V(net_2)$strength > 10,"dodgerblue2",
                                       ifelse(V(net_2)$strength > 5,"dodgerblue1",
                                              ifelse(V(net_2)$strength > 1,"skyblue2","white"))))),
     vertex.label=label,
     vertex.label.color="black",
     vertex.label.cex=1.2,
     vertex.label.dist=3.5,
     vertex.label.degree=lab.locs
)            
dev.off()


#############################Cancer patients

min(E(net_0)$width)
max(V(net_0)$strength)

tiff('/Users/jun/Library/CloudStorage/OneDrive-개인/대장암/Network_Under2.tiff', units="in", width=10, height=7.5, res=1000, compression = 'lzw')
par(mar = c(3,3,3,3))
plot(net_0, 
     layout=la, 
     edge.color=ifelse(E(net_0)$width > 0.05,"dimgrey",NA),
     vertex.color=ifelse(V(net_0)$strength > 30/6,"dodgerblue4",
                         ifelse(V(net_0)$strength > 20/6,"dodgerblue3",
                                ifelse(V(net_0)$strength > 10/6,"dodgerblue2",
                                       ifelse(V(net_0)$strength > 5/6,"dodgerblue1",
                                              ifelse(V(net_0)$strength > 1/6,"skyblue2","white"))))),
     vertex.label=label,
     vertex.label.color="black",
     vertex.label.cex=1.2,
     vertex.label.dist=3.5,
     vertex.label.degree=lab.locs
)            
dev.off()


max(E(net_1)$width)
max(V(net_1)$strength)

tiff('/Users/jun/Library/CloudStorage/OneDrive-개인/대장암/Network_Over2.tiff', units="in", width=10, height=7.5, res=1000, compression = 'lzw')
par(mar = c(3,3,3,3))
plot(net_1, 
     layout=la, 
     edge.color=ifelse(E(net_0)$width > 0.05,"dimgrey",NA),
     vertex.color=ifelse(V(net_1)$strength > 30*2,"dodgerblue4",
                         ifelse(V(net_1)$strength > 20*2,"dodgerblue3",
                                ifelse(V(net_1)$strength > 10*2,"dodgerblue2",
                                       ifelse(V(net_1)$strength > 5*2,"dodgerblue1",
                                              ifelse(V(net_1)$strength > 1*2,"skyblue2","white"))))),
     vertex.label=label,
     vertex.label.color="black",
     vertex.label.cex=1.2,
     vertex.label.dist=3.5,
     vertex.label.degree=lab.locs
)            
dev.off()
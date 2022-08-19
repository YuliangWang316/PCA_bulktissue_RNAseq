data<-read.table("D:/gene_fpkm_matrix.txt",header = T,row.names = 1)
data<-data[,c(1:10,21:40)]
data_t<-as.data.frame(t(data))
library(FactoMineR)
library(ggplot2)
library(ggrepel)

gene.pca <- PCA(data_t, ncp = 2, scale.unit = TRUE, graph = FALSE)

pca_sample <- data.frame(gene.pca$ind$coord[ ,1:2])
pca_sample$Sample=row.names(pca_sample)

pca_eig1 <- round(gene.pca$eig[1,2], 2)
pca_eig2 <- round(gene.pca$eig[2,2],2 )
pca_sample$Group<-c(rep("persist",10),rep("SR",10),rep("para",10))
pca_sample$Group<-c(rep("G1",10),rep("G2",10),rep("G3",10),rep("G4",10))
#pca_sample <- merge(pca_sample, group,by="Sample")
library(ggplot2)
p <- ggplot(data = pca_sample, aes(x = Dim.1, y = Dim.2)) +
  geom_point(aes(color = Group), size = 2) + 
  scale_color_manual(values = c('orange', 'purple','black',"blue")) +  
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), 
        legend.key = element_rect(fill = 'transparent')) + 
  labs(x =  paste('PCA1:', pca_eig1, '%'), y = paste('PCA2:', pca_eig2, '%'), color = '')  

p

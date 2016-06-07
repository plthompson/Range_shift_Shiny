load("Range_shift_heatplots.RData")
library(shiny)
library(igraph)
library(ggplot2)
library(ggExtra)
library(betalink)
library(dplyr)
library(viridis)

nCom<-200
Tmax<-3000
burnL<-2000
sampleV<-seq(2000,7000,by=50)
burn<-rep(1,burnL)
StressV<-c(burn,seq(1,Tmax),rep(Tmax,burnL))
maxStress<-20
Stress<-seq(0,maxStress,(maxStress)/(Tmax-1))
maxEnv1<-80
ComStart<-rev(seq(1,(maxEnv1),(maxEnv1-1)/(nCom-1)))
envStep<-maxEnv1/nCom
climateV<-round(Stress[StressV][sampleV]/envStep)
itime<-2000
dispV<-c(0.001,0.01,0.5)
Web_types<-c("Comp","Mix","FW")
nprey<-40
npred1<-24
npred2<-16

net_f<-function(Com,Ints){
  
  Int_strength<-abs(Ints*rep(Com,each=length(Com)))
  Int_strength[Com==0,]<-0
  mean_Int_strength<-mean(Int_strength[Int_strength>0])
  Int_strength[Int_strength<mean_Int_strength]<-0
  Ints2<-1*Int_strength>0
  hold.df<-t(data.frame(Ints2[Com>0,Com>0]))
  net1<-graph.adjacency(hold.df)
  return(net1) 
}

shinyServer(function(input, output, session) {
  
  data1<-reactive({
    data<-filter(Net_inds_3,Dispersal==dispV[as.numeric(input$disp_select)],Community==c("Competition","Mixed interactions","Food web")[as.numeric(input$com_type)])
    data
  })
  
  data_web<-reactive({
    Com_select<-paste(Web_types[as.numeric(input$com_type)],input$disp_select,sep="")
    net_data<-list(Com=Com_list[[Com_select]], Ints=Int_list[[as.numeric(input$com_type)]])
    net_data
  })
  
  output$heatmap<-renderPlot({
    input$com_type
    
    if(input$contrast==0){
      ggplot(data1(),aes_string(y="patch",x="time",fill=input$response))+
        geom_raster()+
        geom_point(data = filter(data1(),patch==input$i_patch,time==itime),aes_string(y="patch",x="time",fill=input$response),size=5,pch=22, color="black", stroke=2)+
        geom_point(data = filter(data1(),patch==(input$i_patch+as.numeric(input$contrast)),time==round(input$f_time)),aes_string(y="patch",x="time",fill=input$response),size=5,pch=22, color="white", stroke=2)+
        theme_bw(base_size = 16)+
        removeGrid()+
        scale_color_viridis(option = "D",name="")+
        scale_fill_viridis(option = "D",name="")+
        ylab("Patch")+
        xlab("Time")} else {
          
          ggplot(data1(),aes_string(y="patch",x="time",fill=input$response))+
            geom_raster()+
            geom_point(data = filter(data1(),patch==input$i_patch,time==itime),aes_string(y="patch",x="time",fill=input$response),size=5,pch=22, color="black", stroke=2)+
            geom_point(data = filter(data1(),patch==(input$i_patch+climateV[which(sampleV==input$f_time)]),time==round(input$f_time)),aes_string(y="patch",x="time",fill=input$response),size=5,pch=22, color="white", stroke=2)+
            theme_bw(base_size = 16)+
            removeGrid()+
            scale_color_viridis(option = "D",name="")+
            scale_fill_viridis(option = "D",name="")+
            ylab("Patch")+
            xlab("Time")}
  })
  
  
  
  
  
  
  
  
  
  output$netfig<-renderPlot({
    data2<-data_web()
    Com<-data2$Com
    Ints<-data2$Ints
    diag(Ints)<-0
    
    if(as.numeric(input$com_type)==3){
      colnames(Ints)<-rownames(Ints)<-c(paste("p",1:nprey),paste('h',1:npred1),paste("c",1:npred2)) 
      
      net1<-net_f(Com=Com[,which(sampleV==itime),(input$i_patch-50)],Ints)
      if(input$contrast==50){
        net2<-net_f(Com=Com[,which(sampleV==round(input$f_time)),((input$i_patch+climateV[which(sampleV==input$f_time)])-50)],Ints)
      } else {
        net2<-net_f(Com=Com[,which(sampleV==round(input$f_time)),(input$i_patch-50)],Ints)
      }
      
      fullweb<-net_f(rowSums(apply(Com,3,rowSums))>0,Ints)
      
      w2e<-substring(get.edgelist(fullweb),1,1)
      w2e_remove<-w2e[,1]=="h" & w2e[,2]=="p" | w2e[,1]=="c" & w2e[,2]=="h"
      fullweb<-delete_edges(fullweb,E(fullweb)[w2e_remove])
      
      set.seed(2)
      
      lay.mat<-data.frame(x=runif(vcount(fullweb)),y=as.numeric(factor(substring(V(fullweb)$name,1,1),levels = c("p","h","c"),ordered = T)),shape=as.numeric(factor(substring(V(fullweb)$name,1,1),levels = c("p","h","c"),ordered = T)),num=as.numeric(substring(V(fullweb)$name,3,4)), order=1:vcount(fullweb),name=as.character(V(fullweb)$name))
      lay.mat<-lay.mat%>%
        arrange(num)%>%
        group_by(y)%>%
        mutate(x=seq(0,1,length=n())[sample(1:n(),size = n(),replace=F)])%>%
        ungroup()%>%
        mutate(y=replace(y,y==2,2.25), y=replace(y,y==1,sample(seq(0.5,1.5, by=0.01),size = length(y[y==1]),replace = T)))
      
      lay.mat<-lay.mat[order(lay.mat$order),]
      
      lay.mat$name<-as.character(lay.mat$name)
      
      
      nets_namesV<-V(metaweb(list(net1,net2)))$name
      
      lay.mat_sub<-lay.mat%>%
        filter(name %in% nets_namesV)
      
      lay.mat_sub<-lay.mat_sub[match(nets_namesV,lay.mat_sub$name),]
      par(pty="s")
      network_betaplot(net1,net2,layout=as.matrix(lay.mat_sub[,1:2]),ns = "dodgerblue3",nb = "#ff7f00",na = "grey",vertex.label=NA,vertex.size=8,vertex.shape="circle",edge.arrow.size=0.5,rescale=F,asp=F, ylim=c(0.5,3.5),xlim=c(0,1))
    } else{
      colnames(Ints)<-rownames(Ints)<-paste("s",1:nrow(Com))
      
      net1<-net_f(Com=Com[,which(sampleV==itime),(input$i_patch-50)],Ints)
      if(input$contrast==50){
        net2<-net_f(Com=Com[,which(sampleV==round(input$f_time)),((input$i_patch+climateV[which(sampleV==input$f_time)])-50)],Ints)
      } else {
        net2<-net_f(Com=Com[,which(sampleV==round(input$f_time)),(input$i_patch-50)],Ints)}
      
      fullweb<-net_f(rowSums(apply(Com,3,rowSums))>0,Ints)
      
      lay.mat<-as.data.frame(layout_in_circle(fullweb))
      set.seed(2)
      lay.mat<-lay.mat[sample(1:vcount(fullweb),vcount(fullweb),replace=F),]
      lay.mat$name<-V(fullweb)$name
      
      nets_namesV<-V(metaweb(list(net1,net2)))$name
      
      lay.mat_sub<-lay.mat%>%
        filter(name %in% nets_namesV)
      
      lay.mat_sub<-lay.mat_sub[match(nets_namesV,lay.mat_sub$name),]
      par(pty="s")
      network_betaplot(net1,net2,layout=as.matrix(lay.mat_sub[,1:2]),ns = "dodgerblue3",nb = "#ff7f00",na = "grey",vertex.label=NA,vertex.size=8,vertex.shape="circle",edge.arrow.size=0.5,rescale=F,asp=F, xlim=c(-1,1), ylim=c(-1,1))
    }
  })
  output$legend<-renderPlot({
    par(mar=c(0,0,0,0),oma=c(0,0,0,0))
    plot.new()
    legend("top",legend = c("Retained","Lost","Gained"),pch=21,pt.bg=c("dodgerblue3","grey","#ff7f00"),bty='n', cex=2,horiz = F)
  })
  
})

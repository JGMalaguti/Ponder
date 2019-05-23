Amostragem_Ponder_Harmonica_VN=function(Matriz, n,K){
  Tam=dim(Matriz)
  
  MatPop=matrix(1, nrow=Tam[1], ncol = Tam[2])
  MatViz=matrix(0, nrow=Tam[1], ncol = Tam[2])
  
  Pop=sum(MatPop)
  
  Teste=matrix(1:Pop, nrow=Tam[1], ncol = Tam[2], byrow = T)
  
  Am=sample(1:Pop,1)
  W=which(Teste==Am)
  
  Amostra=Matriz[W]
  MatPop[W]=0
  
  if((W-Tam[1])>0){
    MatViz[(W-Tam[1])]=1
  }
  if((W+Tam[1])<=prod(Tam)){
    MatViz[(W+Tam[1])]=1
  }
  if((W-1)%%Tam[1]!=0){
    MatViz[(W-1)]=1
  }
  if((W+1)%%Tam[1]!=1){
    MatViz[(W+1)]=1
  }
  
  Viz1=sum(MatViz)
  Pop=sum(MatPop)
  Pb=Viz1/Pop
  Pa=1-Pb
  
  W=(1-K*Pb)/Pa
  MatPesos=(K/Pop)*MatViz+(W/Pop)*(MatPop-MatViz)
  
  Size=1
  
  while(Size<n){
    
    VetPesos=as.vector(MatPesos)
    Quebras=cumsum(VetPesos)
    U=runif(1)
    Am=which(U<=Quebras)[1]
    
    W=which(Teste==Am)
    
    if(MatPop[W]==1){
      
      Amostra=c(Amostra, Matriz[W])
      MatPop[W]=0
      
      if((W-Tam[1])>0){
        MatViz[(W-Tam[1])]=MatViz[(W-Tam[1])]+1
      }
      if((W+Tam[1])<=prod(Tam)){
        MatViz[(W+Tam[1])]=MatViz[(W+Tam[1])]+1
      }
      if((W-1)%%Tam[1]!=0){
        MatViz[(W-1)]=MatViz[(W-1)]+1
      }
      if((W+1)%%Tam[1]!=1){
        MatViz[(W+1)]=MatViz[(W+1)]+1
      }
      
      MatViz=MatViz*MatPop
      
      Viz1=sum(MatViz==1)
      Viz2=sum(MatViz==2)
      Viz3=sum(MatViz==3)
      Viz4=sum(MatViz==4)
      
      Pop=sum(MatPop)
      
      Pb=Viz1/Pop
      Pc=Viz2/Pop
      Pd=Viz3/Pop
      Pe=Viz4/Pop
      
      Pa=1-(Pb+Pc+Pd+Pe)
      
      W=(1-(K*Pb+(K/2)*Pc+(K/3)*Pd+(K/4)*Pe))/Pa
      MatPesos=(K/Pop)*(MatViz==1)+(K/(2*Pop))*(MatViz==2)+(K/(3*Pop))*(MatViz==3)+(K/(4*Pop))*(MatViz==4)+
        (W/Pop)*(MatViz==0)
      
    }
    
    Size=length(Amostra)
    
    if(sum(MatPop)==0){break}
    
  }
  
  return(Amostra)
  
}
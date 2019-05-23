Amostragem_Ponder_Simples_VN=function(Matriz, n, K){
  
  Tam=dim(Matriz)
  
  MatPop=matrix(1, nrow=Tam[1], ncol = Tam[2])
  MatViz1=matrix(0, nrow=Tam[1], ncol = Tam[2])
  
  Pop=sum(MatPop)
  
  Teste=matrix(1:Pop, nrow=Tam[1], ncol = Tam[2], byrow = T)
  
  Am=sample(1:Pop,1)
  W=which(Teste==Am)
  
  Amostra=Matriz[W]
  MatPop[W]=0
  
  if((W-Tam[1])>0){
    MatViz1[(W-Tam[1])]=1
  }
  if((W+Tam[1])<=prod(Tam)){
    MatViz1[(W+Tam[1])]=1
  }
  if((W-1)%%Tam[1]!=0){
    MatViz1[(W-1)]=1
  }
  if((W+1)%%Tam[1]!=1){
    MatViz1[(W+1)]=1
  }
  
  Pop=sum(MatPop)
  Viz1=sum(MatViz1)
  Pb=Viz1/Pop
  Pa=1-Pb
  
  W=(1-K*Pb)/Pa
  MatPesos=(K/Pop)*MatViz1+(W/Pop)*(MatPop-MatViz1)
  
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
        MatViz1[(W-Tam[1])]=1
      }
      if((W+Tam[1])<=prod(Tam)){
        MatViz1[(W+Tam[1])]=1
      }
      if((W-1)%%Tam[1]!=0){
        MatViz1[(W-1)]=1
      }
      if((W+1)%%Tam[1]!=1){
        MatViz1[(W+1)]=1
      }
      
      MatViz1=MatViz1*MatPop
      Viz1=sum(MatViz1)
      Pop=sum(MatPop)
      Pb=Viz1/Pop
      Pa=1-Pb
      
      W=(1-K*Pb)/Pa
      MatPesos=(K/Pop)*MatViz1+(W/Pop)*(MatPop-MatViz1)
      
    }
    
    Size=length(Amostra)
    
    if(sum(MatPop)==0){break}
    
  }
  
  return(Amostra)
  
}
Amostragem_Ponder_Simples_EVN=function(Matriz, n, K){
  
  Tam=dim(Matriz)
  
  MatPop=matrix(1, nrow=Tam[1], ncol = Tam[2])
  MatViz1=matrix(0, nrow=Tam[1], ncol = Tam[2])
  
  Pop=sum(MatPop)
  
  Teste=matrix(1:Pop, nrow=Tam[1], ncol = Tam[2], byrow = T)
  
  Am=sample(1:Pop,1)
  W=which(Teste==Am)
  
  Amostra=Matriz[W]
  MatPop[W]=0
  
  if((W-Tam[1])>0){
    MatViz1[(W-Tam[1])]=1
    if((W-2*Tam[1])>0){
      MatViz1[(W-2*Tam[1])]=1
    }
  }
  if((W+Tam[1])<=prod(Tam)){
    MatViz1[(W+Tam[1])]=1
    if((W+2*Tam[1])<=prod(Tam)){
      MatViz1[(W+2*Tam[1])]=1
    }
  }
  if((W-1)%%Tam[1]!=0){
    MatViz1[(W-1)]=1
    if((W-2)%%Tam[1]!=0){
      MatViz1[(W-2)]=1
    }
  }
  if((W+1)%%Tam[1]!=1){
    MatViz1[(W+1)]=1
    if((W+2)%%Tam[1]!=1){
      MatViz1[(W+2)]=1
    }
  }
  
  Pop=sum(MatPop)
  Viz1=sum(MatViz1)
  Pb=Viz1/Pop
  Pa=1-Pb
  
  W=(1-K*Pb)/Pa
  MatPesos=(K/Pop)*MatViz1+(W/Pop)*(MatPop-MatViz1)
  
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
        MatViz1[(W-Tam[1])]=1
        if((W-2*Tam[1])>0){
          MatViz1[(W-2*Tam[1])]=1
        }
      }
      if((W+Tam[1])<=prod(Tam)){
        MatViz1[(W+Tam[1])]=1
        if((W+2*Tam[1])<=prod(Tam)){
          MatViz1[(W+2*Tam[1])]=1
        }
      }
      if((W-1)%%Tam[1]!=0){
        MatViz1[(W-1)]=1
        if((W-2)%%Tam[1]!=0){
          MatViz1[(W-2)]=1
        }
      }
      if((W+1)%%Tam[1]!=1){
        MatViz1[(W+1)]=1
        if((W+2)%%Tam[1]!=1){
          MatViz1[(W+2)]=1
        }
      }
      
      Pop=sum(MatPop)
      MatViz1=MatViz1*MatPop
      Viz1=sum(MatViz1)
      Pb=Viz1/Pop
      Pa=1-Pb
      
      W=(1-K*Pb)/Pa
      MatPesos=(K/Pop)*MatViz1+(W/Pop)*(MatPop-MatViz1)
      
    }
    
    Size=length(Amostra)
    
    if(sum(MatPop)==0){break}
    
  }
  
  return(Amostra)
  
}
Amostragem_Ponder_Simples_Moore=function(Matriz, n, K){
  
  Tam=dim(Matriz)
  
  MatPop=matrix(1, nrow=Tam[1], ncol = Tam[2])
  MatViz1=matrix(0, nrow=Tam[1], ncol = Tam[2])
  
  Pop=sum(MatPop)
  
  Teste=matrix(1:Pop, nrow=Tam[1], ncol = Tam[2], byrow = T)
  
  Am=sample(1:Pop,1)
  W=which(Teste==Am)
  
  Amostra=Matriz[W]
  MatPop[W]=0
  
  if((W-Tam[1])>0){
    MatViz1[(W-Tam[1])]=1
    
    if((W-Tam[1]-1)%%Tam[1]!=0){
      MatViz1[(W-Tam[1]-1)]=1
    }
    if((W-Tam[1]+1)%%Tam[1]!=1){
      MatViz1[(W-Tam[1]+1)]=1
    }
    
  }
  if((W+Tam[1])<=prod(Tam)){
    MatViz1[(W+Tam[1])]=1
    
    if((W+Tam[1]-1)%%Tam[1]!=0){
      MatViz1[(W+Tam[1]-1)]=1
    }
    if((W+Tam[1]+1)%%Tam[1]!=1){
      MatViz1[(W+Tam[1]+1)]=1
    }
    
  }
  if((W-1)%%Tam[1]!=0){
    MatViz1[(W-1)]=1
  }
  if((W+1)%%Tam[1]!=1){
    MatViz1[(W+1)]=1
  }
  
  Viz1=sum(MatViz1)
  Pop=sum(MatPop)
  Pb=Viz1/Pop
  Pa=1-Pb
  
  W=(1-K*Pb)/Pa
  MatPesos=(K/Pop)*MatViz1+(W/Pop)*(MatPop-MatViz1)
  
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
        MatViz1[(W-Tam[1])]=1
        
        if((W-Tam[1]-1)%%Tam[1]!=0){
          MatViz1[(W-Tam[1]-1)]=1
        }
        if((W-Tam[1]+1)%%Tam[1]!=1){
          MatViz1[(W-Tam[1]+1)]=1
        }
        
      }
      if((W+Tam[1])<=prod(Tam)){
        MatViz1[(W+Tam[1])]=1
        
        if((W+Tam[1]-1)%%Tam[1]!=0){
          MatViz1[(W+Tam[1]-1)]=1
        }
        if((W+Tam[1]+1)%%Tam[1]!=1){
          MatViz1[(W+Tam[1]+1)]=1
        }
        
      }
      if((W-1)%%Tam[1]!=0){
        MatViz1[(W-1)]=1
      }
      if((W+1)%%Tam[1]!=1){
        MatViz1[(W+1)]=1
      }
      
      MatViz1=MatViz1*MatPop
      Viz1=sum(MatViz1)
      Pop=sum(MatPop)
      Pb=Viz1/Pop
      Pa=1-Pb
      
      W=(1-K*Pb)/Pa
      MatPesos=(K/Pop)*MatViz1+(W/Pop)*(MatPop-MatViz1)
      
    }
    
    Size=length(Amostra)
    
    if(sum(MatPop)==0){break}
    
  }
  
  return(Amostra)
  
  
}
Amostragem_Ponder_Simples_Manhattan=function(Matriz, n, K){
  
  Tam=dim(Matriz)
  
  MatPop=matrix(1, nrow=Tam[1], ncol = Tam[2])
  MatViz1=matrix(0, nrow=Tam[1], ncol = Tam[2])
  
  Pop=sum(MatPop)
  
  Teste=matrix(1:Pop, nrow=Tam[1], ncol = Tam[2], byrow = T)
  
  Am=sample(1:Pop,1)
  W=which(Teste==Am)
  
  Amostra=Matriz[W]
  MatPop[W]=0
  
  if((W-Tam[1])>0){
    MatViz1[(W-Tam[1])]=1
    
    if((W-Tam[1]-1)%%Tam[1]!=0){
      MatViz1[(W-Tam[1]-1)]=1
    }
    if((W-Tam[1]+1)%%Tam[1]!=1){
      MatViz1[(W-Tam[1]+1)]=1
    }
    if((W-2*Tam[1])>0){
      MatViz1[(W-2*Tam[1])]=1
    }
    
  }
  if((W+Tam[1])<=prod(Tam)){
    MatViz1[(W+Tam[1])]=1
    
    if((W+Tam[1]-1)%%Tam[1]!=0){
      MatViz1[(W+Tam[1]-1)]=1
    }
    if((W+Tam[1]+1)%%Tam[1]!=1){
      MatViz1[(W+Tam[1]+1)]=1
    }
    if((W+2*Tam[1])<=prod(Tam)){
      MatViz1[(W+2*Tam[1])]=1
    }
  }
  if((W-1)%%Tam[1]!=0){
    MatViz1[(W-1)]=1
    if((W-2)%%Tam[1]!=0){
      MatViz1[(W-2)]=1
    }
  }
  if((W+1)%%Tam[1]!=1){
    MatViz1[(W+1)]=1
    if((W+2)%%Tam[1]!=1){
      MatViz1[(W+2)]=1
    }
  }
  
  Viz1=sum(MatViz1)
  Pop=sum(MatPop)
  Pb=Viz1/Pop
  Pa=1-Pb
  
  W=(1-K*Pb)/Pa
  MatPesos=(K/Pop)*MatViz1+(W/Pop)*(MatPop-MatViz1)
  
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
        MatViz1[(W-Tam[1])]=1
        
        if((W-Tam[1]-1)%%Tam[1]!=0){
          MatViz1[(W-Tam[1]-1)]=1
        }
        if((W-Tam[1]+1)%%Tam[1]!=1){
          MatViz1[(W-Tam[1]+1)]=1
        }
        if((W-2*Tam[1])>0){
          MatViz1[(W-2*Tam[1])]=1
        }
        
      }
      if((W+Tam[1])<=prod(Tam)){
        MatViz1[(W+Tam[1])]=1
        
        if((W+Tam[1]-1)%%Tam[1]!=0){
          MatViz1[(W+Tam[1]-1)]=1
        }
        if((W+Tam[1]+1)%%Tam[1]!=1){
          MatViz1[(W+Tam[1]+1)]=1
        }
        if((W+2*Tam[1])<=prod(Tam)){
          MatViz1[(W+2*Tam[1])]=1
        }
      }
      if((W-1)%%Tam[1]!=0){
        MatViz1[(W-1)]=1
        if((W-2)%%Tam[1]!=0){
          MatViz1[(W-2)]=1
        }
      }
      if((W+1)%%Tam[1]!=1){
        MatViz1[(W+1)]=1
        if((W+2)%%Tam[1]!=1){
          MatViz1[(W+2)]=1
        }
      }
      
      MatViz1=MatViz1*MatPop
      Viz1=sum(MatViz1)
      Pop=sum(MatPop)
      Pb=Viz1/Pop
      Pa=1-Pb
      
      W=(1-K*Pb)/Pa
      MatPesos=(K/Pop)*MatViz1+(W/Pop)*(MatPop-MatViz1)
      
    }
    
    Size=length(Amostra)
    
    if(sum(MatPop)==0){break}
    
  }
  
  return(Amostra)
  
}

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
Amostragem_Ponder_Harmonica_EVN=function(Matriz, n, K){
  
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
    if((W-2*Tam[1])>0){
      MatViz[(W-2*Tam[1])]=1
    }
  }
  if((W+Tam[1])<=prod(Tam)){
    MatViz[(W+Tam[1])]=1
    if((W+2*Tam[1])<=prod(Tam)){
      MatViz[(W+2*Tam[1])]=1
    }
  }
  if((W-1)%%Tam[1]!=0){
    MatViz[(W-1)]=1
    if((W-2)%%Tam[1]!=0){
      MatViz[(W-2)]=1
    }
  }
  if((W+1)%%Tam[1]!=1){
    MatViz[(W+1)]=1
    if((W+2)%%Tam[1]!=1){
      MatViz[(W+2)]=1
    }
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
        if((W-2*Tam[1])>0){
          MatViz[(W-2*Tam[1])]=MatViz[(W-2*Tam[1])]+1
        }
      }
      if((W+Tam[1])<=prod(Tam)){
        MatViz[(W+Tam[1])]=MatViz[(W+Tam[1])]+1
        if((W+2*Tam[1])<=prod(Tam)){
          MatViz[(W+2*Tam[1])]=MatViz[(W+2*Tam[1])]+1
        }
      }
      if((W-1)%%Tam[1]!=0){
        MatViz[(W-1)]=MatViz[(W-1)]+1
        if((W-2)%%Tam[1]!=0){
          MatViz[(W-2)]=MatViz[(W-2)]+1
        }
      }
      if((W+1)%%Tam[1]!=1){
        MatViz[(W+1)]=MatViz[(W+1)]+1
        if((W+2)%%Tam[1]!=1){
          MatViz[(W+2)]=MatViz[(W+2)]+1
        }
      }
      
      MatViz=MatViz*MatPop
      
      Viz1=sum(MatViz==1)
      Viz2=sum(MatViz==2)
      Viz3=sum(MatViz==3)
      Viz4=sum(MatViz==4)
      Viz5=sum(MatViz==5)
      Viz6=sum(MatViz==6)
      Viz7=sum(MatViz==7)
      Viz8=sum(MatViz==8)
      
      Pop=sum(MatPop)
      
      Pb=Viz1/Pop
      Pc=Viz2/Pop
      Pd=Viz3/Pop
      Pe=Viz4/Pop
      Pf=Viz5/Pop
      Pg=Viz6/Pop
      Ph=Viz7/Pop
      Pi=Viz8/Pop
      
      Pa=1-(Pb+Pc+Pd+Pe+Pf+Pg+Ph+Pi)
      
      W=(1-(K*Pb+(K/2)*Pc+(K/3)*Pd+(K/4)*Pe+(K/5)*Pf+(K/6)*Pg+(K/7)*Ph+(K/8)*Pi))/Pa
      MatPesos=(K/Pop)*(MatViz==1)+(K/(2*Pop))*(MatViz==2)+(K/(3*Pop))*(MatViz==3)+(K/(4*Pop))*(MatViz==4)+
        (K/(5*Pop))*(MatViz==5)+(K/(6*Pop))*(MatViz==6)+(K/(7*Pop))*(MatViz==7)+(K/(8*Pop))*(MatViz==8)+
        (W/Pop)*(MatViz==0)
      
    }
    
    Size=length(Amostra)
    
    if(sum(MatPop)==0){break}
    
  }
  
  return(Amostra)
  
  
}
Amostragem_Ponder_Harmonica_Moore=function(Matriz, n, K){
  
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
    
    if((W-Tam[1]-1)%%Tam[1]!=0){
      MatViz[(W-Tam[1]-1)]=1
    }
    if((W-Tam[1]+1)%%Tam[1]!=1){
      MatViz[(W-Tam[1]+1)]=1
    }
    
  }
  if((W+Tam[1])<=prod(Tam)){
    MatViz[(W+Tam[1])]=1
    
    if((W+Tam[1]-1)%%Tam[1]!=0){
      MatViz[(W+Tam[1]-1)]=1
    }
    if((W+Tam[1]+1)%%Tam[1]!=1){
      MatViz[(W+Tam[1]+1)]=1
    }
    
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
        
        if((W-Tam[1]-1)%%Tam[1]!=0){
          MatViz[(W-Tam[1]-1)]=MatViz[(W-Tam[1]-1)]+1
        }
        if((W-Tam[1]+1)%%Tam[1]!=1){
          MatViz[(W-Tam[1]+1)]=MatViz[(W-Tam[1]+1)]+1
        }
        
      }
      if((W+Tam[1])<=prod(Tam)){
        MatViz[(W+Tam[1])]=MatViz[(W+Tam[1])]+1
        
        
        if((W+Tam[1]-1)%%Tam[1]!=0){
          MatViz[(W+Tam[1]-1)]=MatViz[(W+Tam[1]-1)]+1
        }
        if((W+Tam[1]+1)%%Tam[1]!=1){
          MatViz[(W+Tam[1]+1)]=MatViz[(W+Tam[1]+1)]+1
        }
        
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
      Viz5=sum(MatViz==5)
      Viz6=sum(MatViz==6)
      Viz7=sum(MatViz==7)
      Viz8=sum(MatViz==8)
      
      Pop=sum(MatPop)
      
      Pb=Viz1/Pop
      Pc=Viz2/Pop
      Pd=Viz3/Pop
      Pe=Viz4/Pop
      Pf=Viz5/Pop
      Pg=Viz6/Pop
      Ph=Viz7/Pop
      Pi=Viz8/Pop
      
      Pa=1-(Pb+Pc+Pd+Pe+Pf+Pg+Ph+Pi)
      
      W=(1-(K*Pb+(K/2)*Pc+(K/3)*Pd+(K/4)*Pe+(K/5)*Pf+(K/6)*Pg+(K/7)*Ph+(K/8)*Pi))/Pa
      MatPesos=(K/Pop)*(MatViz==1)+(K/(2*Pop))*(MatViz==2)+(K/(3*Pop))*(MatViz==3)+(K/(4*Pop))*(MatViz==4)+
        (K/(5*Pop))*(MatViz==5)+(K/(6*Pop))*(MatViz==6)+(K/(7*Pop))*(MatViz==7)+(K/(8*Pop))*(MatViz==8)+
        (W/Pop)*(MatViz==0)
      
    }
    
    Size=length(Amostra)
    
    if(sum(MatPop)==0){break}
    
  }
  
  return(Amostra)
  
  
}
Amostragem_Ponder_Harmonica_Manhattan=function(Matriz, n, K){
  
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
    
    if((W-Tam[1]-1)%%Tam[1]!=0){
      MatViz[(W-Tam[1]-1)]=1
    }
    if((W-Tam[1]+1)%%Tam[1]!=1){
      MatViz[(W-Tam[1]+1)]=1
    }
    if((W-2*Tam[1])>0){
      MatViz[(W-2*Tam[1])]=1
    }
    
  }
  if((W+Tam[1])<=prod(Tam)){
    MatViz[(W+Tam[1])]=1
    
    if((W+Tam[1]-1)%%Tam[1]!=0){
      MatViz[(W+Tam[1]-1)]=1
    }
    if((W+Tam[1]+1)%%Tam[1]!=1){
      MatViz[(W+Tam[1]+1)]=1
    }
    if((W+2*Tam[1])<=prod(Tam)){
      MatViz[(W+2*Tam[1])]=1
    }
  }
  if((W-1)%%Tam[1]!=0){
    MatViz[(W-1)]=1
    if((W-2)%%Tam[1]!=0){
      MatViz[(W-2)]=1
    }
  }
  if((W+1)%%Tam[1]!=1){
    MatViz[(W+1)]=1
    if((W+2)%%Tam[1]!=1){
      MatViz[(W+2)]=1
    }
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
        
        if((W-Tam[1]-1)%%Tam[1]!=0){
          MatViz[(W-Tam[1]-1)]=1+MatViz[(W-Tam[1]-1)]
        }
        if((W-Tam[1]+1)%%Tam[1]!=1){
          MatViz[(W-Tam[1]+1)]=1+MatViz[(W-Tam[1]+1)]
        }
        if((W-2*Tam[1])>0){
          MatViz[(W-2*Tam[1])]=1+MatViz[(W-2*Tam[1])]
        }
        
      }
      if((W+Tam[1])<=prod(Tam)){
        MatViz[(W+Tam[1])]=1+MatViz[(W+Tam[1])]
        
        if((W+Tam[1]-1)%%Tam[1]!=0){
          MatViz[(W+Tam[1]-1)]=1+MatViz[(W+Tam[1]-1)]
        }
        if((W+Tam[1]+1)%%Tam[1]!=1){
          MatViz[(W+Tam[1]+1)]=1+MatViz[(W+Tam[1]+1)]
        }
        if((W+2*Tam[1])<=prod(Tam)){
          MatViz[(W+2*Tam[1])]=1+MatViz[(W+2*Tam[1])]
        }
      }
      if((W-1)%%Tam[1]!=0){
        MatViz[(W-1)]=1+MatViz[(W-1)]
        if((W-2)%%Tam[1]!=0){
          MatViz[(W-2)]=1+MatViz[(W-2)]
        }
      }
      if((W+1)%%Tam[1]!=1){
        MatViz[(W+1)]=1+MatViz[(W+1)]
        if((W+2)%%Tam[1]!=1){
          MatViz[(W+2)]=1+MatViz[(W+2)]
        }
      }
      
      MatViz=MatViz*MatPop
      
      Viz1=sum(MatViz==1)
      Viz2=sum(MatViz==2)
      Viz3=sum(MatViz==3)
      Viz4=sum(MatViz==4)
      Viz5=sum(MatViz==5)
      Viz6=sum(MatViz==6)
      Viz7=sum(MatViz==7)
      Viz8=sum(MatViz==8)
      Viz9=sum(MatViz==9)
      Viz10=sum(MatViz==10)
      Viz11=sum(MatViz==11)
      Viz12=sum(MatViz==12)
      
      Pop=sum(MatPop)
      
      Pb=Viz1/Pop
      Pc=Viz2/Pop
      Pd=Viz3/Pop
      Pe=Viz4/Pop
      Pf=Viz5/Pop
      Pg=Viz6/Pop
      Ph=Viz7/Pop
      Pi=Viz8/Pop
      Pj=Viz9/Pop
      Pk=Viz10/Pop
      Pl=Viz11/Pop
      Pm=Viz12/Pop
      
      Pa=1-(Pb+Pc+Pd+Pe+Pf+Pg+Ph+Pi+Pj+Pk+Pl+Pm)
      
      W=(1-(K*Pb+(K/2)*Pc+(K/3)*Pd+(K/4)*Pe+(K/5)*Pf+(K/6)*Pg+(K/7)*Ph+(K/8)*Pi+(K/9)*Pj
            +(K/10)*Pk+(K/11)*Pl+(K/12)*Pm))/Pa
      MatPesos=(K/Pop)*(MatViz==1)+(K/(2*Pop))*(MatViz==2)+(K/(3*Pop))*(MatViz==3)+(K/(4*Pop))*(MatViz==4)+
        (K/(5*Pop))*(MatViz==5)+(K/(6*Pop))*(MatViz==6)+(K/(7*Pop))*(MatViz==7)+(K/(8*Pop))*(MatViz==8)+
        (K/(9*Pop))*(MatViz==9)+(K/(10*Pop))*(MatViz==10)+(K/(11*Pop))*(MatViz==11)+(K/(12*Pop))*(MatViz==12)+
        (W/Pop)*(MatViz==0)
      
    }
    
    Size=length(Amostra)
    
    if(sum(MatPop)==0){break}
    
  }
  
  return(Amostra)
  
  
}

Amostragem_Ponder_Exponencial_Simples_VN=function(Matriz, n, K){
  
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
      
      W=(1-(K*Pb+(K^2)*Pc+(K^3)*Pd+(K^4)*Pe))/Pa
      MatPesos=(K/Pop)*(MatViz==1)+((K^2)/Pop)*(MatViz==2)+((K^3)/Pop)*(MatViz==3)+((K^4)/Pop)*(MatViz==4)+
        (W/Pop)*(MatViz==0)
      
    }
    
    Size=length(Amostra)
    
    if(sum(MatPop)==0){break}
    
  }
  
  return(Amostra)
  
  
}
Amostragem_Ponder_Exponencial_Simples_EVN=function(Matriz, n, K){
  
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
    if((W-2*Tam[1])>0){
      MatViz[(W-2*Tam[1])]=1
    }
  }
  if((W+Tam[1])<=prod(Tam)){
    MatViz[(W+Tam[1])]=1
    if((W+2*Tam[1])<=prod(Tam)){
      MatViz[(W+2*Tam[1])]=1
    }
  }
  if((W-1)%%Tam[1]!=0){
    MatViz[(W-1)]=1
    if((W-2)%%Tam[1]!=0){
      MatViz[(W-2)]=1
    }
  }
  if((W+1)%%Tam[1]!=1){
    MatViz[(W+1)]=1
    if((W+2)%%Tam[1]!=1){
      MatViz[(W+2)]=1
    }
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
        if((W-2*Tam[1])>0){
          MatViz[(W-2*Tam[1])]=MatViz[(W-2*Tam[1])]+1
        }
      }
      if((W+Tam[1])<=prod(Tam)){
        MatViz[(W+Tam[1])]=MatViz[(W+Tam[1])]+1
        if((W+2*Tam[1])<=prod(Tam)){
          MatViz[(W+2*Tam[1])]=MatViz[(W+2*Tam[1])]+1
        }
      }
      if((W-1)%%Tam[1]!=0){
        MatViz[(W-1)]=MatViz[(W-1)]+1
        if((W-2)%%Tam[1]!=0){
          MatViz[(W-2)]=MatViz[(W-2)]+1
        }
      }
      if((W+1)%%Tam[1]!=1){
        MatViz[(W+1)]=MatViz[(W+1)]+1
        if((W+2)%%Tam[1]!=1){
          MatViz[(W+2)]=MatViz[(W+2)]+1
        }
      }
      
      MatViz=MatViz*MatPop
      
      Viz1=sum(MatViz==1)
      Viz2=sum(MatViz==2)
      Viz3=sum(MatViz==3)
      Viz4=sum(MatViz==4)
      Viz5=sum(MatViz==5)
      Viz6=sum(MatViz==6)
      Viz7=sum(MatViz==7)
      Viz8=sum(MatViz==8)
      
      Pop=sum(MatPop)
      
      Pb=Viz1/Pop
      Pc=Viz2/Pop
      Pd=Viz3/Pop
      Pe=Viz4/Pop
      Pf=Viz5/Pop
      Pg=Viz6/Pop
      Ph=Viz7/Pop
      Pi=Viz8/Pop
      
      Pa=1-(Pb+Pc+Pd+Pe+Pf+Pg+Ph+Pi)
      
      W=(1-(K*Pb+(K^2)*Pc+(K^3)*Pd+(K^4)*Pe+(K^5)*Pf+(K^6)*Pg+(K^7)*Ph+(K^8)*Pi))/Pa
      MatPesos=(K/Pop)*(MatViz==1)+((K^2)/Pop)*(MatViz==2)+((K^3)/Pop)*(MatViz==3)+((K^4)/Pop)*(MatViz==4)+
        ((K^5)/Pop)*(MatViz==5)+((K^6)/Pop)*(MatViz==6)+((K^7)/Pop)*(MatViz==7)+((K^8)/Pop)*(MatViz==8)+
        (W/Pop)*(MatViz==0)
      
    }
    
    Size=length(Amostra)
    
    if(sum(MatPop)==0){break}
    
  }
  
  return(Amostra)
  
  
}
Amostragem_Ponder_Exponencial_Simples_Moore=function(Matriz, n, K){
  
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
    
    if((W-Tam[1]-1)%%Tam[1]!=0){
      MatViz[(W-Tam[1]-1)]=1
    }
    if((W-Tam[1]+1)%%Tam[1]!=1){
      MatViz[(W-Tam[1]+1)]=1
    }
    
  }
  if((W+Tam[1])<=prod(Tam)){
    MatViz[(W+Tam[1])]=1
    
    if((W+Tam[1]-1)%%Tam[1]!=0){
      MatViz[(W+Tam[1]-1)]=1
    }
    if((W+Tam[1]+1)%%Tam[1]!=1){
      MatViz[(W+Tam[1]+1)]=1
    }
    
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
        
        if((W-Tam[1]-1)%%Tam[1]!=0){
          MatViz[(W-Tam[1]-1)]=MatViz[(W-Tam[1]-1)]+1
        }
        if((W-Tam[1]+1)%%Tam[1]!=1){
          MatViz[(W-Tam[1]+1)]=MatViz[(W-Tam[1]+1)]+1
        }
        
      }
      if((W+Tam[1])<=prod(Tam)){
        MatViz[(W+Tam[1])]=MatViz[(W+Tam[1])]+1
        
        
        if((W+Tam[1]-1)%%Tam[1]!=0){
          MatViz[(W+Tam[1]-1)]=MatViz[(W+Tam[1]-1)]+1
        }
        if((W+Tam[1]+1)%%Tam[1]!=1){
          MatViz[(W+Tam[1]+1)]=MatViz[(W+Tam[1]+1)]+1
        }
        
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
      Viz5=sum(MatViz==5)
      Viz6=sum(MatViz==6)
      Viz7=sum(MatViz==7)
      Viz8=sum(MatViz==8)
      
      Pop=sum(MatPop)
      
      Pb=Viz1/Pop
      Pc=Viz2/Pop
      Pd=Viz3/Pop
      Pe=Viz4/Pop
      Pf=Viz5/Pop
      Pg=Viz6/Pop
      Ph=Viz7/Pop
      Pi=Viz8/Pop
      
      Pa=1-(Pb+Pc+Pd+Pe+Pf+Pg+Ph+Pi)
      
      W=(1-(K*Pb+(K^2)*Pc+(K^3)*Pd+(K^4)*Pe+(K^5)*Pf+(K^6)*Pg+(K^7)*Ph+(K^8)*Pi))/Pa
      MatPesos=(K/Pop)*(MatViz==1)+((K^2)/Pop)*(MatViz==2)+((K^3)/Pop)*(MatViz==3)+((K^4)/Pop)*(MatViz==4)+
        ((K^5)/Pop)*(MatViz==5)+((K^6)/Pop)*(MatViz==6)+((K^7)/Pop)*(MatViz==7)+((K^8)/Pop)*(MatViz==8)+
        (W/Pop)*(MatViz==0)
      
    }
    
    Size=length(Amostra)
    
    if(sum(MatPop)==0){break}
    
  }
  
  return(Amostra)
  
  
}
Amostragem_Ponder_Exponencial_Simples_Manhattan=function(Matriz, n, K){
  
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
    
    if((W-Tam[1]-1)%%Tam[1]!=0){
      MatViz[(W-Tam[1]-1)]=1
    }
    if((W-Tam[1]+1)%%Tam[1]!=1){
      MatViz[(W-Tam[1]+1)]=1
    }
    if((W-2*Tam[1])>0){
      MatViz[(W-2*Tam[1])]=1
    }
    
  }
  if((W+Tam[1])<=prod(Tam)){
    MatViz[(W+Tam[1])]=1
    
    if((W+Tam[1]-1)%%Tam[1]!=0){
      MatViz[(W+Tam[1]-1)]=1
    }
    if((W+Tam[1]+1)%%Tam[1]!=1){
      MatViz[(W+Tam[1]+1)]=1
    }
    if((W+2*Tam[1])<=prod(Tam)){
      MatViz[(W+2*Tam[1])]=1
    }
  }
  if((W-1)%%Tam[1]!=0){
    MatViz[(W-1)]=1
    if((W-2)%%Tam[1]!=0){
      MatViz[(W-2)]=1
    }
  }
  if((W+1)%%Tam[1]!=1){
    MatViz[(W+1)]=1
    if((W+2)%%Tam[1]!=1){
      MatViz[(W+2)]=1
    }
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
        
        if((W-Tam[1]-1)%%Tam[1]!=0){
          MatViz[(W-Tam[1]-1)]=1+MatViz[(W-Tam[1]-1)]
        }
        if((W-Tam[1]+1)%%Tam[1]!=1){
          MatViz[(W-Tam[1]+1)]=1+MatViz[(W-Tam[1]+1)]
        }
        if((W-2*Tam[1])>0){
          MatViz[(W-2*Tam[1])]=1+MatViz[(W-2*Tam[1])]
        }
        
      }
      if((W+Tam[1])<=prod(Tam)){
        MatViz[(W+Tam[1])]=1+MatViz[(W+Tam[1])]
        
        if((W+Tam[1]-1)%%Tam[1]!=0){
          MatViz[(W+Tam[1]-1)]=1+MatViz[(W+Tam[1]-1)]
        }
        if((W+Tam[1]+1)%%Tam[1]!=1){
          MatViz[(W+Tam[1]+1)]=1+MatViz[(W+Tam[1]+1)]
        }
        if((W+2*Tam[1])<=prod(Tam)){
          MatViz[(W+2*Tam[1])]=1+MatViz[(W+2*Tam[1])]
        }
      }
      if((W-1)%%Tam[1]!=0){
        MatViz[(W-1)]=1+MatViz[(W-1)]
        if((W-2)%%Tam[1]!=0){
          MatViz[(W-2)]=1+MatViz[(W-2)]
        }
      }
      if((W+1)%%Tam[1]!=1){
        MatViz[(W+1)]=1+MatViz[(W+1)]
        if((W+2)%%Tam[1]!=1){
          MatViz[(W+2)]=1+MatViz[(W+2)]
        }
      }
      
      MatViz=MatViz*MatPop
      
      Viz1=sum(MatViz==1)
      Viz2=sum(MatViz==2)
      Viz3=sum(MatViz==3)
      Viz4=sum(MatViz==4)
      Viz5=sum(MatViz==5)
      Viz6=sum(MatViz==6)
      Viz7=sum(MatViz==7)
      Viz8=sum(MatViz==8)
      Viz9=sum(MatViz==9)
      Viz10=sum(MatViz==10)
      Viz11=sum(MatViz==11)
      Viz12=sum(MatViz==12)
      
      Pop=sum(MatPop)
      
      Pb=Viz1/Pop
      Pc=Viz2/Pop
      Pd=Viz3/Pop
      Pe=Viz4/Pop
      Pf=Viz5/Pop
      Pg=Viz6/Pop
      Ph=Viz7/Pop
      Pi=Viz8/Pop
      Pj=Viz9/Pop
      Pk=Viz10/Pop
      Pl=Viz11/Pop
      Pm=Viz12/Pop
      
      Pa=1-(Pb+Pc+Pd+Pe+Pf+Pg+Ph+Pi+Pj+Pk+Pl+Pm)
      
      W=(1-(K*Pb+(K^2)*Pc+(K^3)*Pd+(K^4)*Pe+(K^5)*Pf+(K^6)*Pg+(K^7)*Ph+(K^8)*Pi+(K^9)*Pj+(K^10)*Pk+(K^11)*Pl+(K^12)*Pm))/Pa
      MatPesos=(K/Pop)*(MatViz==1)+((K^2)/Pop)*(MatViz==2)+((K^3)/Pop)*(MatViz==3)+((K^4)/Pop)*(MatViz==4)+
        ((K^5)/Pop)*(MatViz==5)+((K^6)/Pop)*(MatViz==6)+((K^7)/Pop)*(MatViz==7)+((K^8)/Pop)*(MatViz==8)+
        ((K^9)/Pop)*(MatViz==9)+((K^10)/Pop)*(MatViz==10)+((K^11)/Pop)*(MatViz==11)+((K^12)/Pop)*(MatViz==12)+
        (W/Pop)*(MatViz==0)
      
    }
    
    Size=length(Amostra)
    
    if(sum(MatPop)==0){break}
    
  }
  
  return(Amostra)
  
  
}

Amostragem_Ponder_Exponencial_Complexa_VN=function(Matriz, n, K){
  
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
      
      W=(1-(K*Pb+(K^2)*Pc+(K^(2^2))*Pd+(K^(2^3))*Pe))/Pa
      
      MatPesos=(K/Pop)*(MatViz==1)+((K^2)/Pop)*(MatViz==2)+((K^(2^2))/Pop)*(MatViz==3)+((K^(2^3))/Pop)*(MatViz==4)+
        (W/Pop)*(MatViz==0)
      
    }
    
    Size=length(Amostra)
    
    if(sum(MatPop)==0){break}
    
  }
  
  return(Amostra)
  
  
}
Amostragem_Ponder_Exponencial_Complexa_EVN=function(Matriz, n, K){
  
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
    if((W-2*Tam[1])>0){
      MatViz[(W-2*Tam[1])]=1
    }
  }
  if((W+Tam[1])<=prod(Tam)){
    MatViz[(W+Tam[1])]=1
    if((W+2*Tam[1])<=prod(Tam)){
      MatViz[(W+2*Tam[1])]=1
    }
  }
  if((W-1)%%Tam[1]!=0){
    MatViz[(W-1)]=1
    if((W-2)%%Tam[1]!=0){
      MatViz[(W-2)]=1
    }
  }
  if((W+1)%%Tam[1]!=1){
    MatViz[(W+1)]=1
    if((W+2)%%Tam[1]!=1){
      MatViz[(W+2)]=1
    }
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
        if((W-2*Tam[1])>0){
          MatViz[(W-2*Tam[1])]=MatViz[(W-2*Tam[1])]+1
        }
      }
      if((W+Tam[1])<=prod(Tam)){
        MatViz[(W+Tam[1])]=MatViz[(W+Tam[1])]+1
        if((W+2*Tam[1])<=prod(Tam)){
          MatViz[(W+2*Tam[1])]=MatViz[(W+2*Tam[1])]+1
        }
      }
      if((W-1)%%Tam[1]!=0){
        MatViz[(W-1)]=MatViz[(W-1)]+1
        if((W-2)%%Tam[1]!=0){
          MatViz[(W-2)]=MatViz[(W-2)]+1
        }
      }
      if((W+1)%%Tam[1]!=1){
        MatViz[(W+1)]=MatViz[(W+1)]+1
        if((W+2)%%Tam[1]!=1){
          MatViz[(W+2)]=MatViz[(W+2)]+1
        }
      }
      
      MatViz=MatViz*MatPop
      
      Viz1=sum(MatViz==1)
      Viz2=sum(MatViz==2)
      Viz3=sum(MatViz==3)
      Viz4=sum(MatViz==4)
      Viz5=sum(MatViz==5)
      Viz6=sum(MatViz==6)
      Viz7=sum(MatViz==7)
      Viz8=sum(MatViz==8)
      
      Pop=sum(MatPop)
      
      Pb=Viz1/Pop
      Pc=Viz2/Pop
      Pd=Viz3/Pop
      Pe=Viz4/Pop
      Pf=Viz5/Pop
      Pg=Viz6/Pop
      Ph=Viz7/Pop
      Pi=Viz8/Pop
      
      Pa=1-(Pb+Pc+Pd+Pe+Pf+Pg+Ph+Pi)
      
      W=(1-(K*Pb+(K^2)*Pc+(K^(2^2))*Pd+(K^(2^3))*Pe+(K^(2^4))*Pf+(K^(2^5))*Pg+(K^(2^6))*Ph+(K^(2^7))*Pi))/Pa
      
      MatPesos=(K/Pop)*(MatViz==1)+((K^2)/Pop)*(MatViz==2)+((K^(2^2))/Pop)*(MatViz==3)+((K^(2^3))/Pop)*(MatViz==4)+
        ((K^(2^4))/Pop)*(MatViz==5)+((K^(2^5))/Pop)*(MatViz==6)+((K^(2^6))/Pop)*(MatViz==7)+((K^(2^7))/Pop)*(MatViz==8)+
        (W/Pop)*(MatViz==0)
      
    }
    
    Size=length(Amostra)
    
    if(sum(MatPop)==0){break}
    
  }
  
  return(Amostra)
  
  
}
Amostragem_Ponder_Exponencial_Complexa_Moore=function(Matriz, n, K){
  
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
    
    if((W-Tam[1]-1)%%Tam[1]!=0){
      MatViz[(W-Tam[1]-1)]=1
    }
    if((W-Tam[1]+1)%%Tam[1]!=1){
      MatViz[(W-Tam[1]+1)]=1
    }
    
  }
  if((W+Tam[1])<=prod(Tam)){
    MatViz[(W+Tam[1])]=1
    
    if((W+Tam[1]-1)%%Tam[1]!=0){
      MatViz[(W+Tam[1]-1)]=1
    }
    if((W+Tam[1]+1)%%Tam[1]!=1){
      MatViz[(W+Tam[1]+1)]=1
    }
    
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
        
        if((W-Tam[1]-1)%%Tam[1]!=0){
          MatViz[(W-Tam[1]-1)]=MatViz[(W-Tam[1]-1)]+1
        }
        if((W-Tam[1]+1)%%Tam[1]!=1){
          MatViz[(W-Tam[1]+1)]=MatViz[(W-Tam[1]+1)]+1
        }
        
      }
      if((W+Tam[1])<=prod(Tam)){
        MatViz[(W+Tam[1])]=MatViz[(W+Tam[1])]+1
        
        
        if((W+Tam[1]-1)%%Tam[1]!=0){
          MatViz[(W+Tam[1]-1)]=MatViz[(W+Tam[1]-1)]+1
        }
        if((W+Tam[1]+1)%%Tam[1]!=1){
          MatViz[(W+Tam[1]+1)]=MatViz[(W+Tam[1]+1)]+1
        }
        
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
      Viz5=sum(MatViz==5)
      Viz6=sum(MatViz==6)
      Viz7=sum(MatViz==7)
      Viz8=sum(MatViz==8)
      
      Pop=sum(MatPop)
      
      Pb=Viz1/Pop
      Pc=Viz2/Pop
      Pd=Viz3/Pop
      Pe=Viz4/Pop
      Pf=Viz5/Pop
      Pg=Viz6/Pop
      Ph=Viz7/Pop
      Pi=Viz8/Pop
      
      Pa=1-(Pb+Pc+Pd+Pe+Pf+Pg+Ph+Pi)
      
      W=(1-(K*Pb+(K^2)*Pc+(K^(2^2))*Pd+(K^(2^3))*Pe+(K^(2^4))*Pf+(K^(2^5))*Pg+(K^(2^6))*Ph+(K^(2^7))*Pi))/Pa
      
      MatPesos=(K/Pop)*(MatViz==1)+((K^2)/Pop)*(MatViz==2)+((K^(2^2))/Pop)*(MatViz==3)+((K^(2^3))/Pop)*(MatViz==4)+
        ((K^(2^4))/Pop)*(MatViz==5)+((K^(2^5))/Pop)*(MatViz==6)+((K^(2^6))/Pop)*(MatViz==7)+((K^(2^7))/Pop)*(MatViz==8)+
        (W/Pop)*(MatViz==0)
      
    }
    
    Size=length(Amostra)
    
    if(sum(MatPop)==0){break}
    
  }
  
  return(Amostra)
  
  
}
Amostragem_Ponder_Exponencial_Complexa_Manhattan=function(Matriz, n, K){
  
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
    
    if((W-Tam[1]-1)%%Tam[1]!=0){
      MatViz[(W-Tam[1]-1)]=1
    }
    if((W-Tam[1]+1)%%Tam[1]!=1){
      MatViz[(W-Tam[1]+1)]=1
    }
    if((W-2*Tam[1])>0){
      MatViz[(W-2*Tam[1])]=1
    }
    
  }
  if((W+Tam[1])<=prod(Tam)){
    MatViz[(W+Tam[1])]=1
    
    if((W+Tam[1]-1)%%Tam[1]!=0){
      MatViz[(W+Tam[1]-1)]=1
    }
    if((W+Tam[1]+1)%%Tam[1]!=1){
      MatViz[(W+Tam[1]+1)]=1
    }
    if((W+2*Tam[1])<=prod(Tam)){
      MatViz[(W+2*Tam[1])]=1
    }
  }
  if((W-1)%%Tam[1]!=0){
    MatViz[(W-1)]=1
    if((W-2)%%Tam[1]!=0){
      MatViz[(W-2)]=1
    }
  }
  if((W+1)%%Tam[1]!=1){
    MatViz[(W+1)]=1
    if((W+2)%%Tam[1]!=1){
      MatViz[(W+2)]=1
    }
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
        
        if((W-Tam[1]-1)%%Tam[1]!=0){
          MatViz[(W-Tam[1]-1)]=1+MatViz[(W-Tam[1]-1)]
        }
        if((W-Tam[1]+1)%%Tam[1]!=1){
          MatViz[(W-Tam[1]+1)]=1+MatViz[(W-Tam[1]+1)]
        }
        if((W-2*Tam[1])>0){
          MatViz[(W-2*Tam[1])]=1+MatViz[(W-2*Tam[1])]
        }
        
      }
      if((W+Tam[1])<=prod(Tam)){
        MatViz[(W+Tam[1])]=1+MatViz[(W+Tam[1])]
        
        if((W+Tam[1]-1)%%Tam[1]!=0){
          MatViz[(W+Tam[1]-1)]=1+MatViz[(W+Tam[1]-1)]
        }
        if((W+Tam[1]+1)%%Tam[1]!=1){
          MatViz[(W+Tam[1]+1)]=1+MatViz[(W+Tam[1]+1)]
        }
        if((W+2*Tam[1])<=prod(Tam)){
          MatViz[(W+2*Tam[1])]=1+MatViz[(W+2*Tam[1])]
        }
      }
      if((W-1)%%Tam[1]!=0){
        MatViz[(W-1)]=1+MatViz[(W-1)]
        if((W-2)%%Tam[1]!=0){
          MatViz[(W-2)]=1+MatViz[(W-2)]
        }
      }
      if((W+1)%%Tam[1]!=1){
        MatViz[(W+1)]=1+MatViz[(W+1)]
        if((W+2)%%Tam[1]!=1){
          MatViz[(W+2)]=1+MatViz[(W+2)]
        }
      }
      
      MatViz=MatViz*MatPop
      
      Viz1=sum(MatViz==1)
      Viz2=sum(MatViz==2)
      Viz3=sum(MatViz==3)
      Viz4=sum(MatViz==4)
      Viz5=sum(MatViz==5)
      Viz6=sum(MatViz==6)
      Viz7=sum(MatViz==7)
      Viz8=sum(MatViz==8)
      Viz9=sum(MatViz==9)
      Viz10=sum(MatViz==10)
      Viz11=sum(MatViz==11)
      Viz12=sum(MatViz==12)
      
      Pop=sum(MatPop)
      
      Pb=Viz1/Pop
      Pc=Viz2/Pop
      Pd=Viz3/Pop
      Pe=Viz4/Pop
      Pf=Viz5/Pop
      Pg=Viz6/Pop
      Ph=Viz7/Pop
      Pi=Viz8/Pop
      Pj=Viz9/Pop
      Pk=Viz10/Pop
      Pl=Viz11/Pop
      Pm=Viz12/Pop
      
      Pa=1-(Pb+Pc+Pd+Pe+Pf+Pg+Ph+Pi+Pj+Pk+Pl+Pm)
      
      W=(1-(K*Pb+(K^2)*Pc+(K^(2^2))*Pd+(K^(2^3))*Pe+(K^(2^4))*Pf+(K^(2^5))*Pg+(K^(2^6))*Ph+(K^(2^7))*Pi+(K^(2^8))*Pj+(K^(2^9))*Pk+(K^(2^10))*Pl+(K^(2^11))*Pm))/Pa
      
      MatPesos=(K/Pop)*(MatViz==1)+((K^2)/Pop)*(MatViz==2)+((K^(2^2))/Pop)*(MatViz==3)+((K^(2^3))/Pop)*(MatViz==4)+
        ((K^(2^4))/Pop)*(MatViz==5)+((K^(2^5))/Pop)*(MatViz==6)+((K^(2^6))/Pop)*(MatViz==7)+((K^(2^7))/Pop)*(MatViz==8)+
        ((K^(2^8))/Pop)*(MatViz==9)+((K^(2^9))/Pop)*(MatViz==10)+((K^(2^10))/Pop)*(MatViz==11)+((K^(2^11))/Pop)*(MatViz==12)+
        (W/Pop)*(MatViz==0)
      
    }
    
    Size=length(Amostra)
    
    if(sum(MatPop)==0){break}
    
  }
  
  return(Amostra)
  
  
}

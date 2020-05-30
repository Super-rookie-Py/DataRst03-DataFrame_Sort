# Quick_Sort DataFrame


# 데이터 프레임(정렬대상)
set.seed(1234)
x1=rep(1:10,each=2,time=5)
x2=rep(1:5,each=20)
y=x1+x2+rnorm(100,0,1)
index = sample(1:100)

data_1 <- data.frame(index,y,x1,x2)
data_1



###########################################
Quick_Sort_Df<-function(d, col_num=1){
  
  size_d=length(d[,col_num])
  
  if(size_d<3){
    if(size_d==2 && d[1,col_num]>d[2,col_num]){
      return(rbind(d[2,],d[1,]))
    }else{
      return(d)
    }
  }
  
  left=1
  right=size_d
  pivot=d[size_d,col_num]
  
  repeat{
    while(d[left,col_num]<=pivot && left<=size_d){left=left+1}
    while(d[right,col_num]>pivot && right>=1){right=right-1}
    if(left>=right){
      break
    }
    if(left<right){
      # Swap
      temp_value=d[right,]
      d[right,]=d[left,]
      d[left,]=temp_value
    }
  }
  # Partition
  if(left==1){
    return(rbind(d[1,],Quick_Sort_Df(d[2:size_d,], col_num)))
  }else if(right==size_d){
    return(rbind(Quick_Sort_Df(d[1:(size_d-1),], col_num),d[size_d,]))
  }else{
    return(rbind(Quick_Sort_Df(d[1:right,], col_num),Quick_Sort_Df(d[(right+1):size_d,], col_num)))
  }
}


######################################################################################
Quick_Sort_Df(data_1,col_num=1)

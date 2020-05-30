# MergeSort Data Frame



Merge_Sort_Df <- function(d, col_num=1){
  size_d = length(d[,col_num])
  
  if(size_d > 2){
    # Decompose
    size_first_half= floor(size_d/2) # 내림
    size_second_half = size_d-size_first_half
    
    d1=Merge_Sort_Df(d[1:size_first_half,], col_num)
    d2=Merge_Sort_Df(d[(size_first_half+1):size_d,], col_num)
    
    # Merge
    i1=1
    i2=1
    i=1
    repeat{
      if(d1[i1,col_num]<d2[i2,col_num]){
        d[i,]=d1[i1,]
        i1 = i1+1
      }else{
        d[i,]=d2[i2,]
        i2=i2+1
      }
      i=i+1
      
      if(i1>size_first_half){
        d[i:size_d,]=d2[i2:size_second_half,]
        return(d)
      }else if(i2>size_second_half){
        d[i:size_d,]=d1[i1:size_first_half,]
        return(d)
      }
    }
  }else if(size_d == 2){
    if(d[1, col_num]>d[2, col_num]){
      return(rbind(d[2,],d[1,])) # 두 개를 바꾸고 묶어서 반환 
    }else{
      return(d)
    }
  }else{
    return(d) #크기가 1이면 그냥 반환 
  }
}

Merge_Sort_Df(data_1, 1)

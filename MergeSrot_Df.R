# MergeSort Data Frame

# 벡터기준으로 정렬을 진행할때와 다르게 데이터프레임값의 특정 열에 대해서 정렬을 진행 하려면, 다음의 조건을 신경써주면 간단하게 바꿀 수 있다.

# 먼저 size를 정할때는 length(d) 가 아닌 lenth(d[ , col_num])로 열을 고정한 채로 정해둬야 한다는 것. length(d)만 입력할 시 데이터프레임의 
# 열 개수만 등록된다.

# 값을 비교할때는 d[a, col_num] > d[b, col_num]으로 정렬을 진행할 열을 고정해둔 채로 값을 비교한다.
# 값을 바꿔야할 때는 d[a, ] = d[b, ] 처럼 행 전체의 값을 변경시켜줘야 한다.
# return을 통해 값을 반활할 때 c(d[a], d[b]])로 반환하면 안되고 rbind(d[a, ] , d[b, ])식으로 행단위로 묶어서 반환시킨다.

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

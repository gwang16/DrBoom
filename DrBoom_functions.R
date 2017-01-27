target_selection <- function(T, T1, target){
  if(target == 1){
    x <- T[sample(1:length(T),1,replace=TRUE)]
  } 
  if(target == 2){
    x <- T1[sample(1:length(T1),1,replace=TRUE)]
  }
  return(x)
}

damage_selection <- function(type){
  M = 4 #Max bomb damage
  if (type=="bomb"){
  	dmg <- sample(1:M, 1, replace=TRUE)
  } 
  if (type=="missle"){
  	dmg <- 1
  }
  return(dmg)
}

bomb_chain <- function(T, T1, target){
  #select bomb target     
  x_bomb <- target_selection(T, T1, target) 
  #select bomb damage
  dmg_bomb <- damage_selection("bomb")  

  #damage resolution	 
	if (x_bomb == "A") {
    return(list(dmg_bomb,T,T1)) #return bomb damage to face and resulting board state
	}
  else if (x_bomb %in% c("C","D")){
    T <- setdiff(T, x_bomb)
    #target is always the opposite team from the bomb
    #so if bomb is of team 1, then target is 3-1=2
    target <- 3-target 
    bomb_chain(T, T1, target)
  }
	else if (x_bomb %in% c("C1","D1")){
    T1 <- setdiff(T1, x_bomb)
    target <- 3-target
  	bomb_chain(T, T1, target)
	}
  else return(list(0,T,T1))
}
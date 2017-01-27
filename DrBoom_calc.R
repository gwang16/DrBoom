#Scenario:
#You're going first as mage versus warlock. It's turn 7, you flamestrike to clear your opponents board. Now the board is empty and your opponent is in topdeck mode. You feel pretty smug, with an arcane missles left in your hand and are 10 health to your opponent's 4. Missles plus ping would be lethal if your opponent doesn't play anything. Your opponent draws, emotes "Greetings", and slams down Dr. Boom, a 7/7 creature with two 1/1s, each of which also has deathrattles that deal 1-4 random damage to a random enemy character. End turn.
#Neverlucky. You right click his face to mute him. Turn 8. Draw. Your very own Dr. Boom. You stare at your hand and face a difficult decision- do you missles plus ping and hope for all face, or play Dr. Boom and missles and hope that the missles hit your opponents bomb which then hits your bomb which then hits his face? 

#The probability of winning this turn with missles + ping is 1.56% (=1/4^3), which is the event that all three missles hit face. 
#What is the probability of dealing 4 damage to the oppponent's face when both of your boards consist of Dr. Boom, and you play arcane missles?

#We use a Monte Carlo simulation approach to estimate this probability.

#           A
#     C     B     D
#     C1    B1    D1
#           A1

#rm(list=ls())
source("DrBoom_functions.R")
s <- c()
for (trial in 1:10000){
  T <-c("A", "B", "C", "D") #Define opponent's board 
  T1 <-c("A1", "B1", "C1", "D1") #Define own board
  N = 3 #Number of missles
  DMGtoFace = 0

  #Simulation of 3 arcane missles
  for (i in 1:N){
    # param 1 indicates that a target is selected from opponent's board
    x_missle <- target_selection(T, T1, 1)
    dmg_missle <- damage_selection("missle")
    # Arcane missles to the face
    if (x_missle == "A"){
      DMGtoFace <- DMGtoFace + dmg_missle
    }
    # Arcane missles to the opposing bombs triger a chain reaction
    if (x_missle %in% c("C","D")){
      T <- setdiff(T, x_missle)
      bomb_chain_result <- bomb_chain(T, T1, 2) 
      DMGtoFace <- DMGtoFace + bomb_chain_result[[1]] 
      T <- bomb_chain_result[[2]]
      T1 <- bomb_chain_result[[3]]
    }		
  }	
  s <- c(s, DMGtoFace)
}

length(which(s >= 4))/length(s)

library(ggplot2)
ggplot() + geom_hist(x=s)
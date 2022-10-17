library(tidyverse)
library(ggforce)
library(data.table)


at_bat <- function(player)
{
  prob <- round(runif(1,min=0,max=1),6)
  
  if(between(prob, 0, player$K_RATE)) 
  {
    return(-1) #K prob
  }else if(between(prob, player$K_RATE, player$K_RATE + player$BB_RATE)) 
  {
    return(5)#walk prop
  }else if(between(prob, player$K_RATE + player$BB_RATE, 
                   player$K_RATE + player$BB_RATE + player$HBP_RATE))
  {
    return(6) #hbp prop
  }else if(between(prob,player$K_RATE + player$BB_RATE + player$HBP_RATE, 
                   player$K_RATE + player$BB_RATE + player$HBP_RATE + player$BIP_RATE))
  {
    hit_yn <- round(runif(1,min=0,max=1),6)
    if(between(hit_yn, 0, player$HIT_RATE))
    {
      hit_type <- round(runif(1,min=0,max=1),6)
      if(between(hit_type, 0, player$single_rate))
      {
        return(1) #single
      }else if(between(hit_type, player$single_rate, player$single_rate + player$DOUBLE_RATE))
      {
        return(2) #double
      }else if(between(hit_type, player$single_rate + player$DOUBLE_RATE, 
                       player$single_rate + player$DOUBLE_RATE + player$TRIPLE_RATE))
      {
        return(3) #triple
      }else{
        return(4) #HR
      }
      
    }else{
      out_type <- round(runif(1,min=0,max=1),6)
      if(between(out_type, 0, player$PO_RATE))
      {
        return(-7) #pop up
      }else if(between(out_type, player$PO_RATE, player$PO_RATE + player$GB_RATE))
      {
        return(-8) #gb
      }else if(between(out_type, player$PO_RATE + player$GB_RATE, 
                       player$PO_RATE + player$GB_RATE + player$FB_RATE))
      {
        return(-9) #fb
      }else{
        return(-10) #ld
      }
    }
  }
}

play_game <- function(hitters)
{
  tot_runs = 0
  whos_up <- 1

  for(i in 1:9) 
  {
    bases_sit <- data.frame(state = c(FALSE, FALSE, FALSE), 
                            name = c("_", "_", "_"))
    numOuts = 0
    while(numOuts < 3) # per inning
    {

      outcome <- at_bat(hitters[whos_up,])
      cat("\nBatting:", hitters[whos_up,]$NAME,
          "\nOutcome", ifelse(outcome == 1, "Single", ifelse(outcome == 2, "Double", ifelse(outcome == 3, "Triple", ifelse(outcome == 4, "Home Run",
                        ifelse(outcome == "-1",  "K", ifelse(outcome == -8, "Groundout", ifelse(outcome == -9, "Flyout", ifelse(outcome == -7, "Popout",
                        ifelse(outcome == -10, "Lineout", ifelse(outcome == 5, "Walk", "HBP")))))))))),
          "\nOuts:", numOuts,
          "\nInning:", i,
          "\nBases:",bases_sit$name,
          "\nBases:", bases_sit$state,
          "Runs:", tot_runs)

      if(!bases_sit$state[1] & !bases_sit$state[2] & !bases_sit$state[3])
      {
        if(outcome == 5 | outcome == 1 | outcome == 6)
        {
          bases_sit$state[1] <- TRUE
          bases_sit$name[1] <- hitters[whos_up,]$NAME
        }else if(outcome == 2)
        {
          bases_sit$state[2] <- TRUE
          bases_sit$name[2] <- hitters[whos_up,]$NAME
          
        }else if(outcome == 3)
        {
          bases_sit$state[3] <- TRUE
          bases_sit$name[3] <- hitters[whos_up,]$NAME
          
        }else if(outcome == 4)
        {
          tot_runs = tot_runs + 1
        }else if(outcome < 0)
        {
          numOuts = numOuts + 1
        }
        
    }else if(bases_sit$state[1] & !bases_sit$state[2] & !bases_sit$state[3])
      {
        
        if(outcome == 5 | outcome == 6)
        {
          bases_sit$state[2] <- TRUE
          bases_sit$name[2] <- bases_sit$name[1]
          bases_sit$name[1] <- hitters[whos_up,]$NAME
          
        }else if(outcome == -8)
        {
          speed = hitters[whos_up,]$speed_range
          prob = runif(1,0,1)
          speed_x <- ifelse(speed == "very fast", .40,
                            ifelse(speed == "fast", .47,
                                   ifelse(speed == "slow", .54,.60)))
          
          if(prob < speed_x & numOuts < 2)
          {
            bases_sit$state[1] <- FALSE
            bases_sit$name[1] <- "_"
            
            numOuts = numOuts + 2
          }else{
            bases_sit$name[1] <- hitters[whos_up,]$NAME
            numOuts = numOuts + 1
            
          }
          
        }else if(outcome == 2)
        {
          bases_sit$state[2] <- TRUE
          bases_sit$name[2] <- hitters[whos_up,]$NAME
          
          hitters %>% 
            filter(NAME == bases_sit$name[1]) %>% 
              pull(speed_range) -> speed
          
          speed_x <- ifelse(speed == "very fast", .52,
                            ifelse(speed == "fast", .41,
                                   ifelse(speed == "slow", .30, .20)))
          if(runif(1,0,1) < speed_x)
          {
            tot_runs = tot_runs + 1
            bases_sit$state[1] <- FALSE
            bases_sit$name[1] <- "_"
          }else{
            bases_sit$state[3] <- TRUE
            bases_sit$name[3] <- bases_sit$name[1]
            bases_sit$name[1] <- "_"
            bases_sit$state[1] <- FALSE
            
          }
          
        }else if(outcome == 3)
        {
          bases_sit$state[1] <- FALSE
          bases_sit$name[1] <- "_"
          bases_sit$state[3] <- TRUE
          bases_sit$name[3] <-  hitters[whos_up,]$NAME
          tot_runs = tot_runs + 1
        }else if(outcome == 4)
        {
          bases_sit$state[1] <- FALSE
          bases_sit$name[1] <- "_"
          tot_runs = tot_runs + 2
        }else if(outcome == 1)
        {            

          hitters %>% 
            filter(NAME == bases_sit$name[1]) %>% 
            pull(speed_range) -> speed
          
          speed_x <- ifelse(speed == "very fast", .4,
                            ifelse(speed == "fast", .35,
                                   ifelse(speed == "slow", .3, .25)))
          
          if(runif(1,0,1) < speed_x)
          {
            bases_sit$state[3] <- TRUE
            bases_sit$name[3] <- bases_sit$name[1]
            bases_sit$name[1] <- hitters[whos_up,]$NAME

          }else{
            bases_sit$state[2] <- TRUE
            bases_sit$name[2] <- bases_sit$name[1]
            bases_sit$name[1] <- hitters[whos_up,]$NAME
            
          }
        }else if(outcome < 0)
        {
          numOuts = numOuts + 1
        }
      
  }else if(!bases_sit$state[1] & bases_sit$state[2] & !bases_sit$state[3])
    {
      # on second
      if(outcome == 5 | outcome == 6)
      {
        bases_sit$state[1] <- TRUE
        bases_sit$name[1] <-  hitters[whos_up,]$NAME
      }else if(outcome == 4)
      {
        bases_sit$state[2] <- FALSE
        bases_sit$name[2] <- "_"
        tot_runs = tot_runs + 2

      }else if(outcome == 3)
      {
        bases_sit$state[2] <- FALSE
        bases_sit$name[2] <- "_"
        bases_sit$state[3] <- TRUE
        bases_sit$name[3] <- hitters[whos_up,]$NAME
        tot_runs = tot_runs + 1
        
      }else if(outcome == 2)
      {
        tot_runs = tot_runs + 1
        bases_sit$name[2] <- hitters[whos_up,]$NAME
      }else if(outcome == 1)
      {
        hitters %>% 
          filter(NAME == bases_sit$name[2]) %>% 
          pull(speed_range) -> speed
        
        speed_x <- ifelse(speed == "very fast", .85,
                          ifelse(speed == "fast", .70,
                                 ifelse(speed == "slow", .60, .50)))
        
        if(runif(1,0,1) < speed_x)
        {
          bases_sit$state[1] <- TRUE
          bases_sit$name[1] <- hitters[whos_up,]$NAME
          bases_sit$state[2] <- FALSE
          bases_sit$name[2] <- "_"
          tot_runs = tot_runs + 1
        }else
        {
          bases_sit$state[1] <- TRUE
          bases_sit$name[1] <- hitters[whos_up,]$NAME
          bases_sit$state[3] <- TRUE
          bases_sit$name[3] <- bases_sit$name[2]
          bases_sit$state[2] <- FALSE
          bases_sit$name[2] <- "_"
          
        }
      }else if(outcome == -9)
      {
        hitters %>% 
          filter(NAME == bases_sit$name[2]) %>% 
          pull(speed_range) -> speed
        
        speed_x <- ifelse(speed == "very fast", .45,
                          ifelse(speed == "fast", .35,
                                 ifelse(speed == "slow", .28, .20)))
        prob = runif(1,0,1)
        if(prob < speed_x)
        {
          bases_sit$state[3] = TRUE
          bases_sit$name[3] <- bases_sit$name[2]
          bases_sit$state[2] = FALSE
          bases_sit$name[2] <- "_"
          numOuts = numOuts + 1
          
        }else if(prob > .97)
        {
          bases_sit$state[2] <- FALSE
          bases_sit$name[2] <- "_"
          numOuts = numOuts + 2
        }
      }else if(outcome < 0 )
      {
        numOuts = numOuts + 1
      }
      
  }else if(!bases_sit$state[1] & !bases_sit$state[2] & bases_sit$state[3])
  {
    # on third
    if(outcome == 5 | outcome == 6)
    {
      bases_sit$state[1] <- TRUE
      bases_sit$name[1] <- hitters[whos_up,]$NAME
    }else if(outcome == 4)
    {
      bases_sit$state[3] <- FALSE
      bases_sit$name[3] <- "_"
      tot_runs = tot_runs + 2
    }else if(outcome == 3)
    {
      bases_sit$name[3] <- hitters[whos_up,]$NAME
      tot_runs = tot_runs +  1
    }else if(outcome == 2)
    {
      bases_sit$state[3] <- FALSE
      bases_sit$name[3] <- "_"
      bases_sit$state[2] <- TRUE
      bases_sit$name[2] <- hitters[whos_up,]$NAME
      tot_runs = tot_runs + 1
    }else if(outcome == 1)
    {
      bases_sit$state[1] <- TRUE
      bases_sit$name[1] <- hitters[whos_up,]$NAME
      bases_sit$state[3] <- FALSE
      bases_sit$name[3] <- "_"
      tot_runs = tot_runs + 1
    }else if(outcome == -9)
    {
      hitters %>% 
        filter(NAME == bases_sit$name[3]) %>% 
        pull(speed_range) -> speed
      
      speed_x <- ifelse(speed == "very fast", .93,
                        ifelse(speed == "fast", .83,
                               ifelse(speed == "slow", .73, .68)))
      prob = runif(1,0,1)
      if(prob < speed_x & numOuts < 2)
      {
        numOuts = numOuts + 1
        bases_sit$state[3] <- FALSE
        bases_sit$name[3] <- "_"
        tot_runs = tot_runs + 1
      }else if(runif(1,0,1) > .93 & numOuts < 2)
      {
        numOuts = numOuts + 1
        bases_sit$state[3] <- FALSE
        bases_sit$name[3] <- "_"
        numOuts = numOuts + 1
      }else{
        numOuts = numOuts + 1
      }
      
    }else if(outcome < 0 )
    {
      numOuts = numOuts + 1
    }
    
    
  }else if(bases_sit$state[1] & bases_sit$state[2] & !bases_sit$state[3])
  {
    # first and second
    if(outcome == 5 | outcome == 6)
    {
      bases_sit$state[3] <- TRUE
      bases_sit$name[3] <- bases_sit$name[2]
      bases_sit$name[2] <- bases_sit$name[1]
      bases_sit$name[1] <- hitters[whos_up,]$NAME
    }else if(outcome == 4)
    {
      bases_sit$state[1] <- FALSE
      bases_sit$name[1] <- "_"
      bases_sit$state[2] <- FALSE
      bases_sit$name[2] <- "_"
      tot_runs = tot_runs + 3
      
    }else if(outcome == 3)
    {
      bases_sit$state[1] <- FALSE
      bases_sit$name[1] <- "_"
      bases_sit$state[2] <- FALSE 
      bases_sit$name[2] <- "_"
      bases_sit$state[3] <- TRUE
      bases_sit$name[3] <- hitters[whos_up,]$NAME
      tot_runs = tot_runs + 2

    }else if(outcome == 2)
    {
      
      bases_sit$name[2] <- hitters[whos_up,]$NAME
      bases_sit$state[2] <- TRUE
      hitters %>% 
        filter(NAME == bases_sit$name[1]) %>% 
        pull(speed_range) -> speed
      
      speed_x <- ifelse(speed == "very fast", .57,
                        ifelse(speed == "fast", .45,
                               ifelse(speed == "slow", .33, .24)))
      if(runif(1,0,1) < speed_x)
      {
        tot_runs = tot_runs + 2
        bases_sit$state[1] <- FALSE
        bases_sit$name[1] <- "_"
      }else{
        bases_sit$state[3] <- TRUE
        bases_sit$name[3] <- bases_sit$name[1]
        bases_sit$name[1] <- "_"
        bases_sit$state[1] <- FALSE
        tot_runs = tot_runs + 1
        
      }

    }else if(outcome == 1)
    {
      
      hitters %>% 
        filter(NAME == bases_sit$name[2]) %>% 
        pull(speed_range) -> speed
      
      speed_x <- ifelse(speed == "very fast", .85,
                        ifelse(speed == "fast", .70,
                               ifelse(speed == "slow", .60, .50)))
      
      if(runif(1,0,1) < speed_x)
      {
        tot_runs = tot_runs + 1
        hitters %>% 
          filter(NAME == bases_sit$name[1]) %>% 
          pull(speed_range) -> speed
        
        speed_x <- ifelse(speed == "very fast", .4,
                          ifelse(speed == "fast", .35,
                                 ifelse(speed == "slow", .3,.25)))
        
        if(runif(1,0,1) < speed_x)
        {
          bases_sit$state[3] <- TRUE
          bases_sit$name[3] <- bases_sit$name[1]
          bases_sit$state[2] <- FALSE
          bases_sit$name[2] <- "_"
          bases_sit$name[1] <- hitters[whos_up,]$NAME
          
        }else{
          bases_sit$name[2] <- bases_sit$name[1]
          bases_sit$name[1] <- hitters[whos_up,]$NAME
              }
      }else
      {        
        bases_sit$state[3] <- TRUE
        bases_sit$name[3] <- bases_sit$name[2]
        bases_sit$state[2] <- TRUE
        bases_sit$name[2] <- bases_sit$name[1]
        bases_sit$name[1] <- hitters[whos_up,]$NAME
      }
      
      
    }else if(outcome == -8)
    {

      speed = hitters[whos_up,]$speed_range
      speed_x <- ifelse(speed == "very fast", .35,
                        ifelse(speed == "fast", .45,
                               ifelse(speed == "slow", .52,.57)))
      prob = runif(1,0,1)

      if(prob < speed_x & numOuts < 2)
      {
        numOuts = numOuts + 1
        bases_sit$state[1] <- FALSE
        bases_sit$name[1] <- "_"
        bases_sit$state[3] <- TRUE
        bases_sit$name[3] <- bases_sit$name[2]
        bases_sit$state[2] <- FALSE
        bases_sit$name[2] <- "_"
        numOuts = numOuts + 1
      }else{
        numOuts = numOuts + 1
        bases_sit$state[2] <- FALSE
        bases_sit$state[3] <- TRUE
        bases_sit$name[3] <- bases_sit$name[2]
        bases_sit$name[2] <- "_"
        
      }
    }else if(outcome == -9)
    {
      
      hitters %>% 
        filter(NAME == bases_sit$name[2]) %>% 
        pull(speed_range) -> speed
      
      speed_x <- ifelse(speed == "very fast", .45,
                        ifelse(speed == "fast", .35,
                               ifelse(speed == "slow", .28, .20)))
      prob = runif(1,0,1)
      if(prob < speed_x & numOuts < 2)
      {
        numOuts = numOuts + 1
        bases_sit$state[3] = TRUE
        bases_sit$name[3] <- bases_sit$name[2]
        bases_sit$state[2] = FALSE
        bases_sit$name[2] <- "_"
        
      }else if(prob > .97 & numOuts < 2)
      {
        numOuts = numOuts + 1
        bases_sit$state[2] <- FALSE
        bases_sit$name[2] <- "_"
        numOuts = numOuts + 1
      }else {
        numOuts = numOuts + 1
      }
    }else if( outcome < 0)
    {
      numouts = numOuts + 1
    }
    
  }else if(bases_sit$state[1] & !bases_sit$state[2] & bases_sit$state[3])
  {
    # on first and third
    if(outcome == 5 | outcome == 6)
    {
      bases_sit$state[2] <- TRUE
      bases_sit$name[2] <- bases_sit$name[1]
      bases_sit$name[1] <- hitters[whos_up,]$NAME
      
    }else if(outcome == 4)
    {
      bases_sit$state[1] <- FALSE
      bases_sit$name[1] <- "_"
      bases_sit$state[3] <- FALSE
      bases_sit$name[3] <- "_"
      tot_runs = tot_runs  + 3
    }else if(outcome == 3)
    {
      bases_sit$state[1] <- FALSE
      bases_sit$name[3] <- hitters[whos_up,]$NAME
      bases_sit$name[1] <- "_"
      tot_runs = tot_runs  + 2
    }else if(outcome == 2)
    {
      bases_sit$name[2] <- hitters[whos_up,]$NAME
      bases_sit$state[2] <- TRUE
      hitters %>% 
        filter(NAME == bases_sit$name[1]) %>% 
        pull(speed_range) -> speed
      
      speed_x <- ifelse(speed == "very fast", .57,
                        ifelse(speed == "fast", .45,
                               ifelse(speed == "slow", .33, .24)))
      if(runif(1,0,1) < speed_x)
      {
        tot_runs = tot_runs + 2
        bases_sit$state[1] <- FALSE
        bases_sit$name[1] <- "_"
        
      }else{
        bases_sit$name[3] <- bases_sit$name[1]
        bases_sit$name[1] <- "_"
        bases_sit$state[1] <- FALSE
        tot_runs = tot_runs + 1
      }

    }else if(outcome == 1)
    {
      tot_runs = tot_runs  + 1
      
      hitters %>% 
        filter(NAME == bases_sit$name[1]) %>% 
        pull(speed_range) -> speed
      
      speed_x <- ifelse(speed == "very fast", .4,
                        ifelse(speed == "fast", .35,
                               ifelse(speed == "slow", .3, .25)))
      
      if(runif(1,0,1) < speed_x)
      {
        
        bases_sit$name[3] <- bases_sit$name[1]
        bases_sit$name[1] <- hitters[whos_up,]$NAME

        
      }else{
        bases_sit$state[2] <- TRUE
        bases_sit$state[3] <- FALSE
        bases_sit$name[3] <- "_"
        bases_sit$name[2] <- bases_sit$name[1]
        bases_sit$name[1] <- hitters[whos_up,]$NAME
      }
      
    }else if(outcome == -9)
    {
      hitters %>% 
        filter(NAME == bases_sit$name[3]) %>% 
        pull(speed_range) -> speed
      
      speed_x <- ifelse(speed == "very fast", .93,
                        ifelse(speed == "fast", .83,
                               ifelse(speed == "slow", .73,  .68)))
      prob = runif(1,0,1)
      if(prob < speed_x & numOuts < 2)
      {
        numOuts = numOuts + 1
        bases_sit$state[3] <- FALSE
        bases_sit$name[3] <- "_"
        tot_runs = tot_runs + 1
      }else if(runif(1,0,1) > .93 & numOuts < 2)
      {
        numOuts = numOuts + 1
        bases_sit$state[3] <- FALSE
        bases_sit$name[3] <- "_"
        numOuts = numOuts +1
      }else{
        numOuts = numOuts + 1
      }

    }else if(outcome == -8)
    {

      bases_sit$state[2] <- FALSE
      chance <- runif(1,0,1)
      speed = hitters[whos_up,]$speed_range
      speed_x <- ifelse(speed == "very fast", .40,
                        ifelse(speed == "fast", .47,
                               ifelse(speed == "slow", .54, .60)))
      if(chance < speed_x & numOuts < 2)
      {
        numOuts = numOuts + 1
        bases_sit$state[1] <- FALSE
        bases_sit$name[1] <- "_"
        bases_sit$state[3] <- FALSE
        bases_sit$name[3] <- "_"
        numOuts = numOuts +1
        tot_runs = tot_runs + 1

      }else if(chance >  speed_x & numOuts < 2)
      {
        numOuts = numOuts + 1
        bases_sit$state[1] <- TRUE
        bases_sit$state[3] <- FALSE
        tot_runs = tot_runs + 1
        
      }else if(chance < .5 & numOuts == 2)
      {
        
        bases_sit$state[1] <- FALSE
        bases_sit$state[3] <- FALSE
        numOuts = numOuts +1
      }else if(chance > .5 & numOuts == 2)
      {
        tot_runs = tot_runs + 1
        numOuts = numOuts + 1
      }
    }else if(outcome < 0)
    {
      numOuts = numOuts + 1
    }
      
      
  }else if(!bases_sit$state[1] & bases_sit$state[2] & bases_sit$state[3])
  {
    # on second and third
    if(outcome == 5 | outcome == 6)
    {
      bases_sit$state[1] <- TRUE
      bases_sit$name[1] <- hitters[whos_up,]$NAME
    }else if(outcome == 4)
    {
      bases_sit$state[2] <- FALSE
      bases_sit$name[2] <- "_"
      bases_sit$state[3]<- FALSE
      bases_sit$name[3] <- "_"
      tot_runs = tot_runs + 3
    }else if(outcome == 3)
    {
      bases_sit$state[2] <- FALSE
      bases_sit$name[2] <- "_"
      bases_sit$name[3] <- hitters[whos_up,]$NAME
      tot_runs = tot_runs + 2
    }else if(outcome == 2)
    {
      bases_sit$state[3] <- FALSE
      bases_sit$name[2] <- hitters[whos_up,]$NAME
      tot_runs = tot_runs + 2
    }else if(outcome == 1)
    {
      tot_runs = tot_runs + 1
      hitters %>% 
        filter(NAME == bases_sit$name[2]) %>% 
        pull(speed_range) -> speed
      
      speed_x <- ifelse(speed == "very fast", .85,
                        ifelse(speed == "fast", .70,
                               ifelse(speed == "slow", .60, .50)))
      
      if(runif(1,0,1) < speed_x)
      {
        bases_sit$state[1] <- TRUE
        bases_sit$name[1] <- hitters[whos_up,]$NAME
        bases_sit$state[2] <- FALSE
        bases_sit$name[2] <- "_"
        tot_runs = tot_runs + 1
      }else
      {
        bases_sit$state[1] <- TRUE
        bases_sit$name[1] <- hitters[whos_up,]$NAME
        bases_sit$state[3] <- TRUE
        bases_sit$name[3] <- bases_sit$name[2]
        bases_sit$state[2] <- FALSE
        bases_sit$name[2] <- "_"
        
      }
      
    }else if(outcome == -8)
    {
      numOuts = numOuts + 1
      chance <- runif(1,0,1)
      if(chance < .51 )
      {
        tot_runs = tot_runs + 1
        bases_sit$name[3] <- bases_sit$name[2]
        bases_sit$state[2] <- FALSE
        
      }else if(chance > .51 & chance < .96 )
      {
        tot_runs = tot_runs + 1
        bases_sit$name[3] <- "_"
        bases_sit$state[3] <- FALSE
      }else if(chance > .96)
      {
        numOuts = numOuts + 1
        bases_sit$state[1] <- TRUE
        bases_sit$name[1] <- hitters[whos_up,]$NAME
        bases_sit$state[2] <- FALSE
        bases_sit$name[3]<- bases_sit$name[2]
        bases_sit$name[2] <- "_"
      }
    }else if(outcome < 0 )
    {
      numOuts = numOuts + 1
    }
    
  }else if(bases_sit$state[1] & bases_sit$state[2] & bases_sit$state[3])
  {
    if(outcome == 5 | outcome == 6)
    {
      bases_sit$name[3] <- bases_sit$name[2]
      bases_sit$name[2] <- bases_sit$name[1]
      bases_sit$name[1] <- hitters[whos_up,]$NAME
      tot_runs = tot_runs  + 1
    }else if(outcome == 4)
    {
      bases_sit$state[1] <- FALSE
      bases_sit$name[1] <- "_"
      bases_sit$state[2] <- FALSE
      bases_sit$name[2] <- "_"
      bases_sit$state[3] <- FALSE
      bases_sit$name[3] <- "_"
      tot_runs = tot_runs  + 4
    }else if(outcome == 3)
    {
      bases_sit$state[1] <- FALSE
      bases_sit$state[2] <- FALSE
      bases_sit$name[1] <- "_"
      bases_sit$name[2] <- "_"
      bases_sit$name[3] <- hitters[whos_up,]$NAME
      tot_runs = tot_runs  + 3
    }else if(outcome == 2)
    {
      tot_runs = tot_runs + 2
      bases_sit$name[2] <- hitters[whos_up,]$NAME
      hitters %>% 
        filter(NAME == bases_sit$name[1]) %>% 
        pull(speed_range) -> speed
      
      speed_x <- ifelse(speed == "very fast", .57,
                        ifelse(speed == "fast", .45,
                               ifelse(speed == "slow", .33, .24)))
      if(runif(1,0,1) < speed_x)
      {
        tot_runs = tot_runs + 1
        bases_sit$state[1] <- FALSE
        bases_sit$state[3] <- FALSE
        bases_sit$name[1] <- "_"
        bases_sit$name[3] <- "_"
      }else{
        bases_sit$state[3] <- TRUE
        bases_sit$name[3] <- bases_sit$name[1]
        bases_sit$name[1] <- "_"
        bases_sit$state[1] <- FALSE
              
      }
      
    }else if(outcome == 1)
    {
      tot_runs = tot_runs + 1
      hitters %>% 
        filter(NAME == bases_sit$name[2]) %>% 
        pull(speed_range) -> speed
      
      speed_x <- ifelse(speed == "very fast", .85,
                        ifelse(speed == "fast", .70,
                               ifelse(speed == "slow", .60, .50)))
      
      if(runif(1,0,1) < speed_x)
      {
        tot_runs = tot_runs + 1
        bases_sit$name[2] <- "_"
        hitters %>% 
          filter(NAME == bases_sit$name[1]) %>% 
          pull(speed_range) -> speed
        
        speed_x <- ifelse(speed == "very fast", .4,
                          ifelse(speed == "fast", .35,
                                 ifelse(speed == "slow", .3,  .25)))
        
        if(runif(1,0,1) < speed_x)
        {
          bases_sit$state[3] <- TRUE
          bases_sit$name[3] <- bases_sit$name[1]
          bases_sit$name[1] <- hitters[whos_up,]$NAME
          
        }else{
          bases_sit$state[2] <- TRUE
          bases_sit$name[2] <- bases_sit$name[1]
          bases_sit$name[1] <- hitters[whos_up,]$NAME
        }
      }else
      {        
        bases_sit$state[3] <- TRUE
        bases_sit$name[3] <- bases_sit$name[2]
        bases_sit$name[2] <- bases_sit$name[1]
        bases_sit$name[1] <- hitters[whos_up,]$NAME
      }
      

    }else if(outcome == -8)
    {
      chance <- runif(1,0,1)
      if(chance < .81 & numOuts < 2)
      {
        numOuts = numOuts + 1
        bases_sit$state[3] <- FALSE
        tot_runs = tot_runs + 1
      }else if(chance > .81)
      {
        numOuts = numOuts + 1
        bases_sit$state[3] <- FALSE
        numOuts = numOuts +1
      }
    }else if(outcome == -9)
    {
      
      hitters %>% 
        filter(NAME == bases_sit$name[3]) %>% 
        pull(speed_range) -> speed
      
      speed_x <- ifelse(speed == "very fast", .93,
                        ifelse(speed == "fast", .83,
                               ifelse(speed == "slow", .73, .68)))
      prob = runif(1,0,1)
      if(prob < speed_x & numOuts < 2)
      {
        numOuts = numOuts + 1
        bases_sit$state[3] <- FALSE
        bases_sit$name[3] <- "_"
        tot_runs = tot_runs + 1
      }else if(runif(1,0,1) > .93 & numOuts < 2)
      {
        numOuts = numOuts + 1
        bases_sit$state[3] <- FALSE
        bases_sit$name[3] <- "_"
        numOuts = numOuts + 1
      }else{
        numOuts = numOuts + 1
      }
    }else if(outcome < 0)
    {
      numOuts = numOuts + 1
    }
  }
    whos_up <- ifelse(whos_up == 9 , 1, whos_up+1)
    }
  }
  return(tot_runs)
}

best_lineup <- function(lineup, perms)
{
  for(ord in 1:nrow(perms))
  {
    hitters <- lineup[c(perms[ord,1]$V1,perms[ord,2]$V2,perms[ord,3]$V3,
                         perms[ord,4]$V4,perms[ord,5]$V5,perms[ord,6]$V6,
                         perms[ord,7]$V7,perms[ord,8]$V8,perms[ord,9]$V9),]
    run_holder <- c()
    for(games in 1:10) #per all games wanted
    {
      tot_runs <- play_game(hitters)
      
      run_holder <- append(run_holder, tot_runs)
    }
    perms[ord,]$runs = mean(run_holder)
    print(mean(run_holder))
  }
  return(perms)
 }


















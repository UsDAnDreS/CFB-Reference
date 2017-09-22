rm(list=ls())
library(pca3d)
library(stringr)

load.league <- function(year,
            dir="/home/usdandres/Documents/Study_stuff/Sports Research/Football/University of Houston Cougars/"){
  return(list(QBs=read.csv(paste(dir,"All_QBs_",year,".csv",sep=""),skip=1,header=T),
              Offense=read.csv(paste(dir,"All_Teams_Offense_",year,".csv",sep=""),skip=1,header=T),
              Defense=read.csv(paste(dir,"All_Teams_Defense_",year,".csv",sep=""),skip=1,header=T),
              Special=read.csv(paste(dir,"All_Teams_Special_",year,".csv",sep=""),skip=1,header=T),
              WRs=read.csv(paste(dir,"All_Receivers_",year,".csv",sep=""),skip=1,header=T),
              RBs=read.csv(paste(dir,"All_RBs_",year,".csv",sep=""),skip=1,header=T)))
}

load.team <- function(name,year,
                      dir="/home/usdandres/Documents/Study_stuff/Sports Research/Football/University of Houston Cougars/"){
  folder.name <- paste(dir,name,"/",year,"/",sep="")
  return(list(OffDef=read.csv(paste(folder.name,"Team_Stats.csv",sep=""),skip=1,header=T),
              Passing=read.csv(paste(folder.name,"Passing.csv",sep=""),skip=1,header=T),
              KickPunt=read.csv(paste(folder.name,"Kicking&Punting.csv",sep=""),skip=1,header=T),
              #Scoring <- read.csv("Scoring.csv"),
              RushReceive=read.csv(paste(folder.name,"Rushing&Receiving.csv",sep=""),skip=1,header=T),
              DefFumb=read.csv(paste(folder.name,"Defense&Fumbles.csv",sep=""),skip=1,header=T),
              KickPuntReturn=read.csv(paste(folder.name,"Kick&PuntReturns.csv",sep=""),skip=1,header=T)))
}

TeamValAndRankCateg <- function(name,data,vec,dec,val=NULL){
  a <- data[order(vec,decreasing=dec),]
  if (!is.null(val)) return(list(rank=which.min(abs(sort(vec,decreasing=dec)-val)),value=val))
  rank <- which(a$School == name)
  value <- sort(vec,decreasing=dec)[rank]
  
  return(c(Value=value,Rank=rank))
}

## side - the side of the ball: "Offense", "Defense"
## type - "Overall","Passing", "Rushing"
## basis - "Per Game","Per Play"
TeamValAndRank <- function(name,
                           side="Offense",
                           type="Overall",
                           basis="Per Game",
                           data=league.data
                           ){
  if (side == "Offense"){
    if (type == "Overall"){
      if (basis == "Per Game"){
        tab <- matrix(c(TeamValAndRankCateg(name,data$Offense,data$Offense$Plays,dec=T),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$Pts,dec=T),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$No.,dec=F),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$Tot,dec=T),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$Tot.1,dec=F),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$Yds.2,dec=T),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$TD + data$Offense$TD.1,dec=T)),ncol=2,byrow=T)
        colnames(tab) <- c("Value","Rank")
        rownames(tab) <- c("Plays","Points","Penalties","1stDowns","Turnovers","Yards","TDs")
      }
    if (basis == "Per Play"){
      tab <- matrix(c(TeamValAndRankCateg(name,data$Offense,data$Offense$Pts/data$Offense$Plays,dec=T),
                      TeamValAndRankCateg(name,data$Offense,data$Offense$No./data$Offense$Plays,dec=F),
                      TeamValAndRankCateg(name,data$Offense,data$Offense$Tot/data$Offense$Plays,dec=T),
                      TeamValAndRankCateg(name,data$Offense,data$Offense$Tot.1/data$Offense$Plays,dec=F),
                      TeamValAndRankCateg(name,data$Offense,data$Offense$Yds.2/data$Offense$Plays,dec=T),
                      TeamValAndRankCateg(name,data$Offense,(data$Offense$TD + data$Offense$TD.1)/data$Offense$Plays,dec=T)),ncol=2,byrow=T)
      colnames(tab) <- c("Value","Rank")
      rownames(tab) <- c("Points","Penalties","1stDowns","Turnovers","Yards","TDs")
    }
    }
    if (type == "Passing"){
      if (basis == "Per Game"){
        tab <- matrix(c(TeamValAndRankCateg(name,data$Offense,data$Offense$Att,dec=T),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$Pct,dec=T),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$Yds,dec=T),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$TD,dec=T),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$Pass,dec=T),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$Int,dec=F)),ncol=2,byrow=T)
        colnames(tab) <- c("Value","Rank")
        rownames(tab) <- c("Plays","Comp Pct","Yards","TD","1stDowns","INT")
      }
      if (basis == "Per Play"){
        tab <- matrix(c(TeamValAndRankCateg(name,data$Offense,data$Offense$Yds/data$Offense$Att,dec=T),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$TD/data$Offense$Att,dec=T),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$Pass/data$Offense$Tot,dec=T),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$Int/data$Offense$Att,dec=F)),ncol=2,byrow=T)
        colnames(tab) <- c("Value","Rank")
        rownames(tab) <- c("Yards","TD","1stDowns","INT")
      }
    }
    
    if (type == "Rushing"){
      if (basis == "Per Game"){
        tab <- matrix(c(TeamValAndRankCateg(name,data$Offense,data$Offense$Att.1,dec=T),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$Yds.1,dec=T),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$TD.1,dec=T),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$Rush,dec=T),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$Fum,dec=F)),ncol=2,byrow=T)
        colnames(tab) <- c("Value","Rank")
        rownames(tab) <- c("Plays","Yards","TD","1stDowns","FUM")
      }
      if (basis == "Per Play"){
        tab <- matrix(c(TeamValAndRankCateg(name,data$Offense,data$Offense$Yds.1/data$Offense$Att.1,dec=T),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$TD.1/data$Offense$Att.1,dec=T),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$Rush/data$Offense$Tot,dec=T),
                        TeamValAndRankCateg(name,data$Offense,data$Offense$Fum/data$Offense$Att.1,dec=F)),ncol=2,byrow=T)
        colnames(tab) <- c("Value","Rank")
        rownames(tab) <- c("Yards","TD","1stDowns","FUM")
      }
    }
  }
  
  
  if (side == "Defense"){
    if (type == "Overall"){
      if (basis == "Per Game"){
        tab <- matrix(c(TeamValAndRankCateg(name,data$Defense,data$Defense$Plays,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$Pts,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$No.,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$Tot,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$TO,dec=T),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$Yds.2,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$TD+data$Defense$TD.1,dec=F)),ncol=2,byrow=T)
        colnames(tab) <- c("Value","Rank")
        rownames(tab) <- c("Plays","Points","Penalties","1stDowns","Turnovers","Yards","TDs")
      }
      if (basis == "Per Play"){
        tab <- matrix(c(TeamValAndRankCateg(name,data$Defense,data$Defense$Pts/data$Defense$Plays,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$No./data$Defense$Plays,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$Tot/data$Defense$Plays,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$TO/data$Defense$Plays,dec=T),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$Yds.2/data$Defense$Plays,dec=F),
                        TeamValAndRankCateg(name,data$Defense,(data$Defense$TD+data$Defense$TD.1)/data$Defense$Plays,dec=F)),ncol=2,byrow=T)
        colnames(tab) <- c("Value","Rank")
        rownames(tab) <- c("Points","Penalties","1stDowns","Turnovers","Yards","TDs")
      }
    }
    if (type == "Passing"){
      if (basis == "Per Game"){
        tab <- matrix(c(TeamValAndRankCateg(name,data$Defense,data$Defense$Att,dec=T),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$Pct,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$Yds,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$TD,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$Pass,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$Int,dec=T)),ncol=2,byrow=T)
        colnames(tab) <- c("Value","Rank")
        rownames(tab) <- c("Plays","Comp Pct","Yards","TD","1stDowns","INT")
      }
      if (basis == "Per Play"){
        tab <- matrix(c(TeamValAndRankCateg(name,data$Defense,data$Defense$Yds/data$Defense$Att,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$TD/data$Defense$Att,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$Pass/data$Defense$Tot,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$Int/data$Defense$Att,dec=T)),ncol=2,byrow=T)
        colnames(tab) <- c("Value","Rank")
        rownames(tab) <- c("Yards","TD","1stDowns","INT")
      }
    }
    
    if (type == "Rushing"){
      if (basis == "Per Game"){
        tab <- matrix(c(TeamValAndRankCateg(name,data$Defense,data$Defense$Att.1,dec=T),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$Yds.1,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$TD.1,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$Rush,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$Fum,dec=T)),ncol=2,byrow=T)
        colnames(tab) <- c("Value","Rank")
        rownames(tab) <- c("Plays","Yards","TD","1stDowns","FUM")
      }
      if (basis == "Per Play"){
        tab <- matrix(c(TeamValAndRankCateg(name,data$Defense,data$Defense$Yds.1/data$Defense$Att.1,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$TD.1/data$Defense$Att.1,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$Rush/data$Defense$Tot,dec=F),
                        TeamValAndRankCateg(name,data$Defense,data$Defense$Fum/data$Defense$Att.1,dec=T)),ncol=2,byrow=T)
        colnames(tab) <- c("Value","Rank")
        rownames(tab) <- c("Yards","TD","1stDowns","FUM")
      }
    }
  }
  
  
 # print(tab)
  tab <- as.data.frame(tab)
  tab$Rank <- as.integer(tab$Rank)
  return(tab)
}


## Ranks players according to a category
## Vector 'vec' is a vector of values of this category for each player
PlayerValAndRankCateg <- function(name,data,vec,dec,val=NULL){
  a <- data[order(vec,decreasing=dec),]
  if (!is.null(val)) return(list(rank=which.min(abs(sort(vec,decreasing=dec)-val)),value=val))
  rank <- which(grepl(name,a$Player))
  value <- sort(vec,decreasing=dec)[rank]
  return(c(Value=value,Rank=rank))
}

## Prints out a summary of values and ranks for players
## who are qualified for the position (QB,RB,WR)
PlayerValAndRank <- function(name,
                             pos="QB",
                             basis="Per Game",
                             data=league.data
                             ){
  if (pos == "QB"){
      if (basis == "Per Game"){
        tab <- matrix(c(PlayerValAndRankCateg(name,data$QBs,data$QBs$Att/data$QBs$G,dec=T),
                        PlayerValAndRankCateg(name,data$QBs,data$QBs$Cmp/data$QBs$G,dec=T),
                        PlayerValAndRankCateg(name,data$QBs,data$QBs$Pct,dec=T),
                        PlayerValAndRankCateg(name,data$QBs,data$QBs$Yds/data$QBs$G,dec=T),
                        PlayerValAndRankCateg(name,data$QBs,data$QBs$TD/data$QBs$G,dec=T),
                        PlayerValAndRankCateg(name,data$QBs,data$QBs$Int/data$QBs$G,dec=F),
                        PlayerValAndRankCateg(name,data$QBs,data$QBs$Rate,dec=T),
                        PlayerValAndRankCateg(name,data$QBs,data$QBs$Att.1/data$QBs$G,dec=T),
                        PlayerValAndRankCateg(name,data$QBs,data$QBs$Yds.1/data$QBs$G,dec=T),
                        PlayerValAndRankCateg(name,data$QBs,data$QBs$TD.1/data$QBs$G,dec=T)),ncol=2,byrow=T)
        
        colnames(tab) <- c("Value","Rank")
        rownames(tab) <- c("Pass Att","Pass Compl","Compl Pct","Pass Yards","Pass TD","INT","Rate",
                           "Rush Att","Rush Yards", "Rush TD")
      }
    
    if (basis=="Per Play"){
      tab <- matrix(c(PlayerValAndRankCateg(name,data$QBs,data$QBs$Y.A,dec=T),
                      PlayerValAndRankCateg(name,data$QBs,data$QBs$TD/data$QBs$Att,dec=T),
                      PlayerValAndRankCateg(name,data$QBs,data$QBs$Int/data$QBs$Att,dec=F),
                      PlayerValAndRankCateg(name,data$QBs,data$QBs$Avg,dec=T),
                      PlayerValAndRankCateg(name,data$QBs,data$QBs$TD.1/data$QBs$Att.1,dec=T)),ncol=2,byrow=T)
      colnames(tab) <- c("Value","Rank")
      rownames(tab) <- c("Pass Y/A","Pass TD/A","INT/A","Rush Y/A","Rush TD/A")
    }
  }
  
  
  if (pos == "RB"){
    if (basis == "Per Game"){
      tab <- matrix(c(PlayerValAndRankCateg(name,data$RBs,data$RBs$Att/data$RBs$G,dec=T),
                      PlayerValAndRankCateg(name,data$RBs,data$RBs$Yds/data$RBs$G,dec=T),
                      PlayerValAndRankCateg(name,data$RBs,data$RBs$TD/data$RBs$G,dec=T),
                      PlayerValAndRankCateg(name,data$RBs,data$RBs$Rec/data$RBs$G,dec=T),
                      PlayerValAndRankCateg(name,data$RBs,data$RBs$Yds.1/data$RBs$G,dec=T),
                      PlayerValAndRankCateg(name,data$RBs,data$RBs$TD.1/data$RBs$G,dec=T),
                      PlayerValAndRankCateg(name,data$RBs,data$RBs$Plays/data$RBs$G,dec=T),
                      PlayerValAndRankCateg(name,data$RBs,data$RBs$Yds.2/data$RBs$G,dec=T),
                      PlayerValAndRankCateg(name,data$RBs,data$RBs$TD.2/data$RBs$G,dec=T)),ncol=2,byrow=T)
      colnames(tab) <- c("Value","Rank")
      rownames(tab) <- c("Rush Att","Rush Yards","Rush TD",
                         "Receptions","Rec Yards", "Rec TD",
                         "Tot Plays","Tot Yards","Tot TD")
    }
    
    if (basis=="Per Play"){
      tab <- matrix(c(PlayerValAndRankCateg(name,data$RBs,data$RBs$Avg,dec=T),
                      PlayerValAndRankCateg(name,data$RBs,data$RBs$TD/data$RBs$Att,dec=T),
                      PlayerValAndRankCateg(name,data$RBs,data$RBs$Avg.1,dec=T),
                      PlayerValAndRankCateg(name,data$RBs,ifelse(data$RBs$Rec == 0,0,data$RBs$TD.1/data$RBs$Rec),dec=T),
                      PlayerValAndRankCateg(name,data$RBs,data$RBs$Avg.2,dec=T),
                      PlayerValAndRankCateg(name,data$RBs,data$RBs$TD.2/data$RBs$Plays,dec=T)),ncol=2,byrow=T)
      colnames(tab) <- c("Value","Rank")
      rownames(tab) <- c("Rush Y/A","Rush TD/A","Rec Y/A","Rec TD/A","Total Y/A","Total TD/A")
    }
  }
  
  if (pos == "WR"){
    if (basis == "Per Game"){
      tab <- matrix(c(PlayerValAndRankCateg(name,data$WRs,data$WRs$Rec/data$WRs$G,dec=T),
                      PlayerValAndRankCateg(name,data$WRs,data$WRs$Yds/data$WRs$G,dec=T),
                      PlayerValAndRankCateg(name,data$WRs,data$WRs$TD/data$WRs$G,dec=T),
                      PlayerValAndRankCateg(name,data$WRs,data$WRs$Att/data$WRs$G,dec=T),
                      PlayerValAndRankCateg(name,data$WRs,data$WRs$Yds.1/data$WRs$G,dec=T),
                      PlayerValAndRankCateg(name,data$WRs,data$WRs$TD.1/data$WRs$G,dec=T),
                      PlayerValAndRankCateg(name,data$WRs,data$WRs$Plays/data$WRs$G,dec=T),
                      PlayerValAndRankCateg(name,data$WRs,data$WRs$Yds.2/data$WRs$G,dec=T),
                      PlayerValAndRankCateg(name,data$WRs,data$WRs$TD.2/data$WRs$G,dec=T)),ncol=2,byrow=T)
      colnames(tab) <- c("Value","Rank")
      rownames(tab) <- c("Receptions","Pass Yards","Pass TD",
                         "Rush Att","Rush Yards", "Rush TD",
                         "Tot Plays","Tot Yards","Tot TD")
    }
    
    if (basis=="Per Play"){
      tab <- matrix(c(PlayerValAndRankCateg(name,data$WRs,data$WRs$Avg,dec=T),
                      PlayerValAndRankCateg(name,data$WRs,data$WRs$TD/data$WRs$Rec,dec=T),
                      PlayerValAndRankCateg(name,data$WRs,data$WRs$Avg.1,dec=T),
                      PlayerValAndRankCateg(name,data$WRs,ifelse(data$WRs$Att == 0,0,data$WRs$TD.1/data$WRs$Att),dec=T),
                      PlayerValAndRankCateg(name,data$WRs,data$WRs$Avg.2,dec=T),
                      PlayerValAndRankCateg(name,data$WRs,data$WRs$TD.2/data$WRs$Plays,dec=T)),ncol=2,byrow=T)
      colnames(tab) <- c("Value","Rank")
      rownames(tab) <- c("Rec Y/A","Rec TD/A","Rush Y/A","Rush TD/A","Total Y/A","Total TD/A")
    }
  }
  
  
  tab <- as.data.frame(tab)
  tab$Rank <- as.integer(tab$Rank)
  return(tab)
}


TeamVsOpponents <- function(name,
                            year,
                            dir="/home/usdandres/Documents/Study_stuff/Sports Research/Football/University of Houston Cougars/")
{
  
  #######################
  ### Offensive and Defensive stats for all opponents from the schedule
  #######################
  # print(paste(dir,name,"/",year,"/",sep=""))
  # Opponent_names <- list.files(path=paste(dir,name,"/",year,"/",sep=""),pattern=('Team_Stats_.*'))
  # Opp_data <- list()
  # n.opp <- length(Opponent_names)
  # for (j in 1:n.opp){
  #   Opp_data[[j]] <- read.csv(paste(dir,name,"/",year,"/",Opponent_names[j],sep=""),skip=1,as.is=T)
  #   Opp_data[[j]]$Team <- Opponent_names[j]
  #   # Offense <- rbind(Offense,)
  # }
  # 
  # ### offensive yards
  # Opp_Offense <- NULL
  # for (j in 1:n.opp){
  #   Opp_Offense <- rbind(Opp_Offense,Opp_data[[j]][1,])
  # }
  # 
  # ### Defensive yards
  # Opp_Defense <- NULL
  # for (j in 1:n.opp){
  #   Opp_Defense <- rbind(Opp_Defense,Opp_data[[j]][2,])
  # }
  
  ############  
  ### OPPONENTS RECORDS and POINTS DIFFERENTIALS IN GAMES AGAINST THEM
  ############
  
  Standings <- read.csv(paste(dir,"Standings_",year,".csv",sep=""),
                        skip=1,header=T,as.is=T)
  Scores_vs_Opp <- read.csv(paste(dir,name,"/",year,"/Scores_vs_Opp.csv",sep=""),as.is=T)
  Scores_vs_Opp <- subset(Scores_vs_Opp,Conf != "Non-Major")
  Scores_vs_Opp$PointDiff <- Scores_vs_Opp$Pts - Scores_vs_Opp$Opp
  Scores_vs_Opp$Opponent <- sapply(Scores_vs_Opp$Opponent,function(x) ifelse(substr(x,1,1) == '(',
                                                                             ifelse(substr(x,3,3) == ')',
                                                                                    substr(x,5,nchar(x)),
                                                                                    substr(x,6,nchar(x))),
                                                                             x))
  Scores_vs_Opp$OppWins <- Standings[sapply(Scores_vs_Opp$Opponent, 
                                            function(x) which(Standings$School == x)),]$W
  
 # Scores_vs_Opp <- Scores_vs_Opp[order(Scores_vs_Opp$Opponent),]
  
  
  ######
  
  return(list(Scores_vs_Opp=Scores_vs_Opp))
              #Opp_Offense = Opp_Offense,
              #Opp_Defense = Opp_Defense))
}

opp.plot <- function(Scores,side,type,rank=F,
                     x.lims=c(0,0),y.lims=c(0,0),my.cex=1){
  
  par(cex=my.cex)
  #################
  ### BALANCE OF THE OFFENSES FACED
  ###
  ### METRIC: CENTERED AND SCALED NUMBER OF PASSING PLAYS RUN
  ##              PLOTTED VVS
  ##          CENTERED AND SCALED NUMBER OF RUNNING PLAYS
  ################
  
  if (type == "Balance"){
    Yds.Pass <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Passing",basis="Per Game")['Plays',]$Value)
    Yds.Rush <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Rushing",basis="Per Game")['Plays',]$Value)
    Yds.Pass.center <- (Yds.Pass - mean(Yds.Pass))/sd(Yds.Pass)
    Yds.Rush.center <- (Yds.Rush - mean(Yds.Rush))/sd(Yds.Rush)
    x <- Yds.Pass.center
    y <- Yds.Rush.center
    
    plot(x,y,
         type='n',
         axes=T,
         xlim=ifelse(x.lims == 0,c(-3,3),x.lims),
         ylim=ifelse(y.lims == 0,c(-3,3),y.lims),
         xlab="Passing Plays (Per Game)",
         ylab="Rushing Plays (Per Game)",
         main=paste(Scores$School[1],"Opponents Balance on ",side," in 2016"))
    
    Scores$Outcome <- paste(Scores$X,
                            Scores$X.1,
                            Scores$Pts,
                            Scores$Opp)
    text(x,y,labels=Scores$Outcome)
    print(x)
    print(y)
    abline(0,1)
    return()
  }
  
  
  if (side=="Overall"){
    if (rank == T){
      if (type == "Per Game"){
        Yds.Off <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Offense",type=side,basis=type)['Yards',]$Rank)
        Yds.Def <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Defense",type=side,basis=type)['Yards',]$Rank)
        x <- Yds.Off
        y <- Yds.Def
      }
      if (type == "Per Play"){
        Yds.Off.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Offense",type=side,basis=type)['Yards',]$Rank)
        Yds.Def.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Defense",type=side,basis=type)['Yards',]$Rank)
        x <- Yds.Off.PP
        y <- Yds.Def.PP
      }
      plot(x,y,
           type='n',
           #axes=F,
           xlim=ifelse(x.lims == 0,c(min(x)-20,max(x)+20),x.lims),
           ylim=ifelse(y.lims == 0,c(min(y)-20,max(y)+20),y.lims),
           xlab=paste("Opp Offense RANK  (",type,")",sep=""),
           ylab=paste("Opp Defense RANK (",type,")",sep=""),
           main=paste(Scores$School[1]," in 2016 vs Opponents Ranks",sep=""))
      Scores$Outcome <- paste(Scores$X,
                              Scores$X.1,
                              Scores$Pts,
                              Scores$Opp)
      text(x,y,labels=Scores$Outcome)
    }
    
    if (rank == F){
      if (type == "Per Game"){
        Yds.Off <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Offense",type=side,basis=type)['Yards',]$Value)
        Yds.Def <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Defense",type=side,basis=type)['Yards',]$Value)
        x <- Yds.Off
        y <- Yds.Def
      }
      if (type == "Per Play"){
        Yds.Off.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Offense",type=side,basis=type)['Yards',]$Value)
        Yds.Def.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Defense",type=side,basis=type)['Yards',]$Value)
        x <- Yds.Off.PP
        y <- Yds.Def.PP
      }
      plot(x,y,
           type='n',
           # axes=F,
           xlim=ifelse(x.lims == 0,c(min(x)-1,max(x)+1),x.lims),
           ylim=ifelse(y.lims == 0,c(min(y)-1,max(y)+1),y.lims),
           xlab=paste("Opp Offense Yards Allowed (",type,")",sep=""),
           ylab=paste("Opp Defense Yards Allowed (",type,")",sep=""),
           main=paste(Scores$School[1],"in 2016 vs Opponents",sep=""))
      Scores$Outcome <- paste(Scores$X,
                              Scores$X.1,
                              Scores$Pts,
                              Scores$Opp)
      text(x,y,labels=Scores$Outcome)
    }
  }
  
  if (side != "Overall"){
    if (rank == T){
      if (type == "Per Game"){
        Yds.Pass <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Passing",basis=type)['Yards',]$Rank)
        Yds.Rush <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Rushing",basis=type)['Yards',]$Rank)
        x <- Yds.Pass
        y <- Yds.Rush
      }
      
      if (type == "Per Play"){
        Yds.Pass.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Passing",basis=type)['Yards',]$Rank)
        Yds.Rush.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Rushing",basis=type)['Yards',]$Rank)
        x <- Yds.Pass.PP
        y <- Yds.Rush.PP
      }
      
      plot(x,y,type='n',
           xlim=ifelse(x.lims == 0,c(min(x)-20,max(x)+20),x.lims),
           ylim=ifelse(y.lims == 0,c(min(y)-20,max(y)+20),y.lims),
           xlab=paste("Opp RANK in Passing Yds (",type,")",sep=""),
           ylab=paste("Opp RANK in Rushing Yds (",type,")",sep=""),
           main=paste(Scores$School[1]," vs ",side," in 2016",sep=""))
      Scores$Outcome <- paste(Scores$X,
                              Scores$X.1,
                              Scores$Pts,
                              Scores$Opp)
      text(x,y,labels=Scores$Outcome)
    }
    
    if (rank == F){
      if (type == "Per Game"){
        Yds.Pass <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Passing",basis=type)['Yards',]$Value)
        Yds.Rush <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Rushing",basis=type)['Yards',]$Value)
        x <- Yds.Pass
        y <- Yds.Rush
        print(x)
        print(y)
      }
      if (type == "Per Play"){
        Yds.Pass.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Passing",basis=type)['Yards',]$Value)
        Yds.Rush.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Rushing",basis=type)['Yards',]$Value)
        x <- Yds.Pass.PP
        y <- Yds.Rush.PP
      }
      plot(x,y,type='n',
           xlim=ifelse(x.lims == 0,c(min(x)-1,max(x)+1),x.lims),
           ylim=ifelse(y.lims == 0,c(min(y)-1,max(y)+1),y.lims),
           xlab=paste("Opp Passing Yds (",type,")",sep=""),
           ylab=paste("Opp Rushing Yds (",type,")",sep=""),
           main=paste(Scores$School[1]," vs ",side," in 2016",sep=""))
      Scores$Outcome <- paste(Scores$X,
                              Scores$X.1,
                              Scores$Pts,
                              Scores$Opp)
      text(x,y,labels=Scores$Outcome)
    }
  }
  par(cex=1)
}

opp.PCA <- function(Scores,side="Offense",type="Per Game",rank=F,
                    dim.pca="2D",components=c(1:3),
                    x.lims=c(0,0),y.lims=c(0,0)){
  if (side=="Overall"){
    if (rank == T){
      if (type == "Per Game"){
        Yds.Off.Rank <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Offense",type=side,basis=type)['Yards',]$Rank)
        Yds.Def.Rank <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Defense",type=side,basis=type)['Yards',]$Rank)
        TDs.Off.Rank <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Offense",type=side,basis=type)['TD',]$Rank)
        TDs.Def.Rank <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Defense",type=side,basis=type)['TD',]$Rank)
        TOs.Off.Rank <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Offense",type=side,basis=type)['Turnovers',]$Rank)
        TOs.Def.Rank <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Defense",type=side,basis=type)['Turnovers',]$Rank)
        
        Data_PCA <- data.frame(-Yds.Off.Rank,-Yds.Def.Rank,-TDs.Off.Rank,-TDs.Def.Rank,-TOs.Off.Rank,-TOs.Def.Rank)
        colnames(Data_PCA) <- c("Off.Yds", "Def.Yds","Off.TD","Def.TD","Off.TO","Def.TO")
      }
      if (type == "Per Play"){
        Yds.Off.Rank.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Offense",type=side,basis=type)['Yards',]$Rank)
        Yds.Def.Rank.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Defense",type=side,basis=type)['Yards',]$Rank)
        TDs.Off.Rank.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Offense",type=side,basis=type)['TD',]$Rank)
        TDs.Def.Rank.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Defense",type=side,basis=type)['TD',]$Rank)
        TOs.Off.Rank.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Offense",type=side,basis=type)['Turnovers',]$Rank)
        TOs.Def.Rank.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Defense",type=side,basis=type)['Turnovers',]$Rank)
        
        Data_PCA <- data.frame(-Yds.Off.Rank.PP,-Yds.Def.Rank.PP,-TDs.Off.Rank.PP,-TDs.Def.Rank.PP,-TOs.Off.Rank.PP,-TOs.Def.Rank.PP)
        colnames(Data_PCA) <- c("Off.Yds", "Def.Yds","Off.TD","Def.TD","Off.TO","Def.TO")
      }
    }
    
    if (rank == F){
      if (type == "Per Game"){
        Yds.Off <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Offense",type=side,basis=type)['Yards',]$Value)
        Yds.Def <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Defense",type=side,basis=type)['Yards',]$Value)
        TDs.Off <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Offense",type=side,basis=type)['TD',]$Value)
        TDs.Def <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Defense",type=side,basis=type)['TD',]$Value)
        TOs.Off <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Offense",type=side,basis=type)['Turnovers',]$Value)
        TOs.Def <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Defense",type=side,basis=type)['Turnovers',]$Value)
        
        Data_PCA <- data.frame(Yds.Off,-Yds.Def,TDs.Off,-TDs.Def,-TOs.Off,TOs.Def)
        colnames(Data_PCA) <- c("Off.Yds", "Def.Yds","Off.TD","Def.TD","Off.TO","Def.TO")
      }
      if (type == "Per Play"){
        Yds.Off.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Offense",type=side,basis=type)['Yards',]$Value)
        Yds.Def.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Defense",type=side,basis=type)['Yards',]$Value)
        TDs.Off.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Offense",type=side,basis=type)['TD',]$Value)
        TDs.Def.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Defense",type=side,basis=type)['TD',]$Value) 
        TOs.Off.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Offense",type=side,basis=type)['Turnovers',]$Value)
        TOs.Def.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side="Defense",type=side,basis=type)['Turnovers',]$Value)
        
        Data_PCA <- data.frame(Yds.Off.PP,-Yds.Def.PP,TDs.Off.PP,-TDs.Def.PP,-TOs.Off.PP,TOs.Def.PP)
        colnames(Data_PCA) <- c("Off.Yds", "Def.Yds","Off.TD","Def.TD","Off.TO","Def.TO")
      }
    }
  }
  
  if (side != "Overall"){
    if (rank == T){
      if (type == "Per Game"){
        Pass.Yards.Rank <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Passing",basis=type)['Yards',]$Rank)
        Rush.Yards.Rank <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Rushing",basis=type)['Yards',]$Rank)
        Pass.TDs.Rank <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Passing",basis=type)['TD',]$Rank)
        Rush.TDs.Rank <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Rushing",basis=type)['TD',]$Rank)
        INT.Rank <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Passing",basis=type)['INT',]$Rank)
        FUM.Rank <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Rushing",basis=type)['FUM',]$Rank)
        
        if (side == "Offense") Data_PCA <- data.frame(-Pass.Yards.Rank,-Rush.Yards.Rank,-Pass.TDs.Rank,-Rush.TDs.Rank,-INT.Rank,-FUM.Rank)
        if (side == "Defense") Data_PCA <- data.frame(-Pass.Yards.Rank,-Rush.Yards.Rank,-Pass.TDs.Rank,-Rush.TDs.Rank,-INT.Rank,-FUM.Rank)
        
        colnames(Data_PCA) <- c("Pass.Yds", "Rush.Yds","Pass.TD","Rush.TD","Int","Fumb")
      }
      
      if (type == "Per Play"){
        Pass.Yards.Rank.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Passing",basis=type)['Yards',]$Rank)
        Rush.Yards.Rank.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Rushing",basis=type)['Yards',]$Rank)
        Pass.TDs.Rank.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Passing",basis=type)['TD',]$Rank)
        Rush.TDs.Rank.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Rushing",basis=type)['TD',]$Rank)
        INT.Rank.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Passing",basis=type)['INT',]$Rank)
        FUM.Rank.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Rushing",basis=type)['FUM',]$Rank)
        
        if (side == "Offense") Data_PCA <- data.frame(-Pass.Yards.Rank.PP,-Rush.Yards.Rank.PP,-Pass.TDs.Rank.PP,-Rush.TDs.Rank.PP,-INT.Rank.PP,-FUM.Rank.PP)
        if (side == "Defense") Data_PCA <- data.frame(-Pass.Yards.Rank.PP,-Rush.Yards.Rank.PP,-Pass.TDs.Rank.PP,-Rush.TDs.Rank.PP,-INT.Rank.PP,-FUM.Rank.PP)
        
        colnames(Data_PCA) <- c("Pass.Yds", "Rush.Yds","Pass.TD","Rush.TD","Int","Fumb")
      }
    }
    
    if (rank == F){
      if (type == "Per Game"){
        Yds.Pass <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Passing",basis=type)['Yards',]$Value)
        Yds.Rush <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Rushing",basis=type)['Yards',]$Value)
        TDs.Pass <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Passing",basis=type)['TD',]$Value)
        TDs.Rush <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Rushing",basis=type)['TD',]$Value)
        INTs.Pass <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Passing",basis=type)['INT',]$Value) 
        FUMs.Rush <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Rushing",basis=type)['FUM',]$Value) 
        
        if (side=="Offense") Data_PCA <- data.frame(Yds.Pass,Yds.Rush,TDs.Pass,TDs.Rush,-INTs.Pass,-FUMs.Rush)
        if (side == "Defense") Data_PCA <- data.frame(-Yds.Pass,-Yds.Rush,-TDs.Pass,-TDs.Rush,INTs.Pass,FUMs.Rush)
        
        colnames(Data_PCA) <- c("Pass.Yds", "Rush.Yds","Pass.TD","Rush.TD","Int","Fumb")
      }
      if (type == "Per Play"){
        Yds.Pass.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Passing",basis=type)['Yards',]$Value)
        Yds.Rush.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Rushing",basis=type)['Yards',]$Value)
        TDs.Pass.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Passing",basis=type)['TD',]$Value)
        TDs.Rush.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Rushing",basis=type)['TD',]$Value)
        INTs.Pass.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Passing",basis=type)['INT',]$Value) 
        FUMs.Rush.PP <- sapply(Scores$Opponent,function(x) TeamValAndRank(x,side=side,type="Rushing",basis=type)['FUM',]$Value) 
        
        if (side=="Offense") Data_PCA <- data.frame(Yds.Pass.PP,Yds.Rush.PP,TDs.Pass.PP,TDs.Rush.PP,-INTs.Pass.PP,-FUMs.Rush.PP)
        if (side == "Defense") Data_PCA <- data.frame(-Yds.Pass.PP,-Yds.Rush.PP,-TDs.Pass.PP,-TDs.Rush.PP,INTs.Pass.PP,FUMs.Rush.PP)
        
        colnames(Data_PCA) <- c("Pass.Yds", "Rush.Yds","Pass.TD","Rush.TD","Int","Fumb")
      }
    }
  }
  print(Data_PCA)
  obs.names <- paste(Scores$X,Scores$X.1,Scores$Pts,Scores$Opp)
  rownames(Data_PCA) = obs.names
  # obs.names[4] <- paste(obs.names[4]," ")
  # rownames(Data_PCA)=obs.names
  # Data_Offense_PCA <- Data_Offense
  #  colnames(Data_Offense_PCA)[c(25,26,29,30)] <- c("Pass.Avg", "Rush.Avg","Int.Avg","Fumb.Avg")
  Actual_PCA <- prcomp(~.,Data_PCA,scale=T)
  
  if (dim.pca == "2D") biplot(Actual_PCA,
                              xlim=ifelse(x.lims == 0,c(-0.7,0.7),x.lims),
                              ylims=ifelse(y.lims == 0,c(-0.7,0.7),y.lims),
                              xlab="",
                              ylab="",
                              main=paste(Scores$School[1]," vs Opposing ",side," in ",str_sub(Scores_vs_Opp$Date[1],-4)))
  if (dim.pca == "3D") pca3d(Actual_PCA,biplot=T,show.labels=T, show.centroids = T,
                             components=components)
  
  return(Actual_PCA)
  # text(x,y,labels=Scores$Outcome)
}

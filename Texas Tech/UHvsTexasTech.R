### USE QUITE SOME 5-WIDE RECEIVER SETS !!!


####
## IN THE LAST GAME - A TD THROW TO CANTRELL ON 3rd AND 18 !!! about a 50yd TD
## AT THAT MOMENT - 11/14 FOR 3 TD, 160 YDs
## TEXAS FIRST THREE SCORING DRIVES: 3m50s, 50s, 1m30s

## SENIOR SHIMENEC LOOKS TO ENJOY HIS FIRST YEAR AS A STARTER after HAVING BACKED UP
## ALL THE AMAZING QBs

## CUTEE and CANTRELL ARE IMMENSE PLAYMAKERS

## BIG FUMBLE BY TEXAS TECH after being WITHIN INCHES of a TD - BY NISBY!!!!!!!
## (tried to reach out for a TD)


## GIANT CONNECTION with COUTEE, COUTEE - CLEAR-CUT NUMBER ONE RECEIVER

##  BUT CANTRELL - PRETTY CLEAR SECOND OPTION


## TD by the TIGHT END  MASON(?) REED, 
## who BARELLED THROUGH THE DEFENSIVE PLAYERS AFTER CATCH

## A SWEET TD to CANTRELL



##############################
############################

#library(gridExtra)
## USE GRID.TABLE FOR NICE TABLES

source("/home/usdandres/Documents/Study_stuff/Sports Research/Football/University of Houston Cougars/Football.Functions.R")

year <- 2016
league.data <- load.league(year)
TexasTech.data <- load.team("vs Texas Tech",year)

##############
### OFFENSE
##############

## OVERALL:

### Per GAME: Clear-cut #1 in Plays run (86.8 a game), 1st downs obtained (30), YARDS (566) and TDs (5.7)
##            Also top 5 in points, 
##            and top 50 in turnovers (so with such a huge volume of plays, managed to limit those)
##            In the bottom-10 with 7.8 penalties a game

## Per PLAY: naturally ranks will go down, as they RAN THE BIGGEST # OF PLAYS
##           still Top-20 in TDs and Turnovers per play, and top-30 in points scored and yars per play
##           ranked 80 in penalties

TeamValAndRank("Texas Tech",side="Offense",type="Overall",basis="Per Game")
TeamValAndRank("Texas Tech",side="Offense",type="Overall",basis="Per Play")


## PASSING
## 
##  PER GAME: Clear #1 in plays run (54.4), yards (463), TDs (3.9) and 1st downs (20)
##            #10 in COMPLETION %
##            #70 in INTs
##

##  PER PLAY: top-20 in yards, TDs and INT per pass attempt, #1 in 1st downs obtained per play

TeamValAndRank("Texas Tech",side="Offense",type="Passing",basis="Per Game")
TeamValAndRank("Texas Tech",side="Offense",type="Passing",basis="Per Play")


## RUSHING
##
##  PER GAME: bottom-15 in plays (32.4) and yards (103.6)
##            # 26 in fumbles (0.5), mid-of-the pack in TDs (1.8)

##  PER PLAY: bottom 5 in yards per play (3.2)
##            top-30 in scores per play though 
##            bottom 3 in 1st downs obtained BY RUN per attempt


TeamValAndRank("Texas Tech",side="Offense",type="Rushing",basis="Per Game")
TeamValAndRank("Texas Tech",side="Offense",type="Rushing",basis="Per Play")



################
### DEFENSE ####
################

## OVERALL:
## 
##    Per GAME: most of numbers are due to offense tempo, defense had to play a ton of snaps too
##              bottom-10 with 78.6 plays a game
##              bottom-15 with 1.1 turnovers forced
##              BOTTOM-TWO in 1st downs (27) and TDs (5.4) given up
##              BOTTOM-ONE in POINTS (43.5) and YARDS (554.3) GIVEN UP
##              
##
##    Per PLAY: bottom-2 in points, 1st downs, yards and TDs allowed 
##              bottom-15 in turnovers
##              mid-of-the-pack in penalties

TeamValAndRank("Texas Tech",side="Defense",type="Overall",basis="Per Game")
TeamValAndRank("Texas Tech",side="Defense",type="Overall",basis="Per Play")


## PASSING:
##
##  Per GAME: bottom-4 in yards (316), 1st downs (14.3) given up and INTs forced (0.4)
##            bottom-10 in TDs allowed
##            97th in 61.2% completions
##
##  Per PLAY: bottom-10 in yards given up (8.5) and INTs forced
##            97th in TDs

TeamValAndRank("Texas Tech",side="Defense",type="Passing",basis="Per Game")
TeamValAndRank("Texas Tech",side="Defense",type="Passing",basis="Per Play")

## RUSHING:
##
##  Per GAME: BOTTOM-2 in TDs allowed (3.1)
##            bottom-20 in Yards (239) and 1st downs (11.5) given up
##            mid-of-the-pack in fumbles forced (0.7)
##
##  Per PLAY: BOTTOM-2 in TDs allowed
##            BOTTOM-6 in YARDS ALLOWED (5.7)
##            mid-of the pack in fumbles forced
TeamValAndRank("Texas Tech",side="Defense",type="Rushing",basis="Per Game")
TeamValAndRank("Texas Tech",side="Defense",type="Rushing",basis="Per Play")

######
## WILL NEED ARIZONA STATE STUFF IN HERE
######

league.data <- load.league(2017)

## OFFENSE
##
##  OVERALL
##
##    Per GAME: lower quartile in plays run (66.5), points scored (28.5), yards (371) and 1st downs obtained (18)
##              mid-of-the-pack in TDs (3.5)
##              TOP-25 in TURNOVERS (0.5)
##              TOP-6 in PENALTIES (3)
##
##    Per PLAY: mid-of-pack in points, TDs scored
##              lower quartile in 1st downs and yards (5.6)
##              top-20 in TOs and penalties

TeamValAndRank("Arizona State",side="Offense",type="Overall",basis="Per Game")
TeamValAndRank("Arizona State",side="Offense",type="Overall",basis="Per Play")


##  PASSING:
##      
##    Per GAME:  TOP-25 with 70% COMP PCT, 309.5 yards and 0 INTS !!!!
##               Top-40 with 12 first downs by pass
##               mid-of-pack in plays and TDs
##
##    Per PLAY: TOP-25 in yards (9.8), INTs and 1st downs
##              mid-of-pack in TDs

TeamValAndRank("Arizona State",side="Offense",type="Passing",basis="Per Game")
TeamValAndRank("Arizona State",side="Offense",type="Passing",basis="Per Play")

##  RUSHING:
##
##   Per GAME: lower-quartile with 35 plays, 1.5 TDs
##             bottom-10 in YARDS (62) and FIRST DOWNS (4.5)
##             middle with 0.5 fumbles
##
##   Per PLAY: BOTTOM-2 WITH 1.77 YARDS PER PLAY
##             bottom-15 with 0.25
##             middle with TDs and FUM

TeamValAndRank("Arizona State",side="Offense",type="Rushing",basis="Per Game")
TeamValAndRank("Arizona State",side="Offense",type="Rushing",basis="Per Play")


## DEFENSE:
##
## OVERALL:
##
##  Per GAME:  lower-quart with 74 plays, 30.5 points allowed, 3.5 TDs
##             outside top-100 with 451.5 yards and 9 penalties
##             99th with 1 turnover forced
##
##  Per PLAY:  lower quart in points, 1st downs and TDs
##             outside top-100 in penalties and yards allowed (6.1)

TeamValAndRank("Arizona State",side="Defense",type="Overall",basis="Per Game")
TeamValAndRank("Arizona State",side="Defense",type="Overall",basis="Per Play")

## PASSING:
##
##  Per GAME: lower-quart in yards (235.5) and 1st downs (10.5) allowed
##            around 100th in comp pct (64.5) and TDs (2.0) allowed
##            middle in INTs (1.0)
##
##  Per PLAY: middle in yards (6.2) and INTs per play
##            lower quart in TDs 
TeamValAndRank("Arizona State",side="Defense",type="Passing",basis="Per Game")
TeamValAndRank("Arizona State",side="Defense",type="Passing",basis="Per Play")

## RUSHING:
##
##  Per GAME: lower-quart in TDs (1.5) and 1st downs (9.5)  given up
##            108th in yards (216)
##            BOTTOM-6 WITH 0 FUMBLES FORCED!!!!
##
##  Per PLAY: bottom-10 in YARDS (6.0) and FUMBLES (0) allowed per play
##            lower-quart in TDs
TeamValAndRank("Arizona State",side="Defense",type="Rushing",basis="Per Game")
TeamValAndRank("Arizona State",side="Defense",type="Rushing",basis="Per Play")

######



### WRs FROM 2016, that ALSO PLAY IN 2017: 
###  - CANTRELL, 
###  - COUTEE

######################
#### QBs #############
######################

##################################################
### PATRICK MAHOMES - GOT DRAFTED BY KC CHIEFS ###
##################################################

league.data <- load.league(2016)

## 6-3 JUNIOR in 2016

## LED TEAM IN RUSHING ATTEMPTS (131) AND TDs (12)


### PER GAME:
###
###   NUMBER 1 with 421 PASSING YARDS PER GAME
###   NUMBER 2 with 50 attempts, 32.3 completions and 3.4 passing TDs a game
###   NUMBER 4 with 1 RUSHING TD PER GAME
###   NUMBER 12 with 65.7% cmpt and QBR of 157
###   TOP-25 with 10.9 RUSH ATTEMPTS/GAME (not scoring with feet much though, only )
###   TOP-40 with 23.75 RUSHING YARDS/GAME
###   middle in INTs (0.83)
PlayerValAndRank("Patrick Mahomes",basis="Per Game")

###  PER PLAY:
###
###   TOP-5 IN RUSH TDs
###   TOP-20 with 8.5 yds/a and in INTs/a
###   TOP-25 in PASS TDs/a
###   mid-of-pack with 2.2 yds/rush

PlayerValAndRank("Patrick Mahomes",basis="Per Play")


############################
############################

### NIC SHIMONEK - 6'3 SENIOR

league.data <- load.league(2017)

## ONLY PLAYED ONE GAME SO FAR THOUGH... WAITING ON THE ARIZONA STATE GAME

##  PER GAME:
##    NUMBER 
PlayerValAndRank("Nic Shimonek",basis="Per Game")
PlayerValAndRank("Nic Shimonek",basis="Per Play")

############################



##########################
### RUNNING BACKS ########
##########################

############
### 2016 ###
############

league.data <- load.league(2016)
TexasTech.data <- load.team("vs Texas Tech",2016)
TexasTech.data$RushReceive


####
## DA'LEON WARD - 5'10, 180lbs SOPHOMORE
##   WILL MISS AN UNKNOWN AMOUNT OF TIME due to ACADEMIC ISSUES
##   
#######

## rushed 103 times for 428 yards (4.2 ypr) and 3 TDs; CATCHING 18 PASSES for 131 yards, 7.3y/catch
## PER GAME: top-40 with 2.25 receptions
##           top-60 with 16.375 RECEIVING yds/game
## 
## PER PLAY: everything is mid of the pack or worse
##

PlayerValAndRank("Da'Leon Ward",pos="RB",basis="Per Game")
PlayerValAndRank("Da'Leon Ward",pos="RB",basis="Per Play")

sum(TexasTech.data$RushReceive$Yds,na.rm=T)

##############
## DEMARCUS FELTON - 5'7, 205 lbs, JUNIOR
##############

## Demarcus Felton rushed 64 times for 354 yards (5.5ypc) and 3 TDs;
##                 caught 7 passes for 31 yds;
##

## THE MOST EFFICIENT RUSHER ON THE TEAM - 5.5 ypc, and THREE SCORES TOO


#############
###  2017 ###
#############

league.data <- load.league(2017)
TexasTech.data <- load.team("vs Texas Tech",2017)
TexasTech.data$RushReceive

## JUSTIN STOCKTON: 20 rushes for 94 (4.7) and  2 TDs,
##                   6  catches for  57 (9.5) 

## Scored a RUSHING TD in EACH GAME, RANKS HIM in TOP-70 WITH 1 PER GAME
## BELOW-AVG in ATTEMPTS and YARDS per game
## TOP-20 WITH 3 REC/GAME and TOP-25 WITH 28.5 Y/C

PlayerValAndRank("Justin Stockton", pos="RB",basis="Per Game")
PlayerValAndRank("Justin Stockton", pos="RB",basis="Per Play")


### DESMON NISBY - 6'1 JUNIOR
### 
###   Desmond Nisby\\desmond-nisby-1  15 rushes for 102 yds (6.8),  0 scores
###                                   no catches
### 
###  Ranks in top-50 (out of 303 RBs) with his 6.8 yards per rush 
PlayerValAndRank("Desmond Nisby", pos="RB",basis="Per Game")
PlayerValAndRank("Desmond Nisby", pos="RB",basis="Per Play")

### The other 6 ball carriers (outside QB and WRs) only had 24 rushes for 100 yards and one score combined


#############
### JUSTIN STOCKTON - 5'10, 205 lbs, SENIOR
##############

## rushed 53 times for 154 (just 2.9 ypc) and 1TD, 
## BUT CAUGHT 21 PASSES for 220 YARDS AND 2 SCORES (10.5), 
##  which definitely puts him around the top of pass-catching RBs (of "pass-catching backs") in the nation



#################
#####  2017 #####
#################

league.data <- load.league(2017)
TexasTech.data <- load.team("vs Texas Tech",2017)
TexasTech.data$RushReceive



####################################
#### WIDE RECEIVERS & TIGHT ENDS ###
####################################


### WRs FROM 2016, that ALSO PLAY IN 2017: 
###  - CANTRELL, 
###  - COUTEE

league.data <- load.league(2016)
TexasTech.data <- load.team("vs Texas Tech",2016)

TexasTech.data$RushReceive

## Derrick Willies\\derrick-willies-1  NA  NA   NA NA  18   288  16.0    2    18   288  16.0    2
## 19 19     Reginald Davis\\reginald-davis-1  NA  NA   NA NA  15   247  16.5    2    15   247  16.5    2
## Ian Sadler\\ian-sadler-1  NA  NA   NA NA  24   363  15.1    0    24   363  15.1    0
##16 16 Devin Lauderdale\\devin-lauderdale-1  NA  NA   NA NA  31   307   9.9    2    31   307   9.9    2
##7   7           Keke Coutee\\keke-coutee-1   3   2  0.7  1  55   890  16.2    7    58   892  15.4    8
##15 15     Dylan Cantrell\\dylan-cantrell-1  NA  NA   NA NA  58   675  11.6    8    58   675  11.6    8
##10 10     Cameron Batson\\cameron-batson-1   2   4  2.0  0  60   644  10.7    8    62   648  10.5    8
## 14 14     Jonathan Giles\\jonathan-giles-1  NA  NA   NA NA  69  1158  16.8   13    69  1158  16.8   13

#####
## JONATHAN GILES - 6'0; 
##    69 catches for 1158 (16.8 ypc) and 13 TDs 
##    ===> TRANSFERRED TO LSU !!!
#####

## PER GAME: #7 in TD receptions (with 1.2 per game), 
##           #12 with 105 rec yds 
##           # 24 with 6.3 rec a game
##   ALL OF THAT IS OUT OF 462 RECEIVERS !!!!!!!!!!!!!!!!!
##
## PER PLAY: #53 with 16.8 yds per catch,
##           #30 with TDs per catch

PlayerValAndRank("Jonathan Giles",pos="WR",basis="Per Game")
PlayerValAndRank("Jonathan Giles",pos="WR",basis="Per Play")



########
###  CAMERON BATSON - 5'9 SENIOR , COMING BACK FOR 2017
########

## Caught 60 passes for 644 yds (10.7 ypc) and 8 TDs

## Around Top-70 with 5 rec/game and 0.66 TD rec a game
## Nothing impressive in PER PLAY stats (all outside top-100)

PlayerValAndRank("Cameron Batson",pos="WR",basis="Per Game")
PlayerValAndRank("Cameron Batson",pos="WR",basis="Per Play")


#############
### DYLAN CANTRELL - 5th YEAR SENIOR, COMING BACK FOR 2017
###   caught 58 balls for 675yds (11.6 ypc) and 8 TDs 
############

## RANKS IN TOP-40 WITH:  5.8 REC/GAME, 0.8 TD REC/GAME;
##          TOP-80 WITH 67.5 REC YDS/GAME

##  RANKS AROUND TOP-100 IN TDs per RECEPTION

PlayerValAndRank("Dylan Cantrell",pos="WR",basis="Per Game")
PlayerValAndRank("Dylan Cantrell",pos="WR",basis="Per Play")


#######
### KEKE COUTEE - 5'11 JUNIOR, COMING BACK FOR 2017
###   caught 55 passes for   890 yards (16.2 ypc) and 7 TD
#######

## PER GAME:
##  TOP-70 in both rec yards per game (74) and TDs per game (0.6)
##  AROUND TOP-100 WITH 4.6 REC/GAME
##  ALSO SCORED A RUSH TD

## PER PLAY: 
##  top-75 with 16.2 yds per catch
##  and top-25% of WRs with TDs per catch

PlayerValAndRank("Keke Coutee",pos="WR",basis="Per Game")
PlayerValAndRank("Keke Coutee",pos="WR",basis="Per Play")


#######
## DEVIN LAUDERDALE - GRADUATED, 5'10;
## 31 catches for 307 yds (9.9 ypc) and 2 TDs
#######

## mid-pack or lower-half in everything
PlayerValAndRank("Devin Lauderdale",pos="WR",basis="Per Game")
PlayerValAndRank("Devin Lauderdale",pos="WR",basis="Per Play")


########
### IAN SADLER - 5'11 SENIOR
###  Caught 24 balls for  363 (15.1 ypc), 0 TDs 
########

## close to top-100 with his 15.1 YARD PER CATCH
PlayerValAndRank("Ian Sadler",pos="WR",basis="Per Game")
PlayerValAndRank("Ian Sadler",pos="WR",basis="Per Play")

######
## DERRICK WILLIES - 6'4 SENIOR, BACK FOR 2017 !!
## Caught 18 passes for 288 yds (16.0 ypc) and 2 TDs 
#########

#####
## REGINALD DAVIS - 6'0, GRADUATED;
##   15 catches for 247 (16.5) and 2 TDs  
######






##################
## WRs IN 2017 ###
##################


league.data <- load.league(2017)
TexasTech.data <- load.team("vs Texas Tech",2017)

TexasTech.data$RushReceive


## 12 12  Keke Coutee - 17 catches for  285 (16.8), 3 TDs   

## RANKS TOP-10 OUT OF 482 WRs in REC, YDS and TDs per game
##  Ranks IN THE BOTTOM OF TOP-100 if BREAK IT DOWN BY PLAYS
PlayerValAndRank("Keke Coutee",pos="WR",basis="Per Game")
PlayerValAndRank("Keke Coutee",pos="WR",basis="Per Play")


###
###
### 10 10   Dylan Cantrell 12 catches for  195 yds (16.3),  2 TDs

##  TOP-50 IN REC, YDS and TDs per game
##  Just OUTSIDE TOP-100 PER ATTEMPT
##  Has ONE RUSHING TD on ONE ATTEMPT
PlayerValAndRank("Dylan Cantrell",pos="WR",basis="Per Game")
PlayerValAndRank("Dylan Cantrell",pos="WR",basis="Per Play")



### Cameron Batson - 12 catches for  112 (9.3),    1 TD
###
### The most 'quiet' so far with 12 catches .. 1 TD

PlayerValAndRank("Cameron Batson",pos="WR",basis="Per Game")
PlayerValAndRank("Cameron Batson",pos="WR",basis="Per Play")



## 11 Derrick Willies - 8 catches for  182 (22.8),  2 TDs
## Only 4 rec per game BUT 22.8 ypc and 2 TDs already, ranking him in top-40 on per-catch basis

PlayerValAndRank("Derrick Willies",pos="WR",basis="Per Game")
PlayerValAndRank("Derrick Willies",pos="WR",basis="Per Play")

## 11 11 Derrick Willies - 8 catches for  182 (22.8),  2 TDs
## 12 12  Keke Coutee - 17 catches for  285 (16.8), 3 TDs   
## 13 13   Cameron Batson - 12 catches for  112 (9.3),    1 TD


#####################################
####  DEFENSIVE PLAYERS #############
#####################################


league.data <- load.league(2016)
head(league.data)

TexasTech.data <- load.team("vs Texas Tech",2016)
TexasTech.data$DefFumb

### TACKLERS:
### 1   1               Jordyn Brooks\\jordyn-brooks-1   61  24  85  5.0 1.0  NA  NA  NA NA  3 NA    NA   NA  1
##2   2        Jah'Shawn Johnson\\jahshawn-johnson-1   57  19  76  3.0 1.0   2  15 7.5  0  6  1    NA   NA  3
##3   3               Malik Jenkins\\malik-jenkins-1   53  22  75  2.5 0.0  NA  NA  NA NA  2  1    NA   NA  1
##4   4               Justis Nelson\\justis-nelson-1   46  13  59  0.5 0.0  NA  NA  NA NA 12 NA    NA   NA NA
## 5   5                     Luke Stice\\luke-stice-1   37  14  51  2.0 1.0  NA  NA  NA NA NA  1    NA   NA  0
##6   6                 Kisean Allen\\kisean-allen-1   34   9  43  0.0 0.0  NA  NA  NA NA NA NA    NA   NA NA

### SACKS:
###   KRIS WILLIAMS - TEAM HIGH 4.5 BY


### SECONDARY:
###  Just 4 interceptions, 
###      JAN'SHAWN JOHNSON-   2 of those BY JAN'SHAWN JOHNSON; WHO ALSO HAD 3 FORCED FUMBLES !!! AND 6 PASSES DEFENDED AS WELL
###      JUSTIS NELSON - 12 PASSES DEFENDED
### 
###   


league.data <- load.league(2017)
head(league.data)

TexasTech.data <- load.team("vs Texas Tech",2017)
TexasTech.data$DefFumb


### TACKLERS:
### 1   1 Jah'Shawn Johnson\\jahshawn-johnson-1    5   2   7  0.5  0  NA  NA  NA NA  1 NA    NA   NA  1
### 2   2        Jordyn Brooks\\jordyn-brooks-1    4   3   7  0.0  0  NA  NA  NA NA NA NA    NA   NA NA
### 3   3          Dakota Allen\\dakota-allen-1    2   4   6  0.5  0  NA  NA  NA NA NA NA    NA   NA NA
#   4   4  Brayden Stringer\\brayden-stringer-1    4   1   5  0.0  0  NA  NA  NA NA NA NA    NA   NA NA
#   5   5  Octavious Morgan\\octavious-morgan-1    3   1   4  0.0  0  NA  NA  NA NA  1 NA    NA   NA NA

## JAHSHAWN JOHNSON - ALREADY ONE FORCED FUMBLE

### SECONDARY:
###
###  INTERCEPTIONS:
###    10 10            Tony Jones\\tony-jones-169    1   1   2  1.0  1   1   2   2  0  0 NA    NA   NA NA
##     11 11          Willie Sykes\\willie-sykes-1    0   2   2  0.0  0   1  33  33  1  1 NA    NA   NA NA
#####################
#### 2017 ###########
####################



######################################
### 2016 SCHEDULE AND STUFF ##########
######################################

source("/home/usdandres/Documents/Study_stuff/Sports Research/Football/University of Houston Cougars/Football.Functions.R")

year <- 2016
league.data <- load.league(year)
TexasTech.data <- load.team("vs Texas Tech",year)

RiceVsOpp <- TeamVsOpponents("vs Texas Tech",year)
Scores_vs_Opp <- RiceVsOpp$Scores_vs_Opp

## need to exclude some games?
# exclude <- c(2,7,5,9,11)         # only with Dawkins and @ home
#exclude <- c(2,7,11)                # only with Dawkins
exclude <- NULL

if (!is.null(exclude)){
  Scores_vs_Opp <- Scores_vs_Opp[-exclude,]
  Opp_Offense <- Opp_Offense[-exclude,]
  Opp_Defense <- Opp_Defense[-exclude,]
  Opp_Overall <- Opp_Overall[-exclude,]
}

###############################################################
### SIMPLE 2D PLOTS OF OPPONENT'S OFFENSE/DEFENSE  ############
###############################################################

## Vast majority of games vs RUSH-DOMINANT OFFENSE (Upper Triangle) => LOSSES (most of them at home too)
## Also mostly losses against BALANCED TEAMS
## Mostly WINS against PASSING-DOMINANT TEAMS

## => more chances to lose against them if trying to OUTGUN them
## Gotta control the ball and run on them

opp.plot(Scores_vs_Opp,side="Offense",type="Balance",rank=T,
         x.lims=c(-2.75,2),y.lims=c(-2.75,2),my.cex=0.8)

opp.plot(Scores_vs_Opp,side="Overall",type="Per Game",rank=T,my.cex=0.8)
        # x.lims=c(-2.75,2),y.lims=c(-2.75,2),my.cex=0.8)
opp.plot(Scores_vs_Opp,side="Overall",type="Per Play",rank=T,my.cex=0.8)


## 1-4 record vs top-40 rushing offenses (total yardage)
opp.plot(Scores_vs_Opp,side="Offense",type="Per Game",rank=T,
         x.lims=c(-10,125),y.lims=c(0,125),my.cex=0.85)
## 1-4 vs those same opponents, with 220+ avg rushing
opp.plot(Scores_vs_Opp,side="Offense",type="Per Game",rank=F)

## ??
opp.plot(Scores_vs_Opp,side="Offense",type="Per Play",rank=T)
## ??
opp.plot(Scores_vs_Opp,side="Offense",type="Per Play",rank=F)



##
opp.plot(Scores_vs_Opp,side="Defense",type="Per Game",rank=T,
         x.lims=c(50,135),y.lims=c(10,120))
opp.plot(Scores_vs_Opp,side="Defense",type="Per Game",rank=F)

## 
opp.plot(Scores_vs_Opp,side="Defense",type="Per Play",rank=T,
         x.lims=c(40,130),y.lims=c(20,115))
opp.plot(Scores_vs_Opp,side="Defense",type="Per Play",rank=F)


## types = "Per Game", "Per Play", "Balance
## 

######################
#####   PCAs  ########
######################

league.data <- load.league(as.numeric(str_sub(Scores_vs_Opp$Date[1],-4)))
pca.obj <- opp.PCA(Scores_vs_Opp,side="Offense",type="Per Game",rank=T,
                   dim.pca = "3D",
                   components=c(1:3))
summary(pca.obj)

TeamValAndRank("North Texas",side="Defense",type="Rushing",basis="Per Play")
TeamValAndRank("UTEP",side="Defense",type="Rushing",basis="Per Play")
TeamValAndRank("Florida Atlantic",side="Defense",type="Rushing",basis="Per Play")

TeamValAndRank("Louisiana Tech",side="Offense",type="Passing",basis="Per Game")
TeamValAndRank("Louisiana Tech",side="Offense",type="Rushing",basis="Per Game")

TeamValAndRank("Kansas",side="Offense",type="Passing",basis="Per Game")
TeamValAndRank("Kansas",side="Offense",type="Rushing",basis="Per Game")

TeamValAndRank("Texas Christian",side="Offense",type="Passing",basis="Per Game")
TeamValAndRank("Texas Christian",side="Offense",type="Rushing",basis="Per Game")


#################
#################
## PLOT OF RICE POINT DIFFERENTIALS AT HOME AND ON THE ROAD
################
################


neutral <- ifelse(Scores_vs_Opp$X == "N",T,F)
home <- ifelse(Scores_vs_Opp$X[neutral == F] != "@","Home","Away")
#result <- ifelse(grepl("W",Data_Overall$Outcome[neutral == F]),"W","L")

wins <- Scores_vs_Opp$OppWins[neutral == F]
pointdiff <- Scores_vs_Opp$PointDiff[neutral == F]
wins[home=="Home"]
pointdiff[home=="Home"]

Scores_vs_Opp$Outcome <- paste(Scores_vs_Opp$X,
                               Scores_vs_Opp$X.1,
                               Scores_vs_Opp$Pts,
                               Scores_vs_Opp$Opp)


plot(wins,pointdiff,type="n",
     xlab="Wins by Opponent in 2016", 
     ylab="Point Differential",
     main="Rice Point Differential vs Opponents in 2016",
     xlim=c(1,11.5))

o <- order(wins[home=="Home"])
points(wins[home=="Home"][o],pointdiff[home=="Home"][o],type='l',col=2)

#home.labels <- rownames(Data_Offense)[neutral==F][home=="Home"][o]
home.labels <- Scores_vs_Opp$Outcome[neutral==F][home=="Home"][o]
#home.labels[3] <- paste("\n",home.labels[3])
text(wins[home=="Home"][o],pointdiff[home=="Home"][o],
     labels=home.labels)

o <- order(wins[home=="Away"])
points(wins[home=="Away"][o],pointdiff[home=="Away"][o],type='l',lty=2,col=4)

away.labels <- Scores_vs_Opp$Outcome[neutral==F][home=="Away"][o]
text(wins[home=="Away"][o],pointdiff[home=="Away"][o],
     labels=away.labels)

legend("topright", legend=c("Home", "Away"),
       col=c("red", "blue"), lty=c(1:2), cex=0.8)


########################################
## PLOTTING OFFENSE and DEFENSE on same plot
########################################

#########
## Rice played competitive games ONLY VS TEAMS WITH TERRIBLE TOTAL OFFENSE and DEFENSE
#########

####################
###  VS OFFENSES: TOTAL PASSING and TOTAL RUSHING OFFENSE on the same plot
####################
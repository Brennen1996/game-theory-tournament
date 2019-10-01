library(R6)

Agent <- R6Class("Agent",
                 
                 public = list(
                   #The basic attributes needed to function well in the tournament
                   bid = NULL,
                   book = NULL,
                   greeting = "It's over Anakin! I have the high ground!",
                   id = NULL,
                   opponent_id = NULL,
                   round = NULL,
                   response = NULL,
                   
                   #To be able to use the scores and moves of myself and my opponent across different functions, they are saved as attributes of the agent class.
                   opponent_score = NULL,
                   my_score = NULL,
                   
                   all_opponents_actions = NULL,
                   all_my_actions = NULL,
                   my_actions_vs_opponent = NULL,
                   
                   
                   set_book = function(book=NA){
                     self$book <- book
                   },
                   
                   set_id = function(id=NA) {
                     self$id = id
                   },
                   
                   set_opponent_id = function(opponent_id=NA) {
                     self$opponent_id = opponent_id
                   },
                   
                   set_response = function(response=NA) {
                     self$response = response
                   },
                   
                   set_round = function(round=NA) {
                     self$round = round
                   },
                   #Setters are made to update the score that ha been achieved so far by me and my opponent
                   set_opponent_score = function(opponent_score=NA) {
                     self$opponent_score = opponent_score
                   },
                   
                   set_my_score = function(my_score = NA) {
                     self$my_score = my_score
                   },
                   
                   
                   #Setters are made to update the opponents moves and my moves as well
                   set_all_opponents_actions = function(all_opponents_actions=NA) {
                     self$all_opponents_actions = all_opponents_actions
                   },
                   
                   set_all_my_actions = function(all_my_actions = NA){
                     self$all_my_actions = all_my_actions
                   },
                   
                   set_my_actions_vs_opponent = function(my_actions_vs_opponent=NA) {
                     self$my_actions_vs_opponent = my_actions_vs_opponent
                   },
                   
                   
                   get_bid = function() {
                     
                     #The book is processed and the interesting variables are saved. If not enough rounds have been played, this book will not be used.
                     self$process_book()
                     
                     #The scores that have been achieved so far by both myself and my opponent are calculated and saved, except for the first round, where the scores are 0 for everyone.
                     self$calculate_scores()
                     
                     
                     # Then the opponents moves get assessed to see if their strategy is one of the strategies we have considered,
                     # and if so, I now know their strategy and can use this against them
                     opponents_strategy = self$analyze_bot()
                     
                     #Some variables are calculated: the number of rounds that have already been played between me and my opponent
                     round_number_opp = nrow(self$my_actions_vs_opponent)
                     #A random variable percentage tot compare to later for making a strategy choice
                     choice_maker = runif(1) * 100
                     #This factor is the number of times my opponent cooperated divided by the number of rounds that he has played so far. This indicates how friendly he is.
                     coopfac = (nrow(self$all_opponents_actions[(self$all_opponents_actions$opp_bid == "cooperate"),])) / (nrow(self$all_opponents_actions)) * 100
                     
                     
                     #Here my strategy begins. I play the "Detective" which means I always do (cooperate, defect, cooperate, cooperate) in my first four moves in order to find out infomration about my opponent.
                     if (round_number_opp == 0){
                       bid = "cooperate"
                       
                     } else if(round_number_opp == 1){
                       bid = "defect"
                       
                     } else if (round_number_opp == 2){
                       bid = "cooperate"
                       
                     } else if (round_number_opp == 3){
                       bid = "cooperate"
                       
                       #When the first four rounds have been played against my current opponent, I try to use his strategy against him.
                     } else if (round_number_opp >= 4) {
                       #If the opponent seems to only cooperate, I take advantage of this by defecting
                       if (opponents_strategy == "always_cooperate"){
                         bid = "defect"
                         
                         #If the opponent seems to only defect, I take advantage of this by defecting  
                       } else if (opponents_strategy == "always_defect"){
                         bid = "defect"
                         #If our opponent plays the tit for tat strategy, we copy this for optimal results        
                       } else if (opponents_strategy == "tit_for_tat"){
                         #I figure out what the last action of my opponent was when he played me, and I copy this move
                         n = max(self$my_actions_vs_opponent$round)
                         last = self$my_actions_vs_opponent[self$my_actions_vs_opponent$round == n, "opp_bid"]
                         bid = last
                         #If I am not able to figure out the strategy of my opponent, I decide which of my own strategies I will use this round.  
                       } else if (opponents_strategy == "unknown"){
                         #The more rounds I played against my opponent, the bigger the chance that I will base my strategy on the whole dataset.
                         if ((round_number_opp / 4) > choice_maker){
                           #If my opponent has a better overall score so far than me and if he is more likely to cooperate than to defect, I will try to take him down by defecting
                           if (self$my_score < self$opponent_score){
                             if (coopfac > 50){
                               bid = "defect"
                               #If he is more likely to defect, I will cooperate to try to turn him into a better agent for the future.
                             } else {
                               bid = "cooperate"
                               
                               
                             }
                             #If my score is higher than my opponents and he is likely to cooperate, I gladly work along
                           } else {
                             if (coopfac > 50){
                               bid = "cooperate"
                             } else {
                               bid = "defect"
                             }
                           }
                           #If the chance did not allow for it to use my strategy based on the whole data, I play the tit for tat, which turned out to be optimal in the tournament we ran between our own agents
                           #More about this can be found in the markdown
                         } else{
                           n = max(self$my_actions_vs_opponent$round)
                           last = self$my_actions_vs_opponent[self$my_actions_vs_opponent$round == n, "opp_bid"]
                           bid = last
                           
                         }
                       }
                     }
                     #Last but not least, we discover Scott's agents that always defect, and we don't fall for his defecting strategy, by defecting ourselves.
                     if ("Lemon!" %in% self$response) {
                       bid = "defect"
                     }
                     
                     self$bid = bid
                     
                     
                   },
                   
                   
                   process_book = function(){
                     
                     #make a list of all the rounds our opponent played in. There are two because my opponent can be id1 as well as id2
                     all_actions1 = self$book[(self$book$id1 == self$opponent_id), c("tradeno", "bid1", "bid2")]
                     all_actions2 = self$book[(self$book$id2 == self$opponent_id), c("tradeno", "bid1", "bid2")]
                     #Rename the variables to my liking, opp_bid is the current opponent and trash_bid is about the opponents of our current opponent (which can sometimes be myself)
                     #Also the order of naming makes sure that when the dataframes are binded, the opp_bid will always be the first column.
                     names(all_actions1) = c("round", "opp_bid", "trash_bid")
                     names(all_actions2) = c("round", "trash_bid", "opp_bid")
                     #Bind the cases to get one big dataframe of all our opponents actions so far
                     all_actions_opp = rbind(all_actions1, all_actions2)
                     all_actions_opp = all_actions_opp[order(all_actions_opp$round),]
                     #Set the attribute
                     self$set_all_opponents_actions(all_actions_opp)
                     
                     #make a list of all the rounds I played in. There are two because I can be id1 as well as id2
                     all_my_actions1 = self$book[(self$book$id1 == self$id),]
                     all_my_actions2 = self$book[(self$book$id2 == self$id),]
                     #Rename the variables to my liking, also the naming order makes sure that my_bid will always be the first column after binding
                     names(all_my_actions1) = c("my_id", "opp_id","round", "my_bid", "opp_bid")
                     names(all_my_actions2) = c("opp_id", "my_id", "round", "opp_bid", "my_bid")
                     #Bind the cases together to make a complete dataframe
                     all_my_actions = rbind(all_my_actions1, all_my_actions2)
                     all_my_actions = all_my_actions[order(all_my_actions$round),]
                     #Set the attribute
                     self$set_all_my_actions(all_my_actions)
                     
                     #Make a list of our opponents moves against us. This is easy because this is just a subdataframe of the one already created
                     my_actions_vs_opponent = all_my_actions[(all_my_actions$opp_id == self$opponent_id),]
                     self$set_my_actions_vs_opponent(my_actions_vs_opponent)
                     
                     
                   },
                   
                   
                   calculate_scores = function(){
                     
                     #Calculate the score so far of my opponent
                     score_opp = 0
                     #Here I use the pay-off matrix. I only calculate if the book is not empty to prevent errors from taking place
                     if (nrow(self$all_opponents_actions) > 1) {
                       for(i in 1:nrow(self$all_opponents_actions)){
                         
                         if(self$all_opponents_actions$opp_bid[i] == "defect"){
                           if(self$all_opponents_actions$trash_bid[i] == "cooperate"){
                             score_opp = score_opp + 5
                           } else{score_opp = score_opp + 2}
                         } else{
                           if(self$all_opponents_actions$trash_bid[i] == "cooperate"){
                             score_opp = score_opp + 4
                           }
                         }
                         
                       }
                       self$set_opponent_score(score_opp)
                       
                       #Calculate the score so far of myself. This is quite the same as the calculation for my opponents score
                       my_score = 0
                       
                       for(i in 1:nrow(self$all_my_actions)){
                         
                         if(self$all_my_actions$my_bid[i] == "defect"){
                           if(self$all_my_actions$opp_bid[i] == "cooperate"){
                             my_score = my_score + 5
                           } else{my_score = my_score + 2}
                         } else{
                           if(self$all_my_actions$opp_bid[i] == "cooperate"){
                             my_score = my_score + 4
                           }
                         }
                         self$set_my_score(my_score)  
                       }
                     }
                   },
                   
                   
                   analyze_bot = function() {
                     #Here I analyze my opponent, by looking at the percentage of times that he cooperated. Also I calculate the number of rounds we have already played against each other
                     coopfac = (nrow(self$all_opponents_actions[(self$all_opponents_actions$opp_bid == "cooperate"),])) / (nrow(self$all_opponents_actions)) * 100
                     deffac = 100 - coopfac
                     round_number_opp = nrow(self$my_actions_vs_opponent)
                     
                     
                     #I start with unknown and change this when I figure out his strategy 
                     agent_type = "unknown"
                     #I only calculate when the book is not empty to prevent errors
                     if (is.null(round_number_opp)) {
                       
                     } else {
                       #Here my detective strategy comes in handy to recognise a tit for tat agent. The first round doesn't matter as a tit for tat agent can start with either cooperate or defect.
                       if (round_number_opp > 3 
                           #The second round a tit for tat agent would react to our cooperate in the first round and play cooperate
                           & self$my_actions_vs_opponent$opp_bid[2] == "cooperate" 
                           #In the third round a tit for tat agent would react to our defect in the second round
                           & self$my_actions_vs_opponent$opp_bid[3] == "defect" 
                           #In the fourth round a tit for tat player would react to our cooperate in the third round
                           & self$my_actions_vs_opponent$opp_bid[4] == "cooperate"){
                         #If the opponent reacted to all three of our first moves, I conclude he is a tit for tat player
                         agent_type = "tit_for_tat"
                         #Using the percentage that our opponent cooperated, we can find out if he always cooperates even if his opponent has defected sometimes
                       } else if ((coopfac == 100) & ("defect" %in% self$all_opponents_actions$trash_bid)) {
                         agent_type = "always_cooperate"
                         #The same goes for defecting.
                       } else if ((deffac == 100) & ("cooperate" %in% self$all_opponents_actions$trash_bid)) {
                         agent_type = "always_defect"
                       }
                     }
                     
                     return(agent_type)
                   }
                 )
)
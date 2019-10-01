library(R6)

Agent <- R6Class("Agent",
  
  public = list(
    bid = NULL,
    book = NULL,
    greeting = "Hi!",
    id = NULL,
    opponent_id = NULL,
    round = NULL,
    response = NULL,
      
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
  
  get_bid = function() {
    bid_vector <- c("cooperate", "defect")
    
    t <- self$book
    
    rec1 <- t[(t$id1 == self$id & t$id2 == self$opponent_id), c("tradeno", "bid2")]
    rec2 <- t[(t$id1 == self$opponent_id & t$id2 == self$id), c("tradeno", "bid1")]
    
    names(rec1) <- c("round", "bid")
    names(rec2) <- c("round", "bid")
    all_rec <- rbind(rec1, rec2)
    
    my_moves1 <- t[(t$id1 == self$opponent_id & t$id2 == self$id), c("tradeno", "bid2")]
    my_moves2 <- t[(t$id1 == self$id & t$id2 == self$opponent_id), c("tradeno", "bid1")]
    
    names(my_moves1) <- c("round", "bid")
    names(my_moves2) <- c("round", "bid")
    my_moves <- rbind(my_moves1, my_moves2)
    
    if (nrow(all_rec) > 2) {
      round <- all_rec[order(all_rec$round), 1]
      n = max(round)
      round2 = head(round, -1)
      n2 = max(round2)
      round3 = head(round, -2)
      n3 = max(round3)
      
      last = all_rec[all_rec$round == n, "bid"]
      one_before_last = all_rec[all_rec$round == n2, "bid"]
      two_before_last = all_rec[all_rec$round == n3, "bid"]
      
    } else {
      last = NA
      one_before_last = NA
      two_before_last = NA
    }
    
    if (nrow(my_moves) > 2) {
      round <- my_moves[order(my_moves$round), 1]
      n = max(round)
      round2 = head(round, -1)
      n2 = max(round2)
      
      my_last = my_moves[my_moves$round == n, "bid"]
      my_one_before_last = my_moves[my_moves$round == n2, "bid"]
    } else {
      my_last = NA
      my_one_before_last = NA
    }
    
    if (is.na(two_before_last)) {
      bid = "cooperate"
    } else {
      if (last == "defect" & one_before_last == "defect") {
        bid = "defect"
      } else if (one_before_last == "defect" & two_before_last == "defect") {
        bid = "defect"
      } else if (my_one_before_last == "defect" & my_last == "cooperate") {
        bid = "cooperate"
      } else {
        bid = "cooperate"
      }
    }
    
    if ("Lemon!" %in% self$response) {
      bid = "defect"
    }
    
    self$bid <- bid
  }
)
)
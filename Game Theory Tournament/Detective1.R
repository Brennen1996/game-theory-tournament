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
    
    if (nrow(all_rec) > 1) {
      round <- all_rec[order(all_rec$round), 1]
      n = max(round)

      last = all_rec[all_rec$round == n, "bid"]
    } else {
      last = NA
    }
    
    if (nrow(all_rec) == 0) {
      bid = "cooperate"
    } else {
      if (nrow(all_rec) == 1) {
        bid = "defect"
      } else {
        if (nrow(all_rec) == 2) {
          bid ="cooperate"
        } else {
        if (nrow(all_rec) == 3) {
          bid = "cooperate"
        } else {
          if (nrow(all_rec) > 3) {
            if ("defect" %in% all_rec$bid) {
              if (last == "defect") {
                bid = "defect"
              } else {
                bid = "cooperate"
              }
            } else {
              bid = "defect"
            }
          }
          }
        }
      }
    }
    
    if ("Lemon!" %in% self$response) {
      bid = "defect"
    }
    
    self$bid <- bid
  }
  )
)

#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#' Step 1: Create a new game
#'   `create_game()` generates a new game that consists of doors
#'   with goats behind them, and cars behind the other doors.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... the function takes 2 positional arguments,
#' number of cars and number of goats.
#'
#' @return The function returns a character vector
#'   indicating the positions of goats and the cars.
#'
#' @examples
#'   create_game(3,7)
#' @export

create_game <- function(num.cars, num.goats) {

  a.game <- sample(x = c(rep("car", num.cars),
                         rep("goat", num.goats)),
                   size = (num.cars + num.goats),
                   replace = FALSE)
  # Return both the game and the variables as a list
  return(a.game)
}


#' @title Step 2: Contestant selects a door
#' @description The contestant makes their first selection
#' @details this function receives one positional
#' argument and it should be the return value of
#' the create_game function
#' @param ... NO arguments
#' @return returns an integer
#' @export

select_door <- function()

{

  doors <- c(1:10)
  a.pick <- sample (doors, size = 1)
  return( a.pick )  # number between 1 and 10

}


#' @title Step 3 : Host opens goat door
#' @description opens a door
#' @details This function receives two positional
#' arguments, game and a.pick
#' @param ... game, the return value of create_game call
#' a.pick is the return value of select_door call
#' @return an integer representing the opened door
#' @export

open_goat_door <- function( game, a.pick )
{

  doors <- c(1:10)
  # if contestant selected car,
  # randomly select one goat door
  if( game[ a.pick ] == "car" )
  {
    goat.doors <- doors[ game != "car" ]
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  {
    opened.doors <- doors[ game != "car" & doors != a.pick ]
    opened.door <- sample( opened.doors, size=1 )
  }
  return( opened.door ) # number between 1 and 10
}


#' @title Step 4: Change doors
#' @description Gives the option for the player to change choice
#' of door.this function receives two positional arguments
# ( opened_door and a.pick) and one keyword argument stay
#' with a default value of True (stay=True)
#' @details opened_door is the return value of the open_goat_door call
#' a.pick is the return value of the select_door call
#' @param ... two positional arguments and one keyword argument
#' @return an integer representing the final choice of door selected by
#' player
#' @export

change_door <- function( stay=T, opened.door, a.pick )
{

  # YOUR CODE HERE...
  doors <- c(1:10)

  if (stay) {
    final.pick <- a.pick  # Contestant stays with their first choice
  }
  else {
    # Switch: pick the door that is NOT the contestant's pick and NOT the opened goat door
    for (d in doors)
    {
      if (d != a.pick & d != opened.door)
      {
        final.pick <- d
      }
    }
  }

  return( final.pick )  # number between 1 and 3

}


#' @title Step 5: Determine if contestant has won
#' @description determines whether the contestant won or lost
#' @details this function takes two positional arguments,
#' final.pick and game. final.pick is the return value of the
#' change door call game is the return value of
#' the create_game call
#' @param ... two positional arguments
#' @return WIN OR LOSE
#' @export

determine_winner <- function( final.pick, game )
{
  if( game[final.pick] == "car")
  {
    return( "WIN" )
  }
  if(game[final.pick] == "goat")
  {
    return( "LOSE" )
  }

}


#' @title Wrapper function for the game
#' @description Makes it easier to simulate
#' @details The function takes two positional arguments, number of cars,
#' and number of goats
#' @param ... two integer positional arguments (number of cars and number of goats)
#' @return returns the game result
#' @export


play_game <- function(num.cars, num.goats)
{
  new.game <- create_game(num.cars, num.goats)
  #this.game <- as.character(new.game)
  first.pick <- select_door()
  opened.door <- open_goat_door(new.game, first.pick)
  final.pick.stay <- change_door(stay=T, opened.door, first.pick)
  final.pick.switch <- change_door(stay=F, opened.door, first.pick)
  outcome.stay <- determine_winner(final.pick.stay, new.game)
  outcome.switch <- determine_winner(final.pick.switch, new.game)
  strategy <- c("stay", "switch")
  outcome <- c(outcome.stay, outcome.switch)

  game.results <- data.frame(strategy, outcome, stringsAsFactors=F)


  return(game.results)
}


#' @title Simulation Wrapper
#' @description Runs 10,000 simulations of the game
#' @details Outputs table to report results
#' @param ... The function takes two positional arguments, number of cars and number of goats
#' @return returns a table of results
#' @export


simulation <- function(num.cars, num.goats)
{
  results.df <- NULL    # collector
  for( i in 1:10000 )  # iterator
  {
    game.outcome <- play_game(num.cars, num.goats)
    # binding step
    results.df <- rbind( results.df, game.outcome )
  }
  return(table(results.df) / 10000)

}

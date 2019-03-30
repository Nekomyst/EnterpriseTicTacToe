// -----------------------------------------------------------
// Logging
// -----------------------------------------------------------
namespace ETTT

open TicTacToeDomain

module Logger = 
    open TicTacToeDomain
     
    let logXMove (PlayerXPos cellPos)= 
        printfn "X played %A" cellPos

    let logOMove (PlayerOPos cellPos)= 
        printfn "O played %A" cellPos

    /// inject logging into the API
    let injectLogging api =

        // make a logged version of the game function 
        let playerXMoves state move = 
            logXMove move 
            api.playerXMoves state move 

        // make a logged version of the game function 
        let playerOMoves state move = 
            logOMove move 
            api.playerOMoves state move 
        
        // create a new API with                             
        // the move functions replaced
        // with logged versions
        { api with
            playerXMoves = playerXMoves
            playerOMoves = playerOMoves
            }

// -----------------------------------------------------------
// ConsoleUi 
// -----------------------------------------------------------
namespace ETTT

open System
open TicTacToeDomain

/// Console based user interface
module ConsoleUi =
    open TicTacToeDomain
    
    /// Track the UI state
    type UserAction<'a> =
        | ContinuePlay of 'a
        | ExitGame

    /// Print each available move on the console
    let displayAvailableMoves moves = 
        moves
        |> List.iteri (fun i move -> 
            printfn "%i) %A" i move )

    /// Get the move corresponding to the 
    /// index selected by the user
    let getMove moveIndex moves = 
        if moveIndex < List.length moves then
            let move = List.nth moves moveIndex 
            Some move
        else
            None

    /// Given that the user has not quit, attempt to parse
    /// the input text into a index and then find the move
    /// corresponding to that index
    let processMoveIndex (inputStr: string) gameState availableMoves makeMove processInputAgain =
        match Int32.TryParse inputStr with
        // TryParse will output a tuple (parsed?,int)
        | true,inputIndex ->
            // parsed ok, now try to find the corresponding move
            match getMove inputIndex availableMoves with
            | Some move -> 
                // corresponding move found, so make a move
                let moveResult = makeMove gameState move 
                ContinuePlay moveResult // return it
            | None ->
                // no corresponding move found
                printfn "...No move found for inputIndex %i. Try again" inputIndex 
                // try again
                processInputAgain()
        | false, _ -> 
            // int was not parsed
            printfn "...Please enter an int corresponding to a displayed move."             
            // try again
            processInputAgain()

    /// Ask the user for input. Process the string entered as 
    /// a move index or a "quit" command
    let rec processInput gameState availableMoves makeMove = 

        // helper that calls this function again with exactly
        // the same parameters
        let processInputAgain() = 
            processInput gameState availableMoves makeMove 

        printfn "Enter an int corresponding to a displayed move or q to quit:" 
        let inputStr = Console.ReadLine()
        if inputStr = "q" then
            ExitGame
        else
            processMoveIndex inputStr gameState availableMoves makeMove processInputAgain
            
    /// Display the cells on the console in a grid
    let displayCells cells = 
        let cellToStr cell = 
            match cell.state with
            | Empty -> "-"            
            | Played player ->
                match player with
                | PlayerO -> "O"
                | PlayerX -> "X"

        let printCells cells  = 
            cells
            |> List.map cellToStr
            |> List.reduce (fun s1 s2 -> s1 + "|" + s2) 
            |> printfn "|%s|"

        let topCells = 
            cells |> List.filter (fun cell -> snd cell.pos = Top) 
        let centerCells = 
            cells |> List.filter (fun cell -> snd cell.pos = VCenter) 
        let bottomCells = 
            cells |> List.filter (fun cell -> snd cell.pos = Bottom) 
        
        printCells topCells
        printCells centerCells 
        printCells bottomCells 
        printfn ""   // add some space
        
    /// After each game is finished,
    /// ask whether to play again.
    let rec askToPlayAgain api  = 
        printfn "Would you like to play again (y/n)?"             
        match Console.ReadLine() with
        | "y" -> 
            ContinuePlay api.newGame
        | "n" -> 
            ExitGame
        | _ -> askToPlayAgain api 

    /// The main game loop, repeated
    /// for each user input
    let rec gameLoop api userAction = 
        printfn "\n------------------------------\n"  // a separator between moves
        
        match userAction with
        | ExitGame -> 
            printfn "Exiting game."             
        | ContinuePlay (state,moveResult) -> 
            // first, update the display
            state |> api.getCells |> displayCells

            // then handle each case of the result
            match moveResult with
            | GameTied -> 
                printfn "GAME OVER - Tie"             
                printfn ""             
                let nextUserAction = askToPlayAgain api 
                gameLoop api nextUserAction
            | GameWon player -> 
                printfn "GAME WON by %A" player            
                printfn ""             
                let nextUserAction = askToPlayAgain api 
                gameLoop api nextUserAction
            | PlayerOToMove availableMoves -> 
                printfn "Player O to move" 
                displayAvailableMoves availableMoves
                let newResult = processInput state availableMoves api.playerOMoves
                gameLoop api newResult 
            | PlayerXToMove availableMoves -> 
                printfn "Player X to move" 
                displayAvailableMoves availableMoves
                let newResult = processInput state availableMoves api.playerXMoves
                gameLoop api newResult 

    /// start the game with the given API
    let startGame api =
        let userAction = ContinuePlay api.newGame
        gameLoop api userAction 
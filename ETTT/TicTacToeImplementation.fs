// -----------------------------------------------------------
// TicTacToeImplementation 
// -----------------------------------------------------------
namespace ETTT

open TicTacToeDomain

module TicTacToeImplementation =
    open TicTacToeDomain

    /// private implementation of game state
    type GameState = {
        cells : Cell list
        }

    /// the list of all horizontal positions
    let allHorizPositions = [Left; HCenter; Right]
    
    /// the list of all horizontal positions
    let allVertPositions = [Top; VCenter; Bottom]

    /// A type to store the list of cell positions in a line
    type Line = Line of CellPosition list

    /// a list of the eight lines to check for 3 in a row
    let linesToCheck = 
        let mkHLine v = Line [for h in allHorizPositions do yield (h,v)]
        let hLines= [for v in allVertPositions do yield mkHLine v] 

        let mkVLine h = Line [for v in allVertPositions do yield (h,v)]
        let vLines = [for h in allHorizPositions do yield mkVLine h] 

        let diagonalLine1 = Line [Left,Top; HCenter,VCenter; Right,Bottom]
        let diagonalLine2 = Line [Left,Bottom; HCenter,VCenter; Right,Top]

        // return all the lines to check
        [
        yield! hLines
        yield! vLines
        yield diagonalLine1 
        yield diagonalLine2 
        ]

    /// get the cells from the gameState
    let getCells gameState = 
        gameState.cells 

    /// get the cell corresponding to the cell position
    let getCell gameState posToFind = 
        gameState.cells 
        |> List.find (fun cell -> cell.pos = posToFind)

    /// update a particular cell in the GameState 
    /// and return a new GameState
    let private updateCell newCell gameState =

        // create a helper function
        let substituteNewCell oldCell =
            if oldCell.pos = newCell.pos then
                newCell
            else 
                oldCell                 

        // get a copy of the cells, with the new cell swapped in
        let newCells = gameState.cells |> List.map substituteNewCell 
        
        // return a new game state with the new cells
        {gameState with cells = newCells }

    /// Return true if the game was won by the specified player
    let private isGameWonBy player gameState = 
        
        // helper to check if a cell was played by a particular player
        let cellWasPlayedBy playerToCompare cell = 
            match cell.state with
            | Played player -> player = playerToCompare
            | Empty -> false

        // helper to see if every cell in the Line has been played by the same player
        let lineIsAllSamePlayer player (Line cellPosList) = 
            cellPosList 
            |> List.map (getCell gameState)
            |> List.forall (cellWasPlayedBy player)

        linesToCheck
        |> List.exists (lineIsAllSamePlayer player)


    /// Return true if all cells have been played
    let private isGameTied gameState = 
        // helper to check if a cell was played by any player
        let cellWasPlayed cell = 
            match cell.state with
            | Played _ -> true
            | Empty -> false

        gameState.cells
        |> List.forall cellWasPlayed 

    /// determine the remaining moves for a player
    let private remainingMovesForPlayer playerMove gameState = 

        // helper to return Some if a cell is playable
        let playableCell cell = 
            match cell.state with
            | Played player -> None
            | Empty -> Some (playerMove cell.pos)

        gameState.cells
        |> List.choose playableCell


    /// create the state of a new game
    let newGame = 

        // allPositions is the cross-product of the positions
        let allPositions = [
            for h in allHorizPositions do 
            for v in allVertPositions do 
                yield (h,v)
            ]

        // all cells are empty initially
        let emptyCells = 
            allPositions 
            |> List.map (fun pos -> {pos = pos; state = Empty})
        
        // create initial game state
        let gameState = { cells=emptyCells }            

        // initial of valid moves for player X is all positions
        let validMoves = 
            allPositions 
            |> List.map PlayerXPos

        // return new game
        gameState, PlayerXToMove validMoves

    // player X makes a move
    let playerXMoved gameState (PlayerXPos cellPos) = 
        let newCell = {pos = cellPos; state = Played PlayerX}
        let newGameState = gameState |> updateCell newCell 
        
        if newGameState |> isGameWonBy PlayerX then
            // return the new state and the move result
            newGameState, GameWon PlayerX
        elif newGameState |> isGameTied then
            // return the new state and the move result
            newGameState, GameTied  
        else
            let remainingMoves = 
                newGameState |> remainingMovesForPlayer PlayerOPos
            newGameState, PlayerOToMove remainingMoves

    // player O makes a move
    let playerOMoved gameState (PlayerOPos cellPos) = 
        let newCell = {pos = cellPos; state = Played PlayerO}
        let newGameState = gameState |> updateCell newCell 
        
        if newGameState |> isGameWonBy PlayerO then
            // return the new state and the move result
            newGameState, GameWon PlayerO
        elif newGameState |> isGameTied then
            // return the new state and the move result
            newGameState, GameTied 
        else
            let remainingMoves = 
                newGameState |> remainingMovesForPlayer PlayerXPos
            newGameState, PlayerXToMove remainingMoves

        // Exercise - refactor to remove the duplicate code from                 
        // playerXMoved  and playerOMoved 


    /// export the API to the application
    let api = {
        newGame = newGame 
        playerXMoves = playerXMoved 
        playerOMoves = playerOMoved 
        getCells = getCells
        }

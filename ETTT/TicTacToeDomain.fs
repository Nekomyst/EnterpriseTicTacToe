// -----------------------------------------------------------
// TicTacToeDomain 
// -----------------------------------------------------------
namespace ETTT

module TicTacToeDomain =

    type HorizPosition = Left | HCenter | Right
    type VertPosition = Top | VCenter | Bottom
    type CellPosition = HorizPosition * VertPosition 

    type Player = PlayerO | PlayerX

    type CellState = 
        | Played of Player 
        | Empty

    type Cell = {
        pos : CellPosition 
        state : CellState 
        }

    type PlayerXPos = PlayerXPos of CellPosition 
    type PlayerOPos = PlayerOPos of CellPosition 

    type ValidMovesForPlayerX = PlayerXPos list
    type ValidMovesForPlayerO = PlayerOPos list
        
    type MoveResult = 
        | PlayerXToMove of ValidMovesForPlayerX 
        | PlayerOToMove of ValidMovesForPlayerO 
        | GameWon of Player 
        | GameTied 

    // the "use-cases"        
    type NewGame<'GameState> = 
        'GameState * MoveResult      
    type PlayerXMoves<'GameState> = 
        'GameState -> PlayerXPos -> 'GameState * MoveResult
    type PlayerOMoves<'GameState> = 
        'GameState -> PlayerOPos -> 'GameState * MoveResult

    // helper function
    type GetCells<'GameState> = 
        'GameState -> Cell list

    // the functions exported from the implementation
    // for the UI to use.
    type TicTacToeAPI<'GameState>  = 
        {
        newGame : NewGame<'GameState>
        playerXMoves : PlayerXMoves<'GameState> 
        playerOMoves : PlayerOMoves<'GameState> 
        getCells : GetCells<'GameState>
        }

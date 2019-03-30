// -----------------------------------------------------------
// ConsoleApplication 
// -----------------------------------------------------------
namespace ETTT

module ConsoleApplication = 

    let startGame() =
        let api = TicTacToeImplementation.api
        let loggedApi = Logger.injectLogging api
        ConsoleUi.startGame loggedApi 


// ConsoleApplication.startGame() 

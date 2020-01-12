namespace Informedica.GenSolver.Lib

module Logger =

    type IMessage = interface end
    
    type Message =
       | StartCalulation of obj
       | StartSolvingEquation of obj
       | FinishedCalculation of obj
       | VariableChanged of obj
       | FinishedSolvingEquation of obj
       | LoopSolvingEquation of obj
       | LoopSolverQue of obj
       | InvalidEquationsException of obj
       | ConstraintsOrder of obj
       | ConstraintVariableNotFound of obj
       | ConstraintSettingLimit of obj
       | ConstrainedEquations of obj
       | ConstrainedEquationsSolved of obj
       | ApiSettingVariable of obj
       | ApiEquationsSolved of obj
       | ApiAppliedConstraints of obj
       interface IMessage


    let createMessage o (m : obj -> Message) : IMessage = 
        o  |> box |> m :> IMessage


    type Level =
        | Informative
        | Warning
        | Error


    type Logger =   
        {
            LogMessage : Level -> IMessage -> unit
        }


    let logMessage level (logger : Logger) msg = 
        logger.LogMessage level msg

    let logInfo logger msg = logMessage Informative logger msg

    let logWarning logger msg = logMessage Warning logger msg

    let logError logger msg = logMessage Error logger msg
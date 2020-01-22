namespace Informedica.GenSolver.Lib

module Logging =

    open Types.Logging

    let logMessage level (logger : Logger) msg = 
        logger.Log level msg

    let logInfo logger msg = logMessage Informative logger msg

    let logWarning logger msg = logMessage Warning logger msg

    let logError logger msg = 
        msg
        |> ExceptionMessage
        |> logMessage Error logger
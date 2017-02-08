data LogMessage = LogMessage {getMachineId :: String, getDescription :: String}

makeLogMessage :: String -> LogMessage
--Precondition: message is a valid log message.
makeLogMessage message = LogMessage { getMachineId = (takeWhile (/= ':') message), getDescription = (tail $ dropWhile (/= ':') message)}

containsWord :: LogMessage -> Bool
containsWord = flip elem . words . getDescription
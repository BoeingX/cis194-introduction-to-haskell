{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage str = case strWords of 
                     "I":timestamp:message -> LogMessage Info (read timestamp::Int) (unwords message)
                     "W":timestamp:message -> LogMessage Warning (read timestamp::Int) (unwords message)
                     "E":level:timestamp:message -> LogMessage (Error (read level::Int)) (read timestamp::Int) (unwords message)
                     _ -> Unknown str
                    where strWords = words str

parse :: String -> [LogMessage]
parse str = map parseMessage (lines str)

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage Info timestamp _) = timestamp
getTimeStamp (LogMessage Warning timestamp _) = timestamp
getTimeStamp (LogMessage (Error _) timestamp _) = timestamp
getTimeStamp _ = error "Timestamp is unknown"

insert :: LogMessage -> MessageTree -> MessageTree
insert logMessage tree = case logMessage of 
                           Unknown _ -> tree
                           _ -> case tree of
                                  Leaf -> Node Leaf logMessage Leaf
                                  Node lTree logMessage2 rTree -> if timestamp1 < timestamp2
                                                                     then Node (insert logMessage lTree) logMessage2 rTree
                                                                     else Node lTree logMessage2 (insert logMessage rTree)
                                                                         where timestamp1 = getTimeStamp logMessage
                                                                               timestamp2 = getTimeStamp logMessage2

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lTree logMessage rTree) = inOrder lTree ++ [logMessage] ++ inOrder rTree

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error level) _ _) = level >= 50
isSevere _ = False

errorMessage :: LogMessage -> String
errorMessage (LogMessage (Error _) _ message) = message
errorMessage _ = error "Not defined"

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMessages = map errorMessage (filter isSevere . inOrder . build $ logMessages)

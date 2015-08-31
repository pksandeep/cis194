{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage x = case words x of
                  "I":t:xs -> LogMessage Info ts msg
                                where ts = read t :: Int
                                      msg = unwords xs
                  "W":t:xs -> LogMessage Warning ts msg
                                where ts = read t :: Int
                                      msg = unwords xs
                  "E":e:t:xs -> LogMessage (Error l) ts msg
                                  where ts = read t :: Int
                                        l = read e :: Int
                                        msg = unwords xs
                  m -> Unknown msg
                          where msg = unwords m

parse :: String -> [LogMessage]
--parse x = parseMessageHelper (lines x)
parse x = map parseMessage (lines x)


--parseMessageHelper :: [String] -> [LogMessage]
--parseMessageHelper [] = []
--parseMessageHelper (x:xs) = parseMessage x : parseMessageHelper xs

insert :: LogMessage -> MessageTree -> MessageTree
insert l Leaf = Node Leaf l Leaf
insert (Unknown _) m = m
insert lm@(LogMessage _ ts1 _) (Node lt root@(LogMessage _ ts2 _) rt )
    | ts1 < ts2 = Node (insert lm lt) root rt
    | otherwise = Node lt root (insert lm rt)
insert _ (Node lt (Unknown msg) rt) = Node lt (Unknown msg) rt

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt lm rt) = inOrder lt ++ (lm : inOrder rt)

whatWentWrong :: [LogMessage] -> [String]

--whatWentWrong lm = filterMessages $ inOrder (build lm)
--whatWentWrong lm = map getMessage (inOrder(build (filter isError lm)))
whatWentWrong lm = map getMessage . inOrder . build . filter isError lm

filterMessages :: [LogMessage] -> [String]
filterMessages [] = []
filterMessages (LogMessage mt1 _ msg1 : xs) = case mt1 of
                                                     Error n -> if n>50 then
                                                                  msg1 : filterMessages xs
                                                                else
                                                                  filterMessages xs
                                                     _ -> filterMessages xs

filterMessages (Unknown _ : xs) = filterMessages xs

isError :: LogMessage -> Bool
isError (LogMessage (Error n) _ _) = n > 50
isError _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg
getMessage (Unknown  msg) = msg

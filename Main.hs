module Main where

import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Word8
import Text.Parsec
import Text.Parsec.String

type Byte = Word8

data Tape = Tape [Byte] [Byte] deriving (Show)

left :: Tape -> Tape
left (Tape (x:xs) ys) = Tape xs (x:ys)
left t = t

right :: Tape -> Tape
right (Tape xs (y:ys)) = Tape (y:xs) ys
right t = t

cur :: Tape -> Byte
cur (Tape _ (y:_)) = y

set :: Byte -> Tape -> Tape
set b (Tape xs (_:ys)) = Tape xs (b:ys)

type BF = StateT Tape IO ()

data Cmd = IncP | DecP | IncB | DecB | Out | In Byte | Loop [Cmd] deriving (Show)

validBytes :: [Byte]
validBytes = [0..]

parseByte :: Parser Byte
parseByte = do let chars = fmap (chr . fromIntegral) validBytes
               c <- oneOf chars
               return . fromIntegral . ord $ c

parseCmd :: Parser Cmd
parseCmd = choice [ char '>' >> return IncP
                  , char '<' >> return DecP
                  , char '+' >> return IncB
                  , char '-' >> return DecB
                  , char '.' >> return Out
                  , char ',' >> parseByte >>= return . In
                  , between (char '[') (char ']') (many1 parseCmd) >>= return . Loop
                  ]

type Prog = [Cmd]

parseSep :: Parser ()
parseSep = void space <|> void endOfLine

parseProg :: Parser Prog
parseProg = between (optional parseSep) (optional parseSep) parseCmd `manyTill` eof

run :: Cmd -> BF
run cmd = case cmd of
            IncP -> get >>= put . right
            DecP -> get >>= put . left
            IncB -> get >>= put . ((1 + ) . cur >>= set)
            DecB -> get >>= put . (subtract 1 . cur >>= set)
            Out -> get >>= liftIO . putChar . chr . fromIntegral . cur
            In b -> get >>= put . set b
            Loop cmds -> get >>= (\t -> put t >> unless (cur t == 0) (exec cmds >> run (Loop cmds)))

exec :: Prog -> BF
exec prog = forM_ prog run

main :: IO ()
main = do s <- getContents
          prog <- either (error . show) return (parse parseProg "" s)
          evalStateT (exec prog) $ Tape (repeat 0) (repeat 0)

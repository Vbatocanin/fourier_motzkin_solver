import System.Environment
import System.IO
import Data.Char
import Control.Monad
import Control.Exception


data Variable = Variable String Float 
                
instance Show Variable where
    show (Variable s f) = (show f) ++ s

data Function = Function [Variable] Float

data InEqu = InEqu [Variable] String Float
data Problem = Problem Function [InEqu] 

main :: IO()
main = do
            fileInput <- openFile "./input.txt" ReadMode
            input <- hGetContents fileInput
            parseInput input
            

parseInput :: String -> IO()
parseInput input = do
            let inputLines = lines input
            let foo = formatFunction $ head inputLines
            let inEq = formatIneqs $ tail inputLines
            putStrLn $ show foo


isFunc :: String -> Bool
isFunc str = if ((dropWhile isSpace str) !! 0 == 'f') 
                then True
                else False 
            

formatFunction :: String -> [Variable]
formatFunction str = map (\(x,y) -> Variable y x) $ map (\(x,y) -> (stringToFloat x,y)) $ map (break isAlpha) $ filter ( \x -> x /= "" ) $ words $ filter ( \x -> x /= '=' && x /= 'f' ) str 

stringToFloat :: [Char] -> Float
stringToFloat (x:xs) 
                | x == '-' = read (x:xs)::Float
                | x == '+' = read xs::Float
                | isDigit x = read (x:xs)::Float
                | otherwise = error "to float conversion error"

formatIneqs :: [String] -> [InEqu] 
formatIneqs allLines =  map (formatIneq) $ allLines

formatIneq :: String -> InEqu 
formatIneq ineqString =  InEqu vars (fst rightSidePair) (snd rightSidePair)
                    where 
                        pairs = map (break isAlpha) $ words ineqString 
                        varsStrings = removeLast pairs
                        vars = map (\(x,y) -> Variable y (stringToFloat x)) varsStrings
                        rightSidePair = convert $ break isDigit $ fst $ (head.reverse) pairs
                        
convert :: (String,String) -> (String,Float)
convert (s1,s2) = (s1,stringToFloat s2)

removeLast :: Eq t => [t] -> [t]
removeLast [] = []
removeLast [x] = []
removeLast (x:xs) = (x:removeLast xs)


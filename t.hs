import Control.Applicative hiding (many,optional,(<|>))
import Control.Monad (when)
import Control.Monad.Error (throwError)
import Control.Exception (tryJust)
import Data.List (intercalate)
import Data.Time
import System.Directory (copyFile)
import System.Environment (getEnv, getArgs)
import System.IO.Error (isDoesNotExistError)
import System.Random (getStdGen, randoms)
import Text.Parsec hiding (spaces,newline)
import Text.Parsec.String (Parser, parseFromFile)

data TodoAtom = Description String |
                Date Day |
                Tags [String] |
                Repeat Integer |
                Active Bool |
                Mark Bool
                deriving Show


data Todo = Todo {
    todoDescription :: String,
    todoDate :: Day,
    todoTags :: [String],
    todoRepeat :: Integer,
    todoActive :: Bool,
    todoMark :: Bool
} deriving Eq

type TodoList = [Todo]
type Tag = String
type TodoError = Either String

parseDescription :: Parser TodoAtom
parseDescription = Description <$> manyTill anyChar (try (string " | "))

parseDate :: Parser TodoAtom
parseDate = Date <$> stringToDay <$> (string "date=" *> many1 (digit <|> char '-'))

stringToDay :: String -> Day
stringToDay str = fromGregorian (read y) (read m) (read d)
    where (y:m:d:[]) = split '-' str

split :: Char -> String -> [String]
split delim = foldr split' [[]]
    where split' x acc = if x == delim then []:acc else (x : head acc) : tail acc


parseTags :: Parser TodoAtom
parseTags = Tags <$> (string "tag=" *> sepBy (many1 letter) (char ','))

parseRepeat :: Parser TodoAtom
parseRepeat = Repeat <$> read <$> (string "repeat=" *> many1 digit)

parseActive :: Parser TodoAtom
parseActive = Active <$> (== '1') <$> (string "active=" *> digit)

parseMark :: Parser TodoAtom
parseMark = Mark <$> (== '1') <$> (string "mark=" *> digit)

parseTodo :: Parser Todo
parseTodo = atomListToTodo <$> ((:) <$> (parseDescription <* spaces) <*> (sepBy (choice [parseDate,parseTags,parseRepeat,parseActive,parseMark]) spaces <* (newline <|> eof)))

atomListToTodo :: [TodoAtom] -> Todo
atomListToTodo xs = Todo {
                        todoDescription = head desc,
                        todoDate = head date,
                        todoTags = head tags,
                        todoRepeat = if null repeat then 0 else head repeat,
                        todoActive = if null active then False else head active,
                        todoMark = if null mark then False else head mark
                    }
    where desc = [x | Description x <- xs]
          date = [x | Date x <- xs]
          tags = [x | Tags x <- xs] 
          repeat = [x | Repeat x <- xs]
          active = [x | Active x <- xs]
          mark = [x | Mark x <- xs]


parseLines :: Parser TodoList
parseLines = many1 parseTodo

spaces :: Parser ()
spaces = () <$ many (char ' ')

newline :: Parser ()
newline = () <$ char '\n'

readTodoFile :: IO (TodoError TodoList)
readTodoFile = do
    gen <- getStdGen
    let randomStr = map (['A'..'Z'] !!) $ map (`mod` 26) $ take 5 (randoms gen :: [Int])
    let tmpFile = "/tmp/todo-backup." ++ randomStr
    todoFile' <- todoFile
    copyFile todoFile' tmpFile
    p <- parseFromFile parseLines tmpFile
    case p of
        Left x -> return $ throwError $ show x
        Right x -> return $ return $ x

todoColor :: Day -> Todo -> String
todoColor today todo =
    case () of _
                 | daysSince <= 7 -> blue
                 | daysSince <= 21 -> green
                 | otherwise -> red
    where daysSince = diffDays today $ todoDate todo

underlinedRed = "\27[4;1;31m"
red = "\27[0;1;31m"
--orange = "\27[0;33m"
--yellow = "\27[0;1;33m"
green = "\27[0;1;32m"
blue = "\27[0;1;36m"
normalColor = "\27[0m"

-- printList, fileUpdateBool, printAllBool

data OutInfo = OutInfo {
    outList :: [Bool],
    outFile :: Bool,
    outAll :: Bool
}


insert :: Todo -> TodoList -> TodoList
insert newTodo = fst . foldr f ([],False)
    where f oldTodo (newList, inserted) = if inserted == False && (todoDate newTodo >= todoDate oldTodo)
                                            then (oldTodo:newTodo:newList, True)
                                            else (oldTodo:newList, inserted)
--TODO: maybe use a Writer monad or similar to store the stdout output
--TODO: insert sorted?
--TODO: use that compiler flag that allows alternate syntax to _ _ _
add :: [String] -> Day -> Tag -> TodoList -> TodoError (TodoList, OutInfo)
add [] _ _ _ = throwError "missing todo"
add args today tag todos = return (newTodoList, outInfo)
    where newTodo = Todo {
                           todoDescription = intercalate " " args,
                           todoDate = today,
                           todoTags = [tag],
                           todoRepeat = 0,
                           todoActive = False,
                           todoMark = False
                         }
          newTodoList = insert newTodo todos
          outInfo = OutInfo {
              outList = map (== newTodo) newTodoList,
              outFile = True,
              outAll = False
          }

rm :: [String] -> Day -> Tag -> TodoList -> TodoError (TodoList, OutInfo)
rm [] _ _ _ = throwError "missing line #"
rm (_:_:_) _ _ _ = throwError "too many args"
rm args today tag todos
    | arg >= length todos = throwError "bad line #"
    | otherwise = return (newTodoList, outInfo)
    where arg = read (head args) - 1
          (xs,todo:ys) = splitAt arg todos
          newTodoList = xs ++ ys ++ if todoRepeat todo == 0 then [] else [newTodo]
          newTodo = todo { todoDate = newDay, todoMark = False }
          newDay = addDays (todoRepeat todo) today
          outInfo = OutInfo {
              outList = mapRelevant today tag newTodoList,
              outFile = True,
              outAll = False
          }

touch :: [String] -> Day -> Tag -> TodoList -> TodoError (TodoList, OutInfo)
touch [] _ _ _ = throwError "missing line #"
touch (_:_:_) _ _ _ = throwError "too many args"
touch args today tag todos
    | arg >= length todos = throwError "bad line #"
    | otherwise = return (newTodoList, outInfo)
    where arg = read (head args) - 1
          (xs,todo:ys) = splitAt arg todos
          newTodo = todo { todoDate = today, todoActive = True, todoMark = False }
          newTodoList = insert newTodo $ xs ++ ys
          outInfo = OutInfo {
              outList = mapRelevant today tag newTodoList,
              outFile = True,
              outAll = False
          }

mv :: [String] -> TodoList -> TodoError (TodoList, OutInfo)
mv [] _ = throwError "missing line #"
mv [_] _ = throwError "missing description"
mv args todos
    | index >= length todos = throwError "bad line #"
    | otherwise = return (newTodoList, outInfo)
    where index = read (head args) - 1
          (xs,todo:ys) = splitAt index todos
          newTodoList = xs ++ newTodo:ys
          newTodo = todo { todoDescription = intercalate " " $ tail args }
          outInfo = OutInfo {
              outList = map (== newTodo) newTodoList,
              outFile = True,
              outAll = False
          }

changeTags :: [String] -> TodoList -> TodoError (TodoList, OutInfo)
changeTags [] _ = throwError "missing line #"
changeTags [_] _ = throwError "missing tags"
changeTags (_:_:_:_) _ = throwError "too many args"
changeTags args todos
    | index >= length todos = throwError "bad line #"
    | otherwise = return (newTodoList, outInfo)
    where index = read (head args) - 1
          (xs,todo:ys) = splitAt index todos
          newTags = split ',' $ last args
          newTodo = todo { todoTags = newTags }
          newTodoList = xs ++ newTodo:ys
          outInfo = OutInfo {
              outList = map (== newTodo) newTodoList,
              outFile = True,
              outAll = True
          }

changeRepeat :: [String] -> TodoList -> TodoError (TodoList, OutInfo)
changeRepeat [] _ = throwError "missing line #"
changeRepeat [_] _ = throwError "missing repeat value"
changeRepeat (_:_:_:_) _ = throwError "too many args"
changeRepeat args todos
    | index >= length todos = throwError "bad line #"
    | otherwise = return (newTodoList, outInfo)
    where index = read (head args) - 1
          (xs,todo:ys) = splitAt index todos
          newRepeat = read $ last args
          newTodo = todo { todoRepeat = newRepeat }
          newTodoList = xs ++ newTodo:ys
          outInfo = OutInfo {
              outList = map (== newTodo) newTodoList,
              outFile = True,
              outAll = True
          }
 
--TODO: DRY it up, lots of repeating in these functions
mark :: [String] -> Tag -> TodoList -> TodoError (TodoList, OutInfo)
mark args tag todos
    | null args = return (todos, outInfo)
    | any (>= length todos) indexes = throwError "bad line #"
    | otherwise = return (newTodoList, outInfo)
    where indexes = map (pred . read) args
          newTodoList = zipWith f [0..] todos
          f index todo = if index `elem` indexes
                            then todo { todoMark = not $ todoMark todo }
                            else todo
          outInfo = OutInfo {
              outList = map (\x -> todoMark x && tag `elem` todoTags x) newTodoList,
              outFile = not $ null args,
              outAll = False
          }

listRelevant :: Day -> Tag -> TodoList -> TodoError (TodoList, OutInfo)
listRelevant today tag todos = return (todos, outInfo)
    where outInfo = OutInfo {
        outList = mapRelevant today tag todos,
        outFile = False,
        outAll = False
    }

listAll :: [String] -> TodoList -> TodoError (TodoList, OutInfo)
listAll args todos
    | length args > 1 = throwError "too many args"
    | index > length todos = throwError "bad line #"
    | otherwise = return (todos, outInfo)
    where index = if null args then 0 else read (head args) - 1
          (_,t:_) = splitAt index todos
          outInfo = OutInfo {
            outList = if null args then map (const True) todos else map (== t) todos,
            outFile = False,
            outAll = True
            }

--http://www.scs.stanford.edu/11au-cs240h/notes/zipper-slides.html#(23)
--TODO: what is a zipper and would it be useful.  and alos look at common monads like reader, writer, state, lenses, etc
mapRelevant :: Day -> Tag -> TodoList -> [Bool]
mapRelevant today tag todos = map isRelevant todos
    where isRelevant x = todoDate x <= today && tag `elem` (todoTags x)

--todoFile = "/Users/joshpoimboeuf/Dropbox/todo.txt"
todoFile :: IO String
todoFile = do
    homeDir <- getEnv "HOME"
    return $ homeDir ++ "/Dropbox/todo.txt"

todoToOutputStr :: Int -> Day -> Bool -> Todo -> String
todoToOutputStr index today doAll t =
    (if todoRepeat t > 0 then underlinedRed else todoColor') ++
    (if todoMark t then "*" else " ") ++
    todoColor' ++ " " ++
    (show index) ++ " " ++
    (if doAll then todoToFileStr t else todoDescription t) ++
    normalColor
    where todoColor' = todoColor today t

todoToFileStr :: Todo -> String
todoToFileStr t =
    (todoDescription t) ++
    " | date=" ++ (showGregorian $ todoDate t) ++
    " tag=" ++ (intercalate "," $ todoTags t) ++
    (if todoRepeat t > 0 then " repeat=" ++ show (todoRepeat t) else "") ++
    (if todoActive t then " active=1" else "") ++
    (if todoMark t then " mark=1" else "")


outputTodos :: Day -> (TodoList, OutInfo) -> IO ()
outputTodos today (todos, outInfo) = do
    todoFile' <- todoFile
    putStr $ unlines $ map todoToOutputStr' $ filter snd3 $ zip3 [1..] (outList outInfo) todos
    when (outFile outInfo) (writeFile todoFile' $ unlines $ map todoToFileStr todos)
    where
        todoToOutputStr' (index, _, todo) = todoToOutputStr index today (outAll outInfo) todo
        snd3 (_, x, _) = x


processArgs :: [String] -> Day -> Tag -> TodoList -> TodoError (TodoList, OutInfo)
processArgs args today tag =
    case args of
        [] -> listRelevant today tag
        ("all":argsTail) -> listAll argsTail
        ("add":argsTail) -> add argsTail today tag
        ("rm":argsTail) -> rm argsTail today tag
        ("touch":argsTail) -> touch argsTail today tag
        ("mv":argsTail) -> mv argsTail
        ("tag":argsTail) -> changeTags argsTail
        ("repeat":argsTail) -> changeRepeat argsTail
        ("mark":argsTail) -> mark argsTail tag
        _ -> const $ throwError "bad command"

main = do
    now <- getZonedTime
    let today = localDay $ zonedTimeToLocalTime $ now

    tag' <- tryJust (return . isDoesNotExistError) (getEnv "TAG")
    let tag = either (const "work") id tag'

    oldTodos <- readTodoFile

    args <- getArgs

    let newTodos = oldTodos >>= processArgs args today tag
    case newTodos of
        Left x -> putStrLn x
        Right x -> outputTodos today x


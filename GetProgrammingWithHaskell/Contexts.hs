import qualified Data.Map as Map

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = mconcat ["Hello, ", name, "!"]

helloName :: IO ()
helloName =
  askForName >> getLine >>= (\name -> return $ nameStatement name) >>= putStrLn

doHelloName :: IO ()
doHelloName = do
  askForName
  name <- getLine
  putStrLn $ nameStatement name

--

data Grade = F | D | C | B | A deriving (Enum, Eq, Ord, Read, Show)

data Degree = HS | BA | MS | PhD deriving (Enum, Eq, Ord, Read, Show)

data Candidate = Candidate
  { candidateId :: Int
  , codeReview :: Grade
  , cultureFit :: Grade
  , education :: Degree } deriving Show

viable :: Candidate -> Bool
viable candidate = all
  (== True)
  [ codeReview candidate > B
  , cultureFit candidate > C
  , education candidate >= MS
  ]

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readGradeDo :: IO Grade
readGradeDo = do
  grade <- getLine
  return $ read grade


readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readCandidate :: IO Candidate
readCandidate = do
  putStrLn "enter id:"
  id <- readInt
  putStrLn "enter code grade:"
  codeGrade <- readGrade
  putStrLn "enter culture fit grade:"
  cultureGrade <- readGrade
  putStrLn "enter education:"
  degree <- readDegree
  return $ Candidate
    { candidateId = id
    , codeReview  = codeGrade
    , cultureFit  = cultureGrade
    , education   = degree
    }

assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed    = viable candidate
  let statement = if passed then "passed" else "failed"
  return statement

--

candidate1 :: Candidate
candidate1 =
  Candidate {candidateId = 1, codeReview = A, cultureFit = A, education = BA}

candidate2 :: Candidate
candidate2 =
  Candidate {candidateId = 2, codeReview = C, cultureFit = A, education = PhD}

candidate3 :: Candidate
candidate3 =
  Candidate {candidateId = 3, codeReview = A, cultureFit = B, education = MS}

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1, candidate1), (2, candidate2), (3, candidate3)]

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe id = do
  candidate <- Map.lookup id candidateDB
  let passed    = viable candidate
  let statement = if passed then "passed" else "failed"
  return statement

passFail :: Maybe String -> String
passFail (Just result) = result
passFail Nothing       = "error, id not found"

--

candidates :: [Candidate]
candidates = [candidate1, candidate2, candidate3]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed    = viable candidate
  let statement = if passed then "passed" else "failed"
  return statement

-- alt
assessCandidates :: [Candidate] -> [String]
assessCandidates candidates =
  map (\passed -> if passed then "passed" else "failed") $ map viable candidates

--

-- works on IO, Maybe, and List
assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidates = do
  candidate <- candidates
  let passed    = viable candidate
  let statement = if passed then "passed" else "failed"
  return statement

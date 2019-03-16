import Control.Applicative
import Control.Monad

data Name = Name
  { firstName :: String
  , lastName :: String
  }

instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

data GradeLevel = Freshman | Sophomore | Junior | Senior deriving (Enum, Eq, Ord, Show)

data Student = Student
  { studentId :: Int
  , gradeLevel :: GradeLevel
  , studentName :: Name
  } deriving Show

students :: [Student]
students =
  [ (Student 1 Senior (Name "Audre" "Lorde"))
  , (Student 2 Junior (Name "Leslie" "Silko"))
  , (Student 3 Freshman (Name "Judith" "Butler"))
  , (Student 4 Senior (Name "Guy" "Debord"))
  , (Student 5 Sophomore (Name "Jean" "Baudrillard"))
  , (Student 6 Junior (Name "Julia" "Kristeva"))
  ]

-- _select :: (a -> b) -> [a] -> [b]
_select :: Monad m => (a -> b) -> m a -> m b
_select prop vals = do
  val <- vals
  return $ prop val

-- _where :: (a -> Bool) -> [a] -> [a]
_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where test vals = do
  val <- vals
  guard $ test val
  return val

startsWith :: Char -> String -> Bool
startsWith char string = char == (head string)
-- startsWith char string = (==) char $ head string
-- startsWith c (x:_) = c == x

data Teacher = Teacher
  { teacherId :: Int
  , teacherName :: Name
  } deriving Show

teachers :: [Teacher]
teachers =
  [ Teacher 100 (Name "Simone" "De Beauvoir")
  , Teacher 200 (Name "Susan" "Sontag")
  ]

data Course = Course
  { courseId :: Int
  , courseTitle :: String
  , teacher :: Int
  } deriving Show

courses :: [Course]
courses = [Course 101 "French" 100, Course 201 "English" 200]

-- _join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a, b)]
_join
  :: (Monad m, Alternative m, Eq c)
  => m a
  -> m b
  -> (a -> c)
  -> (b -> c)
  -> m (a, b)
_join data1 data2 prop1 prop2 = do
  d1 <- data1
  d2 <- data2
  let dpairs = (d1, d2)
  guard $ (prop1 $ fst dpairs) == (prop2 $ snd dpairs)
  return dpairs

_hinq selectQuery joinQuery whereQuery =
  (\joinData -> (\whereResult -> selectQuery whereResult) (whereQuery joinData))
    joinQuery

-- takes a Monad, data type, result type
-- HINQ data constructor takes 3 functions: _select, _join (or plain data), _where
-- HINQ_ omits the _where type signature
data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a)
                | HINQ_ (m a -> m b) (m a)

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause $ _where (\_ -> True)

query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ (_select $ teacherName . fst)
              (_join teachers courses teacherId teacher)
              (_where ((== "English") . courseTitle . snd))

query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName) teachers

possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 = HINQ (_select $ teacherName . fst)
                   (_join possibleTeacher possibleCourse teacherId teacher)
                   (_where $ (== "French") . courseTitle . snd)

missingCourse :: Maybe Course
missingCourse = Nothing

maybeQuery2 :: HINQ Maybe (Teacher, Course) Name
maybeQuery2 = HINQ (_select $ teacherName . fst)
                   (_join possibleTeacher missingCourse teacherId teacher)
                   (_where $ (== "French") . courseTitle . snd)

data Enrollment = Enrollment
  { student :: Int
  , course :: Int
  } deriving Show

enrollments :: [Enrollment]
enrollments =
  [ Enrollment 1 101
  , Enrollment 2 101
  , Enrollment 2 201
  , Enrollment 3 101
  , Enrollment 4 201
  , Enrollment 4 101
  , Enrollment 5 101
  , Enrollment 6 201
  ]

studentEnrollmentsQ :: HINQ [] (Student, Enrollment) (Name, Int)
studentEnrollmentsQ = HINQ_
  (_select (\(student, enrollment) -> (studentName student, course enrollment)))
  (_join students enrollments studentId student)

studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQ

englishStudentsQ :: HINQ [] ((Name, Int), Course) Name
englishStudentsQ = HINQ (_select $ fst . fst)
                        (_join studentEnrollments courses snd courseId)
                        (_where $ (== "English") . courseTitle . snd)

englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQ

getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
 where
  courseQuery = HINQ (_select $ fst . fst)
                     (_join studentEnrollments courses snd courseId)
                     (_where $ (== courseName) . courseTitle . snd)

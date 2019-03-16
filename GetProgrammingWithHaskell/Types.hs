halve :: Int -> Int
halve = (`div` 2)

printDouble :: Int -> String
printDouble = show . (* 2)

type FirstName = String
type LastName = String
type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l            ) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

data Sex = Male | Female deriving (Read, Show)

data RhType = Pos | Neg deriving (Read, Show)
data ABOType = A | AB | B | O deriving (Read, Show)
data BloodType = BloodType ABOType RhType deriving (Read, Show)

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _                = True
canDonateTo _               (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A  _) = True
canDonateTo (BloodType B _) (BloodType B  _) = True
canDonateTo _               _                = False

data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType
                       }

me :: Patient
me = Patient
  { name      = NameWithMiddle "Anderson" "McCall" "Cook"
  , sex       = Male
  , age       = 36
  , height    = 72
  , weight    = 170
  , bloodType = BloodType O Pos
  }

patientCanDonateTo :: Patient -> Patient -> Bool
patientCanDonateTo p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

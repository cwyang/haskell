import Control.Applicative

type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

ageOkay age = case age >= 0 of
  True -> Right age
  False -> Left [AgeTooLow]
nameOkay name = case name /= "" of
  True -> Right name
  False -> Left [NameEmpty]

--mkPerson :: Name -> Age -> Validation [PersonInvalid] Person
mkPerson name age =
  liftA2 Person (nameOkay name) (ageOkay age)

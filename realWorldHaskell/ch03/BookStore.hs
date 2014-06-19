data BookInfo = Book Int String [String]
                deriving (Show)
-- Type constructor -- Value Constructor     
data MagazineInfo = Magazine Int String [String]
                    deriving (Show)
                             
myInfo = Book 9760135062455 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

-- in Haskell names of types and values are independent of each other
-- type synonyms
type CustomerID = Int
type ReviewBody = String

data BookReview = BookReview BookInfo CustomerID ReviewBody

-- Algebraic Data types can have more than one constructor
-- ex. data Bool = False | True

type CardHolder = String
type CardNumber = String
type Address = [String]

-- three ways to bill cutsomer
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)
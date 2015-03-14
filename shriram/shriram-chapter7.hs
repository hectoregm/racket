scoreToLetter :: Int -> Char
scoreToLetter n
    | n > 90 = 'A'
    | n > 80 = 'B'
    | n > 70 = 'C'
    | otherwise = 'F'

len [] = 0
len (x:s) = 1 + len s

listCopy [] = []
listCopy (x:s) = x : listCopy s

ones = 1 : ones

front :: Int -> [a] -> [a]
front _ [] = []
front 0 (x:s) = []
front n (x:s) = x : front (n-1) s

tB :: [String] -> [Int] -> [(String, Int)]
tB [] _ = []
tB (f:fs) (b:bs) = (f,b) : tB fs bs

timeBonuses finishers =
  tB finishers ([20, 12, 8] ++ cycle[0])

zipOp :: (a -> b -> c) -> [a] -> [b] -> [c]
zipOp f [] _ = []
zipOp f _ [] = []
zipOp f (a:as) (b:bs) = (f a b) : zipOp f as bs

myZip = zipOp (\ a b -> (a,b))

fibs = 1 : 1 : zipOp (+) fibs (tail fibs)

type Identifier = String
type Value = Int

type Env = [(Identifier, Value)]
data WAE = Num Int
         | Add WAE WAE
         | Id Identifier
         | With Identifier WAE WAE

mlookup :: Identifier -> Env -> Value
mlookup var ((i,v):r)
  | (var == i) = v
  | otherwise = mlookup var r

extend :: Env -> Identifier -> Value -> Env
extend env i v = (i,v):env

--interp :: WAE -> Env -> Value
interp (Num n) env = n
interp (Add lhs rhs) env = interp lhs env + interp rhs env
interp (Id i) env = mlookup i env
interp (With bound_id named_expr bound_body) env =
           interp bound_body
           (extend env bound_id (interp named_expr env))

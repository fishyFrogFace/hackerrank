import Text.Printf (printf)

data Interval = Interval { left :: Double
                         , right :: Double
                         } deriving (Show)

data Function = Function { a :: [Double]
                         , b :: [Double]
                         } deriving (Show)

value :: Function -> Double -> Double 
value (Function [] _) x  = 0
value (Function _ []) x = 0
value (Function (y:ys) (z:zs)) x = y*x**z + value (Function ys zs) x 

coef :: Double -> Double -> Double
coef a b = a/(b+1) 

integrate :: Function -> Function
integrate (Function a b) = Function (zipWith coef a b) (map (+1) b)   

defint :: Function -> Interval -> Double
defint func (Interval l r) = value func r - value func l

square :: Function -> Function
square (Function a b) = Function [i*n | i <- a, n <- a] [i+n | i <- b, n <- b]


-- This function should return a list [area, volume].
solve :: Function -> Interval -> [Double]
solve func interval = [defint (integrate func) interval
                             , pi*defint (integrate (square func)) interval
                             ] 

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). 
                           (\[a, b, [l, r]] 
                           -> solve (Function a b) (Interval l r)). 
                           map (map read. words). lines

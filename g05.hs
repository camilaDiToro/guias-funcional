import Data.Sequence (Seq(Empty), empty, singleton)


--- Ejercicio 7
data Set a = S (a -> Bool)

belongs (S f) = f 
empty = S (const False)
singleton x = S (== x)
union (S f1) (S f2) = S (\x -> f1 x || f2 x)
intersection (S f1) (S f2) = S (\x -> f1 x && f2 x)


--- Ejercicio 8
data MayFail a = Raise Exception | Ok a
data Exception = DivByZero | NotFound | NullPointer| Other String
type ExHandler a = Exception -> a

tryCatch (Raise e) _ exHandler = exHandler e
tryCatch (Ok a) nextStep _ = nextStep a 


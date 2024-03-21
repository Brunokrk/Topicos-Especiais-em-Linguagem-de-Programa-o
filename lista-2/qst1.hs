-- Aluno : Bruno Marchi Pires

data SigBin = Neg Bin | Pos Bin deriving (Eq) --Binários Sinalizados
data Bin = V | Z Bin | U Bin deriving (Eq)  -- Binários sem sinal

--Classe Show apresentando Binário como String
instance Show SigBin where
    show (Pos b) = show b
    show (Neg b) = "-" ++ show b --negativos

instance Show Bin where
    show V = ""
    show (Z b) = show b ++ "0"
    show (U b) = show b ++ "1"

--Métodos Classe Num (https://hackage.haskell.org/package/base-4.19.1.0/docs/GHC-Num.html)
-- Interessante transformar binário em int para facilitar (+)(-)(*)
-- (+)
-- (*)
-- (-)
-- (abs)
-- (fromInteger)
-- (signum)

-- Transformação em Inteiro-----
toIntegerSigBin :: SigBin -> Integer
toIntegerSigBin (Pos b) = toIntegerBin b
toIntegerSigBin (Neg b) = - (toIntegerBin b)

toIntegerBin :: Bin -> Integer
toIntegerBin V = 0
toIntegerBin (Z b) = 2 * toIntegerBin b
toIntegerBin (U b) = 1 + 2 * toIntegerBin b
----------------------------------


instance Num SigBin where
    (+) a b = fromInteger (toIntegerSigBin a + toIntegerSigBin b)
    (*) a b = fromInteger (toIntegerSigBin a * toIntegerSigBin b)
    (-) a b = fromInteger (toIntegerSigBin a - toIntegerSigBin b)
    abs (Neg b) = Pos b
    abs p = p
    signum (Pos _) = 1
    signum (Neg _) = -1
    fromInteger n 
        | n < 0     = Neg (fromIntegerBin (abs n))
        | otherwise = Pos (fromIntegerBin n)

fromIntegerBin :: Integer -> Bin
fromIntegerBin 0 = V
fromIntegerBin n 
    | even n    = Z (fromIntegerBin (div n 2))
    | otherwise = U (fromIntegerBin (div n 2))

main :: IO ()
--Após compilar, chamar "main"
main = do
    let num1 = Pos (U (Z (U V))) -- 101 , 5 
    let num2 = Neg (U (U V))    -- -11 , -3 
    
    putStrLn "Testes de conversão:"
    print (toIntegerSigBin num1)
    print (toIntegerSigBin num2) 
    print (fromInteger 5 :: SigBin)
    print (fromInteger (-3) :: SigBin)
    
    putStrLn "\nTestando métodos da classe"
    print (num1 + num2) 
    print (num1 - num2) 
    print (num1 * num2) 
    print (abs num2)
    print (signum num1) 
    print (signum num2) 

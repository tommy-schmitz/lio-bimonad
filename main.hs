data Label
data Labeled a
data LIO a
data LIORef a

instance Monad LIO where
  return = undefined
  (>>=) = undefined

label :: Label -> a -> LIO (Labeled a)
label = undefined
unlabel :: Labeled a -> LIO a
unlabel = undefined
toLabeled :: Label -> LIO a -> LIO (Labeled a)
toLabeled = undefined
newLIORef :: Label -> a -> LIO (LIORef a)
newLIORef = undefined
readLIORef :: LIORef a -> LIO a
readLIORef = undefined
writeLIORef :: LIORef a -> a -> LIO ()
writeLIORef = undefined
labelOf :: Labeled a -> Label
labelOf = undefined

bottomLabel :: Label
bottomLabel = undefined


-- Syntax.
data Term =
    Var String                 -- Lambdas
  | Lam String Term
  | App Term Term
  | Const Value                -- Constants

data NewLabeled a =
    NL0 a
  | NL1 (Labeled (NewLabeled a))
{-
instance Monad NewLabeled where
  return = NL0
  NL0 a >>= c = c a
  NL1 lna >>= c = do
    k <- labelOf lna
    lnb <- toLabeled k $ do
      na <- unlabel lna
      na >>= c
    return $ NL1 lnb
-}

openUp :: NewLabeled a -> (a -> LIO (NewLabeled b)) -> LIO (NewLabeled b)
openUp (NL0 a) c = c a
openUp (NL1 lna) c = do
  let k = labelOf lna
  lnb <- toLabeled k $ do
    na <- unlabel lna
    openUp na c
  return $ NL1 lnb

-- Runtime data structures.
data RawValue =
    CharVal Char               -- Characters
  | LocVal (LIORef Value)      -- Mutable references
  | FnVal (Value -> Action)    -- Functions
type Value  = NewLabeled RawValue
type Action = LIO Value
type Env    = String -> Value

-- Helper functions.
extend :: Env -> String -> Value -> Env
extend e x v y | x == y    = v
               | otherwise = e y

eval :: Env -> Term -> Action
eval e (Var x) = return $ e x
eval e (Lam x t) = return $ NL0 $ FnVal f where
  f v = eval (extend e x v) t
eval e (App t1 t2) = do
  v1 <- eval e t1
  v2 <- eval e t2
  openUp v1 $ \(FnVal f1) ->
    f1 v2
eval e (Const v) = return v

{-
-- Interpreter.
eval :: Env -> Term -> Action
eval e (Var x)     = return $ e x
eval e (Lam x t)   = return $ return $ FnVal $ \v -> eval (extend e x v) t
eval e (App t1 t2) = do v1 <- eval e t1             -- working in FIO monad
                        v2 <- eval e t2
                        prod $ do
                          FnVal f <- v1             -- working in Faceted monad
                          return $ f v2
eval e (Const v)   = return v

-- Constants.
makeHighSecurity :: RawValue
makeHighSecurity =
  FnVal $ \v ->
    return $ makeFaceted "H" v bottom
ref :: RawValue
ref =
  FnVal $ \v -> do                                  -- working in FIO monad
    ref <- newFIORef v
    return $ return $ LocVal ref
deref :: RawValue
deref =
  FnVal $ \v -> prod $ do                           -- working in Faceted monad
    LocVal ref <- v
    return $ readFIORef ref
assign :: RawValue
assign =
  FnVal $ \v1 ->
    return $ return $ FnVal $ \v2 -> prod $ do      -- working in Faceted monad
      LocVal ref <- v1
      rv2 <- v2
      return $ do                                   -- working in FIO monad
        writeFIORef ref v2
        return $ return rv2
printChar :: RawValue
printChar =
  FnVal $ \v -> prod $ do                           -- working in Faceted monad
    CharVal c <- v
    return $ do                                     -- working in FIO monad
      h <- openFileF [] "output.txt" AppendMode
      hPutCharF h (return c)
      hCloseF h
      return $ return $ CharVal c
-}

------------------------------
-- Main program. (unimportant)

main = return ()

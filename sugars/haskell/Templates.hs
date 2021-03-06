{-# LANGUAGE TemplateHaskell #-}
module Templates where
  import Language.Haskell.TH
  import Data.Char (ord, chr)

  {- Test desugaring order -}
    
  incChar :: Char -> Char
  incChar c = chr ((ord c) + 1)

  inner :: Q Exp -> Q Exp
  inner exp = do
    exp <- exp
    return $ case exp of -- notice the deconstruction
      LitE (CharL c) -> LitE (CharL (incChar c))
      exp -> exp

  outer :: Q Exp -> Q Exp
  outer exp = do
    exp <- exp
    return $ case exp of
      LitE (CharL c) -> LitE (CharL (incChar (incChar c)))
      exp -> exp

  -- Haskell Templates are scope unsafe. See that this template only
  -- produces an error when it is used, not when it is defined.
  scopeUnsafe :: Q Exp
  scopeUnsafe = do
    x <- newName "x"
    return (VarE x)

  -- Haskell Templates are type unsafe. See that this template only
  -- produces an error when it is used, not when it is defined.
  typeUnsafe :: Q Exp
  typeUnsafe = do
    plus <- [| (+) |]
    return $ AppE (AppE plus (LitE (IntegerL 1))) (LitE (CharL '2'))

  -- Haskell Templates are AST safe. See that this does not compile:
  -- astSafe :: Q Exp -> Q Stmt
  -- astSafe exp = exp

  -- Haskell Templates can abstract over any sort
  power :: Q Stmt
  power = do
    name <- newName "x"
    return $ LetS [ValD (VarP name) (NormalB (LitE (CharL 'a'))) []]

  -- Checking if quotations can be nested. (It's easy to construct, but hard to run?)
  phases :: Q (Q Exp)
  phases = return $ return $ LitE (StringL "two phases")

  -- Can use either abstract or concrete syntax
  testAbstract :: Q Exp
  testAbstract = return $ AppE (AppE (VarE '(+)) (LitE (IntegerL 1))) (LitE (IntegerL 2))

  testConcrete :: Q Exp
  testConcrete = [| 1 + 2 |]

  -- Hygiene test
  unhygienic :: Q Pat -> Q Exp -> Q Exp
  unhygienic v body =
    [| let x = "macro" in
       let $(v) = "user" in
       do
         putStrLn ("macro->" ++ show x)
         let x = "macro" in $(body)
    |]

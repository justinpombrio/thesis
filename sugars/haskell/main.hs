{-# LANGUAGE TemplateHaskell #-}
import Templates

main :: IO ()
main = do
  {- Test desugaring order -}
  -- The fact that this succeeds shows that templates are desugared in IO order.
  putStrLn $ show $(outer (inner [|'a'|]))
  
  -- Haskell Templates are scope unsafe:
  --   putStrLn $(scopeUnsafe)
  
  -- Haskell Templates are type unsafe:
  --   putStrLn $ show $(typeUnsafe)

  -- Haskell Templates are syntax safe.
  
  -- Haskell Templates don't allow nested evaluation (at least not easily)
  -- putStrLn $ $($(phases)) -- Type error
  
  -- Haskell Templates aren't always hygienic
  $(unhygienic [p| x|] [| putStrLn $ "user->" ++ show x |])

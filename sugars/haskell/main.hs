{-# LANGUAGE TemplateHaskell #-}
import Templates

main :: IO ()
main = do
  {- Test evaluation order -}
  -- The fact that this succeeds shows that templates are evaluated in IO order.
  putStrLn $ show $(outer (inner [|'a'|]))
  -- Haskell Templates are scope unsafe:
  --   putStrLn $(scopeUnsafe)
  -- Haskell Templates are type unsafe:
  --   putStrLn $ show $(typeUnsafe)
  -- Haskell Templates are AST safe.

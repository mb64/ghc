-- test reification of local definitions
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Syntax
import System.IO

-- Sidestep the staging restriction
-- printTypeOf :: String -> Q ()
#define printTypeOf(n) (addModFinalizer $ do \
                       { VarI _ t _ <- reify (mkName (n)) \
                       ; runIO $ hPutStrLn stderr (show t) \
                       })

main :: IO ()
main = print (f 1 "", g 'a' 2, h True 3)
  where
    f x y = ( x :: Int
            , let ff $(do printTypeOf("y")
                          [p| z |]
                      ) = z :: $(do printTypeOf("z")
                                    [t| () |]
                                )
               in $(do printTypeOf("x")
                       [| y :: String |]
                   )
            )
    g x y = ( $(do printTypeOf("x")
                   [| y :: Int |]
               )
            , x :: Char
            )
    h x y = ( $$(do printTypeOf("x")
                    [|| y :: Int ||]
                )
            , x :: Bool
            )

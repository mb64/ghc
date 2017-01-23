-- Tests that reify in nested splices can see definitions introduced
-- in brackets.
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module TH_reifyNestedSplice where

import Language.Haskell.TH.Syntax as TH
import System.IO

-- Sidestep the staging restriction
-- printTypeOf :: String -> Q ()
#define printTypeOf(n) (addModFinalizer $ do \
                       { VarI _ t _ <- reify (mkName (n)) \
                       ; runIO $ hPutStrLn stderr (n ++ " :: " ++ show t) \
                       })

foo :: ()
foo = $([| let x = True
               y = False
               z = False
            in $(do printTypeOf("x")
                    [| case x of
                         $(printTypeOf("y") >> [p| True |]) ->
                           () :: $( printTypeOf("z") >> [t| () |])
                         _      -> ()
                     |]
                )
        |])

$([d|
 bar :: ()
 bar = $([| let x1 = True
                y1 = False
                z1 = False
             in $(do printTypeOf("x1")
                     [| case x1 of
                          $(printTypeOf("y1") >> [p| True |]) ->
                            () :: $( printTypeOf("z1") >> [t| () |])
                          _      -> ()
                      |]
                 )
         |])

 |])


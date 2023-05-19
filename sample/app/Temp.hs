{-# LANGUAGE OverloadedStrings, DerivingStrategies, TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TypeApplications, DataKinds, FlexibleInstances, TypeFamilies, UndecidableInstances #-}
{-# Language DuplicateRecordFields #-}

module Temp where

import Data.Text
import Control.Lens
import Control.Lens.Combinators


data Company = Company {name :: String, owner :: String}

data Person = Person {name :: String, owner :: String}

display :: Company -> String
display c = "My company is " ++ c.name

temp :: IO ()
temp
  = do x <- return ()
       print "Hello"

-- ht-debug changeReturnPureToLet /Users/salokya.kumar/Work/sample/app Temp -package-db /nix/store/mfn9kvfsab6dys96j7hwdmmq82g3lhrn-ghc-8.8.4-with-packages/lib/ghc-8.8.4/package.conf.d -XTypeApplications -XDataKinds -XFlexibleInstances -XTypeFamilies -XUndecidableInstances -XDerivingStrategies -XOverloadedStrings -XTemplateHaskell -XMultiParamTypeClasses -XFunctionalDependencies -XDuplicateRecordFields -fplugin=RecordDotPreprocessor
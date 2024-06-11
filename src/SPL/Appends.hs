{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import Data.Typeable (Typeable, typeOf)

-- Our custom function
showOrId :: forall a. (Typeable a, Show a) => a -> String
showOrId x = 
  if typeOf x == typeOf (undefined :: String)
  then x
  else show x


(<+) :: Show b => a -> b -> String 
(<+) a b = showOrId a ++ showOrId b
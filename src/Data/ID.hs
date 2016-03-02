{-# LANGUAGE DeriveGeneric #-}
module Data.ID
  (
    ID(ID)
  , WithID(WithID,_object,__ID)
  , object,_ID
  , randomID
  ) where


import qualified Data.Serialize as C
import           Data.Serialize.Text   ()
import           Data.Text             (Text,pack,unpack)
import           Data.Typeable         (Typeable)
import           GHC.Generics          (Generic)
import           System.Random         (newStdGen,randomRs)


newtype ID a
  = ID Text
  deriving (Eq, Ord, Generic, Typeable, Show)

instance C.Serialize (ID a) where
instance (C.Serialize a) => C.Serialize (WithID a) where

data WithID a
  = WithID
    {
      __ID :: ID a
    , _object :: a
    }
  deriving (Eq, Generic, Typeable, Show)

object :: Functor f => (a -> f b) -> WithID a -> f (WithID b)
object f (WithID (ID t) a) = fmap (WithID (ID t)) (f a)

_ID :: Functor f => (ID a -> f (ID a)) -> WithID a -> f (WithID a)
_ID f (WithID i a) = fmap (flip WithID a) (f i)

randomID :: Int -> IO (ID a)
randomID l = ID . pack . take l . randomRs ('a','z') <$> newStdGen

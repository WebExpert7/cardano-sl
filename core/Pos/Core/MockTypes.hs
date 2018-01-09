{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
module Pos.Core.MockTypes(
  Text,
    Format,
    Raw,
    Builder,
--    Buildable,
    Millisecond,
    AddressHash,
    AddressHashPublicKey,
    Signature,
    Byte,
    Hash,
    HashMap,
    Lens',
    Some,
    Getter,
    MonadError,
    liftLensSome,
    PublicKey)
  where
--liftLensSome :: (forall a. c a => ALens' a b)
liftLensSome l = l
data Getter x5 a5 = Getter { x5:: Raw, a5 :: Raw }
data Lens' x3 a3 = Lens' { x2:: Raw, a3 :: Raw }
--data Some x4 = Some { x4:: Raw }
data Some c where
      Some :: c a => a -> Some c

data MonadError x6 e7 = MonadError { x6 :: Raw , e7 :: Raw }
data Some1 c a where
      Some1 :: c f => f a -> Some1 c a
      
data Signature x = Signature { sig :: Raw }
data Raw = Raw -- raw 
--data   Builder =   Builder
--data   Format =   Format

newtype Builder = Builder {
  field :: Raw
       }
newtype Format r a =
    Format {runFormat :: (Builder -> r) -> a}
    
data AbstractHashBlake2b_224 = AbstractHashBlake2b_224
data AddressHashPublicKey = AddressHashPublicKey
-- this is hack for  AddressHash PublicKey

data PublicKey = PublicKey

-- | Hash used to identify address.
type AddressHash = AbstractHashBlake2b_224

-- | Stakeholder identifier (stakeholders are identified by their public keys)


data HashMap f1 g1 = HashMap { f1 :: MyHash, g1 :: MyHash }
--data Text x8 = Text { x8 :: Raw }
data Text  = Text 

data MyHash = MyHash {
  v :: Raw
  }
  
--data Hash f g = Hash  { f :: MyHash, g :: MyHash }
data Hash f  = Hash  { f :: MyHash }
data Byte = Byte
data Millisecond = Millisecond

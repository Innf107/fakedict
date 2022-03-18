{-#LANGUAGE ScopedTypeVariables, KindSignatures, GADTs, ConstraintKinds, FunctionalDependencies, RankNTypes#-}
{-#LANGUAGE AllowAmbiguousTypes#-}
module Fakedict.Internal where

import Unsafe.Coerce (unsafeCoerce)

import Data.Kind (Constraint, Type)
import Data.Coerce (Coercible)

data Dict (c :: Constraint) where
    Dict :: c => Dict c

withLocal :: forall c a b d. (c a, Coercible a b) => (c b => d) -> d
withLocal x = case unsafeCoerce (Dict :: Dict (c a)) :: Dict (c b) of Dict -> x

data FakeDict d = FakeDict d

class FakeDictFor (c :: Constraint) (d :: Type) | d -> c

withFakeDictUnsafe :: forall c d a. d -> (c => a) -> a
withFakeDictUnsafe d x = case unsafeCoerce (FakeDict d) :: Dict c of
    Dict -> x

withFakeDict :: forall d c a. FakeDictFor c d => d -> (c => a) -> a
withFakeDict = withFakeDictUnsafe


module Fakedict (module Export) where

import Fakedict.Internal as Export (
        withLocal
    ,   withFakeDictUnsafe
    ,   withFakeDict
    )
import Fakedict.TH as Export (makeDict)

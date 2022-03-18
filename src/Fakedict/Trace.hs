{-#LANGUAGE TemplateHaskell, KindSignatures, GADTs, MultiParamTypeClasses, RankNTypes#-} -- Necessary extensions for @makeDict@
{-#LANGUAGE TypeApplications, ScopedTypeVariables, TypeFamilies, ConstraintKinds, OverloadedStrings#-}
module Fakedict.Trace (
    Trace
,   trace

,   traceId
,   traceShow
,   traceShowId
,   traceM
,   traceShowM
,   traceShowWith

,   runTrace
,   ignoreTrace
,   runTraceStderr
,   runTraceStderrWith
,   runTraceStderrFiltered
) where

import Fakedict

import qualified Debug.Trace

import Data.Text (Text, pack, unpack)


-- | Trace_ is the internal type class that actually carries the trace
-- implementation. It is not exported, to make it impossible to write top-level instances.
class Trace_ lvl where
    trace_ :: lvl -> Text -> a -> a

makeDict ''Trace_

-- | Trace is simply a wrapper for Trace_.
-- This way, Trace can safely be exported and used in constraints, but making an instance
-- of Trace_ is impossible, since it is *not* exported.
type Trace = Trace_

trace :: Trace lvl => lvl -> Text -> a -> a
trace = trace_

traceId :: Trace lvl => lvl -> Text -> Text
traceId lvl a = trace lvl a a

traceShow :: (Trace lvl, Show a) => lvl -> a -> b -> b
traceShow lvl = trace lvl . pack . show

traceShowId :: (Trace lvl, Show a) => lvl -> a -> a
traceShowId lvl a = trace lvl (pack (show a)) a

traceM :: (Trace lvl, Applicative f) => lvl -> Text -> f ()
traceM lvl msg = trace lvl msg $ pure ()

traceShowM :: (Trace lvl, Applicative f) => lvl -> Text -> f ()
traceShowM lvl = traceM lvl . pack . show

traceShowWith :: (Trace lvl, Show b) => lvl -> (a -> b) -> a -> a
traceShowWith lvl f x = traceShow lvl (f x) x

-- | Primitive for running computations with Trace constraints.
-- The argument is exactly the implementation of @trace@.
-- This is usually going to be some variation of @Debug.Trace.trace@
runTrace :: (forall a. lvl -> Text -> a -> a) -> (Trace lvl => b) -> b
runTrace impl = withFakeDict (Trace_Dict {_trace_ = impl})

-- | Ignore all calls to @trace@.
ignoreTrace :: (Trace lvl => b) -> b
ignoreTrace = runTrace (\_ _ x -> x)

-- | Write all traces to stderr.
-- This is just @Debug.Trace.trace@
runTraceStderr :: (Trace lvl => b) -> b
runTraceStderr = runTrace (\_ msg x -> Debug.Trace.trace (unpack msg) x)

-- | Write traces to stderr, but first apply a function.
-- If the given function returns `Nothing`, no traces are written.
--
-- > λ> runTraceStderrWith (\() x -> if x == "ignore this" then Nothing else Just ("[passed] " <> x)) $ trace () "ignore this" $ trace () "don't ignore this!" $ 5
-- > [passed] don't ignore this!
-- > 5
runTraceStderrWith :: (lvl -> Text -> Maybe Text) -> (Trace lvl => b) -> b
runTraceStderrWith f = runTrace $ \lvl msg x -> case f lvl msg of
                                                Nothing     -> x
                                                Just msg'   -> Debug.Trace.trace (unpack msg') x 

-- | Write traces to stdout whose trace level matches the
-- given predicate. 
runTraceStderrFiltered :: (lvl -> Bool) -> (Trace lvl => b) -> b
runTraceStderrFiltered pred = runTrace $ \lvl msg x -> if pred lvl 
                                                       then Debug.Trace.trace (unpack msg) x
                                                       else x

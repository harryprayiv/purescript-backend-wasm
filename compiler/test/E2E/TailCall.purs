-- | End-to-end test of **tail-call elimination** for direct (top-level) tail
-- | recursion. A direct call in tail position (`Let s (RCallKnown …) (Return s)`)
-- | is emitted as `return_call`, so the frame is replaced rather than grown — a
-- | tail-recursive chain runs in constant stack. Without it, `countdown 1_000_000`
-- | overflows the wasm stack (around 100k frames); with it, it returns.
-- |
-- | (Closure tail recursion — a `where go = …` helper — is *not* covered here; that
-- | needs `return_call_ref` or lifting the local function to top level.)
module Test.E2E.TailCall (spec) where

import Prelude

import Effect.Class (liftEffect)
import Test.E2E.Wasm (callI32x1, callI32x2, instantiateLinked)
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Tail-call elimination (e2e): direct tail recursion -> wasm -> run"
    $ before
        ( liftEffect
            ( instantiateLinked [ [ "TailRec" ] ]
                [ "compiler/test/fixtures/TailRec.corefn.json"
                , "compiler/test/fixtures/Data.Eq.corefn.json"
                , "compiler/test/fixtures/Data.Ring.corefn.json"
                , "compiler/test/fixtures/Data.Semiring.corefn.json"
                ]
            )
        )
    $ do
        -- 1_000_000 iterations: overflows without TCE, returns 42 with it.
        it "runs deep tail recursion in constant stack (no stack overflow)" \inst -> do
          deep <- liftEffect (callI32x1 inst "countdown" 1000000)
          deep `shouldEqual` 42

        -- value correctness of a tail-recursive accumulator at a small count.
        it "computes a tail-recursive accumulator correctly" \inst -> do
          s <- liftEffect (callI32x1 inst "run" 100) -- sum 1..100
          s `shouldEqual` 5050

        -- the `where go = …` idiom (fib's loop): a closure self-call that only runs
        -- in constant stack because lambda-lifting hoisted `go` to top level. Deep
        -- iteration would overflow otherwise.
        it "TCEs a lambda-lifted closure self-recursion (the where-go idiom)" \inst -> do
          deep <- liftEffect (callI32x1 inst "loopWhere" 1000000)
          deep `shouldEqual` 1000000

        it "TCEs a lifted closure self-recursion that captures a free variable" \inst -> do
          cap <- liftEffect (callI32x2 inst "loopCapture" 2 500000) -- 2 * 500000
          cap `shouldEqual` 1000000
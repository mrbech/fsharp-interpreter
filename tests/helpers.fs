module Helpers

open NUnit.Framework

let assertTrue (b: bool) = Assert.IsTrue b
let assertEqual (a: 'a) (e: 'a) = Assert.AreEqual(e, a)
let assertNotEqual (a: 'a) (e: 'a) = Assert.AreNotEqual(e, a)
let assertOK = function
    | Ok v -> v
    | Error e -> failwithf "Expected result to be OK but was error: %A" e

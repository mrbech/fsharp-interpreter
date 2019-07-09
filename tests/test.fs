module Tests

open NUnit.Framework

open Helpers

let expectEval i s = s |> Parser.fromString |> assertOK |> Eval.run |> (fun r -> assertEqual r i)

[<Test>]
let fibnaccio() =
    "
    let ge2 x = 2 < x
    in 
        let fib n = 
            if ge2 n then (fib(n-1)) + (fib(n-2)) else 1 
        in 
            fib 25 
        end
    end" |> expectEval 75025

[<Test>]
let basicExpr() = "5+7" |> expectEval 12

[<Test>]
let nestedExpr() = "3+(2+7)" |> expectEval 12

[<Test>]
let lackOfAssociation() = "2+7*10" |> expectEval 90

[<Test>]
let addFun() = "let f1 x = x + 1 in f1 11 end" |> expectEval 12

[<Test>]
let factorial() = "let fac x = if x=0 then 1 else x * (fac (x-1)) in fac 10 end" |> expectEval 3628800

[<Test>]
let deepRecursion() = "let deep x = if x=0 then 1 else deep(x-1) in deep 99999 end" |> expectEval 1

[<Test>]
let staticScope() = 
    "let y = 11 in 
        let f x = x + y in
            let y = 22 in f 3 end 
        end
    end" |> expectEval 14

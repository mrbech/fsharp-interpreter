let ge2 x = 2 < x
in 
    let fib n = 
        if ge2 n then (fib(n-1)) + (fib(n-2)) else 1 
    in 
        fib 25 
    end
end

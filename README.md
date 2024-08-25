# OliLang

A frontend for a purely functional language which I hope to use to 
implement some cool stuff in the future.

## Examples

### Simple Functions

```
add x y = x + y
>>> 5 `add` 6
11 :: int
```

### Shadowing

```
six :: flt
six = {
    n = 5.0
    n = 6.0
    n
}
>>> six
6
```

### Recursion

```
isqrt :: int -> int
isqrt n = {
    search :: int -> int -> int
    search lo hi = {
        mid = (lo + hi) `div` 2
        if (mid * mid > n) 
            (search lo (mid - 1)) 
            (if (mid * mid < n) 
                (search mid hi) mid)
    }
    search 0 (n `div` 2)
}
```


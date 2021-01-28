# Ganter's Algorithm

As translated from [Uta Priss's website link](https://www.upriss.org.uk/fca/fcaalgorithms.html)

- [Original algo link](https://www.upriss.org.uk/fca/ganterAlg.jpg)
- [Original example link](https://upriss.github.io/fca/ganterExample.jpg)

## Pseudocode

### Input/Output
- A number `r >= 0`
- A list `i0 < i1 < ... < ir` of numbers (all `<= m`, where `m` is the number of attributes present in the formal context)
- A list `A0 ss A1 ss ... ss Ar` of extents, where `ps` is `is a superset of`
- A list `B0 subset B1 subset ... subset Br` of intents
- Satisfying the following conditions
  - `i0 = 0`, `A0 = G`, `Ar = A` (???)
  - `Aj' = Bj`, `Bj' = Aj` for `j = 0,1,2,...,r`
  - `Bj = (B{j - 1} U m{ij})''`, for `j = 0,1,2,...,r` (as in these attribute sets are closed)
  - mij is the smallest element of B{j} \ B{j-1} for `j=1,2,.....,r`

### Algo

```
a) for i = m; i > 0; i--:
b)  if mi in br then continue
    LABEL_C
c)  if i < i{r} then r--; goto LABEL_C
d)  i{r+1} = i
d)  A{r+1} = Ar intersection {m{i}}'
e)  for j = 1; j < i; j++:
f)    if {m{j}}' subset A{r+1}' then goto LABEL_P
g)    noop;
h)  B{r+1} = B{r} U {m{i}}
i)  for j = i+1; j <= m; j++:
j)    if j in B{r+1} then continue
k)    if {m{j}}' supset A{r+1} then B{r+1} = B{r+1} U {m{j}}
l)    noop;
m)  r++
n)  return
    LABEL_P
p)  noop;
q) DONE (no successor (input was B{r} = M))

// Pair (Ar,Br) of the output list is the next concept
// Calling with r = 0 = i0, A0 = G, B0 = G' with successive runs will return all concepts of (G,M,I)
// For correct input list of successor of an arbitrary concept (A,B) compute:
//  i0 := 0, A0 := G, B0 := G'
```

### Cleaned up Algo, C-like
```

```

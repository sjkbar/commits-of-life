let proc numch1(n) = 1 in
let proc numch10(n) =
    if n<10 then numch1(n) else numch1(n) + numch10(n-10) in
let proc numch100(n) =
    if n<100 then numch10(n) else numch10(n) + numch100(n-100) in
let proc numch500(n) =
    if n<500 then numch100(n) else numch100(n) + numch500(n-500) in
let proc numch1000(n) =
    if n<1000 then numch500(n) else numch500(n) + numch1000(n-1000) in
let proc numch5000(n) =
    if n<5000 then numch1000(n) else numch1000(n) + numch5000(n-5000) in
let proc numch10000(n) =
    if n<10000 then numch5000(n) else numch5000(n) + numch10000(n-10000) in
let proc numch50000(n) =
    if n<50000 then numch10000(n) else numch10000(n) + numch50000(n-50000) in
let proc numch(n) =
    numch50000(n) in
let input := 0 in
(read input; write (numch (input)))

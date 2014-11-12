let treeType := 0 in
let emptryTreeType := 1 in
let leafType := 2 in
let proc leaf(n) = {type := leafType, val := n} in
let proc makeLtree(n, tree) = {type := treeType, val:= n, left := tree, right := {type := emptryTreeType}} in
let proc makeRtree(n, tree) = {type := treeType, val:= n, right := tree, left := {type := emptryTreeType}} in
let proc makeTree(n, lTree, rTree) = {type := treeType, val:= n, right := rTree, left := lTree} in
let proc nodeVal(tree) = tree.val in
let proc isEmpty(tree) = tree.type = emptryTreeType in
let proc rTree(tree) = tree.right in
let proc lTree(tree) = tree.left in
let proc dft(tree) =
    if tree.type = treeType
        then (write nodeVal(tree); dft(lTree(tree)); dft(rTree(tree)))
        else
            (if tree.type = leafType
                then (write nodeVal(tree))
                else unit) in
let null := 0 in
let node := 1 in
let nullNode := {type := null, next:=null} in
let emptyQ := {first := nullNode, last := nullNode} in
let proc isEmptyQ(queue) =
    if queue.last.type = null
        then true
        else (if queue.first.next.type = null
            then true
            else false) in
let proc enQ(queue, element) =
    let newNode := {type := node, value := element, next := nullNode} in
        queue.last.next := newNode; {first := queue.first, last := newNode} in
let proc deQ(queue) =
    let first := queue.first.next in
    {queue := {first := first, last := queue.last}, value := first.value} in
let proc lenQ(queue) = 0 in
let proc bft(tree) =
    let proc is_btf_done(queue, tree) =
        if isEmptyQ(queue)
            then (if tree.type = leafType
                then true
                else false)
            else false in
    let proc btf_helper(queue) =
        if isEmptyQ(queue)
            then unit
            else (let record := deQ(queue) in
            let tree := record.value in
            let newQueue := record.queue in
                (if tree.type = treeType
                    then (
                        let leftTree := lTree(tree) in
                        let rightTree := rTree(tree) in
                        (if not(leftTree.type = emptryTreeType)
                            then newQueue := (enQ(newQueue, leftTree))
                            else unit);
                        (if not(rightTree.type = emptryTreeType)
                            then newQueue := (enQ(newQueue, rightTree))
                            else unit);
                        write nodeVal(tree);
                        btf_helper(newQueue)
                        )
                    else (if tree.type = leafType
                        then (write nodeVal(tree); btf_helper(newQueue))
                        else unit)))
    in btf_helper(enQ(emptyQ, tree))
in

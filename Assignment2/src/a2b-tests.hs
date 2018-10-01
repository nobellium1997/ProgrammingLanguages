import A2b
import Test.HUnit

tests =
  test
    [ "removeAllExcept 'a' 'abc'" ~: "a" ~=? (removeAllExcept 'a' "abc")
    , "removeAllExcept 1 [1, 2, 1, 3]" ~: [1, 1] ~=?
      (removeAllExcept 1 [1, 2, 1, 3])
    , "removeAll 'a' 'abc'" ~: "bc" ~=? (removeAll 'a' "abc")
    , "removeAll 1 [1, 2, 1, 3]" ~: [2, 3] ~=? (removeAll 1 [1, 2, 1, 3])
    , "substitute 'c' 'd' 'abc'" ~: "abd" ~=? (substitute 'c' 'd' "abc")
    , "substitute 1 2 [1, 2, 3]" ~: [2, 2, 3] ~=? (substitute 1 2 [1, 2, 3])
    , "mergeSorted3 [0, 1] [2, 3] [5, 6, 7]" ~: [0, 1, 2, 3, 5, 6, 7] ~=?
      (mergeSorted3 [0, 1] [2, 3] [5, 6, 7])
    , "nodeValue (TriNode 8 EmptyNode EmptyNode EmptyNode)" ~: 8 ~=?
      (nodeValue (TriNode 8 EmptyNode EmptyNode EmptyNode))
    , "leftChild (TriNode 8 EmptyNode EmptyNode EmptyNode)" ~: EmptyNode ~=?
      (leftChild (TriNode 8 EmptyNode EmptyNode EmptyNode))
    , "rightChild (TriNode 8 EmptyNode EmptyNode EmptyNode)" ~: EmptyNode ~=?
      (rightChild (TriNode 8 EmptyNode EmptyNode EmptyNode))
    , "middleChild (TriNode 8 EmptyNode (TriNode 3 EmptyNode EmptyNode EmptyNode) EmptyNode)" ~:
      TriNode 3 EmptyNode EmptyNode EmptyNode ~=?
      (middleChild
         (TriNode
            8
            EmptyNode
            (TriNode 3 EmptyNode EmptyNode EmptyNode)
            EmptyNode))
    , "inTree 3 (TriNode 8 EmptyNode (TriNode 3 EmptyNode EmptyNode EmptyNode) EmptyNode)" ~:
      True ~=?
      (inTree
         3
         (TriNode
            8
            EmptyNode
            (TriNode 3 EmptyNode EmptyNode EmptyNode)
            EmptyNode))
    , "inTree 5 (TriNode 8 EmptyNode (TriNode 3 EmptyNode EmptyNode EmptyNode) EmptyNode)" ~:
        False ~=? (inTree 5 (TriNode 8 EmptyNode (TriNode 3 EmptyNode EmptyNode EmptyNode) EmptyNode))
    , "leafList (TriNode 8 EmptyNode (TriNode 3 EmptyNode EmptyNode EmptyNode) EmptyNode)" ~:
        [3] ~=? (leafList (TriNode 8 EmptyNode (TriNode 3 EmptyNode EmptyNode EmptyNode) EmptyNode))
    , "inOrderMap" ~: TriNode 13 EmptyNode (TriNode 8 EmptyNode EmptyNode EmptyNode) EmptyNode ~=?
        (inOrderMap (\x -> x + 5) (TriNode 8 EmptyNode (TriNode 3 EmptyNode EmptyNode EmptyNode) EmptyNode))
    , "preOrderFold" ~: 24 ~=? (preOrderFold (\x y -> x * y) 1 (TriNode 8 EmptyNode (TriNode 3 EmptyNode EmptyNode EmptyNode) EmptyNode))
    ]

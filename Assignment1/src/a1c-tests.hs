import Test.HUnit
-- notice that we specify the file here as a module to import its bindings, thus
-- Haskell looks for a file called a1c.hs in its current working folder
import A1c
          
-- this defines the set of tests, notice it's a big list!     
tests = test [
  -- 1. sDotProduct
  "sDotProduct 5 (0, 0) (0, 0)" ~: 0 ~=? (sDotProduct 5 (0, 0) (0, 0)),

  -- 2. distance
  "distance (0, 1) (0, 0)" ~: 1.0 ~=? (distance (0, 1) (0, 0)),

  -- 3. tripleDistance
  "tripleDistance (0, 0, 1) (0, 0, 0)" ~: 1.0 ~=? (tripleDistance (0, 0, 1) (0, 0, 0)),

  -- 4. findMax
  "findMax [2, 3, 1]" ~: 3 ~=? (findMax [2, 3, 1]),

  
  -- 5. tupleDotProduct
  "tupleDotProduct [1, 1, 1] [1, 2, 3]" ~: 6 ~=? (tupleDotProduct [1, 1, 1] [1, 2, 3]),

  -- 6. revZip2Lists
  "revZip2Lists [1, 2, 3] ['a', 'b', 'c']" ~: [('c', 3), ('b', 2), ('a', 1)] ~=? (revZip2Lists [1, 2, 3] ['a', 'b', 'c'])
  ]

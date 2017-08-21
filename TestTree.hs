module TestTree where

import Test.HUnit
import Tree

tree = Node 'F' (Node 'B' (leaf 'A')
                          (Node 'D' (leaf 'C')
                                    (leaf 'E')))
                (Node 'G' Empty
                          (Node 'I' (leaf 'H')
                                    Empty)) 

testPreorder :: Test
testPreorder = TestCase $ assertEqual "preorder"
                                      "FBADCEGIH"
                                      (preorder tree)

testInorder :: Test
testInorder = TestCase $ assertEqual "inorder"
                                      "ABCDEFGHI"
                                      (inorder tree)

testPostorder :: Test
testPostorder = TestCase $ assertEqual "postorder"
                                       "ACEDBHIGF"
                                       (postorder tree)

testLevelorder :: Test
testLevelorder = TestCase $ assertEqual "levelorder"
                                        "FBGADICEH"
                                        (levelorder tree)

main = runTestTT $ TestList [testPreorder
                            ,testInorder
                            ,testPostorder
                            ,testLevelorder]

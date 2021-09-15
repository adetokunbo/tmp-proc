-- file doctests.hs
import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/System/TmpProc/TypeLevel/Sort.hs",
                "-isrc", "src/System/TmpProc/TypeLevel.hs"
               ]

-- file doctests.hs
import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/System/TmpProc/TypeLevel.hs"]

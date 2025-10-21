import PentaParser
import Evaluator


main :: IO ()
main = print (
    evaluate (parse "-5 + 2^4 - 2")
  )

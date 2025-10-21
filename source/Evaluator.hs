module Evalutator where


evaluate :: (Tree String) -> Double
evaluate (Leaf value)            = read value
evaluate (Tree left Plus  right) = evaluate left + evaluate right
evaluate (Tree left Minus right) = evaluate left - evaluate right
evaluate (Tree left Mult  right) = evaluate left * evaluate right
evaluate (Tree left Div   right) = evaluate left / evaluate right
evaluate (Tree left Exp   right) = evaluate left ** evaluate right
module Evalutator where


evaluate :: (Tree String) -> Int
evaluate (Leaf value)            = read value
evaluate (Tree left Plus  right) = evaluate left + evaluate right
evaluate (Tree left Minus right) = evaluate left - evaluate right
evaluate (Tree left Mult  right) = evaluate left * evaluate right
evaluate (Tree left Div   right) = evaluate left / evaluate right

Folds <- lcm_Folds$new(mtcars, 5)

Folds$Tr(mtcars, 1)
Folds$P(mtcars, 1)

lapply(Folds$valid_idx(1), \(x) (x + 3) + 1:3)
rep(1:nrow(mtcars), rep(3, nrow(mtcars))) |> length()

96 %/% nrow(mtcars)

which(rep(1:nrow(mtcars), rep(3, nrow(mtcars))) == 10)

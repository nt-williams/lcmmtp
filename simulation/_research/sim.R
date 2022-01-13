lrnrs <- sl3::Lrnr_xgboost$new()

seeds <- replicate(50, floor(runif(1, 500, 1e5)))
datas <- lapply(1:50, function(x) {
    datagen(1000, seeds[x])
})

fme <- lapply(datas, function(data) lcm(data, 1, 0, Np, lrnrs, 5))

lcm(datagen(5e4, 32452), 1, 0, Np, lrnrs, 2)

test_that("Generating correct variable histories", {
    Np <- lcm_Npsem$new(
        L = list(c("L_1"), c("L_2")),
        A = c("A_1", "A_2"),
        Z = list(c("Z_1"), c("Z_2")),
        M = c("M_1", "M_2"),
        Y = "Y"
    )

    expect_equal(Np$history("Y"),c("L_1", "L_2", "A_1", "A_2", "Z_1", "Z_2", "M_1", "M_2"))
    expect_equal(Np$history("M", 2), c("L_1", "A_1", "Z_1", "M_1", "L_2", "A_2", "Z_2"))
    expect_equal(Np$history("Z", 2), c("L_1", "A_1", "Z_1", "M_1", "L_2", "A_2"))
})

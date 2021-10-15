x <- Npsem$new(
    W = c("W1", "W2", "W3"),
    L = list(c("L1_1", "L2_1"), c("L1_2", "L2_2")),
    A = c("A1", "A2"),
    Z = list(c("Z1_1", "Z2_1"), c("Z1_2", "Z2_2")),
    M = c("M1", "M2"),
    Y = "Y"
)

x$parents("Y")
x$parents("M", 2)
x$parents("Z", 2) # how do all the variables of Z at time t affect each other? Does this even matter?

generate_paths <- function(params) {
  set.seed(params$seed)

  cor_matrix <- matrix(c(
    1,  params$cor_inv_infl, params$cor_inv_house, params$cor_inv_salary,
    params$cor_inv_infl, 1, -0.2, 0.6,
    params$cor_inv_house, -0.2, 1, params$cor_house_salary,
    params$cor_inv_salary, 0.6, params$cor_house_salary, 1
  ), 4)

  chol_mat <- chol(cor_matrix)

  Z <- array(
    rnorm(4 * params$mortgage_years * params$n_sim),
    dim = c(4, params$mortgage_years, params$n_sim)
  )

  paths <- array(0, dim(Z))
  for (s in 1:params$n_sim)
    for (y in 1:params$mortgage_years)
      paths[, y, s] <- chol_mat %*% Z[, y, s]

  list(
    inv = params$inv_mean + params$inv_sd * paths[1, , ],
    infl = params$inflation_mean + params$inflation_sd * paths[2, , ],
    house = params$house_mean + params$house_sd * paths[3, , ],
    salary = params$salary_mean + params$salary_sd * paths[4, , ]
  )
}

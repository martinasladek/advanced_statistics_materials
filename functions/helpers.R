squish <- function(x, new_min = min_bound, new_max = max_bound){   
  (x - min(x))/(max(x)-min(x)) * (new_max - new_min) + new_min 
}

rqpois <- function(n, mu, theta) {
  rnbinom(n = n, mu = mu, size = mu/(theta-1))
}

report_pseudo_r2 <- function(glm){
  
  perf = performance(glm)
  
  list(
    percent = (perf$R2_Nagelkerke*100) |> round(2) |> paste0("%"),
    r2 = paste0("*R^2^* = ", round(perf$R2_Nagelkerke, 2))
  )
  
}

report_wald <- function(lm){
  
  lm_wald <- test_wald(lm) |> suppressMessages()
  
  f = lm_wald$F[2] |> round(2)
  df_1 = lm_wald$df_diff[2] |> round(2)
  df_2 = lm_wald$df[2] |> round(2)
  wald_p <- lm_wald$p[2]
  report_p = ifelse(wald_p < 0.001, "*p* < .001", paste0("*p* = ", wald_p))
  
  paste0(
    "*F*(", df_1,  ",", df_2, ") = ", f,  ", ", report_p  
  )
  
}
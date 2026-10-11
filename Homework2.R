# Homework 2 - 

library(dplyr); library(tidyr); library(tibble)
library(readr); library(stringr); library(forcats); library(purrr)

### Problem 1
sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

# Part A
sales_clean <- sales_raw |>
  mutate(
    item_clean = str_to_lower(str_squish(item_raw)),
    amount = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw) / 100,
    net_amount = amount * (1 - discount_rate),
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20 ~ "large",
      .default = "small"
    )
  )

sales_clean |>
  select(sale_id, item_clean, amount, discount_rate, net_amount, size)

# Sales_clean keeps all columns, and chooses sales from either unrecorded, large or small based on their values.
#sale_id item_clean amount discount_rate net_amount size      
#<int> <chr>       <dbl>         <dbl>      <dbl> <chr>     
#  1       1 pen            10          0          10   small     
#2       2 pen            20          0.1        18   small     
#3       3 notebook       NA          0.05       NA   unrecorded
#4       4 pen            15          0          15   small     
#5       5 folder         30          0.2        24   large     
#6       6 folder         25          0.1        22.5 large     
#7       7 eraser         NA          0          NA   unrecorded
#8       8 pen            20          0          20   large    




# Part B
code_pattern <- "^(ST|ON)-[0-9]{3}$"

sales_codes <- sales_clean |>
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(code_clean, code_pattern)
  )

invalid_codes <- sales_codes |>
  filter(!valid_code) |>
  select(sale_id, code_raw, code_clean)
invalid_codes


valid_code_parts <- sales_codes |>
  filter(valid_code) |>
  separate_wider_delim(code_clean, delim = "-",
                       names = c("channel", "number")) |>
  select(sale_id, code_raw, channel, number)
valid_code_parts

sale_tags <- sales_clean |>
  select(sale_id, tags) |>
  separate_longer_delim(tags, delim = ";")
sale_tags

# First codes get cleaned, then are checked against regex. They are then split into new columns and rows
#sale_id code_raw channel number
#<int> <chr>    <chr>   <chr> 
#  1       1 ST-101   ST      101   
#2       2 ON-102   ON      102   
#3       3 ST-103   ST      103   
#4       4 ON-104   ON      104   
#5       6 ST-106   ST      106   
#6       8 on-108   ON      108   

#sale_id tags  
#<int> <chr> 
#  1       1 gift  
#2       1 urgent
#3       2 online
#4       3 gift  
#5       4 member
#6       5 online
#7       5 member
#8       6 gift  
#9       7 member
#10       8 gift  
#11       8 member

# Part C
customer_summary <- sales_clean |>
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(net_amount >= 20, na.rm = TRUE),
    large_share = if_else(known_n > 0, large_n / known_n, NA_real_),
    .by = customer_id
  ) |>
  arrange(customer_id)
customer_summary


sales_factors <- sales_clean |>
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group = fct_lump_n(factor(item_clean), n = 2, other_level = "Other")
  )

levels(sales_factors$item_factor)
sales_factors |> count(item_factor)


levels(sales_factors$item_group)
sales_factors |> count(item_group)

# Gives summarise, one row per customer. C1 large_share is 0/1. Numerator is 0 because C1 only known amount is 10, which is less then 20. Also denominator is 1 because only 1 of C1 sales are known. 
#customer_id sales_n known_n large_n large_share
#<chr>         <int>   <int>   <int>       <dbl>
#  1 C1                2       1       0       0    
#2 C2                3       3       2       0.667
#3 C3                1       1       0       0    
#4 C4                1       1       1       1    
#5 C5                1       0       0      NA    

# item_group     n
#<fct>      <int>
#  1 folder         2
#2 pen            4
#3 Other          2


### Problem 2
reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)

customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

# Part A
reports_labeled <- reports_raw |>
  arrange(form, line) |>
  group_by(form) |>
  fill(customer_id, .direction = "down") |>
  ungroup()
reports_labeled


reports_clean <- reports_labeled |>
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )

reports_clean |>
  select(form, line, customer_id, round, primary_amount, backup_amount, amount)


reports_complete <- reports_clean |>
  select(customer_id, round, amount) |>
  mutate(recorded_row = TRUE) |>
  complete(customer_id, round = 1:3, fill = list(recorded_row = FALSE))
reports_complete

reports_complete |>
  summarise(
    original_rows = sum(recorded_row),
    added_rows = sum(!recorded_row),
    total_rows = n()
  )

reports_clean |> summarise(missing_before = sum(is.na(amount)))
reports_complete |> summarise(missing_after = sum(is.na(amount)))

reports_clean |>
  filter(is.na(amount)) |>
  select(customer_id, round, amount)

reports_complete |>
  filter(!recorded_row) |>
  select(customer_id, round, amount)

# Begins by using fill to copy the ID to blank IDS for each form. Coalesce uses a backup if primary is unknown All values are kept including 0.
#   form   line customer_id round primary_amount backup_amount amount
#<chr> <dbl> <chr>       <dbl>          <dbl>         <dbl>  <dbl>
#  1 A         1 C1              1             10            NA     10
#2 A         2 C1              3           -999            15     15
#3 A         3 C2              1             20            25     20
#4 B         1 C3              1             NA            NA     NA
#5 B         2 C3              2              0            99      0
#6 B         3 C4              1             30            NA     30

##  A tibble: 6 × 3
#customer_id round amount
#<chr>       <dbl>  <dbl>
#  1 C1              2     NA
#2 C2              2     NA
#3 C2              3     NA
#4 C3              3     NA
#5 C4              2     NA
#6 C4              3     NA


# Part B
customers |> count(customer_id) |> filter(n > 1)
customers |> filter(is.na(customer_id))
reports_clean |> count(customer_id, round) |> filter(n > 1)


customers_marked <- customers |>
  mutate(customer_found = TRUE)

reports_joined <- reports_clean |>
  left_join(
    customers_marked,
    by = join_by(customer_id),
    relationship = "many-to-one",
    na_matches = "never"
  ) |>
  mutate(customer_found = coalesce(customer_found, FALSE))

reports_joined |>
  select(customer_id, round, amount, region, customer_found)

reports_clean |>
  anti_join(customers, by = join_by(customer_id), na_matches = "never") |>
  count(customer_id)

customers |>
  anti_join(reports_clean, by = join_by(customer_id), na_matches = "never") |>
  count(customer_id)

# Checks for no duplicate or missing IDs. Then use left join to combine the customer info. Any info that is unmatched is false. Anti join shows some that are only in reports vs only in customers. 
#  customer_id round amount region customer_found
#<chr>       <dbl>  <dbl> <chr>  <lgl>         
#  1 C1              1     10 North  TRUE          
#2 C1              3     15 North  TRUE          
#3 C2              1     20 South  TRUE          
#4 C3              1     NA NA     TRUE          
#5 C3              2      0 NA     TRUE          
#6 C4              1     30 NA     FALSE         

# Part C
customers_bad <- bind_rows(customers,
                           tibble(customer_id = "C2", region = "East"))

reports_duplicated <- reports_clean |>
  left_join(customers_bad, by = join_by(customer_id),
            relationship = "many-to-many")

reports_clean |>
  summarise(rows = n(), total_amount = sum(amount, na.rm = TRUE))
reports_duplicated |>
  summarise(rows = n(), total_amount = sum(amount, na.rm = TRUE))

reports_duplicated |> count(customer_id, round) |> filter(n > 1)


reports_wide <- reports_complete |>
  select(customer_id, round, amount) |>
  pivot_wider(names_from = round, names_prefix = "round_",
              values_from = amount)
reports_wide


reports_wide |>
  pivot_longer(starts_with("round_"), names_to = "round",
               names_prefix = "round_", values_to = "amount",
               values_drop_na = TRUE)

# There are two records in customers_bad, so round 1 reported is duplicated in that case. Rows increase from 6 to 7, and total 75 to 95 because C2 is counted twice. Many to one would create errors and does not fix conflict at all.
#   customer_id round amount
#<chr>       <chr>  <dbl>
#  1 C1          1         10
#2 C1          3         15
#3 C2          1         20
#4 C3          2          0
#5 C4          1         30


### Problem 3

# Part A
two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  n_x <- length(x)
  n_y <- length(y)
  mean_x <- mean(x)
  mean_y <- mean(y)
  var_x <- var(x)
  var_y <- var(y)
  
  if (equal_var) {
    method <- "Pooled"
    s2_p <- ((n_x - 1) * var_x + (n_y - 1) * var_y) / (n_x + n_y - 2)
    se <- sqrt(s2_p * (1 / n_x + 1 / n_y))
    df <- n_x + n_y - 2
  } else {
    method <- "Welch"
    se <- sqrt(var_x / n_x + var_y / n_y)
    df <- (var_x / n_x + var_y / n_y)^2 /
      ((var_x / n_x)^2 / (n_x - 1) + (var_y / n_y)^2 / (n_y - 1))
  }
  
  t_stat <- (mean_x - mean_y) / se
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  reject <- p_value < alpha
  
  list(method = method, t_stat = t_stat, df = df,
       p_value = p_value, reject = reject)
}

x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

welch_mine <- two_sample_t(x_check, y_check)
pooled_mine <- two_sample_t(x_check, y_check, equal_var = TRUE)
welch_mine
pooled_mine

welch_ref <- t.test(x_check, y_check, var.equal = FALSE)
pooled_ref <- t.test(x_check, y_check, var.equal = TRUE)

abs(welch_mine$t_stat - welch_ref$statistic) < 1e-8
abs(welch_mine$df - welch_ref$parameter) < 1e-8
abs(welch_mine$p_value - welch_ref$p.value) < 1e-8

abs(pooled_mine$t_stat - pooled_ref$statistic) < 1e-8
abs(pooled_mine$df - pooled_ref$parameter) < 1e-8
abs(pooled_mine$p_value - pooled_ref$p.value) < 1e-8

# Uses if else expression to check SE DF. Then calculates t and p value. All 6 comparions are true.
# > pooled_mine
#$method
#[1] "Pooled"

#$t_stat
#[1] 3.526436

#$df
#[1] 11

#$p_value
#[1] 0.00474447

#$reject
#[1] TRUE

# Part B
B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null",
               "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)

set.seed(2026)

n_total <- nrow(scenarios) * B * 2
scenario_col <- vector("character", length = n_total)
replicate_col <- vector("numeric", length = n_total)
method_col <- vector("character", length = n_total)
t_col <- vector("numeric", length = n_total)
df_col <- vector("numeric", length = n_total)
p_col <- vector("numeric", length = n_total)
reject_col <- vector("logical", length = n_total)

row_i <- 0
for (s in seq_len(nrow(scenarios))) {
  for (b in seq_len(B)) {
    x <- rnorm(n_x, mean = scenarios$mean_x[s], sd = scenarios$sd_x[s])
    y <- rnorm(n_y, mean = scenarios$mean_y[s], sd = scenarios$sd_y[s])
    
    welch <- two_sample_t(x, y, equal_var = FALSE, alpha = alpha)
    pooled <- two_sample_t(x, y, equal_var = TRUE, alpha = alpha)
    
    row_i <- row_i + 1
    scenario_col[row_i] <- scenarios$scenario[s]
    replicate_col[row_i] <- b
    method_col[row_i] <- welch$method
    t_col[row_i] <- welch$t_stat
    df_col[row_i] <- welch$df
    p_col[row_i] <- welch$p_value
    reject_col[row_i] <- welch$reject
    
    row_i <- row_i + 1
    scenario_col[row_i] <- scenarios$scenario[s]
    replicate_col[row_i] <- b
    method_col[row_i] <- pooled$method
    t_col[row_i] <- pooled$t_stat
    df_col[row_i] <- pooled$df
    p_col[row_i] <- pooled$p_value
    reject_col[row_i] <- pooled$reject
  }
}

simulation_results <- data.frame(
  scenario = scenario_col,
  replicate = replicate_col,
  method = method_col,
  t_stat = t_col,
  df = df_col,
  p_value = p_col,
  reject = reject_col
)

head(simulation_results)
nrow(simulation_results)


simulation_summary <- simulation_results |>
  summarise(
    mean_t = round(mean(t_stat), 3),
    sd_t = round(sd(t_stat), 3),
    rejection_rate = round(mean(reject), 3),
    .by = c(scenario, method)
  )
simulation_summary

# Nested for loops use  rnorm to get new samples and run tests. Summarise then displays the mean, sd, and rejection. The rejection rates are (0.044, 0.061) under null and equal variance rate is close to 0.05 (0.038 observed). Observed that pooled test rejects 13.9% of the time, this creates high chance of 
# Type 1 error. Welch is more reliabnle when variances differ. 

#> simulation_summary
#scenario method mean_t  sd_t rejection_rate
#1    equal_null  Welch -0.019 1.038          0.044
#2    equal_null Pooled -0.018 1.024          0.038
#3  unequal_null  Welch  0.036 1.075          0.061
#4  unequal_null Pooled  0.048 1.402          0.139
#5   equal_shift  Welch -3.768 1.116          0.956
#6   equal_shift Pooled -3.757 1.078          0.957
#7 unequal_shift  Welch -1.542 1.048          0.282
#8 unequal_shift Pooled -2.019 1.357          0.497

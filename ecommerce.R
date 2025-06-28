# Нові категорійні змінні
median_price <- median(df_lab2$price, na.rm = TRUE)
median_weight <- median(df_lab2$product_weight_g, na.rm = TRUE)
median_distance <- median(df_lab2$distance_km, na.rm = TRUE)

df_test <- df_lab2 %>%
  mutate(price_category = ifelse(price <= 150, "cheap", "expensive"),
         weight_category = ifelse(product_weight_g <= 2000, "light", "heavy"),
         distance_category = ifelse(distance_km <= 500, "close", "far"),
         on_time = actual_delivery_time <= estimated_delivery_time,
         customer_region = case_when(
           customer_state %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "North",
           customer_state %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Northeast",
           customer_state %in% c("DF", "GO", "MT", "MS") ~ "Central-West",
           customer_state %in% c("ES", "MG", "RJ", "SP") ~ "Southeast",
           customer_state %in% c("PR", "RS", "SC") ~ "South"
           review_score = as.numeric(as.character(review_score))
         ))


# Датасет унікальних товарів з їх вагою, ціною, категорією
df_products_unique <- df_test %>%
  filter(!is.na(product_id), !is.na(price), !is.na(product_weight_g)) %>%
  group_by(product_id) %>%
  summarize(price = first(price), product_weight_g = first(product_weight_g), product_category_name = first(product_category_name)) %>%
  ungroup()


# Датасет топ 8 найпопулярніших категорій
top_categories <- df_test %>%
  count(product_category_name, sort = TRUE) %>%
  top_n(8, n) %>%
  pull(product_category_name)
df_top_categories <- df_lab2 %>% filter(product_category_name %in% top_categories) # Топ 8 найпопулярніших категорій


# Функції для побудови ДІ для медіан для різних груп
bootstrap_median_ci <- function(x, R = 1000, conf = 0.95) {
  boot_stat <- function(data, indices) median(data[indices])
  set.seed(123)
  boot_res <- boot(x, statistic = boot_stat, R = R, simple = TRUE)
  se <- sd(boot_res$t)
  median_hat <- median(x)
  error <- qnorm((1 + conf)/2) * se
  ci <- c(median_hat - error, median_hat + error)
  return(list(median = median_hat, lower = ci[1], upper = ci[2]))
}

group_bootstrap_median_ci <- function(data, group_col, value_col, conf = 0.95, caption = NULL, digits = 2) {
  fmt <- paste0("[%.", digits, "f; %.", digits, "f]")
  ci_result <- data %>%
    filter(!is.na(.data[[group_col]]), !is.na(.data[[value_col]])) %>%
    group_by(.data[[group_col]]) %>%
    summarise(result = list(bootstrap_median_ci(.data[[value_col]], conf = conf)), .groups = "drop") %>%
    unnest_wider(result) %>%
    arrange(median)
  
  ci_result %>%
    mutate(`95% Довірчий інтервал` = sprintf(fmt, lower, upper)) %>%
    select(Група = !!sym(group_col), Медіана = median, `95% Довірчий інтервал`) %>%
    gt() %>%
    tab_header(title = caption) %>%
    tab_options(
      table.width = px(700),
      column_labels.font.size = px(17),
      table.font.size = px(15)
    ) %>%
    cols_align(
      align = "center", 
      columns = c(`95% Довірчий інтервал`)
    ) %>%
    tab_style(
      style = cell_text(color = "black"),
      locations = cells_body()
    )
}


# Функція для тестування різниці середніх (тест Волда)
wald_test_diff_means <- function(df, group_col, value_col, group1, group2, alternative = "greater") {
  estimates <- df %>%
    filter(!is.na(.data[[value_col]])) %>%
    group_by(.data[[group_col]]) %>%
    summarize(
      mean_hat = mean(.data[[value_col]]),
      var_hat = var(.data[[value_col]]) / n(),
      .groups = "drop"
    )
  
  mean_1 <- estimates %>% filter(.data[[group_col]] == group1) %>% pull(mean_hat)
  mean_2 <- estimates %>% filter(.data[[group_col]] == group2) %>% pull(mean_hat)
  var_1 <- estimates %>% filter(.data[[group_col]] == group1) %>% pull(var_hat)
  var_2 <- estimates %>% filter(.data[[group_col]] == group2) %>% pull(var_hat)
  se <- sqrt(var_1 + var_2)
  T <- (mean_1 - mean_2) / se
  
  p_value <- switch(
    alternative,
    "greater" = 1 - pnorm(T),
    "less" = pnorm(T),
    "two.sided" = 2 * (1 - pnorm(abs(T)))
  )
  
  ci <- switch(
    alternative,
    "greater" = c(mean_1 - mean_2 - qnorm(0.95) * se, Inf),
    "less" = c(-Inf, mean_1 - mean_2 + qnorm(0.95) * se),
    "two.sided" = mean_1 - mean_2 + c(-1, 1) * qnorm(0.975) * se
  )
  
  return(list(
    estimate_1 = mean_1,
    estimate_2 = mean_2,
    statistic = T,
    p_value = p_value,
    conf_int = ci
  ))
}


# Функція для тестування різниці медіан
bootstrap_median_diff_indep <- function(x, y, R = 1000, alternative = "greater", seed = 123) {
  set.seed(seed)
  
  # Реальна різниця медіан
  true_diff <- median(x) - median(y)
  
  # Бутстреп
  boot_diffs <- replicate(R, {
    x_star <- sample(x, length(x), replace = TRUE)
    y_star <- sample(y, length(y), replace = TRUE)
    median(x_star) - median(y_star)
  })
  
  se_boot <- sd(boot_diffs)
  T <- true_diff / se_boot
  
  p_value <- switch(
    alternative,
    "greater" = 1 - pnorm(T),
    "less" = pnorm(T),
    "two.sided" = 2 * (1 - pnorm(abs(T)))
  )
  
  conf.int <- switch(
    alternative,
    "greater" = c(true_diff - qnorm(0.95) * se_boot, Inf),
    "less" = c(-Inf, true_diff + qnorm(0.95) * se_boot),
    "two.sided" = true_diff + c(-1, 1) * qnorm(0.975) * se_boot)
  
  return(list(
    estimate_1 = median(x),
    estimate_2 = median(y),
    diff = true_diff,
    se = se_boot,
    statistic = T,
    p_value = p_value,
    conf_int = conf.int
  ))
}



# ДІ медіани вартості доставки для різних регіонів покупця 
group_bootstrap_median_ci(data = df_test, group_col = "customer_region", value_col = "freight_value",
                          caption = "ДІ медіани вартості доставки за регіоном покупця")


# ДІ медіани вартості доставки для різних категорій (топ 8) 
group_bootstrap_median_ci(data = df_top_categories, group_col = "product_category_name", value_col = "freight_value",
                          caption = "ДІ медіани вартості доставки за категорією товару")


# ДІ медіани вартості доставки для різних оцінок покупця 
group_bootstrap_median_ci(data = df_test, group_col = "review_score", value_col = "freight_value",
                          caption = "ДІ медіани вартості доставки за оцінкою покупця")


# ДІ медіани вартості доставки за ціною товару 
group_bootstrap_median_ci(data = df_test %>% filter(num_items == 1) %>% mutate(price_group = case_when(price <= 70 ~ "cheap",
                                                                                                       price <= 300 ~ "medium", 
                                                                                                       TRUE ~ "expensive")), 
                          group_col = "price_group", value_col = "freight_value",
                          caption = "ДІ медіани вартості доставки за ціною товару")


# ДІ медіани вартості доставки за вагою товару 
group_bootstrap_median_ci(data = df_test %>% filter(num_items == 1) %>% mutate(weight_group = case_when(product_weight_g <= 500 ~ "light",
                                                                                                        product_weight_g <= 5000 ~ "medium", 
                                                                                                        TRUE ~ "heavy")), 
                          group_col = "weight_group", value_col = "freight_value",
                          caption = "ДІ медіани вартості доставки за вагою товару")


# ДІ медіани вартості доставки за відстанню 
group_bootstrap_median_ci(data = df_test %>% filter(num_items == 1) %>% mutate(distance_group = case_when(distance_km <= 0 ~ "close",
                                                                                                          distance_km <= 1000 ~ "medium", 
                                                                                                          TRUE ~ "far")), 
                          group_col = "distance_group", value_col = "freight_value",
                          caption = "ДІ медіани вартості доставки за відстанню")


# ДІ медіани часу доставки для різних регіонів покупця 
group_bootstrap_median_ci(data = df_test, group_col = "customer_region", value_col = "actual_delivery_time",
                          caption = "ДІ медіани часу доставки за регіоном покупця")


# ДІ медіани часу доставки для різних категорій (топ 8) 
group_bootstrap_median_ci(data = df_top_categories, group_col = "product_category_name", value_col = "actual_delivery_time",
                          caption = "ДІ медіани часу доставки за категорією товару")


# ДІ медіани часу доставки для різних оцінок покупця 
group_bootstrap_median_ci(data = df_test, group_col = "review_score", value_col = "actual_delivery_time",
                          caption = "ДІ медіани часу доставки за оцінкою покупця")


# ДІ медіани часу доставки за ціною товару 
group_bootstrap_median_ci(data = df_test %>% filter(num_items == 1) %>% mutate(price_group = case_when(price <= 70 ~ "cheap",
                                                                                                       price <= 300 ~ "medium", 
                                                                                                       TRUE ~ "expensive")), 
                          group_col = "price_group", value_col = "actual_delivery_time",
                          caption = "ДІ медіани часу доставки за ціною товару")


# ДІ медіани часу доставки за вагою товару 
group_bootstrap_median_ci(data = df_test %>% filter(num_items == 1) %>% mutate(weight_group = case_when(product_weight_g <= 500 ~ "light",
                                                                                                        product_weight_g <= 5000 ~ "medium", 
                                                                                                        TRUE ~ "heavy")), 
                          group_col = "weight_group", value_col = "actual_delivery_time",
                          caption = "ДІ медіани часу доставки за вагою товару")


# ДІ медіани часу доставки за відстанню 
group_bootstrap_median_ci(data = df_test %>% filter(num_items == 1) %>% mutate(distance_group = case_when(distance_km <= 0 ~ "close",
                                                                                                          distance_km <= 1000 ~ "medium", 
                                                                                                          TRUE ~ "far")), 
                          group_col = "distance_group", value_col = "actual_delivery_time",
                          caption = "ДІ медіани часу доставки за відстанню")


# ДІ медіани ціни товару для різних категорій (топ 8) 
group_bootstrap_median_ci(data = df_products_unique %>% filter(product_category_name %in% top_categories_8), group_col = "product_category_name", value_col = "price",
                          caption = "ДІ медіани ціни товару за категорією товару")


# ДІ медіани ціни товару за вагою товару 
group_bootstrap_median_ci(data = df_products_unique %>% mutate(weight_group = case_when(product_weight_g <= 500 ~ "light",
                                                                                        product_weight_g <= 2000 ~ "medium", 
                                                                                        TRUE ~ "heavy")), 
                          group_col = "weight_group", value_col = "price",
                          caption = "ДІ медіани ціни товару за вагою товару")


# Чи є вартість доставки для дорожчих товарів більшою (різниця середніх)?
res <- wald_test_diff_means(df_test, "price_category", "freight_value", "expensive", "cheap")
cat(sprintf("Середнє вибіркове вартості доставки (дешеві): %.3f\nСереднє вибіркове вартості доставки (дорогі): %.3f\np-value:  %f\nДовірчий інтервал: [%.3f; %.3f)\n", 
            res$estimate_2, res$estimate_1, res$p_value, res$conf_int[1], res$conf_int[2]))
t.test(freight_value ~ price_category, data = df_test, alternative = "less")


# Чи є вартість доставки для дорожчих товарів більшою (різниця медіан)?
df_sub <- df_test %>% filter(num_items == 1)
x <- df_sub %>% filter(price_category == "expensive") %>% pull(freight_value)
y <- df_sub %>% filter(price_category == "cheap") %>% pull(freight_value)

res <- bootstrap_median_diff_indep(x, y, alternative = "greater")
cat(sprintf("Медіана вартості доставки (дешеві): %.3f\nМедіана вартості доставки (дорогі): %.3f\np-value: %f\nДовірчий інтервал: [%.3f; %.3f)\n", 
            res$estimate_2, res$estimate_1, res$p_value, res$conf_int[1], res$conf_int[2]))



# Чи є вартість доставки для важчих товарів більшою (різниця середніх)?
res <- wald_test_diff_means(df_test, "weight_category", "freight_value", "heavy", "light")
cat(sprintf("Середнє вибіркове вартості доставки (легкі): %.3f\nСереднє вибіркове вартості доставки (важкі): %.3f\np-value: %f\nДовірчий інтервал: [%.3f; %.3f)\n", 
            res$estimate_2, res$estimate_1, res$p_value, res$conf_int[1], res$conf_int[2]))


# Чи є вартість доставки для важчих товарів більшою (різниця медіан)?
df_sub <- df_test %>% filter(num_items == 1)
x <- df_sub %>% filter(weight_category == "heavy") %>% pull(freight_value)
y <- df_sub %>% filter(weight_category == "light") %>% pull(freight_value)

res <- bootstrap_median_diff_indep(x, y, alternative = "greater")
cat(sprintf("Медіана вартості доставки (легкі): %.3f\nМедіана вартості доставки (важкі): %.3f\np-value: %f\nДовірчий інтервал: [%.3f; %.3f)\n", 
            res$estimate_2, res$estimate_1, res$p_value, res$conf_int[1], res$conf_int[2]))



# Чи є вартість доставки більшою при більшій відстані між продавцем та покупцем (різниця середніх)?
res <- wald_test_diff_means(df_test, "distance_category", "freight_value", "far", "close")
cat(sprintf("Середнє вибіркове вартості доставки (дешеві): %.3f\nСереднє вибіркове вартості доставки (дорогі): %.3f\np-value:  %f\nДовірчий інтервал: [%.3f; %.3f)\n", 
            res$estimate_2, res$estimate_1, res$p_value, res$conf_int[1], res$conf_int[2]))


# Чи є вартість доставки більшою при більшій відстані між продавцем та покупцем (різниця медіан)?
x <- df_test %>% filter(distance_category == "far") %>% pull(freight_value)
y <- df_test %>% filter(distance_category == "close") %>% pull(freight_value)

res <- bootstrap_median_diff_indep(x, y, alternative = "greater")
cat(sprintf("Медіана вартості доставки (близько): %.3f\nМедіана вартості доставки (далеко): %.3f\np-value: %f\nДовірчий інтервал: [%.3f; %.3f)\n", 
            res$estimate_2, res$estimate_1, res$p_value, res$conf_int[1], res$conf_int[2]))



# Категорія товару - вартість доставки
factor_ttests <- df_top_categories %>%
  select(freight_value, product_category_name) %>% group_by(factor(product_category_name)) %>%
  summarize(across(freight_value, list)) %>%
  tidyr::expand(nesting(category1 = `factor(product_category_name)`, value1 = freight_value),
                nesting(category2 = `factor(product_category_name)`, value2 = freight_value)) %>%
  filter(category1 != category2) %>%
  rowwise %>% mutate(key = paste0(sort(c(category1, category2)), collapse = "-")) %>%
  distinct(key, .keep_all = TRUE) %>%
  select(category1, category2, value1, value2) %>%
  mutate(p_value = t.test(value1, value2)$p.value) %>% ungroup()

factor_ttests %>%
  mutate(p_value_bonf = p.adjust(p_value, method = "bonferroni"),
         p_value_BH = p.adjust(p_value, method = "BH"),
         reject = p_value < 0.05,
         reject_bonf = p_value_bonf < 0.05,
         reject_BH = p_value_BH < 0.05) %>%
  select(-c("value1", "value2")) 


# Вартість доставки для категорій "furniture_decor", "telephony"
t.test(freight_value ~ product_category_name, data = subset(df_test, product_category_name %in% c("furniture_decor", "telephony")),
       alternative = "greater")


# Регіон покупця - вартість доставки
factor_ttests <- df_test %>%
  select(freight_value, customer_region) %>% group_by(factor(customer_region)) %>%
  summarize(across(freight_value, list)) %>%
  tidyr::expand(nesting(category1 = `factor(customer_region)`, value1 = freight_value),
                nesting(category2 = `factor(customer_region)`, value2 = freight_value)) %>%
  filter(category1 != category2) %>%
  rowwise %>% mutate(key = paste0(sort(c(category1, category2)), collapse = "-")) %>%
  distinct(key, .keep_all = TRUE) %>%
  select(category1, category2, value1, value2) %>%
  mutate(p_value = t.test(value1, value2)$p.value) %>% ungroup()

factor_ttests %>%
  mutate(p_value_bonf = p.adjust(p_value, method = "bonferroni"),
         p_value_BH = p.adjust(p_value, method = "BH"),
         reject = p_value < 0.05,
         reject_bonf = p_value_bonf < 0.05,
         reject_BH = p_value_BH < 0.05) %>%
  select(-c("value1", "value2"))


# Вартість доставки для регіонів "Southeast", "North" 
t.test(freight_value ~ customer_region, data = subset(df_test, customer_region %in% c("Southeast", "North")),
       alternative = "greater")


# Чи є ціна товару для важчих товарів більшою (різниця середніх)?
res <- wald_test_diff_means(df_test, "weight_category", "price", "heavy", "light")
cat(sprintf("Середнє вибіркове ціни товару (легкі): %.3f\nСереднє вибіркове ціни товару (важкі): %.3f\np-value:  %f\nДовірчий інтервал: [%.3f; %.3f)\n", 
            res$estimate_2, res$estimate_1, res$p_value, res$conf_int[1], res$conf_int[2]))


# Чи є ціна товару для важчих товарів більшою (різниця медіан)?
df_sub <- df_products_unique %>% mutate(weight_group = case_when(product_weight_g <= 1200 ~ "light", TRUE ~ "heavy"))
x <- df_sub %>% filter(weight_group == "heavy") %>% pull(price)
y <- df_sub %>% filter(weight_group == "light") %>% pull(price)

res <- bootstrap_median_diff_indep(x, y, alternative = "greater")
cat(sprintf("Медіана ціни товару (легкі): %.3f\nМедіана ціни товару (важкі): %.3f\np-value: %f\nДовірчий інтервал: [%.3f; %.3f)\n", 
            res$estimate_2, res$estimate_1, res$p_value, res$conf_int[1], res$conf_int[2]))



# Категорія товару - ціна
factor_ttests <- df_top_categories %>%
  select(price, product_category_name) %>% group_by(factor(product_category_name)) %>%
  summarize(across(price, list)) %>%
  tidyr::expand(nesting(category1 = `factor(product_category_name)`, value1 = price),
                nesting(category2 = `factor(product_category_name)`, value2 = price)) %>%
  filter(category1 != category2) %>%
  rowwise %>% mutate(key = paste0(sort(c(category1, category2)), collapse = "-")) %>%
  distinct(key, .keep_all = TRUE) %>%
  select(category1, category2, value1, value2) %>%
  mutate(p_value = t.test(value1, value2)$p.value) %>% ungroup()

factor_ttests %>%
  mutate(p_value_bonf = p.adjust(p_value, method = "bonferroni"),
         p_value_BH = p.adjust(p_value, method = "BH"),
         reject = p_value < 0.05,
         reject_bonf = p_value_bonf < 0.05,
         reject_BH = p_value_BH < 0.05) %>%
  select(-c("value1", "value2"))  %>%
  print(n = 28)


# Ціна товару для категорій "telephony", "watches_gifts"
t.test(price ~ product_category_name, data = subset(df_test, product_category_name %in% c("telephony", "watches_gifts")),
       alternative = "less")


# Категорія товару - ціна
top_categories <- df_lab2 %>%
  count(product_category_name, sort = TRUE) %>%
  top_n(8, n) %>%
  pull(product_category_name)

df_top_categories <- df_lab2 %>% 
  filter(product_category_name %in% top_categories)

top_categories_ttests <- df_top_categories %>%
  select(price, product_category_name) %>%
  group_by(factor(product_category_name)) %>%
  summarize(across(price, list)) %>%
  tidyr::expand(
    nesting(category1 = `factor(product_category_name)`, value1 = price),
    nesting(category2 = `factor(product_category_name)`, value2 = price)
  ) %>%
  filter(category1 != category2) %>%
  rowwise() %>%
  mutate(key = paste0(sort(c(category1, category2)), collapse = "-")) %>%
  distinct(key, .keep_all = TRUE) %>%
  select(category1, category2, value1, value2) %>%
  mutate(p_value = t.test(value1, value2)$p.value) %>%
  ungroup()

top_categories_ttests %>%
  mutate(
    p_value_bonf = p.adjust(p_value, method = "bonferroni"),
    p_value_BH = p.adjust(p_value, method = "BH"),
    reject = p_value < 0.05,
    reject_bonf = p_value_bonf < 0.05,
    reject_BH = p_value_BH < 0.05
  ) %>%
  select(category1, category2, p_value, p_value_bonf, p_value_BH, reject, reject_bonf) %>%
  arrange(p_value) %>%
  gt() %>%
  fmt_scientific(columns = starts_with("p_value"), decimals = 2) %>%
  tab_header(title = "Порівняння цін між топ-8 категоріями (t-тест)") %>%
  cols_label(
    category1 = "Категорія 1",
    category2 = "Категорія 2",
    p_value = "p-value",
    p_value_bonf = "p-value (Bonferroni)",
    p_value_BH = "p-value (BH)",
    reject = "p < 0.05",
    reject_bonf = "Bonf. < 0.05"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "red"),
    locations = cells_body(columns = c(reject, reject_bonf), rows = reject == TRUE | reject_bonf == TRUE)
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.width = px(1000),
    column_labels.font.size = px(15),
    table.font.size = px(14)
  )


# Чи є час доставки дорогих товарів меншим (різниця середніх)?

estimates_time <- df_test %>%
  filter(!is.na(actual_delivery_time)) %>%
  group_by(price_category) %>%
  summarize(
    mean_hat = mean(actual_delivery_time),
    var_hat = var(actual_delivery_time) / n()
  )

mean_time_exp <- estimates_time %>% filter(price_category == "expensive") %>% pull(mean_hat)
mean_time_cheap <- estimates_time %>% filter(price_category == "cheap") %>% pull(mean_hat)
var_time_exp <- estimates_time %>% filter(price_category == "expensive") %>% pull(var_hat)
var_time_cheap <- estimates_time %>% filter(price_category == "cheap") %>% pull(var_hat)

se_time <- sqrt(var_time_exp + var_time_cheap)
T_time <- (mean_time_exp - mean_time_cheap) / se_time
p_value_time <- 1 - pnorm(T_time)
conf.int_time <- c(mean_time_exp - mean_time_cheap - qnorm(0.95)*se_time, Inf)


cat(sprintf("Середній час доставки (дешеві): %.3f\nСередній час доставки (дорогі): %.3f\np-value:  %f\nДовірчий інтервал: [%.3f; %.3f)\n", 
            mean_time_cheap, mean_time_exp, p_value_time, conf.int_time[1], conf.int_time[2]))


# Чи є час доставки для дорожчих товарів більшим (різниця середніх)? 
Використання функції t.test (робить те саме)
t.test(actual_delivery_time ~ price_category, data = df_test, alternative = "less")


# Чи є час доставки дорогих товарів більшим (різниця медіан)?
set.seed(123)
df_test_boot <- df_test %>% slice_sample(n = 20000)

boot_median_diff_time_expensive <- function(data, indices) {
  d <- data[indices, ]
  med_expensive <- median(d$actual_delivery_time[d$price_category == "expensive"])
  med_cheap <- median(d$actual_delivery_time[d$price_category == "cheap"])
  return(med_expensive - med_cheap)
}

set.seed(123)
boot_result_time_expensive <- boot(data = df_test_boot, statistic = boot_median_diff_time_expensive, R = 1000)

median_expensive_time <- median(df_test_boot$actual_delivery_time[df_test_boot$price_category == "expensive"])
median_cheap_time <- median(df_test_boot$actual_delivery_time[df_test_boot$price_category == "cheap"])
se_boot_time_expensive <- sd(boot_result_time_expensive$t)

T_time_expensive <- (median_expensive_time - median_cheap_time) / se_boot_time_expensive
p_value_time_expensive <- 1 - pnorm(T_time_expensive)
conf.int_time_expensive <- c(median_expensive_time - median_cheap_time - qnorm(0.95) * se_boot_time_expensive, Inf)

cat(sprintf("Медіана часу доставки (дешеві): %.3f\nМедіана часу доставки (дорогі): %.3f\np-value: %f\nДовірчий інтервал: [%.3f; %.3f)\n", 
            median_cheap_time, median_expensive_time, p_value_time_expensive, conf.int_time_expensive[1], conf.int_time_expensive[2]))


# Чи є час доставки важких товарів більшим (різниця середніх)?
t.test(actual_delivery_time ~ weight_category, data = df_test, alternative = "greater")


# Чи є час доставки важких товарів більшим (різниця медіан)?
set.seed(123)
df_test_boot_weight_time <- df_test %>% slice_sample(n = 20000)

boot_median_diff_time_heavy <- function(data, indices) {
  d <- data[indices, ]
  med_heavy_time <- median(d$actual_delivery_time[d$weight_category == "heavy"])
  med_light_time <- median(d$actual_delivery_time[d$weight_category == "light"])
  return(med_heavy_time - med_light_time)
}

set.seed(123)
boot_result_time_heavy_median <- boot(data = df_test_boot_weight_time, statistic = boot_median_diff_time_heavy, R = 1000)

median_heavy_time <- median(df_test_boot_weight_time$actual_delivery_time[df_test_boot_weight_time$weight_category == "heavy"])
median_light_time <- median(df_test_boot_weight_time$actual_delivery_time[df_test_boot_weight_time$weight_category == "light"])
se_boot_time_heavy_median <- sd(boot_result_time_heavy_median$t)

T_time_heavy_median <- (median_heavy_time - median_light_time) / se_boot_time_heavy_median
p_value_time_heavy_median <- 1 - pnorm(T_time_heavy_median)
conf.int_time_heavy_median <- c(median_heavy_time - median_light_time - qnorm(0.95) * se_boot_time_heavy_median, Inf)

cat(sprintf("Медіана часу доставки (легкі): %.3f\nМедіана часу доставки (важкі): %.3f\np-value: %f\nДовірчий інтервал: [%.3f; %.3f)\n", 
            median_light_time, median_heavy_time, p_value_time_heavy_median, conf.int_time_heavy_median[1], conf.int_time_heavy_median[2]))


#Чи є час доставки більшим при більшій відстані між продавцем та покупцем (різниця середніх)?
close_time <- df_test %>% filter(distance_category == "close") %>% pull(actual_delivery_time)
far_time <- df_test %>% filter(distance_category == "far") %>% pull(actual_delivery_time)

mean_close <- mean(close_time, na.rm = TRUE)
mean_far <- mean(far_time, na.rm = TRUE)

t_test_distance <- t.test(far_time, close_time, alternative = "greater")

cat("Різниця середніх\n")
cat(sprintf("Середнє часу доставки (близько): %.3f\n", mean_close))
cat(sprintf("Середнє часу доставки (далеко): %.3f\n", mean_far))
cat(sprintf("p-value: %.6f\n", t_test_distance$p.value))
cat(sprintf("Довірчий інтервал: [%.3f; Inf)\n", t_test_distance$conf.int[1]))


#Чи є час доставки більшим при більшій відстані між продавцем та покупцем (різниця медіан)?
set.seed(123)
df_test_boot_distance_time <- df_test %>% slice_sample(n = 20000)

boot_median_diff_time_distance <- function(data, indices) {
  d <- data[indices, ]
  med_far_time <- median(d$actual_delivery_time[d$distance_category == "far"])
  med_close_time <- median(d$actual_delivery_time[d$distance_category == "close"])
  return(med_far_time - med_close_time)
}

set.seed(123)
boot_result_time_distance_median <- boot(data = df_test_boot_distance_time, statistic = boot_median_diff_time_distance, R = 1000)

median_far_time <- median(df_test_boot_distance_time$actual_delivery_time[df_test_boot_distance_time$distance_category == "far"])
median_close_time <- median(df_test_boot_distance_time$actual_delivery_time[df_test_boot_distance_time$distance_category == "close"])
se_boot_time_distance_median <- sd(boot_result_time_distance_median$t)

T_time_distance_median <- (median_far_time - median_close_time) / se_boot_time_distance_median
p_value_time_distance_median <- 1 - pnorm(T_time_distance_median)
conf.int_time_distance_median <- c(median_far_time - median_close_time - qnorm(0.95) * se_boot_time_distance_median, Inf)

cat(sprintf("Медіана часу доставки (близько): %.3f\nМедіана часу доставки (далеко): %.3f\np-value: %f\nДовірчий інтервал: [%.3f; %.3f)\n", 
            median_close_time, median_far_time, p_value_time_distance_median, conf.int_time_distance_median[1], conf.int_time_distance_median[2]))


#Оцінка замовлення -  час доставки (різниця середніх)
df_test <- df_lab2 %>%
  mutate(
    review_score = as.numeric(as.character(review_score)),  # <- це головне!
    rating_category = ifelse(review_score <= 3, "bad", "good")
  )

t.test(actual_delivery_time ~ rating_category, data = df_test, alternative = "greater")


#Оцінка замовлення -  час доставки (різниця медіан)
# Бутстреп для перевірки різниці медіан
set.seed(123)
df_test_boot_rating_time <- df_test %>% slice_sample(n = 20000)

boot_median_diff_time_rating <- function(data, indices) {
  d <- data[indices, ]
  med_bad_time <- median(d$actual_delivery_time[d$rating_category == "bad"])
  med_good_time <- median(d$actual_delivery_time[d$rating_category == "good"])
  return(med_bad_time - med_good_time)
}

set.seed(123)
boot_result_time_rating_median <- boot(data = df_test_boot_rating_time, statistic = boot_median_diff_time_rating, R = 1000)

median_bad_time <- median(df_test_boot_rating_time$actual_delivery_time[df_test_boot_rating_time$rating_category == "bad"])
median_good_time <- median(df_test_boot_rating_time$actual_delivery_time[df_test_boot_rating_time$rating_category == "good"])
se_boot_time_rating_median <- sd(boot_result_time_rating_median$t)

T_time_rating_median <- (median_bad_time - median_good_time) / se_boot_time_rating_median
p_value_time_rating_median <- 1 - pnorm(T_time_rating_median)
conf.int_time_rating_median <- c(median_bad_time - median_good_time - qnorm(0.95) * se_boot_time_rating_median, Inf)

cat(sprintf("Медіана часу доставки (погані): %.3f\nМедіана часу доставки (хороші): %.3f\np-value: %f\nДовірчий інтервал: [%.3f; %.3f)\n", 
            median_bad_time, median_good_time, p_value_time_rating_median, conf.int_time_rating_median[1], conf.int_time_rating_median[2]))


# Регіон покупця - час доставки
factor_ttests_time <- df_test %>%
  select(actual_delivery_time, customer_region) %>%
  group_by(factor(customer_region)) %>%
  summarize(across(actual_delivery_time, list)) %>%
  tidyr::expand(nesting(category1 = `factor(customer_region)`, value1 = actual_delivery_time),
                nesting(category2 = `factor(customer_region)`, value2 = actual_delivery_time)) %>%
  filter(category1 != category2) %>%factor_ttests_time <- df_test %>%
  select(actual_delivery_time, customer_region) %>%
  group_by(factor(customer_region)) %>%
  summarize(across(actual_delivery_time, list)) %>%
  tidyr::expand(nesting(category1 = `factor(customer_region)`, value1 = actual_delivery_time),
                nesting(category2 = `factor(customer_region)`, value2 = actual_delivery_time)) %>%
  filter(category1 != category2) %>%
  rowwise %>%
  mutate(key = paste0(sort(c(category1, category2)), collapse = "-")) %>%
  distinct(key, .keep_all = TRUE) %>%
  select(category1, category2, value1, value2) %>%
  mutate(p_value = t.test(value1, value2)$p.value) %>%
  ungroup()

factor_ttests_time %>%
  mutate(p_value_bonf = p.adjust(p_value, method = "bonferroni"),
         p_value_BH = p.adjust(p_value, method = "BH"),
         reject = p_value < 0.05,
         reject_bonf = p_value_bonf < 0.05,
         reject_BH = p_value_BH < 0.05) %>%
  select(-c("value1", "value2"))
factor_ttests_time %>%
  mutate(
    p_value_bonf = p.adjust(p_value, method = "bonferroni"),
    p_value_BH = p.adjust(p_value, method = "BH"),
    reject = p_value < 0.05,
    reject_bonf = p_value_bonf < 0.05,
    reject_BH = p_value_BH < 0.05
  ) %>%
  select(category1, category2, p_value, p_value_bonf, p_value_BH, reject, reject_bonf) %>%
  arrange(p_value) %>% 
  gt() %>%
  fmt_scientific(columns = starts_with("p_value"), decimals = 2) %>%
  tab_header(title = "Порівняння регіонів за часом доставки (t-тест)") %>%
  cols_label(
    category1 = "Регіон 1",
    category2 = "Регіон 2",
    p_value = "p-value",
    p_value_bonf = "p-value (Bonferroni)",
    p_value_BH = "p-value (BH)",
    reject = "p < 0.05",
    reject_bonf = "Bonf. < 0.05"
  ) %>%
  tab_style(
    style = list(cell_text(weight = "bold", color = "red")),
    locations = cells_body(columns = c(reject, reject_bonf), rows = reject == TRUE | reject_bonf == TRUE)
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.width = px(950),
    column_labels.font.size = px(15),
    table.font.size = px(14)
  )

  rowwise %>%
  mutate(key = paste0(sort(c(category1, category2)), collapse = "-")) %>%
  distinct(key, .keep_all = TRUE) %>%
  select(category1, category2, value1, value2) %>%
  mutate(p_value = t.test(value1, value2)$p.value) %>%
  ungroup()

factor_ttests_time %>%
  mutate(p_value_bonf = p.adjust(p_value, method = "bonferroni"),
         p_value_BH = p.adjust(p_value, method = "BH"),
         reject = p_value < 0.05,
         reject_bonf = p_value_bonf < 0.05,
         reject_BH = p_value_BH < 0.05) %>%
  select(-c("value1", "value2"))



# Категорія товару - вартість доставки
top_categories <- df_lab2 %>%
  count(product_category_name, sort = TRUE) %>%
  top_n(8, n) %>%
  pull(product_category_name)

df_top_categories <- df_lab2 %>%
  filter(product_category_name %in% top_categories)

# T-тест по вартості доставки
factor_ttests <- df_top_categories %>%
  select(freight_value, product_category_name) %>%
  group_by(factor(product_category_name)) %>%
  summarize(across(freight_value, list)) %>%
  tidyr::expand(
    nesting(category1 = `factor(product_category_name)`, value1 = freight_value),
    nesting(category2 = `factor(product_category_name)`, value2 = freight_value)
  ) %>%
  filter(category1 != category2) %>%
  rowwise() %>%
  mutate(key = paste0(sort(c(category1, category2)), collapse = "-")) %>%
  distinct(key, .keep_all = TRUE) %>%
  select(category1, category2, value1, value2) %>%
  mutate(p_value = t.test(value1, value2)$p.value) %>%
  ungroup()

factor_ttests %>%
  mutate(
    p_value_bonf = p.adjust(p_value, method = "bonferroni"),
    p_value_BH = p.adjust(p_value, method = "BH"),
    reject = p_value < 0.05,
    reject_bonf = p_value_bonf < 0.05,
    reject_BH = p_value_BH < 0.05
  ) %>%
  select(category1, category2, p_value, p_value_bonf, p_value_BH, reject, reject_bonf) %>%
  arrange(p_value) %>%  
  gt() %>%
  fmt_scientific(columns = starts_with("p_value"), decimals = 2) %>%
  tab_header(title = "Порівняння вартості доставки між топ-8 категоріями (t-тест)") %>%
  cols_label(
    category1 = "Категорія 1",
    category2 = "Категорія 2",
    p_value = "p-value",
    p_value_bonf = "p-value (Bonferroni)",
    p_value_BH = "p-value (BH)",
    reject = "p < 0.05",
    reject_bonf = "Bonf. < 0.05"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "red"),
    locations = cells_body(columns = c(reject, reject_bonf), rows = reject == TRUE | reject_bonf == TRUE)
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.width = px(1000),
    column_labels.font.size = px(15),
    table.font.size = px(14)
  )


# Вартість доставки для категорій "furniture_decor", "telephony" (різниця середніх)
t.test(freight_value ~ product_category_name, data = subset(df_test, product_category_name %in% c("furniture_decor", "telephony")),
       alternative = "greater")


# Регіон покупця - вартість доставки
factor_ttests <- df_test %>%
  select(freight_value, customer_region) %>%
  group_by(factor(customer_region)) %>%
  summarize(across(freight_value, list)) %>%
  tidyr::expand(
    nesting(category1 = `factor(customer_region)`, value1 = freight_value),
    nesting(category2 = `factor(customer_region)`, value2 = freight_value)
  ) %>%
  filter(category1 != category2) %>%
  rowwise() %>%
  mutate(key = paste0(sort(c(category1, category2)), collapse = "-")) %>%
  distinct(key, .keep_all = TRUE) %>%
  select(category1, category2, value1, value2) %>%
  mutate(p_value = t.test(value1, value2)$p.value) %>%
  ungroup()

factor_ttests %>%
  mutate(
    p_value_bonf = p.adjust(p_value, method = "bonferroni"),
    p_value_BH = p.adjust(p_value, method = "BH"),
    reject = p_value < 0.05,
    reject_bonf = p_value_bonf < 0.05,
    reject_BH = p_value_BH < 0.05
  ) %>%
  select(category1, category2, p_value, p_value_bonf, p_value_BH, reject, reject_bonf) %>%
  arrange(p_value) %>%  
  gt() %>%
  fmt_scientific(columns = starts_with("p_value"), decimals = 2) %>%
  tab_header(title = "Порівняння вартості доставки між регіонами (t-тест)") %>%
  cols_label(
    category1 = "Регіон 1",
    category2 = "Регіон 2",
    p_value = "p-value",
    p_value_bonf = "p-value (Bonferroni)",
    p_value_BH = "p-value (BH)",
    reject = "p < 0.05",
    reject_bonf = "Bonf. < 0.05"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "red"),
    locations = cells_body(columns = c(reject, reject_bonf), rows = reject == TRUE | reject_bonf == TRUE)
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.width = px(1000),
    column_labels.font.size = px(15),
    table.font.size = px(14)
  )


# Вартість доставки для регіонів "Southeast", "North" (різниця середніх)
t.test(freight_value ~ customer_region, data = subset(df_test, customer_region %in% c("Southeast", "North")),
       alternative = "greater")


# Ціна товару (середнє вибіркове)
ci <- df_lab2 %>% summarize(mean_hat = mean(price), se = sd(price) / sqrt(n()),
                            lower = mean(price) - qnorm(0.975) * sd(price) / sqrt(n()),
                            upper = mean(price) + qnorm(0.975) * sd(price) / sqrt(n()))
cat(sprintf("Середнє вибіркове: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$mean_hat, ci$se, ci$lower, ci$upper))


# Ціна товару (медіана)
boot_median <- function(data, indices) {
  return(median(data[indices]))
}
set.seed(123) 
boot_result <- boot(data = df_lab2$price, statistic = boot_median, R = 1000, simple = TRUE)

se_boot <- sd(boot_result$t)
ci <- df_lab2 %>% summarize(median_hat = median(price), se = se_boot,
                            lower = median(price) - qnorm(0.975) * se_boot,
                            upper = median(price) + qnorm(0.975) * se_boot)
cat(sprintf("Медіана: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$median_hat, se_boot, ci$lower, ci$upper))


# Ціна товару (вибіркова дисперсія)
price = df_lab2$price
se_analyt <- sqrt((mean((price - mean(price))^4) - var(price)^2) / length(price))
ci <- df_lab2 %>% summarize(var_hat = var(price), se = se_analyt,
                            lower = var(price) - qnorm(0.975) * se_analyt,
                            upper = var(price) + qnorm(0.975) * se_analyt)
cat(sprintf("Вибіркова дисперсія: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$var_hat, se_analyt, ci$lower, ci$upper))


# Ціна товару (вибіркове середньоквадратичне відхилення)
price = df_lab2$price
se_analyt <- sqrt((mean((price - mean(price))^4) - var(price)^2) / (4 * var(price) * length(price)))
ci <- df_lab2 %>% summarize(sd_hat = sd(price), se = se_analyt,
                            lower = sd(price) - qnorm(0.975) * se_analyt,
                            upper = sd(price) + qnorm(0.975) * se_analyt)
cat(sprintf("Вибіркове середньоквадратичне відхилення: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$sd_hat, se_analyt, ci$lower, ci$upper))


# Частка вчасно доставлених замовлень
df_lab2 <- df_lab2 %>%
  mutate(on_time = ifelse(actual_delivery_time <= estimated_delivery_time, 1, 0))

ci <- df_lab2 %>% summarize(p_hat = mean(on_time), n = sum(!is.na(on_time)), 
                            se = sqrt(p_hat * (1 - p_hat) / n),
                            lower = p_hat - qnorm(0.975) * se,
                            upper = p_hat + qnorm(0.975) * se
)

cat(sprintf("Частка вчасних доставок: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$p_hat, ci$se, ci$lower, ci$upper))


# Вага (середнє вибіркове) 
ci_mean_weight <- df_lab2 %>%
  summarize(mean_hat = mean(product_weight_g, na.rm = TRUE),
            se = sd(product_weight_g, na.rm = TRUE) / sqrt(sum(!is.na(product_weight_g))),
            lower = mean_hat - qnorm(0.975) * se,
            upper = mean_hat + qnorm(0.975) * se)

cat(sprintf("Середнє: %.2f г\nСтандартна похибка: %.2f\n95%% довірчий інтервал: [%.2f; %.2f]\n",
            ci_mean_weight$mean_hat, ci_mean_weight$se,
            ci_mean_weight$lower, ci_mean_weight$upper))


# Вага (медіана)
boot_median_weight <- function(data, indices) {
  return(median(data[indices], na.rm = TRUE))
}

set.seed(123)
boot_result_weight <- boot(data = df_lab2$product_weight_g, 
                           statistic = boot_median_weight, 
                           R = 1000, 
                           simple = TRUE)

se_boot_weight <- sd(boot_result_weight$t)
median_hat <- median(df_lab2$product_weight_g, na.rm = TRUE)

ci_median_weight <- c(
  lower = median_hat - qnorm(0.975) * se_boot_weight,
  upper = median_hat + qnorm(0.975) * se_boot_weight
)

cat(sprintf("Медіана: %.2f г\nСтандартна похибка: %.2f\n95%% довічрий інтервал: [%.2f; %.2f]\n",
            median_hat, se_boot_weight, ci_median_weight["lower"], ci_median_weight["upper"]))


# Вага (вибіркова дисперсія)
weight <- na.omit(df_lab2$product_weight_g)

se_var_weight <- sqrt((mean((weight - mean(weight))^4) - var(weight)^2) / length(weight))

ci_var_weight <- c(
  var_hat = var(weight),
  lower = var(weight) - qnorm(0.975) * se_var_weight,
  upper = var(weight) + qnorm(0.975) * se_var_weight
)

cat(sprintf("Дисперсія: %.2f\nСтандартна похибка: %.2f\n95%% довірчий інтервал: [%.2f; %.2f]\n",
            ci_var_weight["var_hat"], se_var_weight,
            ci_var_weight["lower"], ci_var_weight["upper"]))


# Вага (вибіркове середньоквадратичне відхилення)
se_sd_weight <- sqrt((mean((weight - mean(weight))^4) - var(weight)^2) / 
                     (4 * var(weight) * length(weight)))

ci_sd_weight <- c(
  sd_hat = sd(weight),
  lower = sd(weight) - qnorm(0.975) * se_sd_weight,
  upper = sd(weight) + qnorm(0.975) * se_sd_weight
)

cat(sprintf("Вибіркове середньоквадратичне відхилення: %.2f\nСтандартна похибка: %.2f\n95%% довірчий інтервал: [%.2f; %.2f]\n",
            ci_sd_weight["sd_hat"], se_sd_weight,
            ci_sd_weight["lower"], ci_sd_weight["upper"]))


# Вартість доставки (середнє вибіркове)
ci_freight <- df_lab2 %>% summarize(
  mean_hat = mean(freight_value, na.rm = TRUE),
  se = sd(freight_value, na.rm = TRUE) / sqrt(n()),
  lower = mean_hat - qnorm(0.975) * se,
  upper = mean_hat + qnorm(0.975) * se
)

cat(sprintf("Середнє: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci_freight$mean_hat, ci_freight$se, ci_freight$lower, ci_freight$upper))


# Вартість доставки (медіана)
boot_median_freight <- function(data, indices) {
  return(median(data[indices]))
}

set.seed(123) 
boot_result_freight <- boot(data = df_lab2$freight_value, statistic = boot_median_freight, R = 1000, simple = TRUE)

se_boot_freight <- sd(boot_result_freight$t)
ci_freight_median <- df_lab2 %>% summarize(
  median_hat = median(freight_value, na.rm = TRUE),
  se = se_boot_freight,
  lower = median_hat - qnorm(0.975) * se,
  upper = median_hat + qnorm(0.975) * se
)

cat(sprintf("Медіана: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci_freight_median$median_hat, se_boot_freight, ci_freight_median$lower, ci_freight_median$upper))


# Вартість доставки (вибіркова дисперсія)
price_var_freight <- var(df_lab2$freight_value, na.rm = TRUE)
n_freight <- sum(!is.na(df_lab2$freight_value))

ci_freight_var <- df_lab2 %>% summarize(
  var_hat = price_var_freight,
  se = sqrt((mean((df_lab2$freight_value - mean(df_lab2$freight_value, na.rm = TRUE))^4) - var(df_lab2$freight_value, na.rm = TRUE)^2) / length(df_lab2$freight_value)),
  lower = price_var_freight - qnorm(0.975) * se,
  upper = price_var_freight + qnorm(0.975) * se
)

cat(sprintf("Дисперсія: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci_freight_var$var_hat, ci_freight_var$se, ci_freight_var$lower, ci_freight_var$upper))


# Вартість доставки (вибіркове середньоквадратичне відхилення)
# Квантилі розподілу хі-квадрат для 95% довірчого інтервалу
alpha <- 0.05
chi2_lower <- qchisq(alpha / 2, df = n_freight - 1)
chi2_upper <- qchisq(1 - alpha / 2, df = n_freight - 1)


SE_sd_freight <- sqrt((2 * (n_freight - 1) * price_var_freight^2) / (chi2_upper * chi2_lower))

ci_sd_freight <- df_lab2 %>% summarize(
  sd_hat = sqrt(price_var_freight),
  se = SE_sd_freight,
  lower = sqrt((n_freight - 1) * price_var_freight / chi2_upper),
  upper = sqrt((n_freight - 1) * price_var_freight / chi2_lower)
)

cat(sprintf("Вибіркове середньоквадратичне відхилення: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n",
            ci_sd_freight$sd_hat, ci_sd_freight$se, ci_sd_freight$lower, ci_sd_freight$upper))


# Довжина товару (середнє вибіркове)
ci <- df_clean %>% summarize(mean_hat = mean(product_length_cm), se = sd(product_length_cm) / sqrt(n()),
                             lower = mean(product_length_cm) - qnorm(0.975) * sd(product_length_cm) / sqrt(n()),
                             upper = mean(product_length_cm) + qnorm(0.975) * sd(product_length_cm) / sqrt(n()))
cat(sprintf("Середнє вибіркове: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$mean_hat, ci$se, ci$lower, ci$upper))


# Довжина товару (вибіркова дисперсія)
x = df_clean$product_length_cm
se_analyt <- sqrt((mean((x - mean(x))^4) - var(x)^2) / length(x))
ci <- df_clean %>% summarize(var_hat = var(product_length_cm), se = se_analyt,
                             lower = var_hat - qnorm(0.975) * se_analyt,
                             upper = var_hat + qnorm(0.975) * se_analyt)
cat(sprintf("Вибіркова дисперсія: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$var_hat, se_analyt, ci$lower, ci$upper))


# Довжина товару (вибіркове середньоквадратичне відхилення)
se_analyt <- sqrt((mean((x - mean(x))^4) - var(x)^2) / (4 * var(x) * length(x)))
ci <- df_clean %>% summarize(sd_hat = sd(product_length_cm), se = se_analyt,
                             lower = sd_hat - qnorm(0.975) * se_analyt,
                             upper = sd_hat + qnorm(0.975) * se_analyt)
cat(sprintf("Вибіркове середньоквадратичне відхилення: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$sd_hat, se_analyt, ci$lower, ci$upper))


# Ширина товару (середнє вибіркове)
ci <- df_clean %>% summarize(mean_hat = mean(product_width_cm), se = sd(product_width_cm) / sqrt(n()),
                             lower = mean(product_width_cm) - qnorm(0.975) * sd(product_width_cm) / sqrt(n()),
                             upper = mean(product_width_cm) + qnorm(0.975) * sd(product_width_cm) / sqrt(n()))
cat(sprintf("Середнє вибіркове: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$mean_hat, ci$se, ci$lower, ci$upper))


# Ширина товару (медіана)
boot_median <- function(data, indices) {
  return(median(data[indices]))
}
set.seed(123) 
boot_result <- boot(data = df_clean$product_width_cm, statistic = boot_median, R = 1000, simple = TRUE)

se_boot <- sd(boot_result$t)
ci <- df_clean %>% summarize(median_hat = median(product_width_cm), se = se_boot,
                             lower = median_hat - qnorm(0.975) * se_boot,
                             upper = median_hat + qnorm(0.975) * se_boot)
cat(sprintf("Медіана: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$median_hat, se_boot, ci$lower, ci$upper))


# Ширина товару (вибіркова дисперсія)
x = df_clean$product_width_cm
se_analyt <- sqrt((mean((x - mean(x))^4) - var(x)^2) / length(x))
ci <- df_clean %>% summarize(var_hat = var(product_width_cm), se = se_analyt,
                             lower = var_hat - qnorm(0.975) * se_analyt,
                             upper = var_hat + qnorm(0.975) * se_analyt)
cat(sprintf("Вибіркова дисперсія: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$var_hat, se_analyt, ci$lower, ci$upper))


# Ширина товару (вибіркове середньоквадратичне відхилення)
se_analyt <- sqrt((mean((x - mean(x))^4) - var(x)^2) / (4 * var(x) * length(x)))
ci <- df_clean %>% summarize(sd_hat = sd(product_width_cm), se = se_analyt,
                             lower = sd_hat - qnorm(0.975) * se_analyt,
                             upper = sd_hat + qnorm(0.975) * se_analyt)
cat(sprintf("Вибіркове середньоквадратичне відхилення: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$sd_hat, se_analyt, ci$lower, ci$upper))


# Висота товару (середнє вибіркове)
ci <- df_clean %>% summarize(mean_hat = mean(product_height_cm), se = sd(product_height_cm) / sqrt(n()),
                             lower = mean(product_height_cm) - qnorm(0.975) * sd(product_height_cm) / sqrt(n()),
                             upper = mean(product_height_cm) + qnorm(0.975) * sd(product_height_cm) / sqrt(n()))
cat(sprintf("Середнє вибіркове: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$mean_hat, ci$se, ci$lower, ci$upper))


# Висота товару (медіана)
boot_median <- function(data, indices) {
  return(median(data[indices]))
}
set.seed(123) 
boot_result <- boot(data = df_clean$product_height_cm, statistic = boot_median, R = 1000, simple = TRUE)

se_boot <- sd(boot_result$t)
ci <- df_clean %>% summarize(median_hat = median(product_height_cm), se = se_boot,
                             lower = median_hat - qnorm(0.975) * se_boot,
                             upper = median_hat + qnorm(0.975) * se_boot)
cat(sprintf("Медіана: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$median_hat, se_boot, ci$lower, ci$upper))


# Висота товару (вибіркова дисперсія)
x = df_clean$product_height_cm
se_analyt <- sqrt((mean((x - mean(x))^4) - var(x)^2) / length(x))
ci <- df_clean %>% summarize(var_hat = var(product_height_cm), se = se_analyt,
                             lower = var_hat - qnorm(0.975) * se_analyt,
                             upper = var_hat + qnorm(0.975) * se_analyt)
cat(sprintf("Вибіркова дисперсія: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$var_hat, se_analyt, ci$lower, ci$upper))


# Висота товару (вибіркове середньоквадратичне відхилення)
se_analyt <- sqrt((mean((x - mean(x))^4) - var(x)^2) / (4 * var(x) * length(x)))
ci <- df_clean %>% summarize(sd_hat = sd(product_height_cm), se = se_analyt,
                             lower = sd_hat - qnorm(0.975) * se_analyt,
                             upper = sd_hat + qnorm(0.975) * se_analyt)
cat(sprintf("Вибіркове середньоквадратичне відхилення: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$sd_hat, se_analyt, ci$lower, ci$upper))


# Назва товару (середнє вибіркове)
ci_mean_name_length <- df_lab2 %>% 
  summarize(mean_hat = mean(product_name_lenght, na.rm = TRUE),
            se = sd(product_name_lenght, na.rm = TRUE) / sqrt(n()),
            lower = mean_hat - qnorm(0.975) * se,
            upper = mean_hat + qnorm(0.975) * se)

cat(sprintf("Середнє: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci_mean_name_length$mean_hat, ci_mean_name_length$se, ci_mean_name_length$lower, ci_mean_name_length$upper))


# Назва товару (медіана)
boot_median_name_length <- function(data, indices) {
  return(median(data[indices]))
}

set.seed(123)
boot_result_median_name_length <- boot(data = df_lab2$product_name_lenght, 
                                        statistic = boot_median_name_length, R = 1000)

se_boot_median <- sd(boot_result_median_name_length$t)

ci_median_name_length <- df_lab2 %>% 
  summarize(median_hat = median(product_name_lenght, na.rm = TRUE), 
            se = se_boot_median,
            lower = median_hat - qnorm(0.975) * se,
            upper = median_hat + qnorm(0.975) * se)

cat(sprintf("Медіана: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci_median_name_length$median_hat, se_boot_median, ci_median_name_length$lower, ci_median_name_length$upper))


# Назва товару (дисперсія)
price = df_lab2$product_name_lenght
se_analyt_var <- sqrt((mean((price - mean(price))^4) - var(price)^2) / length(price))

ci_var_name_length <- df_lab2 %>% 
  summarize(var_hat = var(product_name_lenght, na.rm = TRUE), 
            se = se_analyt_var,
            lower = var_hat - qnorm(0.975) * se,
            upper = var_hat + qnorm(0.975) * se)

cat(sprintf("Дисперсія: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci_var_name_length$var_hat, se_analyt_var, ci_var_name_length$lower, ci_var_name_length$upper))


# Назва товару (вибіркове середньоквадратичне відхилення)
se_analyt_sd <- sqrt((mean((price - mean(price))^4) - var(price)^2) / (4 * var(price) * length(price)))

ci_sd_name_length <- df_lab2 %>% 
  summarize(sd_hat = sd(product_name_lenght, na.rm = TRUE), 
            se = se_analyt_sd,
            lower = sd_hat - qnorm(0.975) * se,
            upper = sd_hat + qnorm(0.975) * se)

cat(sprintf("вибіркове середньоквадратичне відхилення: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci_sd_name_length$sd_hat, se_analyt_sd, ci_sd_name_length$lower, ci_sd_name_length$upper))
df_clean <- df_clean %>%
  mutate(actual_delivery_days = as.numeric(difftime(order_delivered_customer_date, order_purchase_timestamp, units = "days")))

df_clean <- df_clean %>%
  filter(actual_delivery_days > 0)


# Фактичний час доставки (середнє вибіркове)
ci <- df_clean %>% summarize(mean_hat = mean(actual_delivery_days), se = sd(actual_delivery_days) / sqrt(n()),
                             lower = mean(actual_delivery_days) - qnorm(0.975) * sd(actual_delivery_days) / sqrt(n()),
                             upper = mean(actual_delivery_days) + qnorm(0.975) * sd(actual_delivery_days) / sqrt(n()))
cat(sprintf("Середнє вибіркове: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$mean_hat, ci$se, ci$lower, ci$upper))


# Фактичний час доставки (медіана)
boot_median <- function(data, indices) {
  return(median(data[indices]))
}
set.seed(123) 
boot_result <- boot(data = df_clean$actual_delivery_days, statistic = boot_median, R = 1000, simple = TRUE)

se_boot <- sd(boot_result$t)
ci <- df_clean %>% summarize(median_hat = median(actual_delivery_days), se = se_boot,
                             lower = median_hat - qnorm(0.975) * se_boot,
                             upper = median_hat + qnorm(0.975) * se_boot)
cat(sprintf("Медіана: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$median_hat, se_boot, ci$lower, ci$upper))


# Фактичний час доставки (вибіркова дисперсія)
x = df_clean$actual_delivery_days
se_analyt <- sqrt((mean((x - mean(x))^4) - var(x)^2) / length(x))
ci <- df_clean %>% summarize(var_hat = var(actual_delivery_days), se = se_analyt,
                             lower = var_hat - qnorm(0.975) * se_analyt,
                             upper = var_hat + qnorm(0.975) * se_analyt)
cat(sprintf("Вибіркова дисперсія: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$var_hat, se_analyt, ci$lower, ci$upper))


# Фактичний час доставки (вибіркове середньоквадратичне відхилення)
se_analyt <- sqrt((mean((x - mean(x))^4) - var(x)^2) / (4 * var(x) * length(x)))
ci <- df_clean %>% summarize(sd_hat = sd(actual_delivery_days), se = se_analyt,
                             lower = sd_hat - qnorm(0.975) * se_analyt,
                             upper = sd_hat + qnorm(0.975) * se_analyt)
cat(sprintf("Вибіркове середньоквадратичне відхилення: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$sd_hat, se_analyt, ci$lower, ci$upper))

df_clean <- df_clean %>%
  mutate(expected_delivery_days = as.numeric(difftime(order_estimated_delivery_date, order_purchase_timestamp, units = "days")))

df_clean <- df_clean %>%
  filter(expected_delivery_days > 0)


# Очікуваний час доставки (середнє вибіркове)
ci <- df_clean %>% summarize(mean_hat = mean(expected_delivery_days), se = sd(expected_delivery_days) / sqrt(n()),
                             lower = mean(expected_delivery_days) - qnorm(0.975) * sd(expected_delivery_days) / sqrt(n()),
                             upper = mean(expected_delivery_days) + qnorm(0.975) * sd(expected_delivery_days) / sqrt(n()))
cat(sprintf("Середнє вибіркове: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$mean_hat, ci$se, ci$lower, ci$upper))


# Очікуваний час доставки (медіана)
boot_median <- function(data, indices) {
  return(median(data[indices]))
}
set.seed(123) 
boot_result <- boot(data = df_clean$expected_delivery_days, statistic = boot_median, R = 1000, simple = TRUE)

se_boot <- sd(boot_result$t)
ci <- df_clean %>% summarize(median_hat = median(expected_delivery_days), se = se_boot,
                             lower = median_hat - qnorm(0.975) * se_boot,
                             upper = median_hat + qnorm(0.975) * se_boot)
cat(sprintf("Медіана: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$median_hat, se_boot, ci$lower, ci$upper))


# Очікуваний час доставки (вибіркова дисперсія)
x = df_clean$expected_delivery_days
se_analyt <- sqrt((mean((x - mean(x))^4) - var(x)^2) / length(x))
ci <- df_clean %>% summarize(var_hat = var(expected_delivery_days), se = se_analyt,
                             lower = var_hat - qnorm(0.975) * se_analyt,
                             upper = var_hat + qnorm(0.975) * se_analyt)
cat(sprintf("Вибіркова дисперсія: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$var_hat, se_analyt, ci$lower, ci$upper))


# Очікуваний час доставки (вибіркове середньоквадратичне відхилення)
se_analyt <- sqrt((mean((x - mean(x))^4) - var(x)^2) / (4 * var(x) * length(x)))
ci <- df_clean %>% summarize(sd_hat = sd(expected_delivery_days), se = se_analyt,
                             lower = sd_hat - qnorm(0.975) * se_analyt,
                             upper = sd_hat + qnorm(0.975) * se_analyt)
cat(sprintf("Вибіркове середньоквадратичне відхилення: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$sd_hat, se_analyt, ci$lower, ci$upper))

# Довжина опису товару (вибіркове середнє)
ci_mean_description_length <- df_lab2 %>% 
  summarize(mean_hat = mean(product_description_lenght, na.rm = TRUE),
            se = sd(product_description_lenght, na.rm = TRUE) / sqrt(n()),
            lower = mean_hat - qnorm(0.975) * se,
            upper = mean_hat + qnorm(0.975) * se)

cat(sprintf("Середнє: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci_mean_description_length$mean_hat, ci_mean_description_length$se, ci_mean_description_length$lower, ci_mean_description_length$upper))


# Довжина опису товару (медіана)
boot_median_description_length <- function(data, indices) {
  return(median(data[indices]))
}

set.seed(123)
boot_result_median_description_length <- boot(data = df_lab2$product_description_lenght, 
                                              statistic = boot_median_description_length, R = 1000)

# Обчислення стандартної похибки для медіани
se_boot_median_description <- sd(boot_result_median_description_length$t)

ci_median_description_length <- df_lab2 %>% 
  summarize(median_hat = median(product_description_lenght, na.rm = TRUE), 
            se = se_boot_median_description,
            lower = median_hat - qnorm(0.975) * se,
            upper = median_hat + qnorm(0.975) * se)

cat(sprintf("Медіана: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci_median_description_length$median_hat, se_boot_median_description, ci_median_description_length$lower, ci_median_description_length$upper))


# Довжина опису товару (дисперсія)
price_description = df_lab2$product_description_lenght
se_analyt_var_description <- sqrt((mean((price_description - mean(price_description))^4) - var(price_description)^2) / length(price_description))

ci_var_description_length <- df_lab2 %>% 
  summarize(var_hat = var(product_description_lenght, na.rm = TRUE), 
            se = se_analyt_var_description,
            lower = var_hat - qnorm(0.975) * se,
            upper = var_hat + qnorm(0.975) * se)

cat(sprintf("Дисперсія: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci_var_description_length$var_hat, se_analyt_var_description, ci_var_description_length$lower, ci_var_description_length$upper))


# Довжина опису товару (вибіркове середньоквадратичне відхилення)
se_analyt_sd_description <- sqrt((mean((price_description - mean(price_description))^4) - var(price_description)^2) / (4 * var(price_description) * length(price_description)))

ci_sd_description_length <- df_lab2 %>% 
  summarize(sd_hat = sd(product_description_lenght, na.rm = TRUE), 
            se = se_analyt_sd_description,
            lower = sd_hat - qnorm(0.975) * se,
            upper = sd_hat + qnorm(0.975) * se)

cat(sprintf("Середньоквадратичне відхилення: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci_sd_description_length$sd_hat, se_analyt_sd_description, ci_sd_description_length$lower, ci_sd_description_length$upper))

# Відстань (середнє вибіркове)
ci <- df_lab2 %>% summarize(mean_hat = mean(distance_km), se = sd(distance_km) / sqrt(n()),
                            lower = mean(distance_km) - qnorm(0.975) * sd(distance_km) / sqrt(n()),
                            upper = mean(distance_km) + qnorm(0.975) * sd(distance_km) / sqrt(n()))
cat(sprintf("Середнє вибіркове: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$mean_hat, ci$se, ci$lower, ci$upper))


# Відстань (медіана)
boot_median <- function(data, indices) {
  return(median(data[indices]))
}
set.seed(123) 
boot_result <- boot(data = df_lab2$distance_km, statistic = boot_median, R = 1000, simple = TRUE)

se_boot <- sd(boot_result$t)
ci <- df_lab2 %>% summarize(median_hat = median(distance_km), se = se_boot,
                            lower = median(distance_km) - qnorm(0.975) * se_boot,
                            upper = median(distance_km) + qnorm(0.975) * se_boot)
cat(sprintf("Медіана: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$median_hat, se_boot, ci$lower, ci$upper))


# Відстань (вибіркова дисперсія)
distance_km = df_lab2$distance_km
se_analyt <- sqrt((mean((distance_km - mean(distance_km))^4) - var(distance_km)^2) / length(distance_km))
ci <- df_lab2 %>% summarize(var_hat = var(distance_km), se = se_analyt,
                            lower = var(distance_km) - qnorm(0.975) * se_analyt,
                            upper = var(distance_km) + qnorm(0.975) * se_analyt)
cat(sprintf("Вибіркова дисперсія: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$var_hat, se_analyt, ci$lower, ci$upper))


# Відстань (вибіркове середньоквадратичне відхилення)
distance_km = df_lab2$distance_km
se_analyt <- sqrt((mean((distance_km - mean(distance_km))^4) - var(distance_km)^2) / (4 * var(distance_km) * length(distance_km)))
ci <- df_lab2 %>% summarize(sd_hat = sd(distance_km), se = se_analyt,
                            lower = sd(distance_km) - qnorm(0.975) * se_analyt,
                            upper = sd(distance_km) + qnorm(0.975) * se_analyt)
cat(sprintf("Вибіркове середньоквадратичне відхилення: %.3f\nСтандартна похибка: %.3f\n95%% довірчий інтервал: [%.3f; %.3f]\n", 
            ci$sd_hat, se_analyt, ci$lower, ci$upper))


# Корреляція
df_cor <- df_lab2 %>% select(distance_km, freight_value) %>% na.omit()
set.seed(1)
df_cor <- df_cor %>% slice_sample(n = 10000)

boot_cor_with_sd <- function(X, indices, estimate_var = TRUE) {
  cor_bar <- cor(X[indices, ], method = 'spearman')[1, 2]
  
  if (estimate_var) {
    boot_out <- boot(X[indices, ], statistic = boot_cor_with_sd, R = 100, estimate_var = FALSE)
    return(c(cor_bar, var(boot_out$t[, 1])))
  } else {
    return(cor_bar)
  }
}

set.seed(123)
boot_result_cor <- boot(data = df_cor, statistic = boot_cor_with_sd, R = 1000)
boot.ci(boot_result_cor, index = c(1, 2), type=c("perc", "basic", "norm", 'stud', 'bca'))


#Кількість бутстрап-ітерацій та внутрішній бутстрап вдвічі більший 

df_cor <- df_lab2 %>% select(distance_km, freight_value) %>% na.omit()
set.seed(1)
df_cor <- df_cor %>% slice_sample(n = 10000)

boot_cor_with_sd <- function(X, indices, estimate_var = TRUE) {
  cor_bar <- cor(X[indices, ], method = 'spearman')[1, 2]
  
  if (estimate_var) {
    boot_out <- boot(X[indices, ], statistic = boot_cor_with_sd, R = 200, estimate_var = FALSE)
    return(c(cor_bar, var(boot_out$t[, 1])))
  } else {
    return(cor_bar)
  }
}

set.seed(123)
boot_result_cor <- boot(data = df_cor, statistic = boot_cor_with_sd, R = 2000)
boot.ci(boot_result_cor, index = c(1, 2), type=c("perc", "basic", "norm", 'stud', 'bca'))


# на повному датасеті
df_cor <- df_lab2 %>% select(distance_km, freight_value) %>% na.omit()
set.seed(1)

boot_cor_simple <- function(X, indices) {
  cor(X[indices, ], method = 'spearman')[1, 2]
}

set.seed(123)
boot_result_cor <- boot(data = df_cor, statistic = boot_cor_simple, R = 2000)
boot.ci(boot_result_cor, type = c("perc", "basic", "norm", "bca"))

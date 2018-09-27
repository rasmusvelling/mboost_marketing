library(tidyverse)

## Saturating vs non saturating plot

ggplot(data = data.frame(
  x = c(1:100,      110,  120,  130,  200, 1:200),
  y = c(sqrt(1:100), 10.4, 10.6, 10.7, 10.7, (1:200)*.075),
  label = c(rep("Saturating channel", times=104), rep("Non-saturating", times=200))
)) +
  geom_path(aes(x=x, y=y, color=label)) +
  labs(title = "Saturating vs. non-saturating channel", x = "Dose (Impressions)", y = "Effect", colour="") +
  theme(legend.position = "bottom")


## Threshold
ggplot(data = data.frame(
  x = c(0, 99, 100:300),
  y = c(0,  0, .00001*(100:300)^3-10),
  label = "Channel with threshold")
) +
  geom_vline(xintercept=100, linetype="dotted") +
  geom_text(data=data.frame(x=100, event="Threshold"), mapping=aes(x=x, y=5, label=event), size=4, angle=90, vjust=-0.4, hjust=0) +
  geom_path(aes(x=x, y=y, color=label)) +
  labs(title="Channel with minimum-threshold for effect" , x = "Dose (impressions)", y = "Effect", colour="") +
  theme(legend.position = "bottom")


## Simulating data for test
df <- data.frame(
  x1 = runif(1000, 0, 300),
  x2 = runif(1000, 0, 300),
  x3 = runif(1000, 0, 300),
  e  = rnorm(1000, mean = 0, sd = 2)
)

df$y <- .1*df$x1 +
  20/(1+ exp(-.1*(df$x2 - 100))) +
  sqrt(df$x3) +
  df$e

test     <- sample(1:nrow(df), 200)
df_train <- df[-test, ] %>% select(-e)
df_test  <- df[test, ] %>% select(-e)

# Estimation
fit_lm <- lm(y ~ x1 + x2 + x3, data = df_train)

library(mboost)
fit_gam <- mboost::gamboost(
  formula = as.formula(paste0(
    "y ~ bbs(x1, knots=4, degree=3) + ",
    "bbs(x2, knots=4, degree=3) + ", 
    "bbs(x3, knots=4, degree=3)")),
  data = df_train,
  control = boost_control(mstop = 2500)
)


# Test
df_test <- df_test %>% 
  arrange(y) %>% 
  data.frame()

df_plot_yy <- data.frame(
  y = c(
    df_test$y,
    predict(fit_lm, newdata = df_test),
    predict(fit_gam, newdata = df_test)[, 1]),
  index = c(
    1:nrow(df_test),
    1:nrow(df_test),
    1:nrow(df_test)),
  Model = c(
    rep("y, true value", times=nrow(df_test)),
    rep("Linear regression", times=nrow(df_test)), 
    rep("MBOOST", times=nrow(df_test)))
)

ggplot(data = df_plot_yy) +
  labs(title="Predicted value vs. true") +
  geom_point(aes(x=index, y=y, color=Model)) +
  xlab("Observations ordered by y")


# Errors
ggplot(data = data.frame(
  Error = c(
    predict(fit_lm, newdata = df_test) - df_test$y, 
    predict(fit_gam, newdata = df_test) - df_test$y
  ), 
  Model = c(
    rep("Linear regression", times=nrow(df_test)), 
    rep("MBOOST", times=nrow(df_test))
  )
), aes(Error, fill = Model)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', bins=30) +
  ggtitle("Distribution of errors")


## Stylized fits
df_style_x1 <- data.frame(
  x1 = 1:300,
  x2 = 150,
  x3 = 150
)
df_style_x1$y_true <- .1 * df_style_x1$x1 + 20/(1+ exp(-.1*(150 - 100))) + sqrt(150)
df_style_x1$y_lm   <- predict(fit_lm,  newdata = df_style_x1)
df_style_x1$y_gam  <- predict(fit_gam, newdata = df_style_x1)[, 1]

df_style_x1_plot <- df_style_x1 %>% 
  select(x1, y_true, y_lm, y_gam) %>% 
  gather(key, value, -x1) %>% 
  mutate(key = ifelse(key=="y_gam", "MBOOST", key)) %>% 
  mutate(key = ifelse(key=="y_lm", "Linear regression", key)) %>% 
  mutate(key = ifelse(key=="y_true", "y, true value", key))

ggplot(df_style_x1_plot) +
  ggtitle("Stylized fit, x1") +
  geom_path(aes(x=x1, y=value, color=key))



df_style_x2 <- data.frame(
  x1 = 150,
  x2 = 1:300,
  x3 = 150
)
df_style_x2$y_true <- 20/(1+ exp(-.1*(df_style_x2$x2 - 100))) + .1 * 150 + sqrt(150)
df_style_x2$y_lm   <- predict(fit_lm,  newdata = df_style_x2)
df_style_x2$y_gam  <- predict(fit_gam, newdata = df_style_x2)[, 1]

df_style_x2_plot <- df_style_x2 %>% 
  select(x2, y_true, y_lm, y_gam) %>% 
  gather(key, value, -x2) %>% 
  mutate(key = ifelse(key=="y_gam", "MBOOST", key)) %>% 
  mutate(key = ifelse(key=="y_lm", "Linear regression", key)) %>% 
  mutate(key = ifelse(key=="y_true", "y, true value", key))

ggplot(df_style_x2_plot) +
  ggtitle("Stylized fit, x2") +
  geom_path(aes(x=x2, y=value, color=key))



df_style_x3 <- data.frame(
  x1 = 150,
  x2 = 150,
  x3 = 1:300
)
df_style_x3$y_true <- sqrt(df_style_x3$x3) + .1 * 150 + 20/(1+ exp(-.1*(150 - 100)))
df_style_x3$y_lm   <- predict(fit_lm,  newdata = df_style_x3)
df_style_x3$y_gam  <- predict(fit_gam, newdata = df_style_x3)[, 1]

df_style_x3_plot <- df_style_x3 %>% 
  select(x3, y_true, y_lm, y_gam) %>% 
  gather(key, value, -x3) %>% 
  mutate(key = ifelse(key=="y_gam", "MBOOST", key)) %>% 
  mutate(key = ifelse(key=="y_lm", "Linear regression", key)) %>% 
  mutate(key = ifelse(key=="y_true", "y, true value", key))

ggplot(df_style_x3_plot) +
  ggtitle("Stylized fit, x3") +
  geom_path(aes(x=x3, y=value, color=key))


library(dplyr)
library(tidyr)
library(Rcpp)
library(ggplot2)

cppFunction('NumericVector lg_iter(NumericVector x) {
  int n = x.size();
  NumericVector out(n);
  for(int i = 0; i < n; i++){
    int cont = 0;
    while(x[i] > 1) {
      x[i] = log(x[i])/log(2);
      cont = cont + 1;
    }
    out[i] = cont;
  }
  return out;
}')


datos <- data.frame(n = 1:10000) %>% 
  mutate(
    ec_1 = log2(lg_iter(n)),
    ec_2 = 2^(lg_iter(n)),
    ec_3 = sqrt(2)^log2(n),
    ec_4 = n^2,
    ec_5 = factorial(n),
    ec_6 = gamma(log2(n) + 1),
    ec_7 = 1.5^n,
    ec_8 = n^3,
    ec_9 = log2(n) * log2(n),
    ec_10 = log2(factorial(n)),
    ec_11 = 2^2^n,
    ec_12 = 2,
    ec_13 = log(log(n)),
    ec_14 = lg_iter(n),
    ec_15 = n*2^n,
    ec_16 = n^log2(log2(n)),
    ec_17 = log(n),
    ec_18 = 1,
    ec_19 = 2^log2(n),
    ec_20 = log2(n)^log2(n),
    ec_21 = exp(n),
    ec_22 = 4^log2(n),
    ec_23 = factorial(n + 1),
    ec_24 = sqrt(log2(n)),
    ec_25 = lg_iter(log2(n)),
    ec_26 = 2^(sqrt(2*log2(n))),
    ec_27 = n,
    ec_28 = 2^n,
    ec_29 = n*log2(n),
    ec_30 = 2^2^(n+1)
    )


datos %>% 
  select(n, ec_30, ec_11) %>% 
  gather(ec, valor, 2:ncol(.)) %>% 
  filter(n < 3) %>% 
  ggplot() +
  geom_line(aes(n, valor, linetype = ec)) +
  theme_bw() +
  ggsave("graf_01.png")


datos %>% 
  select(n, ec_23, ec_5, ec_21) %>% 
  gather(ec, valor, 2:ncol(.)) %>% 
  filter(n < 8) %>% 
  ggplot() +
  geom_line(aes(n, valor, linetype = ec)) +
  theme_bw() +
  ggsave("graf_02.png")


datos %>% 
  select(n, ec_15, ec_28, ec_7) %>% 
  gather(ec, valor, 2:ncol(.)) %>% 
  filter(n < 10) %>% 
  ggplot() +
  geom_line(aes(n, valor, linetype = ec)) +
  theme_bw() +
  ggsave("graf_03.png")

datos %>% 
  select(n, ec_16, ec_20, ec_6, ec_29, ec_8) %>% 
  gather(ec, valor, 2:ncol(.)) %>% 
  filter(n < 9) %>% 
  ggplot() +
  geom_line(aes(n, valor, linetype = ec)) +
  theme_bw() +
  ggsave("graf_04.png")

datos %>% 
  select(n, ec_4, ec_22, ec_10, ec_19, ec_27, ec_3, ec_26) %>% 
  gather(ec, valor, 2:ncol(.)) %>% 
  filter(n < 10) %>% 
  ggplot() +
  geom_line(aes(n, valor, linetype = ec)) +
  theme_bw() +
  ggsave("graf_05.png")

datos %>% 
  select(n, ec_9, ec_17, ec_24, ec_13) %>% 
  gather(ec, valor, 2:ncol(.)) %>% 
  filter(n < 500) %>% 
  ggplot() +
  geom_line(aes(n, valor, linetype = ec)) +
  theme_bw() +
  ggsave("graf_06.png")

datos %>% 
  select(n, ec_2, ec_14, ec_25, ec_1, ec_12, ec_18) %>% 
  gather(ec, valor, 2:ncol(.)) %>% 
  filter(n < 500) %>% 
  ggplot() +
  geom_line(aes(n, valor, linetype = ec)) +
  theme_bw() +
  ggsave("graf_07.png")












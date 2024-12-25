rm(list = ls())

library(rvest)
library(dplyr)

page = read_html("~/../desktop/table.html")

trs = page %>%
  html_nodes("tr")

df = data.frame(CARGO = NA, FUNCAO = NA, SALARIO = NA)

for (i in 3:length(trs)) {
  tds = trs[i] %>%
    html_nodes("td") %>%
    html_text()
  
  df = rbind(df, c(tds[4], tds[5], tds[24]))
}

df = df %>%
  filter(row_number() != 1) %>%
  mutate(
    SALARIO = SALARIO %>%
      gsub("[.]", "", .) %>%
      gsub("[,]", ".", .) %>%
      as.numeric()
  )

df
  
head(df)
tail(df)

df = transform(df, SALARIO = gsub("[.]", "", SALARIO))
df = transform(df, SALARIO = gsub("[,]", ".", SALARIO))
df = transform(df, SALARIO = as.numeric(SALARIO))

df_agg = aggregate(SALARIO ~ CARGO, FUN = mean, data = df)

df_agg[order(df_agg$SALARIO, decreasing = TRUE),]

df[df$CARGO == "PUBLICITARIO",]
df[df$CARGO == "PEDAGOGO",]
df[df$CARGO == "ANALISTA DE SISTEMAS",]

#### create figures re corpus ####


# library -----------------------------------------------------------------

library(tidyverse)

# data --------------------------------------------------------------------

load(str_c(str_remove(here::here(),"iuropa-.*"),"CJEU-text-corpus/data-final/text_final.RData"))
load(str_c(str_remove(here::here(),"iuropa-.*"),"CJEU-text-corpus/data-working/text_processed.RData"))
load(str_c(str_remove(here::here(),"iuropa-.*"),"CJEU-text-corpus/data-working/caps_processed.RData"))

# create sample toy dataset -----------------------------------------------

sample_texts <- text_final |> 
  filter(text_summary == 0,
         section == "grounds",
         paragraph_type == "paragraph") |> 
  filter(str_sub(file,-2,-1) == "EN") |> 
  filter(str_detect(file, "EU_C")) |> 
  slice((n()-100000):n()) |> 
  summarise(ecli = str_extract(file, "ECLI.*[:digit:]{4}.*(?=_[:digit:]{4})"),
            ecli = str_replace_all(ecli, "_", ":"),
            date_decision = str_replace_all(str_extract(file, "(?<=_)[:digit:]{4}_[:digit:]{2}_[:digit:]{2}(?=_)"),"_","-"),
            year_decision = as.integer(str_extract(ecli, "[:digit:]{4}")),
            language = str_extract(file, "EN|FR"),
            text = text)
  


# basic summary -----------------------------------------------------------

paras_notext <- text_processed |> 
  summarise(para_id = str_c(file, line_id),
            file_id = str_extract(file, "ECLI.*(?=.html)"),
            language = str_extract(str_sub(file, -8,-1), "EN|FR"),
            source = source,
            year = as.integer(str_extract(file, "[:digit:]{4}")),
            court = str_sub(str_extract(file, "EU_[:upper:]"), -1,-1))

paras_notext |> 
  count(court, language, source, year) |> 
  filter(source != "ocr") |> 
  ggplot(aes(x = year, y = n, fill = court)) +
  geom_col(color = "grey93") +
  geom_hline(yintercept = c(100000,200000), lty = 2, color = "grey50") +
  geom_vline(xintercept = 1997, lty = 3, color = "grey50") +
  facet_grid(source ~ language) +
  theme_bw() +
  scale_fill_brewer(type = "qual") +
  theme(legend.position = "bottom",
        panel.grid = element_line(color = "grey96")) +
  labs(title = "Number of paragraphs per year, language and source website")
ggsave("elx_cur_en_fr_year.png", width = 10)


# all caps ----------------------------------------------------------------

caps_processed |> 
  summarise(para_id = str_c(file, line_id),
            file_id = str_extract(file, "ECLI.*"),
            language = str_extract(str_sub(file, -8,-1), "EN|FR"),
            source = "ocr",
            year = as.integer(str_extract(file, "[:digit:]{4}")),
            court = str_sub(str_extract(file, "EU_[:upper:]"), -1,-1)) |> 
  bind_rows(paras_notext) |> 
  filter(year < 1985) |>
  filter(file_id %in% unique(caps_processed$file)) |> 
  count(court, language, source, year) |> 
  ggplot(aes(x = year, y = n, fill = court)) +
  geom_col(color = "grey93", show.legend = FALSE) +
  geom_hline(yintercept = c(3000,6000,9000), lty = 2, color = "grey50") +
  geom_vline(xintercept = c(1960,1970,1980), lty = 3, color = "grey50") +
  facet_grid(source ~ language) +
  theme_bw() +
  scale_fill_brewer(type = "qual") +
  theme(legend.position = "bottom",
        panel.grid = element_line(color = "grey96")) +
  labs(title = "Number of paragraphs per year and language (1952-1984)",
       subtitle = "Documents that only exist in ALL CAPS online")
ggsave("elx_ocr_en_fr_year.png", width = 10)


# final corpus ------------------------------------------------------------

# stats for summary tables
paras_notext_final |> distinct(file_id, source, language) |> count(source, language)
paras_notext |> distinct(file_id, source, language) |> count(source, language)

# plots
ma <- function(x, n = 6){stats::filter(x, rep(1 / n, n), sides = 2)}

doc_vars <- text_final |> 
  dplyr::group_by(file) |> 
  dplyr::summarise(
    n_paras = dplyr::n(),
    n_headings = length(paragraph_type[paragraph_type == "heading"]),
    n_quotes = length(paragraph_type[paragraph_type == "quote"]),
    unique_sections = length(unique(section)),
    unique_para_types = length(unique(paragraph_type)),
    unique_para_nos = length(unique(paragraph_number)),
    sum_nchar = sum(nchar),
    mean_nchar = mean(nchar)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(#source = ifelse(stringr::str_detect(file, "curia"), "Curia", "EurLex"),
    language = ifelse(stringr::str_detect(file, "EN"), "English", "French"),
    date_decision = lubridate::ymd(stringr::str_extract(file, "(?<=_)[:digit:]{4}_[:digit:]{2}_[:digit:]{2}(?=_)")),
    year = as.integer(stringr::str_sub(date_decision, 1, 4)))

doc_vars |> 
  dplyr::mutate(month = lubridate::floor_date(date_decision, unit = "month")) |> 
  dplyr::group_by(month, language) |> 
  dplyr::summarise(n_paras = mean(n_paras)) |> 
  dplyr::group_by(language) |> 
  dplyr::arrange(month) |> 
  dplyr::mutate(ma = ma(n_paras, n = 6)) |> 
  dplyr::ungroup() |> 
  ggplot(aes(x = month, y = n_paras)) +
  geom_point(color = "grey60", alpha = 0.2) +
  geom_line(aes(y = ma), size = 0.5, color = "black") +
  facet_wrap(~language) +
  theme_bw() +
  labs(x = "Date of decision", y = "Average number of paragraphs in a document",
       title = "Average number of paragraphs (6-month moving average)")
ggsave("docs_n_paras_month.png", width = 10)

paras_notext_final <- text_final |> 
  filter(text_summary != 1) |> 
  summarise(para_id = str_c(file, line_id),
            file_id = str_extract(file, "ECLI.*"),
            language = str_extract(str_sub(file, -8,-1), "EN|FR"),
            source = source,
            nchar = nchar,
            date_decision = lubridate::ymd(stringr::str_extract(file, "(?<=_)[:digit:]{4}_[:digit:]{2}_[:digit:]{2}(?=_)")),
            year = as.integer(str_extract(file, "[:digit:]{4}")),
            court = str_sub(str_extract(file, "EU_[:upper:]"), -1,-1))

paras_notext_final |> 
  count(court, language, source, year) |> 
  ggplot(aes(x = year, y = n, fill = court)) +
  geom_col(color = "grey93") +
  geom_hline(yintercept = c(100000,200000), lty = 2, color = "grey50") +
  #geom_vline(xintercept = 1997, lty = 3, color = "grey50") +
  facet_wrap(~language, dir = "v") +
  theme_bw() +
  scale_fill_brewer(type = "qual") +
  theme(legend.position = "bottom",
        panel.grid = element_line(color = "grey96")) +
  labs(title = "Number of paragraphs per year and language")
ggsave("final_en_fr_year.png", width = 10)

# nchar density
paras_notext_final |> 
  sample_n(10000) |> 
  filter(nchar < 1000) |> 
  ggplot(aes(x = year, y = nchar)) +
  geom_density2d_filled() +
  facet_grid(court~language)

# length of paragraphs per month
paras_notext_final |> 
  filter(court != "F") |> 
  dplyr::mutate(month = lubridate::floor_date(date_decision, unit = "month")) |> 
  dplyr::group_by(month, language, court) |> 
  dplyr::summarise(mean_nchar = mean(nchar)) |> 
  dplyr::group_by(language, court) |> 
  dplyr::arrange(month) |> 
  dplyr::mutate(ma = ma(mean_nchar, n = 6)) |> 
  dplyr::ungroup() |> 
  ggplot(aes(x = month, y = mean_nchar)) +
  geom_point(color = "grey60", alpha = 0.2) +
  geom_line(aes(y = ma), size = 0.5, color = "black") +
  facet_grid(court~language) +
  theme_bw() +
  labs(x = "Date of decision", y = "Average length of paragraphs in characters per month",
       title = "Average length of paragraphs per month (6-month moving average)")
ggsave("paras_mean_para_nchar_month.png", width = 10)

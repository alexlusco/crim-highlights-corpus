pacman::p_load(rvest, dplyr, readr, tibble, stringr)

#Index scrape

url <- "https://www.crimsl.utoronto.ca/research-publications/faculty-publications?sort_by=pub_date_value&items_per_page=All"

page <- read_html(url)

headers <- page %>%
  html_node("body") %>%
  html_nodes("h3") %>%
  html_text(trim = TRUE) #headers

page_url <- page %>%
  html_node("body") %>%
  html_nodes("h3 a") %>%
  html_attr("href") %>%
  url_absolute(url) #page urls

pub_type <- page %>%
  html_node("body") %>%
  html_nodes(".taxonomy-term-reference-0") %>%
  html_text(trim = TRUE) #publication type

pub_date <- page %>%
  html_node("body") %>%
  html_nodes(".date-display-single") %>%
  html_text(trim = TRUE) %>%
  as.numeric() #publication date

index_df <- tibble(
  vol_issue_title = headers,
  page_url = page_url,
  pub_type = pub_type,
  pub_date = pub_date
)

#Page scrape

page_urls <- index_df %>% filter(pub_type != "Special Issue") %>% select(page_url) %>% unlist(use.names = FALSE)#remove special issues

output <- list()

for(p in page_urls){
  
  print(glue::glue("Scraping {p}..."))
  
  page <- read_html(p)
  
  headlines <- page %>%
    html_node("body") %>%
    html_nodes(xpath = '//*[(@id = "block-system-main")]//h2') %>%
    html_text(trim = TRUE)
  
  headlines <- headlines[-c(1, 2)]
  
  main_text <- page %>%
    html_node("body") %>%
    html_nodes("h2~ p") %>%
    html_text(trim = TRUE) %>%
    toString() %>%
    as_tibble() %>%
    str_split(pattern = "(?<=Back to top)") %>%
    unlist()
  
  main_text <- main_text[-c(9)]
  
  link_to_pdf <- page %>%
    html_node("body") %>%
    html_node(".file-default a") %>%
    html_attr("href") %>%
    url_absolute(url)
  
  curr_data <- tibble(
    headlines = headlines,
    main_text = main_text,
    pdf_url = link_to_pdf,
    page_url = p
  )
  
  output[[p]] <- curr_data
  
  Sys.sleep(10)
  
}

final_df <- bind_rows(output) %>% left_join(index_df, by = "page_url")

write_csv(final_df, "data/ch_df_raw.csv")

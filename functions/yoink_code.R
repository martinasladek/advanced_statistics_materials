yoink_code <- function(filename_no_ext){
  
  # save the document: 
  #rstudioapi::documentSave()
  
  # get all the codechunks 
  
  # filename_no_ext = "02b_ma_advanced"
  
  qmd_file <- paste0(filename_no_ext, ".qmd")
  
  lines <- readLines(qmd_file)
  
  code_lines <- 
    tibble::tibble(
      lines = lines,
      fence = stringr::str_detect(lines, "```"),
      fence_start = stringr::str_detect(lines, "```\\{r"), 
      fence_end = fence & !fence_start, 
      code_start = ifelse(fence_start, cumsum(fence_start), NA), 
      code_end = ifelse(fence_end, cumsum(fence_end), NA)
    ) |> 
    tidyr::fill(code_start, .direction = "down") |> 
    tidyr::fill(code_end, .direction = "up") |> 
    dplyr::mutate(
      chunk_index = ifelse(code_start == code_end, code_start, 0),
      lines = stringr::str_replace_all(lines, "```$", "```\\\n\\\n"),
      lines = stringr::str_replace_all(lines, "```\\{r", "```\\{r eval=FALSE,"), 
      lines = stringr::str_replace_all(lines, ",,", ","), 
      lines = stringr::str_replace_all(lines, "#<[0-9]+>", "")
    ) |> 
    dplyr::filter(chunk_index != 0)|> 
    dplyr::group_by(chunk_index) |> 
    dplyr::filter(
      any(stringr::str_detect(lines, "code")),
      # !any(stringr::str_detect(lines, "echo=FALSE")), 
      # !any(stringr::str_detect(lines, "echo=FALSE")), 
      # !any(stringr::str_detect(lines, "include=FALSE"))
    ) |> 
    dplyr::ungroup() |> 
    dplyr::filter(
      !stringr::str_detect(lines, "label:"), 
      !stringr::str_detect(lines, "fig-cap:")
    ) |> 
    dplyr::pull(lines) 
  
  output_dir <<- paste0("code/", filename_no_ext, "_code.qmd")
  
  writeLines(code_lines, output_dir)
  
}

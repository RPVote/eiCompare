rpv_toDF <- function (rpv_results = NULL, model = NULL, jurisdiction = "", 
                       preferred_candidate = "", party = "", election_type = "", 
                       year = "", contest = "", candidate = "") 
{
  if (inherits(rpv_results, "eiCompare")) {
    fun <- function(x) {
      df <- summary(rpv_results)[[x]]
      df <- df %>% dplyr::select(-"sd")
      colnames(df) <- paste(x, colnames(df), sep = ".")
      df <- data.frame(original_name = row.names(df), df)
      return(df)
    }
    sink(tempfile())
    smry_dfs <- lapply(names(summary(rpv_results)), fun)
    rpv_data <- suppressMessages(Reduce(dplyr::inner_join, 
                                        smry_dfs))
    sink()
  }
  else if (inherits(rpv_results, "data.frame")) {
    rpv_data <- data.frame(original_name = row.names(rpv_results), 
                           rpv_results)
  }
  else {
    stop("incorrect class type for argument rpv_results")
  }
  rownames(rpv_data) <- 1:nrow(rpv_data)
  colnames(rpv_data) <- colnames(rpv_data) %>% stringr::str_to_lower()
  newcols <- gsub("mean", "Estimate", colnames(rpv_data))
  newcols <- gsub("ci_95_lower", "Lower_Bound", newcols)
  newcols <- gsub("ci_95_upper", "Upper_Bound", newcols)
  colnames(rpv_data) <- newcols
  plotDF <- rpv_data %>% dplyr::mutate(Model = model, Jurisdiction = jurisdiction, 
                                       Election_Type = election_type, Year = as.numeric(year), 
                                       Contest = contest, Candidate = candidate, Party = party, 
                                       Preferred_Candidate = preferred_candidate) %>% tidyr::pivot_longer(cols = grep("\\.", 
                                                                                                                      colnames(rpv_data), value = TRUE), names_to = c("Voter_Race", 
                                                                                                                                                                      ".value"), names_pattern = "(.*)\\.(.*)", names_repair = "unique")
  plotDF$Voter_Race <- gsub("^pct_", "", plotDF$Voter_Race)
  colnames(plotDF) <- gsub("_ei", "", colnames(plotDF))
  colnames(plotDF) <- gsub("_rxc", "", colnames(plotDF))
  return(plotDF)
}

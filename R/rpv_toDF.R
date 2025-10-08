#' @export
#' 
#'@author Rachel Carroll <rachelcarroll4@@gmail.com>
#'@author Kassra Oskooii  <kassrao@@gmail.com>

#' @title Transform RPV results into a simple dataframe
#' object 
#' 
#' @description Create a dataframe from RPV analysis output to facilitate 
#' RPV visualizations. The output dataframe of this function can be used directly 
#' in \code{rpv_plot()}.
#'
#' @param rpv_results RPV analysis results either from the output of 
#' \code{ei_iter()} or \code{ei_rxc()} from the \code{eiCompare} package or from 
#' the internal function \code{ci_cvap_full()}.
#' @param model A string indicating the model used to create \code{rpv_results}. 
#' Examples include "ei", "rxc", "ei cvap", etc. 
#' @param candidate A character vector of candidate names written as they would 
#' appear on a visualization. The candidate names must be listed in the same order
#' as the candidate estimates appear in \code{rpv_results}, i.e the same order 
#' as the \code{cands} argument in \code{eiCompare::ei_iter()} or 
#' \code{eiCompare::ei_rxc()}.
#' @param preferred_candidate A character vector of races indicating racial
#' preference of each candidate. The racial preferences must be listed 
#' in the correct order with respect to \code{candidate}.
#' @param party A character vector containing the political parties of the 
#' candidates. Must be listed in the correct order with respect to 
#' \code{candidate}.
#' @param jurisdiction A string of the jurisdiction.
#' @param election_type A string on the election type (usually "General" or 
#' "Primary")
#' @param year The year of the contest
#' @param contest A string of contest name as it would appear in an rpv visualization
#' (e.g. "President" or "Sec. of State")
#' 
#' @return rpv results in a data.frame
#' 
#' @examples
#' \donttest{
#' #library(eiCompare)
#' #data("south_carolina")
#' #prec_election_demog <- south_carolina[1:50,]
#' 
#' ## run rpv analysis
#' #eiVote <- eiCompare::ei_iter(
#'  # data = prec_election_demog,
#'  # cand_cols = c('pct_mcmaster', 'pct_smith'),
#'  # race_cols = c('pct_white', 'pct_black'),
#'  # totals_col = "total_vap"
#' #) %>%
#'  # rpv_normalize(
#'  #   cand_cols = c('pct_mcmaster', 'pct_smith'), 
#'  #   race_cols = c('pct_white', 'pct_black')
#'  # )
#' 
#' ## use function to create dataframe from rpv results
#' #plotDF <- rpv_toDF(
#'#   rpv_results = eiVote,
#'#   model = "ei vap", #since we used ei_iter model normalized with vap denominator       
#'#   jurisdiction = "Statewide",
#'#   candidate = c("McMaster", "Smith"), #must be in correct order relative to rpv_results                             
#'#   preferred_candidate = c("White", "Black"), #must be in correct order rpv_results  
#'#   party = c("Republican", "Democratic"),
#'#   election_type = "General",  
#'#   year = "2020",
#'#   contest = "Governor"
#'# )
#' }

rpv_toDF <- function(
    rpv_results = NULL,
    model = NULL,
    jurisdiction = "", 
    preferred_candidate = "",
    party = "", 
    election_type = "", # general or primary
   year = "", 
   contest = "", #e.g. "Senate"
   candidate = "" # cand names
   ) {
  
  # -------------------------      Initialize dataframe      ---------------------------
  
  # If rxc or eiiter output
  # NOTE: This new way makes a dataframe from each object of summary(rpv_results)
  #   list then joins them. This is so if the summary(rpv_results) list 
  #   objects have cands in different orders, it doesn't get messed up.
  if (inherits(rpv_results, "eiCompare")) {
    
    # function to use in lapply to make data.frame from each object in 
    # summary(rpv_results)
    fun <- function(x) {
      df <- summary(rpv_results)[[x]]
      df <- df %>% dplyr::select(-"sd")
      colnames(df) <- paste(x, colnames(df), sep = ".")
      df <- data.frame(original_name = row.names(df), df)
      return(df)
    }
    
    sink(tempfile()) # sink to suppress the print from summary(rpv_results)
    smry_dfs <- lapply(names(summary(rpv_results)), fun)
    rpv_data <-suppressMessages(
      Reduce(dplyr::inner_join, smry_dfs)
    ) 
    sink()
    
    # If ci_cvap_full output
  } else if ( inherits(rpv_results, "data.frame") ){
    
    rpv_data <- data.frame(
      original_name = row.names(rpv_results),
      rpv_results)
    
  } else {stop("incorrect class type for argument rpv_results")}
  
  # ------------------------      Edit col/row names     -------------------------
  
  #add row names
  rownames(rpv_data) <- 1:nrow(rpv_data)
  
  #update col names
  colnames(rpv_data) <- colnames(rpv_data) %>% stringr::str_to_lower()
  
  newcols <- gsub("mean", "Estimate", colnames(rpv_data))
  newcols <- gsub("ci_lower", "Lower_Bound", newcols)
  newcols <- gsub("ci_upper", "Upper_Bound", newcols)
  
  # # NOTE: should make ei_cvap_full output names match ei_iter/rxc so dont need these 
  # newcols <- gsub("_pe", ".Estimate", colnames(rpv_data))
  # newcols <- gsub("_ci_95_low", ".Lower_Bound", newcols)
  # newcols <- gsub("_ci_95_high", ".Upper_Bound", newcols)
  
  colnames(rpv_data) <- newcols
  
  # ---------------------      Create Final Dataframe     -----------------------
  
  plotDF <- rpv_data %>% 
    dplyr::mutate(Model = model,
           Jurisdiction = jurisdiction, 
           Election_Type = election_type, 
           Year = as.numeric(year), 
           Contest = contest, 
           Candidate = candidate, 
           Party = party, 
           Preferred_Candidate = preferred_candidate
   ) %>% 
    tidyr::pivot_longer(
     cols = grep("\\.", 
                                                                                                                                                                 ".value"), names_pattern = "(.*)\\.(.*)", names_repair = "unique")
  plotDF$Voter_Race <- gsub("^pct_", "", plotDF$Voter_Race)
  
  # remove "_ei" or "_rxc" from colnames in case user used "name" argument in 
  # ei_iter() or ei_rxc()
  colnames(plotDF) <- gsub("_ei", "", colnames(plotDF))
  colnames(plotDF) <- gsub("_rxc", "", colnames(plotDF))
  
  return (plotDF)
  
} # END rpv_toDF function

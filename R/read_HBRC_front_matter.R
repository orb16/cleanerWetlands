#' Read in wetland-level metadata from HBRC files
#'
#' @param filepath Path to the excel file, including file name and extension
#' @param frontsheet Which sheet of the excel file contains the main metadata. Defaults to 1.
#' @param returnConditionDetailed Whether to return a simple metadata table (defaults to this; ie TRUE), or a more detailed table of the elements of condition, in which case a list is returned.
#' @return Simple metadatatable (unless a detailed version is required)
#' @importFrom rlang .data
#' @export
#'
#'
read_HBRC_front_matter <- function(filepath,
                                   frontsheet = 1,
                                   returnConditionDetailed = FALSE){

  allfront <- suppressMessages(readxl::read_excel(path = filepath,
                                          sheet = frontsheet, skip = 1, col_names = FALSE))

  # get wetland ID
  wetlandID <- allfront[[2]][which(allfront[[1]]== "Wetland ID:")]
  stopifnot("Check wetland ID" = all(any(is.numeric(wetlandID), is.character(wetlandID)),
                                     length(wetlandID > 0)))

  # get wetland name
  wetlandName <- allfront[[2]][which(allfront[[1]] == "Wetland name:")]
  stopifnot("Check wetland name" = all(any(is.numeric(wetlandName), is.character(wetlandName)),
                                       length(wetlandName > 0)))


  # get wetland region
  wetlandRegion <- allfront[[2]][which(allfront[[1]] == "Region:")]
  stopifnot("Check wetland region" = all(any(is.numeric(wetlandRegion), is.character(wetlandRegion)),
                                         length(wetlandRegion > 0)))

  wetlandAltitude <- allfront[[2]][which(allfront[[1]] == "Altitude:")]
  stopifnot("Check wetland altitude" = all(any(is.numeric(wetlandAltitude), is.character(wetlandAltitude)),
                                           length(wetlandAltitude > 0)))

  # 1899 is the origin date I got from the internet for fixing up dates in excel
  wetlandDate <- as.character(as.Date(as.numeric(allfront[[4]][which(allfront[[3]] == "Date:")]),
                                      origin = "1899-12-30"))

  wetlandGPS <- allfront[[4]][which(allfront[[3]] == "GPS/Grid Ref.:")]

  wetlandPlotN <-  as.numeric(allfront[[4]][which(allfront[[3]] == "No. of plots sampled:")])
  if(any(c(wetlandPlotN < 1, is.na(wetlandPlotN)))){
    stop("apparently no plots sampled please double check")
  }

  wetlandClasses <- janitor::row_to_names(data.frame(allfront[6:7, 1:4]),
                                 row_number = 1)
  stopifnot("problem with wetland classes" = names(wetlandClasses) == c(
    "Classification: I System", "IA Subsystem", "II Wetland Class",
    "IIA Wetland Form"
  ))
  names(wetlandClasses)[1] <- "I System"
  names(wetlandClasses) <- gsub(" ", "_", fixed = TRUE,    names(wetlandClasses))
  names(wetlandClasses) <- paste0("wetlandClasses_", names(wetlandClasses))
  row.names(wetlandClasses) <- NULL

  wetlandFieldTeam <- allfront[[2]][which(allfront[[1]] == "Field team:")]

  wetlandCondition <-  janitor::row_to_names(allfront[10:25, 1:4],
                                             row_number = 1) %>%
    tidyr::fill(.data$Indicator) %>%
    dplyr::rename(WCI_Individual_Score = "Score 0- 51") %>%
    dplyr::mutate(WCI_Individual_Score = as.numeric(.data$WCI_Individual_Score))

  wetlandConditionSummarised <- wetlandCondition %>%
    dplyr::group_by(.data$Indicator) %>%
    dplyr::summarise(WCI_meanIndicatorScore = mean(.data$WCI_Individual_Score,
                                            na.rm = TRUE),
              .groups = "drop") %>% data.frame(.)

  wetlandConditionOverall <- wetlandConditionSummarised %>%
    dplyr::summarise(WCI_overallWCI = sum(.data$WCI_meanIndicatorScore)) %>%
    data.frame(.)

  stopifnot("Problem with wetland condition index" = all.equal(dplyr::pull(.data$wetlandConditionOverall),
                                                               as.numeric(allfront[26,5]),
                                                               tolerance = 0.01))

  wetlandVegTypes <-  c(allfront[[2]][which(allfront[[1]] == "Main vegetation types:") : (which(allfront[[1]] == "Native fauna:") -1)], allfront[[3]][which(allfront[[1]] == "Main vegetation types:") : (which(allfront[[1]] == "Native fauna:") -1)])

  # NOT CURRENTLY returning these things as a separate table but could do
  wetlandVegTypes <- wetlandVegTypes[!is.na(wetlandVegTypes)]
  # this is not included - could get longer?
  wetlandNativeFauna <-  c(allfront[[2]][which(allfront[[1]] == "Native fauna:") :
                                           (which(allfront[[1]] == "Other comments:") -1)],
                           allfront[[3]][which(allfront[[1]] == "Native fauna:") :
                                           (which(allfront[[1]] == "Other comments:") -1)])
  wetlandNativeFauna <- wetlandNativeFauna[!is.na(wetlandNativeFauna)]
  # the collapsed part is included
  wetlandVegTypesCollapsed <- paste(wetlandVegTypes,
                                    collapse = ", ")
  wetlandNativeFaunaCollapsed <- paste(wetlandNativeFauna, collapse = ", ")

  wpStart <- which(allfront[[1]] == "Pressure")
  wpEnd <- grep("Wetland isolation", allfront[[1]], fixed = TRUE)
  wetlandPressures <- allfront[wpStart:wpEnd, 1:3] %>%
    janitor::row_to_names(row_number = 1) %>%
    dplyr::rename(PressureScore = .data$Score2) %>%
    dplyr::mutate(PressureScore = as.numeric(.data$PressureScore))

  ret <- data.frame(wetlandID, wetlandName, wetlandRegion, wetlandAltitude, wetlandDate,
                    wetlandFieldTeam,
                    wetlandGPS, wetlandPlotN, wetlandClasses, wetlandConditionOverall, wetlandVegTypesCollapsed,
                    wetlandNativeFaunaCollapsed)
  ret

  # return more detailed list if you want it
  if(returnConditionDetailed){
    ret <- list("metadata" = ret,
                "summarisedCondition" = wetlandConditionSummarised,
                "detailedCondition" = wetlandCondition)
  }

  return(ret)

}

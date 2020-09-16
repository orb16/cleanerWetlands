#' Read in wetland plot data from HBRC files
#'
#' There is a quirk with percentage data: if it has been formatted as a percentage in excel, read_excel(), which is used under the hood, doesn't pick up the formatting in complex sheets. Thus it either comes in as proportion or percentage with no indication. This function checks (based on summed total cover) and adds a warning where values are assumed to be proportion, and have therefore been multiplied by 100 to get to percentage.
#'
#' @param filepath Path to the excel file, including file name and extension
#' @return List with three parts "vegPlotData" (veg recce data), "soilSamples" (id of soil samples collected on each plot), "vegSamples" (id of plant foliage samples collected on each plot)
#' @importFrom rlang .data
#' @export
#'
#'

read_HBRC_veg_plots <- function(filepath){

  allSheets <- readxl::excel_sheets(filepath)
  vegSheets <- allSheets[grepl("wetland plot", tolower(allSheets))]

  vegging <- lapply(vegSheets, read_HBRC_veg_plot_individual, fp = filepath)

  vegplots_bound <- dplyr::bind_rows(lapply(vegging, function(x) x[["vegCover"]]))
  soilSamples_bound <- dplyr::bind_rows(lapply(vegging, function(x) x[["soilSamples"]]))
  vegSamples_bound <- dplyr::bind_rows(lapply(vegging, function(x) x[["vegSamples"]]))

  return(list("vegPlotData" = vegplots_bound,
              "soilSamples" = soilSamples_bound,
              "vegSamples" = vegSamples_bound))

}



# we use this function below to get the data out at a plot level
read_HBRC_veg_plot_individual <- function(fp, vs){

  veg <- data.frame(suppressMessages(readxl::read_excel(path = fp,
                                                col_names = TRUE,
                                                sheet = vs)))

  wetlandID <- veg[[2]][which(veg[[1]]== "Wetland ID:")]
  stopifnot("Check wetland ID" = all(any(is.numeric(wetlandID),
                                         is.character(wetlandID)), length(wetlandID > 0)))

  # get wetland name
  wetlandName <- veg[[2]][which(veg[[1]] == "Wetland name:")]
  stopifnot("Check wetland name" = all(any(is.numeric(wetlandName),
                                           is.character(wetlandName)), length(wetlandName > 0)))

  # 1899 is the origin date I got from the internet for fixing up dates in excel
  plotDate <- as.character(as.Date(as.numeric(veg[[2]][which(veg[[1]] == "Date:")]),
                                   origin = "1899-12-30"))

  plotGPSE <- veg[[2]][which(veg[[1]] == "GPS: E")]
  plotGPSN <- veg[[2]][which(veg[[1]] == "GPS: N")]

  plotRecorder <-  veg[[2]][which(veg[[1]] == "Recorder:")]

  plotVegStructure <- veg[[2]][which(veg[[1]] == "Veg structure:")]

  plotComposition <- veg[[2]][grep("composition", tolower(veg[[1]]))]

  plotVegStart<- grep("species", tolower(veg[[1]]))
  # sometimes this will come up twice, I think this is the most
  # conservative to get the first val
  plotVegStart <- plotVegStart[1]
  plotVegEnd <- grep("litter (total %)", fixed = TRUE,
                     tolower(veg[[1]]))-1
  plotVegEndCol <- as.numeric(grep("specimen collected", tolower(as.character(veg[plotVegStart, ])))) +1

  plotVeg <- data.frame(veg[(plotVegStart+2):plotVegEnd, 1:plotVegEndCol])


  names(plotVeg) <- c("Species", "Cover_percent", "HeightMax_m", "HeightAvg_m",
                      "Cover_class_<0.3m", "Cover_class_0.3-1m",
                      "Cover_class_1-2m", "Cover_class2-5m", "Cover_class_>5m",
                      "Seedling_count", "IS", "Notes", "WasSpecimenCollected",
                      "IsExotic")


  # exclude NA values

  endVeg <- min(which(is.na(plotVeg$Species))) -1

  plotVeg <- plotVeg[1:endVeg, ]
  plotVeg[2:8] <- apply(plotVeg[2:8], 2, as.numeric)

  totalVegCoverBySpecies <- sum(plotVeg$Cover_percent)

  coverConvertedFromProportion <- FALSE
  if(totalVegCoverBySpecies < 5){
    plotVeg[2] <- plotVeg[2] * 100
    coverConvertedFromProportion <- TRUE
    warning(paste("Converting apparent propertion data to percentage\nSite name =", wetlandName))
  }

  fmStart <- grep("field measurements", tolower(veg[[15]])) +1
  fmEnd <- grep("water temp", tolower(veg[[15]]))
  fieldMeasures <- veg[fmStart:fmEnd, 15:16]
  names(fieldMeasures) <- c("Variable", "Value")

  soilCoresDF <- fieldMeasures[grep("soil cores", tolower(fieldMeasures$Variable)), "Value"]
  soilCores <- as.character(stringr::str_split(soilCoresDF, pattern = ", ", simplify = TRUE))

  vegSamplesDF <- fieldMeasures[grep("foliage collected", tolower(fieldMeasures$Variable)), "Value"]
  vegSamples <- as.character(stringr::str_split(vegSamplesDF, pattern = ", ", simplify = TRUE))

  metadat <- data.frame(wetlandID, wetlandName, plotDate, plotGPSE, plotGPSN,
                        plotRecorder, plotVegStructure, plotComposition,
                        coverConvertedFromProportion)

  vf <- data.frame(metadat, plotVeg)

  ff <- data.frame(metadat, vegSamples)
  sf <- data.frame(metadat, soilCores)

  ret2 <- list("vegCover" = vf,
               "vegSamples" = ff,
               "soilSamples" = sf)

  return(ret2)
}
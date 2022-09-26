## install/load required packages
Rversion <- gsub(".+(4..).+", "\\1", R.version.string)
rlib <- file.path("R", Rversion)
# options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!dir.exists(rlib)) dir.create(rlib, recursive = TRUE); .libPaths(rlib, include.site = FALSE)

### In this section, only load the minimum of packages (Require, SpaDES.install) so all packages can be installed with
#    correct version numbering. If we load a package too early and it is an older version that what may be required by
#    a module, then we get an inconsistency
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("PredictiveEcology/Require@development")
library(Require)
Require("PredictiveEcology/SpaDES.project@transition", require = FALSE)

## modules
moduleGitRepos <- c("PredictiveEcology/Biomass_speciesFactorial (>= 0.0.12)"
                    , 'PredictiveEcology/Biomass_borealDataPrep@development (>= 1.5.4)'
                    , "PredictiveEcology/Biomass_speciesParameters@development (>= 1.0.0)"
                    , 'PredictiveEcology/Biomass_yieldTables (>= 0.0.8)'
)
modules <- extractPkgName(moduleGitRepos)

# A user may be using this as a git repository with submodules
clonedRepoModulePath <- "modulesCloned"
usingClonedSubmodules <-
  if (all(dir.exists(file.path(clonedRepoModulePath, modules)))) TRUE else FALSE
modulePath <- if (usingClonedSubmodules) clonedRepoModulePath else "modules"


if (!usingClonedSubmodules) { # if the user is not using submodules, then download the modules directly
  getModule(moduleGitRepos, overwrite = TRUE, modulePath = modulePath)
}

## packages that are required by modules
packagesNeededInModules <- SpaDES.project::packagesInModules(modulePath = modulePath)
# the modules point to GitHub versions of SpaDES.tools and SpaDES.core;
#    there is no need because CRAN versions are newer;
#    plus need RTools to install SpaDES.tools from GitHub. So, over
Require(c("SpaDES.core (>=1.1.0)", "SpaDES.tools (>= 1.0.0)",
          "googledrive", 'RCurl', 'XML',
          unlist(unname(packagesNeededInModules))),
        require = "SpaDES.core", # call `require` only on this package (same as `library`)
        verbose = 1)

#### Up to here, SpaDES.core is not needed so any packages that modules need can be updated with the previous function

# Now load packages that are needed for this global.R
#Require(c("SpaDES.core"))#,
#          "data.table", "sf", "stars"))

## environment setup -- all the functions below rely on knowing where modules
## are located via this command
setPaths(cachePath = "cache",
         inputPath = "inputs",
         modulePath = modulePath, # keep the cloned/uncloned separate
         outputPath = "outputs")


## Module documentation -- please go to these pages to read about each module
##  In some cases, there will be important defaults that a user should be aware of
##  or important objects (like studyArea) that may be essential
## browseURL('https://github.com/PredictiveEcology/Biomass_speciesFactorial/blob/main/Biomass_speciesFactorial.rmd')
## browseURL('https://github.com/PredictiveEcology/Biomass_speciesParameters/blob/EliotTweaks/Biomass_speciesParameters.md')
## browseURL('https://github.com/PredictiveEcology/Biomass_borealDataPrep/blob/development/Biomass_borealDataPrep.md')
## browseURL('https://github.com/PredictiveEcology/Biomass_core/blob/EliotTweaks/Biomass_core.md')


## simulation initialization. These may not be appropriate start and end times for one
## or more of the modules, e.g., they may only be defined with calendar dates

options(spades.moduleCodeChecks = FALSE,
        spades.recoveryMode = FALSE)
if (any(Sys.info()[["user"]] %in% c("emcintir", "elmci1"))) {
  Require("googledrive")
  options(
    gargle_oauth_cache = ".secrets",
    gargle_oauth_email = "eliotmcintire@gmail.com"
  )
}

## OBJECTS
fixRTM <- function(x) {
  x <- raster::raster(x)
  x[!is.na(x[])] <- 1
  RIArtm3 <- terra::rast(x)
  aaa <- terra::focal(RIArtm3, fun = "sum", na.rm = TRUE, w = 5)
  RIArtm2 <- raster::raster(x)
  RIArtm2[aaa[] > 0] <- 1
  RIArtm4 <- terra::rast(RIArtm2)
  bbb <- terra::focal(RIArtm4, fun = "sum", na.rm = TRUE, w = 5)
  ccc <- raster::raster(bbb)[] > 0 & !is.na(x[])
  RIArtm2[ccc] <- 1
  RIArtm2[!ccc & !is.na(x[])] <- 0
  sa <- sf::st_as_sf(stars::st_as_stars(RIArtm2), as_points = FALSE, merge = TRUE)
  sa <- sf::st_buffer(sa, 0)
  sa <- sf::as_Spatial(sa)
  return(sa)
}
SA_ERIntersect <- function(x, studyArea) {
  x <- sf::st_read(x)
  sa_sf <- sf::st_as_sf(studyArea)
  ecoregions <- sf::st_transform(x, sf::st_crs(sa_sf))
  studyAreaER <- sf::st_intersects(ecoregions, sa_sf, sparse = FALSE)
  sf::as_Spatial(ecoregions[studyAreaER,])
}

studyArea <- Cache(prepInputs, url = "https://drive.google.com/file/d/1h7gK44g64dwcoqhij24F2K54hs5e35Ci/view?usp=sharing",
                   destinationPath = Paths$inputPath,
                   fun = fixRTM, overwrite = TRUE)
studyAreaER <- Cache(prepInputs, url =  "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
                     destinationPath = Paths$inputPath, fun = quote(SA_ERIntersect(x = targetFilePath, studyArea)),
                     overwrite = TRUE)

speciesToUse <- c("Abie_las", "Betu_pap", "Pice_gla", "Pice_mar", "Pinu_con",
                  "Popu_tre", "Pice_eng")
speciesNameConvention <- LandR::equivalentNameColumn(speciesToUse, LandR::sppEquivalencies_CA)
sppEquiv <- LandR::sppEquivalencies_CA[LandR::sppEquivalencies_CA[[speciesNameConvention]] %in% speciesToUse,]

# Assign a colour convention for graphics for each species
sppColorVect <- LandR::sppColors(sppEquiv, speciesNameConvention, palette = "Set1")
sppNames <- names(sppColorVect)
sppColorVect <- RColorBrewer::brewer.pal(name = "Set1", 8)
names(sppColorVect) <- c(sppNames, "Mixed")

objects <- list(studyArea = studyArea, studyAreaLarge = studyArea,
                studyAreaANPP = studyAreaER,
                sppEquiv = sppEquiv,
                sppColorVect = sppColorVect)
times <- list(start = 0, end = 350)

parameters <- list(
  .globals = list(
    sppEquivCol = speciesNameConvention
    , ".studyAreaName" = "RIA"
  ),
  Biomass_borealDataPrep = list(
    .studyAreaName = "RIA",
    subsetDataBiomassModel = 50
  ),
  Biomass_speciesFactorial = list(
    .plots = NULL, #"pdf",
    runExperiment = TRUE,
    factorialSize = "medium"),
  Biomass_speciesParameters = list(
    .plots = "png",
    standAgesForFitting = c(0, 125),
    .useCache = c(".inputObjects", "init"),
    speciesFittingApproach = "focal"),
  Biomass_yieldTables = list(
    .plots = "png",
    .useCache = "generateData")
)

# All the action is here
simOut <- simInitAndSpades(
  times = times,
  params = parameters,
  modules = modules,
  objects = objects,
  debug = 1, loadOrder = modules
)


# Extras not used, for plotting some things
if (FALSE) {
  cd1 <- cds[pixelGroup %in% sample(cds$pixelGroup, size = 49)]
  cd1 <- rbindlist(list(cd1, cd1[, list(speciesCode = "Total", B = sum(B)), by = c("age", "pixelGroup")]), use.names = TRUE)
  ggplot(cd1, aes(age, B, colour = speciesCode)) + facet_wrap(~pixelGroup) + geom_line() + theme_bw()

  domAt100 <- cds[age == 100, speciesCode[which.max(B)], by = "pixelGroup"]
  domAt200 <- cds[age == 200, speciesCode[which.max(B)], by = "pixelGroup"]
  domAt300 <- cds[age == 300, speciesCode[which.max(B)], by = "pixelGroup"]

  ageWhoIsDom <- cds[, B[which.max(B)]/sum(B), by = c("age", "pixelGroup")]
  sam <- sample(seq(NROW(ageWhoIsDom)), size = NROW(ageWhoIsDom)/100)
  ageWhoIsDom2 <- ageWhoIsDom[sam]
  ggplot(ageWhoIsDom2, aes(age, V1)) + geom_line() + theme_bw() + geom_smooth()

  ageWhoIsDom2 <- ageWhoIsDom[sam, mean(V1), by = "age"]
  ggplot(ageWhoIsDom2, aes(age, V1)) + geom_line() + theme_bw() + geom_smooth()

  giForCBM <- data.table::fread("growth_increments.csv")

}


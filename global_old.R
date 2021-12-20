if (isTRUE(identical(Sys.info()[["user"]], "elmci1"))) {
  options(repos = c(CRAN = "https://cloud.r-project.org/"),
          "Require.install" = FALSE)
}
options(reproducible.showSimilar = TRUE, 
        reproducible.useMemoise = TRUE, reproducible.cacheSaveFormat = "qs",
        spades.moduleCodeChecks = FALSE, LandR.assertions = FALSE)
if (!require("Require")) {install.packages("Require"); require("Require")}
Require("PredictiveEcology/SpaDES.install")
installSpaDES()
doExperiment <- TRUE

Require(c("PredictiveEcology/reproducible@QuestionMark (>= 1.2.8.9008)", 
          "raster", 'PredictiveEcology/SpaDES.core@development (>= 1.0.9.9004)', 
          "googledrive", "data.table", "stars", "sf", "raster", "terra",
          # "PredictiveEcology/SpaDES.experiment@development",
          "parallelly", "ggplot2", "gridExtra",
          "PredictiveEcology/LandR@development (>= 1.0.6.9004)",
          "PredictiveEcology/SpaDES.install", "SpaDES.tools"), upgrade = FALSE)
Require(c("XML", "RCurl", "httpuv"), upgrade = FALSE, require = FALSE) # need these, but without `require`

if (isTRUE(identical(Sys.info()[["user"]], "elmci1"))) {
  library(googledrive)
  options(
    gargle_oauth_cache = "../.secrets",
    gargle_oauth_email = "eliotmcintire@gmail.com"
  )
} 
drive_auth("eliotmcintire@gmail.com")


setPaths(cachePath = "cache",
         inputPath = "inputs",
         modulePath = "modules",
         outputPath = "outputs")

dataDir <- "data"


#RIArtm <- prepInputs(url = "https://drive.google.com/file/d/1h7gK44g64dwcoqhij24F2K54hs5e35Ci/view?usp=sharing",
#                     destinationPath = dataDir)

fixRTM <- function(x) {
  x <- raster::raster(x)
  x[!is.na(x[])] <- 1
  RIArtm3 <- terra::rast(x)
  aaa <- terra::focal(RIArtm3, fun = "sum", w = 3)
  RIArtm2 <- raster::raster(x)
  RIArtm2[aaa[] > 3] <- 1
  RIArtm4 <- terra::rast(RIArtm2)
  bbb <- terra::focal(RIArtm4, fun = "sum", w = 3)
  ccc <- raster::raster(bbb)[] > 0 & !is.na(x[])
  RIArtm2[ccc] <- 1
  RIArtm2[!ccc & !is.na(x[])] <- 0
  sa <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(RIArtm2), 
                                    as_points = FALSE, merge = TRUE))
  return(sa)
}
studyArea <- Cache(prepInputs, url = "https://drive.google.com/file/d/1h7gK44g64dwcoqhij24F2K54hs5e35Ci/view?usp=sharing",
                   destinationPath = dataDir,
                   fun = fixRTM)

if (FALSE) {
  a <- prepInputs(url = "https://drive.google.com/file/d/1PqeJWDh1ZBVHPokqJ9Hjmr0ZBEUbNHKi/view",
                  destinationPath = dataDir,
                  fun = "SpaDES.core::loadSimList", useCache = FALSE)
  
  RIAcohortData <- a$cohortData
  setkeyv(RIAcohortData, "pixelGroup")
}

# Download them
# moduleNameAndBranch <- c("Biomass_core@cleaningByEliot", "Biomass_borealDataPrep@development", "Biomass_speciesParameters@development")
moduleNameAndBranch <- c("Biomass_core@development", "Biomass_borealDataPrep@development", "Biomass_speciesParameters@EliotTweaks")
lapply(moduleNameAndBranch, function(modName) {
  Cache(getModule, file.path("PredictiveEcology", modName), #modulePath = getPaths()$modulePath, 
        overwrite = TRUE)
})
moduleName <- gsub("@.+", "", moduleNameAndBranch)

times <- list(start = 0, end = 350)

# studyArea <- Cache(randomStudyArea, size = 1e7) # cache this so it creates a random one only once on a machine

# Pick the species you want to work with -- using the naming convention in "Boreal" column of LandR::sppEquivalencies_CA
speciesToUse <- c("Abie_bal", "Betu_pap", "Lari_lar", "Pice_gla", "Pice_mar",
                  "Pinu_ban", "Popu_tre")
speciesNameConvention <- LandR::equivalentNameColumn(speciesToUse, LandR::sppEquivalencies_CA)
sppEquiv <- LandR::sppEquivalencies_CA[LandR::sppEquivalencies_CA[[speciesNameConvention]] %in% speciesToUse,]
# Assign a colour convention for graphics for each species
sppColorVect <- LandR::sppColors(sppEquiv, speciesNameConvention,
                                 #newVals = "Mixed",
                                 palette = "Set1")

## Usage example
modules <- as.list(moduleName)#grep("core", moduleName, value = TRUE))
objects <- list(studyArea = studyArea, studyAreaLarge = studyArea, 
                # rasterToMatch = RIArtm,
                studyAreaANPP = studyArea,
                sppEquiv = sppEquiv, sppColorVect = sppColorVect)
paths <- getPaths()

successionTimestep <- 10L

## keep default values for most parameters
## (ommitted from this list)
parameters <- list(
  # .globals = list(
  #   "sppEquivCol" = speciesNameConvention
  # ),
  Biomass_core = list(
     "sppEquivCol" = speciesNameConvention
    , "successionTimestep" = successionTimestep
    , ".plots" = NULL #c("screen", "object")
    , ".plotInitialTime" = times$start
    , ".plots" = c("screen", "png")
    , ".saveInitialTime" = times$start
    , ".useCache" = "init"
    , ".useParallel" = 1
    , ".maxMemory" = 30
    , "vegLeadingProportion" = 0
    , "calcSummaryBGM" = NULL
    , "seedingAlgorithm" = "noSeeding"
    # , "seedingAlgorithm" = "noDispersal"
    , ".studyAreaName" = "RIA"
  ),
  Biomass_borealDataPrep = list(
    "sppEquivCol" = speciesNameConvention
  ),
  Biomass_speciesParameters = 
    list(GAMMiterations = 2, 
         ".useCache" = c(".inputObjects", "init"),
         constrainMaxANPP = c(3.5, 6),
         constrainGrowthCurve = c(0.65, 0.95),
         GAMMknots = list(
           "Abie_bal" = 3,
           "Abie_las" = 3,
           "Betu_pap" = 3,
           "Lari_lar" = 4,
           "Pice_eng" = 4,
           "Pice_gla" = 3,
           "Pice_mar" = 4,
           "Pinu_ban" = 3,
           "Pinu_con" = 4, 
           "Popu_tre" = 4,
           "Pseu_men" = 3),
         minimumPlotsPerGamm = 40,
         constrainMortalityShape = list(
           "Abie_bal" = c(15,25),
           "Abie_las" = c(15,25),
           "Betu_pap" = c(15,20),
           "Lari_lar" = c(20,25),
           "Pice_eng" = c(20,25),
           "Pice_gla" = c(20,25),
           "Pice_mar" = c(15,25),
           "Pinu_ban" = c(15,25),
           "Pinu_con" = c(15,25), 
           "Popu_tre" = c(20,25),
           "Pseu_men" = c(20,25)
         ),
         # constrainGrowthCurve = list(
         #   "Abie_bal" = c(0, 1),
         #   "Abie_las" = c(0, 1),
         #   "Betu_pap" = c(0, 1),
         #   "Lari_lar" = c(0, 1),
         #   "Pice_eng" = c(0, 1),
         #   "Pice_gla" = c(0, 1),
         #   "Pice_mar" = c(0, 1),
         #   "Pinu_ban" = c(0, 1),
         #   "Pinu_con" = c(0, 1), 
         #   "Popu_tre" = c(0, 1),
         #   "Pseu_men" = c(0, 1)
         # ),
         quantileAgeSubset = list(
           "Abie_bal" = 95, #N = 43 w/ 95 subset
           "Abie_las" = 95, #N = 250 '' 
           "Betu_pap" = 95, #N = 96
           "Lari_lar" = 95, #N = 45
           "Pice_eng" = 95, #N = 130
           "Pice_gla" = 95, #N = 1849
           "Pice_mar" = 95, #N = 785
           "Pinu_ban" = 95, #N = 603
           "Pinu_con" = 99, # N = 3172, 99 not an improvement. Maybe 97
           "Popu_tre" = 99, # N = 1997, trying 99
           "Pseu_men" = 99 # N = 2797, 99 is an improvement
         )
    ))

outputs <- data.frame(expand.grid(objectName = "cohortData",
                                  saveTime = unique(seq(times$start, times$end, by = 10)),
                                  eventPriority = 1, fun = "qs::qsave",
                                  stringsAsFactors = FALSE))

mySim2 <- simInitAndSpades(times = times,
                          params = parameters,
                          modules = modules,
                          objects = objects,
                          outputs = outputs,
                          debug = 1, 
                          events = "init")
mySim <- Copy(mySim2)
# mySim$species[, sexualmature := 1000]
mySim$cohortData$age <- 1
mySim$cohortData$B <- 0
mySim$species[, growthcurve := 0.9]

mySim@events[[length(mySim@events)]] <- NULL


st <- Sys.time()

mySimOutExpName <- "mySimOutExp"
mySimOutFilename <- file.path(outputPath(mySim), paste0(mySimOutExpName, st, ".qs"))

if (!doExperiment) {
  #  pf <- profvis::profvis(
    mySimOut <- spades(mySim, debug = 1)
#  )
  filesAll <- as.data.table(outputs(mySimOut))[saved == TRUE][["file"]]
} else {
  saveSimList(mySim, filename = "mySim.qs")
  
  vals <- unique(round((30:65)/2))/5
  growthcurves <- seq(0.72, 0.9, by = 0.03)
  
  opts <- options()
  #valsDF <- data.table(Popu_tre = vals, Pice_mar = vals, Pice_gla = vals, Betu_pap = vals)
  #valsDF[, Popu_tre := Popu_tre - 0.01]
  #valsDF[, Betu_pap := Betu_pap - 0.01]
  valsDF <- as.data.table(expand.grid(Deciduous = vals, Conifer = vals, growthcurve = growthcurves))
  valsDF <- valsDF[Conifer < Deciduous]
  valsDF <- valsDF[(Conifer + 0.6) >= Deciduous]
  valsDF <- valsDF[(growthcurves >= 0.9 & Deciduous < 6) |
                     (growthcurves >= 0.8 & Deciduous < 5) |
                     (growthcurves >= 0.7 & Deciduous < 4)]
  
  cl <- parallelly::makeClusterPSOCK(6, rscript_libs = .libPaths())
  clusterExport(cl, c("opts", "valsDF", "speciesNameConvention"))
  clusterEvalQ(cl, {
    options(opts)
  })
  valsDFList <- split(valsDF, rep(1:6, length.out = NROW(valsDF)))
  mySimOutExp <- clusterApplyLB(cl = cl, valsDFList, function(valsDF) {
    library(SpaDES.core); library(data.table)
    mySim <- loadSimList(filename = "mySim.qs")
    message("Loaded simList")
    dirNames <- lapply(seq(NROW(valsDF)), function(v) {
      valsDF1 <- data.table(Type = names(valsDF)[1:2], mANPPproportion = as.numeric(valsDF[v, 1:2]))
      mySimInner <- Copy(mySim)
      sp <- mySimInner$species
      
      if (FALSE) { # for growth curve
        sp[match(names(valsDF), species), growthcurve := as.numeric(valsDF[v, ])]
        dirName <- paste0(names(valsDF), "=", valsDF[v,], collapse = " ")
        dirName <- paste0("growthcurve ",dirName)
      }
      
      if (TRUE) { # for mANPPproportion
        # Assign mANPPproportion by deciduous and conif that is in sppEquiv
        colToUse <- c("Type", speciesNameConvention); 
        sp <- sp[, -"mANPPproportion"][mySimInner$sppEquiv[, ..colToUse], on = c("species" = "LandR")][
          valsDF1, on = "Type"][
            , -"Type"]      
        sp[, growthcurve := valsDF[v, "growthcurve"]]
        dirName <- paste0(names(valsDF), "=", valsDF[v,], collapse = " ")
        dirName <- paste0("mANPPproportion ",dirName)
      }
      
      outputPath(mySimInner) <- file.path(outputPath(mySimInner), dirName)
      mySimInner$species <- sp
      spEco <- mySimInner$speciesEcoregion[sp[, c("species", "mANPPproportion")], on = c("speciesCode" = "species")]
      spEco[, maxANPP := maxB * mANPPproportion/100]
      set(spEco, NULL, "mANPPproportion", NULL)
      mySimInner$speciesEcoregion <- spEco
      
      simOut <- spades(mySimInner, debug = 1)
      rm(list = ls(envir(simOut)), envir = envir(simOut))
      return(dirName)
    })
    return(dirNames)
    # simFN <- paste0(outputPath(mySimInner), paste0("sim_growthcurve", v, ".qs"))
    # saveSimList(simOut, filename = simFN)
  })
  stopCluster(cl)
  qs::qsave(mySimOutExp, file = mySimOutFilename)
  #cl <- parallelly::makeClusterPSOCK(2)
  #mySimOutExp <- experiment(mySim, objects = list(species = speciesList), #cl = cl,
  #                          rscript_libs = .libPaths())
  #stopCluster(cl)
  
  # #mySim$species[grep("Pice", species), growthcurve := 0]
  # mySim$species[grep("Popu", species), growthcurve := 0.2]
  # mySim$species[grep("Pice_gl", species), growthcurve := 0.4]
  # 
  # mySim$species[, growthcurve := 0.6]
  # mySim$species[species == "Betu_pap", growthcurve := 0.57]
  # mySim$species[species == "Pice_mar", growthcurve := 0.63]
  # mySim$species[species == "Pice_gla", growthcurve := 0.63]
  # mySimOut <- spades(mySim, debug = TRUE)
  # #files <- dir ("outputs", pattern = "cohortData", full.names = T)
  
  
  # folders <- dir("outputs", pattern = "mANPP", full.names = TRUE)
  # # folders <- dir("outputs", pattern = "growthcurve Pop", full.names = TRUE)
  # # folders <- grep("4$|9$", folders, value = TRUE)
  # folders <- grep("(=5.{0,2}$)|(=6.{0,2})$", folders, value = TRUE)
  # 
  # # onlyEquals <- sapply(strsplit(folders, split = "=| "), function(x) length(unique(na.omit(as.numeric(x)))) == 1)
  # # folders <- folders[onlyEquals]
  # #folders <- dir("outputs", pattern = "(growthcurve P.+0.8)|(growthcurve P.+0.7)", full.names = TRUE)
  if (!exists(mySimOutExpName)) {
    files <- dir(dirname(mySimOutFilename), pattern = mySimOutExpName, full.names = TRUE)
    times <- gsub(paste0(".+",mySimOutExpName,"(.+)\\.qs"), "\\1", files)
    whtimes <- which.max(as.POSIXct(times))
    mySimOutExp <- qs::qread(files[whtimes])
  }
  folders <- file.path(outputPath(mySim), unlist(mySimOutExp))
  folders <- sort(folders)
  filesAll <- lapply(folders, function(fold) {
    dir(fold, pattern = "cohortData", full.names = TRUE)
  })  
  
}


if (FALSE) {
  fi <- file.info(dir("outputs", pattern = "cohortData", full.names = TRUE))
  filesAll <- rownames(fi)[fi[,"mtime"] > ("2021-11-25 15:41:43 PST")]
}
filesAll <- lapply(filesAll, function(ff) {
  grep(".0.qs", ff, value = TRUE)
})
if (!doExperiment)
  filesAll <- list(unlist(filesAll))

ggs <- parallel::mclapply(filesAll, mc.cores = 9, 
              function(files) {
#ggs <- lapply(mySimOutExp, function(mySimOut)  {
  # 
  cds <- lapply(files, function(ff) {
    out <- qs::qread(ff)
    # set(out, NULL, "ecoregionGroup", NULL)
  })
  cds <- rbindlist(cds, fill = TRUE)
  cds <- mySim2$speciesEcoregion[cds, on = c("ecoregionGroup", "speciesCode")]
  setorder(cds, pixelGroup, age)
  # cds <- cds[!is.na(maxANPP)]
  # cds2 <- cds[, `:=`(minAge = min(age), maxAge = max(age)) , by = "pixelGroup"]
  # cds1 <- cds2[minAge < 70]
  # pgs <- sample(unique(cds1$pixelGroup), 49)
  pgs <- c(30747, 30867, 68868, 114534, 117031,  123671, 155483, 167945, 196696, 204108, 
           211737, 214336, 274219, 291592, 294812, 296290, 298916, 299243, 300172, 312314, 325931, 
           337802, 339485, 352797, 367527, 390419, 403041, 403873, 424332, 441858, 456383, 
           467792, 492487, 496643, 498408, 500085, 513810, 513831, 519359, 525566, 525623, 
           533084, 533094, 537801, 538188, 539829, 544625, 546642, 555064)
  cds1 <- cds[pixelGroup %in% pgs]
  growthcurve <- unique(gsub(".*growthcurve (.+)/coh.*", "\\1", files))
  if (doExperiment) {
    title <- paste(growthcurve)
  } else {
    growthcurves <- mySim$species[species %in% unique(cds1$speciesCode)]
    growthcurves <- paste(collapse = ", ", paste0(growthcurves$species, "=", growthcurves$growthcurve))
    title <- paste("Title shows 'ecoregion-maxBiomass x1000' for each species",
                   growthcurves)
  }
    
  cds1[, maxBTitle := paste0(collapse = ", ", paste0(substr(unique(speciesCode), 1, 2), 
                                                     substr(unique(speciesCode), 5, 6), ": ", round(unique(maxB)/1000, 0))), by = "pixelGroup"]
  
  cds1[, maxBTitle := paste0("", as.integer(factor(ecoregionGroup)), "-", maxBTitle)]
  # dat_text <- data.frame(
  #   label = c("4 cylinders", "6 cylinders", "8 cylinders"),
  #   cyl   = c(4, 6, 8)
  # )
  # dat_text <- unique(cds1[, c("pixelGroup", "maxBTitle")])
  # dat_text[, `:=`(x = 0, y = 22000)]
  # cds1 <- cds1[age < 150]
  gg1 <- ggplot(cds1, aes(x = age, y = B, color = speciesCode)) + 
    geom_line() + 
    facet_wrap(~ pixelGroup, nrow = 7, scales="free_x") +
    #facet_wrap(~ maxBTitle, nrow = 7, scales="free_x") +
    #facet_wrap(~ maxBTitle, nrow = 7, scales="fixed") +
    #ggtitle(title) +
    theme(strip.text.x = element_text(size = 5)) #+
    #theme_bw()
  
  # gg1 <- gg1 + geom_text(
  #   size = 1.5,
  #   data    = dat_text,
  #   colour = "black",
  #   #vjust = "top",
  #   #vjust = -5, 
  #   #nudge_x = 0.05,
  #   # mapping = aes(x = -Inf, y = -Inf, label = maxBTitle),
  #   mapping = aes(x = x, y = y, label = maxBTitle),
  #   hjust   = "left",#-0.2,
  #   #vjust   = -5
  # )
  
  # gg2 <- ggplot(cds1, aes(x = age, y = aNPPAct, color = speciesCode)) + 
  #   geom_line() + 
  #   facet_wrap(~ pixelGroup, nrow = 7, scales="free_x") +
  #   ggtitle(title) +
  #   theme_bw()
  rm(cds, cds1)
  #list(gg1 = gg1, gg2 = gg2)
  list(gg1 = gg1)
})
ggAGB <- lapply(ggs, function(gg) gg[[1]])
rm(ggs)

filename <- paste0(ifelse(doExperiment, "Experiment", ""), as.character(st))
qs::qsave(ggAGB, file = paste0(filename, ".qs"))
# ggs <- unlist(ggs, recursive = FALSE)

ml <- marrangeGrob(ggAGB, nrow=1, ncol=1)
## non-interactive use, multipage pdf
ggsave(paste0(filename, ".pdf"), ml, width = 12, height = 7)


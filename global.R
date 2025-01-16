# Get the minimal amount of packages
repos <- c("predictiveecology.r-universe.dev", getOption("repos"))
if (!require("SpaDES.project")){
  Require::Install(c("SpaDES.project", "SpaDES.core", "reproducible"), repos = repos, dependencies = TRUE)
}

out <- SpaDES.project::setupProject(
  Restart = TRUE,
  paths = list(projectPath = getwd()),
  options = options(spades.moduleCodeChecks = FALSE,
                    spades.recoveryMode = FALSE),
  modules = c("PredictiveEcology/Biomass_speciesFactorial@development",
              "PredictiveEcology/Biomass_borealDataPrep@development",
              "PredictiveEcology/Biomass_speciesParameters@development",
              "PredictiveEcology/Biomass_yieldTables@main"
  ),
  #DC 15-01-2025: Why 350?
  #DC 16-01-2025: Can we remove? I don't see where it's used...
  times = list(start = 0, end = 350),
  params = list(
    .globals = list(
      sppEquivCol = LandR::equivalentNameColumn(
        c("Abie_las", "Betu_pap", "Pice_gla", "Pice_mar", "Pinu_con", "Popu_tre",
          "Pice_eng"), LandR::sppEquivalencies_CA), ".studyAreaName" = "RIA"
    ),
    Biomass_borealDataPrep = list(
      .studyAreaName = "RIA",
      subsetDataBiomassModel = 50
    ),
    Biomass_speciesFactorial = list(
      .plots = NULL, #"pdf",
      runExperiment = TRUE,
      factorialSize = "medium"
    ),
    Biomass_speciesParameters = list(
      .plots = "png",
      standAgesForFitting = c(0, 125),
      .useCache = c(".inputObjects", "init"),
      speciesFittingApproach = "focal"
    ),
    Biomass_yieldTables = list(
      moduleNameAndBranch = "PredictiveEcology/Biomass_core@development (>= 1.3.9)",
      .plots = "png",
      .useCache = "generateData"
    )
  ),
  packages = c("googledrive", 'RCurl', 'XML', "stars"),
  useGit = T,
  functions = "R/getRIA.R",
  # Study area is RIA
  studyArea = {
    reproducible::prepInputs(url = "https://drive.google.com/file/d/1LxacDOobTrRUppamkGgVAUFIxNT4iiHU/view?usp=sharing",
                             destinationPath = "inputs",
                             fun = getRIA,
                             overwrite = TRUE)
  },
  studyAreaLarge = studyArea,
  speciesToUse = c("Abie_las", "Betu_pap", "Pice_gla", "Pice_mar", "Pinu_con",
                   "Popu_tre", "Pice_eng"),
  speciesNameConvention = LandR::equivalentNameColumn(speciesToUse, LandR::sppEquivalencies_CA),
  sppEquiv = {
    sppEquiv <- LandR::sppEquivalencies_CA[LandR::sppEquivalencies_CA[[speciesNameConvention]] %in% speciesToUse,]
    sppEquiv
  },
  sppColorVect = {
    sppColorVect <- LandR::sppColors(sppEquiv, speciesNameConvention, palette = "Set1")
    sppNames <- names(sppColorVect)
    sppColorVect <- RColorBrewer::brewer.pal(name = "Set1", 8)
    names(sppColorVect) <- c(sppNames, "Mixed")
    sppColorVect
  }
)

out$loadOrder <- unlist(out$modules)

simOut <- SpaDES.core::simInitAndSpades2(out)



## PR: to remove whole file

## Here we collected some information from gdsr.roche.com



# Data Analysis
vad_labels <- local({
  # standards
  x <- c("Extended Adverse Event Analysis Dataset","Extended Crossover Adverse Event Analysis Dataset","Adverse Event Grouping Definitions","Extended Concomitant Medication Analysis Dataset","Extended Deaths Analysis Dataset","Extended ECG Test Results AEG","Extended User-Defined Baseline ECG Test Results AEG","Extended Exposure Analysis Dataset","Extended Liver Laboratory Test Results AHY","Hys Law Time-To-Event","Extended Laboratory Test Results ALB","Extended User-Defined Baseline Laboratory Test Results ALB","Extended Medical History Analysis Dataset","Subject Level Analysis Dataset","Study Treatment Period Analysis Dataset","Extended Vital Signs Results AVS","Extended User-Defined Baseline Vital Signs Results AVS","Response Dataset","Time to Event Dataset")
  setNames(x, c("AAE","XAAE","AAG","ACM","ADD","AEG","XAEG","AEX","AHY","AHYTE","ALB","XALB","AMH","ASL","ATX","AVS","XAVS","ARS","ATE"))
})


sdtm_labels <- local({
  # from wikipedia
  structure(
    c("Demographics", "Comments", "Subject Elements", "Subject Visits",
      "Concomitant Medications", "Exposure", "Substance Use", "Adverse Events",
      "Disposition", "Medical History", "Protocol Deviations", "Clinical Events",
      "Body Weight", "Body Weight Gain", "Clinical Observations", "Death Diagnosis",
      "Drug Accountability", "ECG Tests", "Findings About Events or Interventions",
      "Food and Water Consumption", "Inclusion/Exclusion Exceptions",
      "Laboratory Tests", "Macroscopic Findings", "Microbiology Specimens",
      "Microbiology Susceptibility", "Microscopic Findings", "Organ Measurements",
      "Palpable Masses", "Pharmacokinetics Concentrations", "Pharmacokinetics Parameters",
      "Physical Examinations", "Questionnaires", "Subject Characteristics",
      "Tumor Findings", "Vital Signs", "Trial Elements", "Trial Arms",
      "Trial Visits", "Trial Inclusion/Exclusion Criteria", "Trial Summary",
      "Supplemental Qualifiers", "Relate Records"),
    .Names = c("DM",
               "CO", "SE", "SV", "CM", "EX", "SU", "AE", "DS", "MH", "DV", "CE",
               "BW", "BG", "CL", "DD", "DA", "EG", "FA", "FW", "IE", "LB", "MA",
               "MB", "MS", "MI", "OM", "PA", "PC", "PP", "PE", "QS", "SC", "TF",
               "VS", "TE", "TA", "TV", "TI", "TS", "SUPPQUAL", "RELREC"))
})


# @examples
# domain_to_label("asl")
# teal:::domain_to_label("MB")
domain_to_label <- function(x, na.string = "-", USE.NAMES=FALSE, addDomain=TRUE, additionalDomains = NULL) {


  vapply(x, function(xi) {
    XI <- toupper(xi)

    lab <- if (!is.null(additionalDomains)) {
      additionalDomains[XI]
    } else {
      NA
    }

    if (is.na(lab)) {
      lab <- if (XI == "ATR") {
        "Tumor Response Analysis Dataset"
      } else if (XI == "RS") {
        "Response"
      } else if (nchar(XI) == 3) {
        vad_labels[XI]
      } else {
        sdtm_labels[XI]
      }
    }

    lab <- if (is.na(lab)) na.string else lab

    if (addDomain) paste(lab, " (", x, ")" , sep="") else lab

  }, character(1), USE.NAMES = USE.NAMES)
}


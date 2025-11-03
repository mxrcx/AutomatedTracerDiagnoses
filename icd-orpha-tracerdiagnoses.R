# ================================
# ICD-Orpha Tracerdiagnoses Script
#
# This script processes the ICD-Orpha mapping data
# from Alpha-ID-SE to identify tracer diagnoses.
# ================================

# ---- Setup ----

required_packages <- c("dplyr", "readr", "readxl", "writexl", "data.table")

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

invisible(lapply(required_packages, install_if_missing))

library(dplyr)
library(readr)
library(readxl)
library(writexl)
library(data.table)

# ---- Load Data ----

alphaidse_path <- "input/alphaidse2025/icd10gm2025_alphaidse_edvtxt_20240927.txt"
column_names <- c(
  "Gültigkeit",
  "Alpha.ID",
  "ICD.Kode",
  "Sternschlüssel",
  "Zusatzschlüssel",
  "Primärschlüssel_2",
  "ORPHACode",
  "Diagnosebezeichnung"
)

alphaidse <- fread(
  input = alphaidse_path,
  sep = "|",
  header = FALSE,
  fill = TRUE,
  col.names = column_names
)

cat("✅ Initial data loaded. Dimensions:", dim(alphaidse), "\n")

# ---- Inital Data Cleaning ----

alphaidse_cleaned <- alphaidse %>%
  filter(Gültigkeit == 1) %>%
  select(ICD.Kode, Orpha.Kode, Diagnosebezeichnung)

alphaidse_cleaned$Orpha.Kode <- as.character(alphaidse_cleaned$Orpha.Kode)

# ---- Add Orphanet Classification Level ----

orphanet_complete <- read_excel("input/Orphanet_complete_list_2025.xlsx", sheet = 3)
names(orphanet_complete)[1] <- "ORPHACode"
names(orphanet_complete)[4] <- "Orpha.Klassifikationsebene"
orphanet_complete$ORPHACode <- sub("^ORPHA:", "", orphanet_complete$ORPHACode)
orphanet_complete_column_subset <- orphanet_complete[, c("ORPHACode", "Orpha.Klassifikationsebene")]

alphaidse_merged <- merge(alphaidse_cleaned, orphanet_complete_column_subset, by = "ORPHACode", all.x = TRUE)
alphaidse_merged <- select(alphaidse_merged, 2, 1, everything())

# ---- Tracerdiagnosen Segment 1: ICD → Unique Valid Orpha Mapping ----

segment1_unique <- alphaidse_merged %>%
  group_by(ICD.Kode) %>%
  filter(
    n_distinct(ORPHACode) == 1,
    all(!is.na(ORPHACode) & ORPHACode != "")
  ) %>%
  distinct(ICD.Kode, ORPHACode, .keep_all = TRUE) %>%
  ungroup()

# ---- Orpha → Unique Valid ICD Mapping ----

orpha_to_icd <- alphaidse_merged %>%
  group_by(ORPHACode) %>%
  filter(
    n_distinct(ICD.Kode) == 1,
    all(!is.na(ICD.Kode) & ICD.Kode != "")
  ) %>%
  distinct(ICD.Kode, ORPHACode, .keep_all = TRUE) %>%
  ungroup()

# ---- Tracerdiagnosen Segment 1a: Bijective Mappings (both directions) ----

segment1a_bijective <- inner_join(
  segment1_unique,
  orpha_to_icd,
  by = c("ICD.Kode", "ORPHACode")
) %>%
  select(ICD.Kode, ORPHACode, Diagnosebezeichnung = Diagnosebezeichnung.x, Orpha.Klassifikationsebene = Orpha.Klassifikationsebene.x)

# ---- Final Data Cleaning ----

clean_codes <- function(data) {
  data %>%
    filter(
      !grepl("\\+$", .data$ICD.Kode),
      !grepl("^[ABSTU]", .data$ICD.Kode)
    )
}

segment1_unique_cleaned <- clean_codes(segment1_unique)
segment1a_bijective_cleaned <- clean_codes(segment1a_bijective)

# ---- Output & Export ----

cat("✅ Segment 1️⃣ : ICD → Unique Orpha. Dimensions:", dim(segment1_unique_cleaned), "\n")
cat("✅ Segment 1️⃣ 🅰️ : Bijective Unique ICD ↔ Unique Orpha. Dimensions:", dim(segment1a_bijective_cleaned), "\n")

write_to_csv <- function() {
  write.csv2(segment1a_bijective_cleaned, "segment1a_tracerdiagnoses.csv", row.names = FALSE)
  write.csv2(segment1_unique_cleaned, "segment1_tracerdiagnoses.csv", row.names = FALSE)
}
write_to_excel_compatible_csv <- function() {
  write_excel_csv(segment1a_bijective_cleaned, "segment1a_tracerdiagnoses_excel.csv")
  write_excel_csv(segment1_unique_cleaned, "segment1_tracerdiagnoses_excel.csv")
} 
write_to_excel <- function() {
  write_xlsx(segment1a_bijective_cleaned, "segment1a_tracerdiagnoses.xlsx")
  write_xlsx(segment1_unique_cleaned, "segment1_tracerdiagnoses.xlsx")
}

write_to_csv()
write_to_excel()

# ---- End of Script ----
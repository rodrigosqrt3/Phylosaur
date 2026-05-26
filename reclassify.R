# =============================================================================
# PHYLOSAUR — Difficulty Reclassifier
# Loads the existing database and pageview cache and reclassifies difficulty
# without touching the API. Tweak the thresholds below and re-run anytime.
#
# Run: source("reclassify.R")
# Requires: tidyverse, jsonlite
# =============================================================================

library(tidyverse)
library(jsonlite)

# ── CONFIG ────────────────────────────────────────────────────────────────────

INPUT_FILE     <- "D:\\Phylosaur\\phylosaur_db_old.json"
PAGEVIEW_CACHE <- "D:\\Phylosaur\\pageview_cache.json"
OUTPUT_FILE    <- "D:\\Phylosaur\\phylosaur_db.json"

# Pageview thresholds (last 30 days of Wikipedia views)
THRESHOLDS <- list(
  muito_facil = 8000,
  facil       = 3000,
  normal      = 1000,
  dificil     = 400
  # below dificil -> muito_dificil
)

HOMONYMS <- c("Thanos", "Qianlong", "Gremlin", "Talos", "Mahakala",
              "Shri", "Moros", "Tiamat", "Lepidus")

# ── LOAD ──────────────────────────────────────────────────────────────────────

cat("Loading database and pageview cache...\n")
db <- fromJSON(INPUT_FILE)
pv <- fromJSON(PAGEVIEW_CACHE, simplifyVector = TRUE)

cat("Taxa:", nrow(db), "\n")
cat("Pageviews cached:", length(pv), "\n")
cat("Coverage:", sum(db$nome %in% names(pv)), "/", nrow(db), "\n\n")

# ── RECLASSIFY ────────────────────────────────────────────────────────────────

classify <- function(name, views) {
  if (name %in% HOMONYMS)             return("muito_dificil")
  v <- as.numeric(views)
  if (is.na(v) || v == 0)             return("muito_dificil")
  if (v >= THRESHOLDS$muito_facil)    return("muito_facil")
  if (v >= THRESHOLDS$facil)          return("facil")
  if (v >= THRESHOLDS$normal)         return("normal")
  if (v >= THRESHOLDS$dificil)        return("dificil")
  return("muito_dificil")
}

db$dificuldade <- sapply(db$nome, function(d) classify(d, pv[[d]]))

# ── REVIEW ────────────────────────────────────────────────────────────────────

cat("Difficulty distribution:\n")
dist <- table(db$dificuldade)
print(dist)
cat("\nPercentages:\n")
print(round(prop.table(dist) * 100, 1))

cat("\nSamples per category:\n")
for (nivel in c("muito_facil", "facil", "normal", "dificil", "muito_dificil")) {
  dinos <- db %>% filter(dificuldade == nivel) %>% pull(nome)
  cat(nivel, "(", length(dinos), "):\n")
  cat(" ", paste(sample(dinos, min(8, length(dinos))), collapse = ", "), "\n\n")
}

# ── SAVE ──────────────────────────────────────────────────────────────────────

write_json(db, OUTPUT_FILE, pretty = TRUE, auto_unbox = TRUE)
cat("Saved:", OUTPUT_FILE, "\n")

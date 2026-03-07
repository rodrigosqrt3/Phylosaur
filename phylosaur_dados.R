# =============================================================================
# PHYLOSAUR — Database Generator
# 1. Fetches dinosaur genera from Paleobiology Database (PBDB)
# 2. Fetches taxonomic lineages from Wikispecies
# 3. Fetches popularity data from Wikipedia Pageviews API
# 4. Outputs dinozooa_db.json for the game
#
# Run: source("generate_database.R")
# Requires: tidyverse, jsonlite, httr, future, furrr
# =============================================================================

library(tidyverse)
library(jsonlite)
library(httr)
library(future)
library(furrr)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────

OUTPUT_FILE      <- "phylosaur_db.json"
LINEAGE_CACHE    <- "lineage_cache.json"
PAGEVIEW_CACHE   <- "pageview_cache.json"

# Parallel workers — 4 is safe, increase to 6 if you want faster
# but risk getting rate-limited by Wikispecies
PARALLEL_WORKERS <- 4
CHUNK_SIZE       <- 50

# Percentiles for difficulty — each level gets ~20% of dinosaurs
PERCENTILES <- list(
  muito_facil = 0.20,
  facil       = 0.40,
  normal      = 0.60,
  dificil     = 0.80
)

# Names that are famous for other things — force to muito_dificil
HOMONYMS <- c("Thanos", "Qianlong", "Gremlin", "Talos", "Mahakala",
              "Shri", "Moros", "Tiamat", "Lepidus")

# Dinosaurs whose Wikispecies page has a different name than PBDB
NAME_REMAPS <- list(
  "Yi"            = "Yi qi",
  "Dreadnoughtus" = "Dreadnoughtus schrani",
  "Leptorhynchos" = "Leptorhynchos (Caenagnathidae)",
  "Mei"           = "Mei (dinosaur)",
  "Kol"           = "Kol ghuva",
  "Gastonia"      = "Gastonia (dinosaur)"
)

# Request delays (seconds)
DELAY_LINEAGE   <- 0.3
DELAY_TEMPLATE  <- 0.1
DELAY_PAGEVIEW  <- 0.5
DELAY_RATELIMIT <- 60

# Wikispecies templates to ignore on the dinosaur's own page
SKIP_PAGE <- c(
  "^int:", "^VN$", "^Commons", "^Taxonbar",
  "^sp$", "^splast", "^Splast", "^a$", "^sfn", "^cite",
  "^Image$", "^image$", "^Fossilworks", "^ISBN", "^aut$",
  "^Italictitle$", "^italictitle$", "^File", "^trib",
  "^gbr$", "^DISPLAYTITLE", "^Taxit", "^BASEPAGENAME"
)

# Wikispecies templates to ignore while following the template chain
SKIP_TEMPLATE <- c(
  SKIP_PAGE,
  "^cbr$", "^fbr$", "^noinclude", "^includeonly",
  "^#", "^TaxonavTheropoda", "^triblast$"
)

# ── 1. FETCH DINOSAUR LIST FROM PBDB ─────────────────────────────────────────

cat("=== 1. Fetching dinosaur genera from PBDB ===\n")

res_dinos <- GET(
  "https://paleobiodb.org/data1.2/taxa/list.json",
  query = list(base_name = "Dinosauria", rank = "genus", show = "attr")
)
res_aves <- GET(
  "https://paleobiodb.org/data1.2/taxa/list.json",
  query = list(base_name = "Aves", rank = "genus", show = "attr")
)

all_records <- fromJSON(content(res_dinos, "text", encoding = "UTF-8"))$records
aves_names  <- fromJSON(content(res_aves,  "text", encoding = "UTF-8"))$records$nam

# ext == 0  -> extinct (no living species) — removes modern birds
# flg == NA -> valid accepted name — removes nomina dubia etc.
# not Aves  -> extra safety net for crown birds
official_list <- all_records %>%
  filter(ext == 0) %>%
  filter(is.na(flg)) %>%
  filter(!nam %in% aves_names) %>%
  pull(nam)

cat("Total valid extinct non-avian genera:", length(official_list), "\n")
cat("Sample:", paste(sample(official_list, 8), collapse = ", "), "\n\n")

# ── 2. FETCH FUNCTIONS ────────────────────────────────────────────────────────

fetch_template <- function(template_name) {
  res <- tryCatch(
    GET("https://species.wikimedia.org/w/api.php",
        query = list(action = "parse",
                     page   = paste0("Template:", template_name),
                     prop   = "wikitext",
                     format = "json"),
        timeout(15)),
    error = function(e) NULL
  )
  if (is.null(res) || status_code(res) != 200) return(NULL)
  wikitext <- tryCatch(
    fromJSON(content(res, "text", encoding = "UTF-8"))$parse$wikitext$`*`,
    error = function(e) NULL
  )
  if (is.null(wikitext)) return(NULL)
  redirect <- str_match(wikitext, "#REDIRECT\\s*\\[\\[Template:([^\\]]+)\\]\\]")[, 2]
  if (!is.na(redirect)) return(fetch_template(str_trim(redirect)))
  wikitext
}

fetch_lineage <- function(dino_name) {
  Sys.sleep(DELAY_LINEAGE)
  res <- tryCatch(
    GET("https://species.wikimedia.org/w/api.php",
        query = list(action = "parse", page = dino_name,
                     prop = "wikitext", format = "json"),
        timeout(15)),
    error = function(e) NULL
  )
  if (is.null(res) || status_code(res) != 200) return(NULL)
  wikitext <- tryCatch(
    fromJSON(content(res, "text", encoding = "UTF-8"))$parse$wikitext$`*`,
    error = function(e) NULL
  )
  if (is.null(wikitext)) return(NULL)
  
  redirect <- str_match(wikitext, "#REDIRECT\\s*\\[\\[([^\\]]+)\\]\\]")[, 2]
  if (!is.na(redirect)) return(fetch_lineage(str_trim(redirect)))
  
  lines <- str_split(wikitext, "\n")[[1]]
  initial_template <- NA
  taxonav_idx <- which(str_detect(lines, "\\{\\{int:Taxonavigation\\}\\}"))
  
  if (length(taxonav_idx) > 0) {
    for (li in (taxonav_idx[1] + 1):min(taxonav_idx[1] + 5, length(lines))) {
      line <- str_trim(lines[li])
      if (line == "") next
      m2 <- str_match(line, regex("\\{\\{Taxonav2\\|([^|}]+)", ignore_case = TRUE))
      if (!is.na(m2[, 2])) { initial_template <- str_trim(m2[, 2]); break }
      m <- str_match(line, "^\\{\\{([^}|#\n]+)\\}\\}")
      if (!is.na(m[, 2])) {
        t <- str_trim(m[, 2])
        if (!any(sapply(SKIP_PAGE, function(p) str_detect(t, p)))) {
          initial_template <- t; break
        }
      }
    }
  }
  
  if (is.na(initial_template)) {
    for (line in lines) {
      m2 <- str_match(line, regex("\\{\\{Taxonav2\\|([^|}]+)", ignore_case = TRUE))
      if (!is.na(m2[, 2])) { initial_template <- str_trim(m2[, 2]); break }
      m <- str_match(str_trim(line), "^\\{\\{([^}|#\n]+)\\}\\}")
      if (!is.na(m[, 2])) {
        t <- str_trim(m[, 2])
        if (!any(sapply(SKIP_PAGE, function(p) str_detect(t, p)))) {
          initial_template <- t; break
        }
      }
    }
  }
  
  if (is.na(initial_template)) return(NULL)
  
  chain   <- list()
  current <- initial_template
  visited <- c()
  
  repeat {
    if (current %in% visited || length(visited) > 60) break
    visited <- c(visited, current)
    Sys.sleep(DELAY_TEMPLATE)
    
    wikitext_t <- fetch_template(current)
    if (is.null(wikitext_t)) break
    
    labels <- str_match_all(wikitext_t,
                            paste0("(Cladus|Familia|Subfamil+ia|Ordo|Subordo|Superfamilia|",
                                   "Tribus|Tribe|Subtribus|Supertribus|Cohors|Sectio):",
                                   "\\s*[†]?(?:\\[\\[|\\{\\{(?:cbr|fbr|gbr|trib|triblast|[^}|]+)\\|)?",
                                   "([A-Z][a-zA-Z]+)"))[[1]]
    if (nrow(labels) > 0) chain[[length(chain) + 1]] <- labels[, 3]
    
    lines_t <- str_split(wikitext_t, "\n")[[1]]
    next_template <- NA
    
    for (line in lines_t) {
      line <- str_trim(line)
      m_nav <- str_match(line, regex("\\{\\{Taxonav\\|([^}]+)\\}\\}", ignore_case = TRUE))
      if (!is.na(m_nav[, 2])) { next_template <- str_trim(m_nav[, 2]); break }
      m_nav2 <- str_match(line, regex("\\{\\{Taxonav2\\|([^|}]+)", ignore_case = TRUE))
      if (!is.na(m_nav2[, 2])) { next_template <- str_trim(m_nav2[, 2]); break }
      all_on_line <- str_match_all(line, "\\{\\{([^}|#\n]+)\\}\\}")[[1]][, 2]
      for (t in all_on_line) {
        t <- str_trim(t)
        if (t == current) next
        if (!any(sapply(SKIP_TEMPLATE, function(p) str_detect(t, p)))) {
          next_template <- t; break
        }
      }
      if (!is.na(next_template)) break
    }
    
    if (!is.na(next_template)) { current <- next_template; next }
    break
  }
  
  if (length(chain) == 0) return(NULL)
  
  lineage <- c()
  for (block in rev(chain)) {
    for (item in block) {
      if (!item %in% lineage) lineage <- c(lineage, item)
    }
  }
  
  dino_idx <- which(lineage == "Dinosauria")
  if (length(dino_idx) == 0) return(NULL)
  lineage[dino_idx[1]:length(lineage)]
}

fetch_pageviews <- function(dino_name) {
  end_date   <- format(Sys.Date(), "%Y%m%d")
  start_date <- format(Sys.Date() - 30, "%Y%m%d")
  for (attempt in c(dino_name, paste0(dino_name, "_(dinosaur)"))) {
    title <- URLencode(gsub(" ", "_", attempt), repeated = TRUE)
    url <- paste0(
      "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/",
      "en.wikipedia/all-access/all-agents/",
      title, "/daily/", start_date, "/", end_date
    )
    res <- tryCatch(
      GET(url, timeout(15), add_headers(`User-Agent` = "Phylosaurgame/1.0")),
      error = function(e) NULL
    )
    if (is.null(res)) next
    if (status_code(res) == 429) return(NA)
    if (status_code(res) == 404) next
    if (status_code(res) != 200) next
    data <- tryCatch(
      fromJSON(content(res, "text", encoding = "UTF-8")),
      error = function(e) NULL
    )
    if (is.null(data) || is.null(data$items)) next
    total <- if (is.data.frame(data$items)) {
      sum(data$items$views, na.rm = TRUE)
    } else {
      sum(sapply(data$items, function(x) x$views), na.rm = TRUE)
    }
    return(total)
  }
  return(0)
}

# ── 3. FETCH LINEAGES IN PARALLEL ────────────────────────────────────────────

cat("=== 2. Fetching lineages from Wikispecies ===\n")

lineage_cache <- if (file.exists(LINEAGE_CACHE)) {
  cat("Resuming from cache:", LINEAGE_CACHE, "\n")
  fromJSON(LINEAGE_CACHE, simplifyVector = FALSE)
} else {
  list()
}

remaining <- official_list[!official_list %in% names(lineage_cache)]
cat("Already fetched:", length(lineage_cache), "| Remaining:", length(remaining), "\n\n")

if (length(remaining) > 0) {
  plan(multisession, workers = PARALLEL_WORKERS)
  chunks <- split(remaining, ceiling(seq_along(remaining) / CHUNK_SIZE))
  
  for (chunk_idx in seq_along(chunks)) {
    chunk <- chunks[[chunk_idx]]
    cat("Chunk", chunk_idx, "/", length(chunks),
        "(", (chunk_idx - 1) * CHUNK_SIZE + 1, "-",
        min(chunk_idx * CHUNK_SIZE, length(remaining)), "of", length(remaining), ")\n")
    
    results <- future_map(
      setNames(chunk, chunk),
      function(dino) {
        wikispecies_name <- ifelse(dino %in% names(NAME_REMAPS),
                                   NAME_REMAPS[[dino]], dino)
        tryCatch(fetch_lineage(wikispecies_name), error = function(e) NULL)
      },
      .progress = TRUE,
      .options = furrr_options(seed = TRUE)
    )
    
    for (dino in names(results)) lineage_cache[[dino]] <- results[[dino]]
    write_json(lineage_cache, LINEAGE_CACHE, pretty = FALSE)
  }
  
  plan(sequential)
}

found_dinos <- names(lineage_cache)[!sapply(lineage_cache, is.null)]
cat("\nLineages found:", length(found_dinos), "/", length(official_list), "\n\n")

# ── 4. FETCH PAGEVIEWS IN PARALLEL ───────────────────────────────────────────

cat("=== 3. Fetching Wikipedia pageviews ===\n")

pageview_cache <- if (file.exists(PAGEVIEW_CACHE)) {
  cat("Resuming from cache:", PAGEVIEW_CACHE, "\n")
  fromJSON(PAGEVIEW_CACHE, simplifyVector = TRUE)
} else {
  c()
}

remaining_pv <- found_dinos[!found_dinos %in% names(pageview_cache)]
cat("Already fetched:", length(pageview_cache), "| Remaining:", length(remaining_pv), "\n\n")

if (length(remaining_pv) > 0) {
  plan(multisession, workers = PARALLEL_WORKERS)
  chunks_pv <- split(remaining_pv, ceiling(seq_along(remaining_pv) / CHUNK_SIZE))
  
  for (chunk_idx in seq_along(chunks_pv)) {
    chunk <- chunks_pv[[chunk_idx]]
    cat("Pageviews chunk", chunk_idx, "/", length(chunks_pv), "\n")
    
    results <- future_map(
      setNames(chunk, chunk),
      function(dino) tryCatch(fetch_pageviews(dino), error = function(e) 0),
      .progress = TRUE,
      .options = furrr_options(seed = TRUE)
    )
    
    for (dino in names(results)) {
      val <- results[[dino]]
      pageview_cache[dino] <- ifelse(is.null(val) || is.na(val), 0, val)
    }
    write_json(as.list(pageview_cache), PAGEVIEW_CACHE, pretty = FALSE)
  }
  
  plan(sequential)
}

cat("Pageviews done!\n\n")

# ── 5. CALCULATE DIFFICULTY ───────────────────────────────────────────────────

cat("=== 4. Calculating difficulty ===\n")

views_numeric  <- sapply(found_dinos, function(d) as.numeric(pageview_cache[[d]]))
views_positive <- sort(
  views_numeric[views_numeric > 0 & !is.na(views_numeric)],
  decreasing = TRUE
)

n          <- length(views_positive)
thresholds <- lapply(PERCENTILES, function(p) views_positive[max(1, floor(n * p))])

cat("Difficulty thresholds (last 30 days of pageviews):\n")
cat("  Muito Facil  >=", thresholds$muito_facil, "\n")
cat("  Facil        >=", thresholds$facil, "\n")
cat("  Normal       >=", thresholds$normal, "\n")
cat("  Dificil      >=", thresholds$dificil, "\n\n")

classify_difficulty <- function(name, views) {
  if (name %in% HOMONYMS)          return("muito_dificil")
  v <- as.numeric(views)
  if (is.na(v) || v == 0)          return("muito_dificil")
  if (v >= thresholds$muito_facil) return("muito_facil")
  if (v >= thresholds$facil)       return("facil")
  if (v >= thresholds$normal)      return("normal")
  if (v >= thresholds$dificil)     return("dificil")
  return("muito_dificil")
}

# ── 6. BUILD AND SAVE GAME DATABASE ──────────────────────────────────────────

cat("=== 5. Building final database ===\n")

final_db <- tibble(
  nome         = found_dinos,
  linhagem     = lapply(found_dinos, function(d) unlist(lineage_cache[[d]])),
  profundidade = sapply(found_dinos, function(d) length(lineage_cache[[d]])),
  dificuldade  = sapply(found_dinos, function(d) classify_difficulty(d, pageview_cache[[d]]))
)

cat("Total taxa:", nrow(final_db), "\n")

cat("\nDifficulty distribution:\n")
dist <- table(final_db$dificuldade)
print(dist)
cat("\nPercentages:\n")
print(round(prop.table(dist) * 100, 1))

cat("\nSpot checks:\n")
for (dino in c("Tyrannosaurus", "Velociraptor", "Triceratops",
               "Brachiosaurus", "Carnotaurus", "Parasaurolophus")) {
  row <- final_db %>% filter(nome == dino)
  if (nrow(row) > 0) {
    views <- as.numeric(pageview_cache[[dino]])
    cat(sprintf("  %-20s %-15s depth: %2d  views: %s\n",
                dino, row$dificuldade, row$profundidade,
                format(views, big.mark = ",")))
  } else {
    cat("  ", dino, ": NOT FOUND\n")
  }
}

dupes <- final_db %>%
  filter(map_lgl(linhagem, function(l) any(table(l) > 1)))
if (nrow(dupes) > 0) {
  cat("\nWARNING: Taxa with duplicate clades:", nrow(dupes), "\n")
  print(dupes$nome)
} else {
  cat("\nNo duplicate clades found.\n")
}

write_json(final_db, OUTPUT_FILE, pretty = TRUE, auto_unbox = TRUE)
cat("\nSaved:", OUTPUT_FILE, "—", nrow(final_db), "taxa\n")

# Verify JSON structure of first record
cat("\nFirst record structure:\n")
check <- fromJSON(OUTPUT_FILE, simplifyVector = FALSE)
cat(toJSON(check[[1]], pretty = TRUE, auto_unbox = TRUE), "\n")


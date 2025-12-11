install_from_cran_github <- function(pkg,
                                     lib = .libPaths()[1],
                                     fields = c("Depends", "Imports", "LinkingTo"),
                                     cache_dir = "~/.cran_github_cache",
                                     verbose = TRUE) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("The 'devtools' package is required. Please install it first.")
  }
  
  if (!nzchar(Sys.which("git"))) {
    stop("No 'git' executable found on PATH. You must have git available.")
  }
  
  cache_dir <- path.expand(cache_dir)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  base_pkgs <- rownames(installed.packages(priority = "base"))
  
  # Hjälpare: parsa "foo", "bar (>= 1.2.3)" -> data.frame(pkg, min_version)
  parse_deps <- function(desc_row, fields) {
    fields <- intersect(fields, colnames(desc_row))
    if (length(fields) == 0) {
      return(data.frame(pkg = character(0), min_version = NA_character_))
    }
    
    x <- desc_row[1, fields, drop = TRUE]
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      return(data.frame(pkg = character(0), min_version = NA_character_))
    }
    
    x <- paste(x, collapse = ",")
    parts <- strsplit(x, ",")[[1]]
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    
    pkgs <- sub("\\s*\\(.*", "", parts)
    ver  <- ifelse(grepl("\\(>=\\s*[^)]+\\)", parts),
                   sub(".*\\(>=\\s*([^\\)]+)\\).*", "\\1", parts),
                   NA_character_)
    
    keep <- pkgs != "R"
    data.frame(pkg = pkgs[keep],
               min_version = ver[keep],
               stringsAsFactors = FALSE)
  }
  
  visited <- character(0)
  
  install_recursive <- function(pkg, required_version = NA_character_) {
    if (pkg %in% visited) return(invisible(NULL))
    visited <<- c(visited, pkg)
    
    if (pkg %in% base_pkgs) {
      if (verbose) message("Skipping base package: ", pkg)
      return(invisible(NULL))
    }
    
    ## 1. Klona / använd cache
    pkg_dir <- file.path(cache_dir, pkg)
    
    if (!dir.exists(pkg_dir) || !file.exists(file.path(pkg_dir, "DESCRIPTION"))) {
      if (dir.exists(pkg_dir) && !file.exists(file.path(pkg_dir, "DESCRIPTION"))) {
        unlink(pkg_dir, recursive = TRUE, force = TRUE)
      }
      url <- sprintf("https://github.com/cran/%s.git", pkg)
      if (verbose) message("Cloning ", url, " into ", pkg_dir)
      res <- system2("git", c("clone", "--depth", "1", url, pkg_dir),
                     stdout = TRUE, stderr = TRUE)
      if (!dir.exists(file.path(pkg_dir, ".git"))) {
        message("Failed to clone ", url, ". Output:\n", paste(res, collapse = "\n"))
        stop("Cannot proceed for package ", pkg)
      }
    } else {
      if (verbose) message("Using cached repo for ", pkg, " in ", pkg_dir)
    }
    
    ## 2. Läs DESCRIPTION för att få version + beroenden
    desc_file <- file.path(pkg_dir, "DESCRIPTION")
    if (!file.exists(desc_file)) {
      stop("No DESCRIPTION file found in ", pkg_dir, " for package ", pkg)
    }
    desc <- read.dcf(desc_file)
    
    own_ver <- if ("Version" %in% colnames(desc)) desc[1, "Version"] else NA_character_
    
    # plocka beroenden (med ev. versionskrav)
    deps <- parse_deps(desc, fields)
    
    if (nrow(deps) > 0 && verbose) {
      msg <- paste(
        apply(deps, 1, function(row) {
          if (is.na(row["min_version"])) row["pkg"] else
            paste0(row["pkg"], " (>= ", row["min_version"], ")")
        }),
        collapse = ", "
      )
      message("Dependencies for ", pkg, ": ", msg)
    }
    
    ## 3. Installera/uppdatera beroenden först (rekursivt)
    if (nrow(deps) > 0) {
      for (i in seq_len(nrow(deps))) {
        d_pkg <- deps$pkg[i]
        d_ver <- deps$min_version[i]
        if (!nzchar(d_ver)) d_ver <- NA_character_
        install_recursive(d_pkg, required_version = d_ver)
      }
    }
    
    ## 4. Bestäm vilken minsta version vi kräver för just det här paketet
    target_min_version <- NA_character_
    if (!is.na(required_version) && !is.na(own_ver)) {
      target_min_version <- as.character(max(package_version(c(required_version, own_ver))))
    } else if (!is.na(required_version)) {
      target_min_version <- required_version
    } else if (!is.na(own_ver)) {
      target_min_version <- own_ver
    }
    
    ## 5. Om paketet redan är installerat med tillräcklig version → hoppa över
    if (requireNamespace(pkg, quietly = TRUE)) {
      inst_ver <- tryCatch(as.character(utils::packageVersion(pkg)),
                           error = function(e) NA_character_)
      if (!is.na(target_min_version) && !is.na(inst_ver)) {
        if (package_version(inst_ver) >= package_version(target_min_version)) {
          if (verbose) {
            message("Already installed and meets target version: ",
                    pkg, " (installed ", inst_ver,
                    ", target ≥ ", target_min_version, ")")
          }
          return(invisible(NULL))
        }
      }
    }
    
    ## 6. Installera/uppdatera från GitHub/cran
    if (verbose) {
      message("Installing/updating package ", pkg,
              if (!is.na(own_ver)) paste0(" (repo version ", own_ver, ")") else "",
              " from ", pkg_dir)
    }
    
    devtools::install(pkg_dir,
                      lib = lib,
                      dependencies = FALSE,
                      upgrade = "never")
    
    invisible(NULL)
  }
  
  install_recursive(pkg, required_version = NA_character_)
  
  if (verbose) {
    message("Done. Visited/attempted packages: ",
            paste(unique(visited), collapse = ", "))
  }
  
  invisible(unique(visited))
}

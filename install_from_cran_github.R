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
  
  # baspaket som vi aldrig installerar själva
  base_pkgs <- rownames(installed.packages(priority = "base"))
  
  # Hjälpare: parsa "foo", "bar (>= 1.2.3)" → data.frame(pkg, min_version)
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
    
    pkgs <- sub("\\s*\\(.*", "", parts)  # ta bort versionsdelen
    # plocka ut ev. (>= X.Y.Z)
    ver  <- ifelse(grepl("\\(>=\\s*[^)]+\\)", parts),
                   sub(".*\\(>=\\s*([^\\)]+)\\).*", "\\1", parts),
                   NA_character_)
    
    # ta bort R
    keep <- pkgs != "R"
    data.frame(pkg = pkgs[keep],
               min_version = ver[keep],
               stringsAsFactors = FALSE)
  }
  
  visited <- character(0)
  
  install_recursive <- function(pkg, required_version = NA_character_) {
    # undvik cykler
    if (pkg %in% visited) return(invisible(NULL))
    visited <<- c(visited, pkg)
    
    # hoppa över baspaket
    if (pkg %in% base_pkgs) {
      if (verbose) message("Skipping base package: ", pkg)
      return(invisible(NULL))
    }
    
    # kolla om paketet redan är installerat och uppfyller ev. minsta version
    if (requireNamespace(pkg, quietly = TRUE)) {
      inst_ver <- tryCatch(as.character(utils::packageVersion(pkg)),
                           error = function(e) NA_character_)
      if (!is.na(required_version) && !is.na(inst_ver)) {
        if (package_version(inst_ver) >= package_version(required_version)) {
          if (verbose) {
            message("Already installed and meets version requirement: ",
                    pkg, " (installed ", inst_ver,
                    ", required ", required_version, ")")
          }
          return(invisible(NULL))
        }
      } else if (is.na(required_version)) {
        # inget versionskrav → nöj dig med befintlig installation
        if (verbose) {
          message("Already installed (no explicit version requirement): ", pkg)
        }
        return(invisible(NULL))
      }
    }
    
    # ==============================
    # Klona eller använd cache
    # ==============================
    pkg_dir <- file.path(cache_dir, pkg)
    
    if (!dir.exists(pkg_dir) || !file.exists(file.path(pkg_dir, "DESCRIPTION"))) {
      if (dir.exists(pkg_dir) && !file.exists(file.path(pkg_dir, "DESCRIPTION"))) {
        # trasig cache → ta bort och börja om
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
    
    # Läs DESCRIPTION
    desc_file <- file.path(pkg_dir, "DESCRIPTION")
    if (!file.exists(desc_file)) {
      stop("No DESCRIPTION file found in ", pkg_dir, " for package ", pkg)
    }
    desc <- read.dcf(desc_file)
    
    # egen version av paketet (på GitHub)
    own_ver <- if ("Version" %in% colnames(desc)) {
      desc[1, "Version"]
    } else {
      NA_character_
    }
    
    # ==============================
    # Hantera beroenden först (rekursivt)
    # ==============================
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
    
    if (nrow(deps) > 0) {
      for (i in seq_len(nrow(deps))) {
        d_pkg <- deps$pkg[i]
        d_ver <- deps$min_version[i]
        if (!nzchar(d_ver)) d_ver <- NA_character_
        install_recursive(d_pkg, required_version = d_ver)
      }
    }
    
    # ==============================
    # Installera detta paket (om behövs)
    # ==============================
    # Kolla om det nu är installerat med tillräcklig version
    if (requireNamespace(pkg, quietly = TRUE)) {
      inst_ver <- tryCatch(as.character(utils::packageVersion(pkg)),
                           error = function(e) NA_character_)
      # om paketet redan finns och är >= own_ver, hoppa över
      if (!is.na(own_ver) && !is.na(inst_ver)) {
        if (package_version(inst_ver) >= package_version(own_ver)) {
          if (verbose) {
            message("Package ", pkg, " already installed with version ",
                    inst_ver, " >= repo version ", own_ver, ". Skipping install.")
          }
          return(invisible(NULL))
        }
      }
    }
    
    if (verbose) {
      message("Installing package ", pkg, 
              if (!is.na(own_ver)) paste0(" (version ", own_ver, ")") else "",
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

environment_scalar <- function(value, fallback = "") {
  value <- as.character(value)
  if (length(value) == 0L || is.na(value[[1L]]) || !nzchar(value[[1L]])) fallback else value[[1L]]
}

environment_extension <- function(extensions, name) {
  if (is.null(extensions) || is.null(names(extensions)) || !(name %in% names(extensions))) "" else extensions[[name]]
}

resolve_zig_executable <- function(root_dir) {
  configured <- Sys.getenv("ZIG", unset = "")
  candidates <- c(
    configured,
    Sys.which("zig"),
    file.path(root_dir, "..", "zig-0.16.0", "zig"),
    file.path(root_dir, "..", "zig-0.16.0", "zig.exe")
  )
  candidates <- candidates[nzchar(candidates)]
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0L) stop("Zig executable not found; set ZIG or install zig")
  normalizePath(existing[[1L]])
}

command_version <- function(executable, label, arguments = "version") {
  output <- tryCatch(
    system2(executable, arguments, stdout = TRUE, stderr = TRUE),
    error = function(error) stop(sprintf("cannot read %s version: %s", label, conditionMessage(error)))
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) stop(sprintf("%s --version exited with code %d", label, status))
  version <- trimws(paste(output, collapse = " "))
  if (!nzchar(version)) stop(sprintf("%s version is empty", label))
  version
}

read_cpu_model <- function() {
  if (file.exists("/proc/cpuinfo")) {
    lines <- tryCatch(readLines("/proc/cpuinfo", warn = FALSE), error = function(error) character(0))
    matches <- grep("^(model name|Hardware|Processor)\\s*:", lines, value = TRUE)
    if (length(matches) > 0L) return(trimws(sub("^[^:]+:\\s*", "", matches[[1L]])))
  }
  processor <- Sys.getenv("PROCESSOR_IDENTIFIER", unset = "")
  if (nzchar(processor)) return(processor)
  sysctl <- Sys.which("sysctl")
  if (nzchar(sysctl)) {
    value <- tryCatch(system2(sysctl, c("-n", "machdep.cpu.brand_string"), stdout = TRUE, stderr = TRUE),
                      error = function(error) character(0))
    value <- trimws(paste(value, collapse = " "))
    if (nzchar(value)) return(value)
  }
  ""
}

source_tree_files <- function(root_dir) {
  git_files <- tryCatch(
    system2(
      "git",
      c("-C", root_dir, "ls-files", "--cached", "--others", "--exclude-standard", "--"),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(error) stop(sprintf("could not enumerate source files with git: %s", conditionMessage(error)))
  )
  status <- attr(git_files, "status")
  if (!is.null(status) && status != 0L) stop("git source-file enumeration failed")
  relative <- gsub("\\\\", "/", git_files[nzchar(git_files)])
  if (length(relative) == 0L) stop("source tree contains no git worktree files")

  relevant <- grepl(
    "^(\\.gitignore|build\\.zig(?:\\.zon)?|\\.github/|src/|tests/|benchmarks/)",
    relative
  )
  generated <- grepl(
    "(^|/)(results|target|tmp|temp|zig-out|zig-cache|\\.zig-cache|\\.zig-global-cache)(/|$)|(^|/)benchmarks/analysis/summary\\.csv$|\\.(so|dll|dylib|o|a|d|obj|exe|stamp|tmp|log)$",
    relative
  )
  relative <- sort(relative[relevant & !generated])
  if (length(relative) == 0L) stop("source tree has no identity files after exclusions")
  list(
    relative = relative,
    included_prefixes = c(".gitignore", "build.zig", "build.zig.zon", ".github/", "src/", "tests/", "benchmarks/"),
    excluded_patterns = c("git ignored files", "**/{results,target,tmp,temp,zig-out,zig-cache,.zig-cache,.zig-global-cache}/", "benchmarks/analysis/summary.csv", "**/*.{so,dll,dylib,o,a,d,obj,exe,stamp,tmp,log}")
  )
}

source_tree_identity <- function(root_dir) {
  root_dir <- normalizePath(root_dir)
  selection <- source_tree_files(root_dir)
  files <- file.path(root_dir, selection$relative)
  file_md5 <- as.character(tools::md5sum(files))
  if (anyNA(file_md5)) stop("could not hash every source identity file")
  lines <- paste(selection$relative, file_md5, sep = "\t")
  temporary <- tempfile("source-tree-")
  on.exit(unlink(temporary), add = TRUE)
  writeLines(lines, temporary, useBytes = TRUE)
  digest <- as.character(tools::md5sum(temporary))
  list(
    method = "git-worktree-md5",
    digest = unname(digest[[1L]]),
    file_count = length(files),
    included_prefixes = selection$included_prefixes,
    excluded_patterns = selection$excluded_patterns
  )
}

shared_library_metadata <- function(root_dir, relative_path) {
  relative_path <- environment_scalar(relative_path)
  if (!nzchar(relative_path)) return(list(configured = FALSE))
  path <- file.path(root_dir, relative_path)
  exists <- file.exists(path)
  info <- if (exists) file.info(path) else NULL
  list(
    configured = TRUE,
    relative_path = relative_path,
    absolute_path = normalizePath(path, mustWork = FALSE),
    exists = exists,
    size_bytes = if (exists) as.numeric(info$size) else NA_real_,
    modified = if (exists) format(info$mtime, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC") else NA_character_,
    md5 = if (exists) unname(as.character(tools::md5sum(path))) else NA_character_
  )
}

runner_environment_metadata <- function(root_dir, runners) {
  lapply(names(runners), function(runner_name) {
    cfg <- runners[[runner_name]]
    so_path <- if (is.null(cfg$so_path)) "" else as.character(cfg$so_path)
    extra_paths <- if (is.null(cfg$extra_so_paths)) character(0) else as.character(unlist(cfg$extra_so_paths, use.names = FALSE))
    list(
      name = runner_name,
      label = environment_scalar(cfg$label, runner_name),
      call_type = environment_scalar(cfg$call_type, "unknown"),
      so_path = so_path,
      extra_so_paths = extra_paths,
      shared_libraries = c(list(main = shared_library_metadata(root_dir, so_path)),
                           setNames(lapply(extra_paths, function(path) shared_library_metadata(root_dir, path)),
                                    if (length(extra_paths) == 0L) character(0) else paste0("extra_", seq_along(extra_paths))))
    )
  })
}

capture_environment_manifest <- function(root_dir, runners, blas_env, build_settings, source_root = root_dir) {
  zig_path <- resolve_zig_executable(root_dir)
  r_extensions <- extSoftVersion()
  environment_names <- c(
    "OPENBLAS_NUM_THREADS", "OMP_NUM_THREADS", "MKL_NUM_THREADS", "BLAS_NUM_THREADS",
    "R_LIBS", "R_LIBS_USER", "R_INCLUDE", "R_LIB", "ZIG", "ZIGR_OPTIMIZE",
    "ZIGR_TARGET", "ZIGR_CPU_FEATURES", "CC", "CFLAGS", "CXXFLAGS", "LDFLAGS", "PKG_CONFIG_PATH",
    "LANG", "LC_ALL", "LC_CTYPE", "LC_NUMERIC", "LC_TIME", "LC_COLLATE", "LC_MONETARY", "LC_MESSAGES"
  )
  process_environment <- as.list(Sys.getenv(environment_names, unset = ""))
  for (entry in blas_env) {
    parts <- strsplit(entry, "=", fixed = TRUE)[[1L]]
    if (length(parts) >= 2L) process_environment[[parts[[1L]]]] <- paste(parts[-1L], collapse = "=")
  }
  blas_value <- environment_scalar(environment_extension(r_extensions, "BLAS"), "")
  lapack_value <- environment_scalar(environment_extension(r_extensions, "LAPACK"), "")
  host_info <- Sys.info()
  safe_locale <- function(category) {
    tryCatch(Sys.getlocale(category), error = function(error) "")
  }
  list(
    schema_version = 1L,
    captured_at = run_manifest_timestamp(),
    source_tree = source_tree_identity(source_root),
    host = list(
      sysname = environment_scalar(host_info[["sysname"]]),
      release = environment_scalar(host_info[["release"]]),
      version = environment_scalar(host_info[["version"]]),
      machine = environment_scalar(host_info[["machine"]]),
      cpu_model = read_cpu_model(),
      logical_cores = parallel::detectCores(logical = TRUE)
    ),
    r = list(
      version = R.version.string,
      major = R.version$major,
      minor = R.version$minor,
      platform = R.version$platform,
      arch = R.version$arch,
      home = R.home(),
      extensions = as.list(r_extensions)
    ),
    zig = list(
      executable = zig_path,
      version = command_version(zig_path, "Zig")
    ),
    build = build_settings,
    blas = list(
      vendor = environment_scalar(Sys.getenv("BLAS_VENDOR", unset = ""), blas_value),
      version_or_path = blas_value,
      lapack = lapack_value,
      configured_threads = process_environment[c("OPENBLAS_NUM_THREADS", "OMP_NUM_THREADS", "MKL_NUM_THREADS", "BLAS_NUM_THREADS")]
    ),
    locale = list(
      LC_ALL = safe_locale("LC_ALL"),
      LC_CTYPE = safe_locale("LC_CTYPE"),
      LC_NUMERIC = safe_locale("LC_NUMERIC"),
      LC_TIME = safe_locale("LC_TIME"),
      LC_COLLATE = safe_locale("LC_COLLATE"),
      LC_MONETARY = safe_locale("LC_MONETARY"),
      LC_MESSAGES = safe_locale("LC_MESSAGES")
    ),
    process_environment = process_environment,
    runner_configs = runner_environment_metadata(root_dir, runners),
    build_command = environment_scalar(build_settings$command)
  )
}

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
  relative <- relative[file.exists(file.path(root_dir, relative))]
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

file_identity_digest <- function(root_dir, relative_paths, label) {
  relative_paths <- sort(unique(as.character(relative_paths)))
  paths <- file.path(root_dir, relative_paths)
  missing <- relative_paths[!file.exists(paths)]
  if (length(missing) > 0L) stop(sprintf("%s identity files are missing: %s", label, paste(missing, collapse = ", ")))
  digests <- unname(as.character(tools::md5sum(paths)))
  identity_file <- tempfile("runner-identity-")
  on.exit(unlink(identity_file), add = TRUE)
  writeLines(paste(relative_paths, digests, sep = "\t"), identity_file, useBytes = TRUE)
  unname(as.character(tools::md5sum(identity_file))[[1L]])
}

absolute_file_identity_digest <- function(paths, label) {
  paths <- sort(unique(normalizePath(as.character(paths), mustWork = FALSE)))
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0L) stop(sprintf("%s identity files are missing: %s", label, paste(missing, collapse = ", ")))
  digests <- unname(as.character(tools::md5sum(paths)))
  identity_file <- tempfile("artifact-identity-")
  on.exit(unlink(identity_file), add = TRUE)
  writeLines(paste(paths, digests, sep = "\t"), identity_file, useBytes = TRUE)
  unname(as.character(tools::md5sum(identity_file))[[1L]])
}

runner_glue_paths <- function(runner_name) {
  switch(runner_name,
    c_call = "src/c_call/register.c",
    cpp11 = c("src/cpp11/src/cpp11.cpp", "src/cpp11/R/cpp11.R", "src/cpp11/NAMESPACE"),
    extendr = c("src/extendr/entrypoint.c", "src/extendr/rust/Cargo.lock"),
    r = "src/r/run_all.R",
    rcpp = "src/cpp/main.cpp",
    savvy = c("src/savvy/init.c", "src/savvy/rust/Cargo.lock"),
    zigr = c("src/zig/main.zig", "../src/export.zig"),
    stop(sprintf("no generated-glue identity rule for runner %s", runner_name))
  )
}

runner_glue_kind <- function(runner_name) {
  switch(runner_name,
    cpp11 = "committed_generated_output",
    zigr = "compile_time_generator_source",
    extendr = "macro_and_registration_source",
    savvy = "handwritten_registration_source",
    c_call = "registered_control_source",
    r = "not_applicable_r_source_identity",
    rcpp = "handwritten_control_source",
    stop(sprintf("no generated-glue kind for runner %s", runner_name))
  )
}

runner_tool_identity <- function(runner_name, cfg) {
  package_identity <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) return(sprintf("%s unavailable", package))
    sprintf("%s %s", package, as.character(utils::packageVersion(package)))
  }
  switch(runner_name,
    cpp11 = package_identity("cpp11"),
    rcpp = package_identity("Rcpp"),
    r = R.version.string,
    c_call = "registered C control",
    extendr = "extendr locked Rust control",
    savvy = "Savvy locked Rust control",
    zigr = "zigr Zig public and diagnostic paths",
    environment_scalar(cfg$label, runner_name)
  )
}

runner_environment_metadata <- function(root_dir, runners) {
  lapply(names(runners), function(runner_name) {
    cfg <- runners[[runner_name]]
    so_path <- if (is.null(cfg$so_path)) "" else as.character(cfg$so_path)
    extra_paths <- if (is.null(cfg$extra_so_paths)) character(0) else as.character(unlist(cfg$extra_so_paths, use.names = FALSE))
    config_path <- file.path("runners", paste0(runner_name, ".json"))
    artifact_path <- if (identical(runner_name, "r")) "src/r/run_all.R" else so_path
    artifact_relative_paths <- c(artifact_path, extra_paths)
    artifact_paths <- normalizePath(file.path(root_dir, artifact_relative_paths), mustWork = FALSE)
    glue_paths <- runner_glue_paths(runner_name)
    list(
      name = runner_name,
      label = environment_scalar(cfg$label, runner_name),
      call_type = environment_scalar(cfg$call_type, "unknown"),
      tool_identity = runner_tool_identity(runner_name, cfg),
      runner_config_digest = file_identity_digest(root_dir, config_path, sprintf("%s runner config", runner_name)),
      generated_glue_kind = runner_glue_kind(runner_name),
      generated_glue_paths = glue_paths,
      generated_glue_digest = file_identity_digest(root_dir, glue_paths, sprintf("%s generated glue", runner_name)),
      artifact_path = artifact_paths[[1L]],
      artifact_paths = as.list(artifact_paths),
      artifact_digest = absolute_file_identity_digest(artifact_paths, sprintf("%s artifacts", runner_name)),
      so_path = so_path,
      extra_so_paths = extra_paths,
      shared_libraries = c(list(main = shared_library_metadata(root_dir, so_path)),
                           setNames(lapply(extra_paths, function(path) shared_library_metadata(root_dir, path)),
                                    if (length(extra_paths) == 0L) character(0) else paste0("extra_", seq_along(extra_paths))))
    )
  })
}

runner_environment_record <- function(environment, runner_name) {
  records <- environment$runner_configs
  matches <- records[vapply(records, function(record) identical(as.character(record$name), runner_name), logical(1))]
  if (length(matches) != 1L) stop(sprintf("environment metadata has no unique runner record for %s", runner_name))
  matches[[1L]]
}

validate_runner_artifact_identity <- function(root_dir, runner_record) {
  runner_name <- as.character(runner_record$name)
  artifact_paths <- as.character(unlist(runner_record$artifact_paths, use.names = FALSE))
  if (length(artifact_paths) == 0L || any(!nzchar(artifact_paths))) stop(sprintf("runner %s has no artifact paths", runner_name))
  actual_artifact_digest <- absolute_file_identity_digest(artifact_paths, sprintf("%s artifacts", runner_name))
  if (!identical(actual_artifact_digest, as.character(runner_record$artifact_digest))) {
    stop(sprintf("artifact drift detected for runner %s", runner_name))
  }
  actual_config <- file_identity_digest(
    root_dir,
    file.path("runners", paste0(runner_name, ".json")),
    sprintf("%s runner config", runner_name)
  )
  if (!identical(actual_config, as.character(runner_record$runner_config_digest))) {
    stop(sprintf("runner config drift detected for %s", runner_name))
  }
  actual_glue <- file_identity_digest(
    root_dir,
    as.character(unlist(runner_record$generated_glue_paths, use.names = FALSE)),
    sprintf("%s generated glue", runner_name)
  )
  if (!identical(actual_glue, as.character(runner_record$generated_glue_digest))) {
    stop(sprintf("generated-glue drift detected for %s", runner_name))
  }
  invisible(runner_record)
}

capture_environment_manifest <- function(root_dir, runners, blas_env, build_settings, source_root = root_dir) {
  zig_path <- resolve_zig_executable(root_dir)
  r_extensions <- extSoftVersion()
  environment_names <- c(
    "OPENBLAS_NUM_THREADS", "OMP_NUM_THREADS", "MKL_NUM_THREADS", "BLAS_NUM_THREADS",
    "R_LIBS", "R_LIBS_USER", "R_INCLUDE", "R_LIB", "ZIG", "ZIGR_OPTIMIZE", "ZIGR_CHECKED_SEXP",
    "ZIGR_TARGET", "ZIGR_CPU_FEATURES", "ZIGR_SEXP_ABI", "CC", "CFLAGS", "CXXFLAGS", "LDFLAGS", "PKG_CONFIG_PATH",
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
    schema_version = 2L,
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

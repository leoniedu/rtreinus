#!/usr/bin/env Rscript
# Development helper script for rtreinus package

cat("═══════════════════════════════════════════════════════════════\n")
cat("  rtreinus Package Development Helper\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# ── Install Development Dependencies ─────────────────────────────────────────

cat("→ Installing development dependencies...\n")

dev_packages <- c(
  "devtools",
  "roxygen2",
  "testthat",
  "pkgdown",
  "usethis",
  "covr",
  "lintr",
  "styler"
)

for (pkg in dev_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("  Installing %s...\n", pkg))
    install.packages(pkg)
  }
}

cat("✓ Development dependencies installed\n\n")

# ── Load Package Development Tools ───────────────────────────────────────────

library(devtools)
library(roxygen2)
library(testthat)
library(usethis)

# ── Generate Documentation ───────────────────────────────────────────────────

cat("→ Generating documentation with roxygen2...\n")
devtools::document()
cat("✓ Documentation generated\n\n")

# ── Run Tests ────────────────────────────────────────────────────────────────

cat("→ Running tests...\n")
test_results <- devtools::test()
cat("✓ Tests completed\n\n")

# ── Check Package ────────────────────────────────────────────────────────────

cat("→ Running R CMD check...\n")
check_results <- devtools::check(
  document = FALSE,  # Already documented above
  args = c("--no-manual"),
  error_on = "warning"
)
cat("✓ Package check completed\n\n")

# ── Build README ─────────────────────────────────────────────────────────────

if (file.exists("README.Rmd")) {
  cat("→ Building README...\n")
  devtools::build_readme()
  cat("✓ README built\n\n")
}

# ── Optional: Build pkgdown site ─────────────────────────────────────────────

cat("Optional: Build pkgdown documentation site?\n")
cat("  Run: pkgdown::build_site()\n\n")

# ── Summary ──────────────────────────────────────────────────────────────────

cat("═══════════════════════════════════════════════════════════════\n")
cat("  Package development tasks completed!\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("Next steps:\n")
cat("  1. Review check results and fix any issues\n")
cat("  2. Update NEWS.md with changes\n")
cat("  3. Test locally: devtools::load_all()\n")
cat("  4. Install: devtools::install()\n")
cat("  5. Build site: pkgdown::build_site()\n\n")

cat("For continuous development, run:\n")
cat("  devtools::load_all()  # Load package for testing\n")
cat("  devtools::document()  # Regenerate documentation\n")
cat("  devtools::test()      # Run tests\n")
cat("  devtools::check()     # Full package check\n\n")

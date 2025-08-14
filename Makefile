# FortCov System Testing Makefile
# Creates coverage reports using both lcov toolchain and fortcov for comparison

.PHONY: external clean-external coverage-comparison clean-coverage help test build
.DEFAULT_GOAL := help

# Configuration
COVERAGE_FLAGS = -fprofile-arcs -ftest-coverage
OUTPUT_DIR = coverage_reports
LCOV_OUTPUT = $(OUTPUT_DIR)/lcov_report.md
FORTCOV_OUTPUT = $(OUTPUT_DIR)/fortcov_report.md
TIMESTAMP = $(shell date '+%Y%m%d_%H%M%S')

help: ## Show this help message
	@echo "FortCov System Testing Makefile"
	@echo "================================"
	@echo ""
	@echo "Targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-20s %s\n", $$1, $$2}'
	@echo ""
	@echo "This generates two markdown reports for comparison:"
	@echo "  1. lcov -> lcov_cobertura -> pycobertura toolchain"
	@echo "  2. fortcov direct analysis"
	@echo ""

# External dependencies
external: external/pycobertura external/gcovr external/lcov external/lcov_cobertura ## Install external comparison tools

external/pycobertura:
	git clone https://github.com/aconrad/pycobertura.git external/pycobertura

external/gcovr:
	git clone https://github.com/gcovr/gcovr.git external/gcovr

external/lcov:
	git clone https://github.com/linux-test-project/lcov.git external/lcov

external/lcov_cobertura:
	git clone https://github.com/eriwen/lcov-to-cobertura-xml.git external/lcov_cobertura

clean-external: ## Remove external dependencies
	rm -rf external/

# FortCov development
build: ## Build fortcov for testing
	@echo "Building fortcov..."
	fpm build --profile release

test: ## Run tests without coverage
	@echo "Running tests..."
	fpm test

# System testing - coverage comparison
coverage-comparison: clean-coverage build external ## Generate both coverage reports for comparison
	@echo "==================================================="
	@echo "FortCov System Testing - Coverage Comparison"
	@echo "==================================================="
	@echo "Timestamp: $(shell date)"
	@echo ""
	@mkdir -p $(OUTPUT_DIR)
	@$(MAKE) generate-lcov-report
	@$(MAKE) generate-fortcov-report
	@$(MAKE) compare-reports
	@echo ""
	@echo "✅ Coverage comparison complete!"
	@echo "Reports available in: $(OUTPUT_DIR)/"

generate-lcov-report: ## Generate coverage report using lcov toolchain
	@echo "📊 Generating coverage using lcov toolchain..."
	@echo "Step 1: Clean and build with coverage flags..."
	fpm clean --skip
	fpm build --flag "$(COVERAGE_FLAGS)"
	
	@echo "Step 2: Running tests with coverage..."
	fpm test --flag "$(COVERAGE_FLAGS)"
	
	@echo "Step 3: Capturing coverage with lcov..."
	./external/lcov/bin/lcov --capture --directory . --output-file $(OUTPUT_DIR)/coverage.info \
		--exclude '*/test/*' --ignore-errors inconsistent,unused,path
	@echo "Validating lcov capture output..."
	@if [ ! -f "$(OUTPUT_DIR)/coverage.info" ] || [ ! -s "$(OUTPUT_DIR)/coverage.info" ]; then \
		echo "Error: lcov capture failed - no coverage.info generated" >&2; \
		echo "# LCOV Toolchain Report\n\nError: lcov capture failed" > $(LCOV_OUTPUT); \
		exit 1; \
	fi
	@if ! grep -q "^TN:" $(OUTPUT_DIR)/coverage.info; then \
		echo "Error: lcov capture failed - invalid coverage.info format" >&2; \
		echo "# LCOV Toolchain Report\n\nError: invalid lcov format" > $(LCOV_OUTPUT); \
		exit 1; \
	fi
	
	@echo "Step 4: Converting to Cobertura XML..."
	python3 external/lcov_cobertura/lcov_cobertura/lcov_cobertura.py $(OUTPUT_DIR)/coverage.info \
		--output $(OUTPUT_DIR)/coverage.xml
	@echo "Validating Cobertura XML output..."
	@if [ ! -f "$(OUTPUT_DIR)/coverage.xml" ] || [ ! -s "$(OUTPUT_DIR)/coverage.xml" ]; then \
		echo "Error: lcov_cobertura conversion failed - no XML generated" >&2; \
		echo "# LCOV Toolchain Report\n\nError: XML conversion failed" > $(LCOV_OUTPUT); \
		exit 1; \
	fi
	@if ! grep -q "<coverage" $(OUTPUT_DIR)/coverage.xml; then \
		echo "Error: lcov_cobertura conversion failed - invalid XML format" >&2; \
		echo "# LCOV Toolchain Report\n\nError: invalid XML format" > $(LCOV_OUTPUT); \
		exit 1; \
	fi
	
	@echo "Step 5: Converting to Markdown with pycobertura..."
	python3 -m pycobertura show $(OUTPUT_DIR)/coverage.xml \
		--format markdown --output $(LCOV_OUTPUT)
	@echo "Validating Markdown output..."
	@if [ ! -f "$(LCOV_OUTPUT)" ] || [ ! -s "$(LCOV_OUTPUT)" ]; then \
		echo "Error: pycobertura conversion failed - no Markdown generated" >&2; \
		echo "# LCOV Toolchain Report\n\nError: Markdown conversion failed" > $(LCOV_OUTPUT); \
		exit 1; \
	fi
	@if ! grep -q "Filename" $(LCOV_OUTPUT); then \
		echo "Error: pycobertura conversion failed - invalid Markdown format" >&2; \
		echo "# LCOV Toolchain Report\n\nError: invalid Markdown format" > $(LCOV_OUTPUT); \
		exit 1; \
	fi
	
	@echo "✅ LCOV toolchain report: $(LCOV_OUTPUT)"

generate-fortcov-report: ## Generate coverage report using fortcov
	@echo "🚀 Generating coverage using fortcov..."
	@echo "Step 1: Building with coverage flags..."
	fpm build --flag "$(COVERAGE_FLAGS)"
	
	@echo "Step 2: Running tests with coverage..."
	fpm test --flag "$(COVERAGE_FLAGS)"
	
	@echo "Step 3: Running fortcov analysis..."
	fpm run -- \
		--input-format=gcov \
		--output-format=markdown \
		--output=$(FORTCOV_OUTPUT) \
		--source=build/**/ \
		--verbose
	@echo "Validating FortCov output..."
	@if [ ! -f "$(FORTCOV_OUTPUT)" ] || [ ! -s "$(FORTCOV_OUTPUT)" ]; then \
		echo "Error: FortCov analysis failed - no report generated" >&2; \
		echo "# FortCov Report\n\nError: Analysis failed" > $(FORTCOV_OUTPUT); \
		exit 1; \
	fi
	@if ! grep -q "Coverage Report" $(FORTCOV_OUTPUT); then \
		echo "Error: FortCov analysis failed - invalid report format" >&2; \
		echo "# FortCov Report\n\nError: Invalid format" > $(FORTCOV_OUTPUT); \
		exit 1; \
	fi
	
	@echo "✅ FortCov report: $(FORTCOV_OUTPUT)"

compare-reports: ## Compare the two generated reports
	@echo ""
	@echo "📋 Coverage Report Comparison"
	@echo "============================="
	@echo ""
	@echo "LCOV Toolchain Report:"
	@echo "----------------------"
	@if [ -f "$(LCOV_OUTPUT)" ]; then \
		wc -l $(LCOV_OUTPUT) | awk '{print "Lines: " $$1}'; \
		echo "Preview:"; \
		head -10 $(LCOV_OUTPUT) 2>/dev/null | sed 's/^/  /'; \
		echo ""; \
		grep -i "coverage\|percent\|%" $(LCOV_OUTPUT) | head -3 2>/dev/null | sed 's/^/  /' || echo "  No coverage statistics found"; \
	else \
		echo "  Report not found: $(LCOV_OUTPUT)"; \
	fi
	@echo ""
	@echo "FortCov Report:"
	@echo "---------------"
	@if [ -f "$(FORTCOV_OUTPUT)" ]; then \
		wc -l $(FORTCOV_OUTPUT) | awk '{print "Lines: " $$1}'; \
		echo "Preview:"; \
		head -10 $(FORTCOV_OUTPUT) 2>/dev/null | sed 's/^/  /'; \
		echo ""; \
		grep -i "coverage\|percent\|%" $(FORTCOV_OUTPUT) | head -3 2>/dev/null | sed 's/^/  /' || echo "  No coverage statistics found"; \
	else \
		echo "  Report not found: $(FORTCOV_OUTPUT)"; \
	fi
	@echo ""
	@echo "Files for manual comparison:"
	@echo "  - LCOV chain:  $(LCOV_OUTPUT)"
	@echo "  - FortCov:     $(FORTCOV_OUTPUT)"

clean-coverage: ## Clean all coverage artifacts
	@echo "🧹 Cleaning coverage artifacts..."
	@rm -rf $(OUTPUT_DIR)
	@find . -name "*.gcno" -delete 2>/dev/null || true
	@find . -name "*.gcda" -delete 2>/dev/null || true
	@find . -name "*.gcov" -delete 2>/dev/null || true
	@echo "✅ Coverage artifacts cleaned"

# Quick debugging and testing
debug-coverage: ## Debug coverage generation process
	@echo "🔍 Debug mode - showing detailed coverage process..."
	@echo "Current directory: $(shell pwd)"
	@echo "FPM build files:"
	@find build -name "*.gcno" 2>/dev/null | head -5 || echo "No .gcno files found"
	@echo "FPM test files:"
	@find build -name "*.gcda" 2>/dev/null | head -5 || echo "No .gcda files found"
	@echo "External tools status:"
	@ls -la external/ 2>/dev/null || echo "External directory not found - run 'make external'"

# Development targets
dev-build: ## Quick development build without coverage
	fpm build

dev-test: ## Quick development test without coverage
	fpm test

dev-run: ## Run fortcov with help
	fpm run -- --help
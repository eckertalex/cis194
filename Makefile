# ==================================================================================== #
# HELPERS
# ==================================================================================== #

## help: print this help message
.PHONY: help
help:
	@echo "Usage:"
	@sed -n "s/^##//p" ${MAKEFILE_LIST} | column -t -s ":" |  sed -e "s/^/ /"

# ==================================================================================== #
# QUALITY CONTROL
# ==================================================================================== #

## audit: run quality control checks
.PHONY: audit
audit:
	@find . -name '*.hs' | xargs ormolu --mode check
	@hlint .

# ==================================================================================== #
# DEVELOPMENT
# ==================================================================================== #

## format: format code using ormolu
format:
	@find . -name '*.hs' | xargs ormolu -i

# vim: set tabstop=4 shiftwidth=4 noexpandtab

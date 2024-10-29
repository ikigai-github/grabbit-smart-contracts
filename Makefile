usage:
	@echo "usage: make <command> [OPTIONS]"
	@echo
	@echo "Available commands:"
	@echo "  hoogle                -- Start local hoogle"
	@echo "  build                 -- Build all nix packages"
	@echo "  test                  -- Test all nix packages"
	@echo "  ghci_scripts          -- Start a repl for the vulcan-scripts repository"
	@echo "  ghci_contracts        -- Start a repl for the vulcan-contracts repository"
	@echo "  format                -- Run the pre-commit formating hook
	@echo "  ci                    -- Execute CI action"
	@echo "  shell        		   -- Start a Nix shell for .#onchain target"

hoogle: requires_nix_shell
	hoogle server --local --port=8070 > /dev/null &

build: requires_nix_shell
	cabal v2-build $(GHC_FLAGS) all

watch: requires_nix_shell
	while sleep 1; do find src vulcan-smart-contracts.cabal | entr -cd make build; done

test: requires_nix_shell
	cabal v2-test all

vulcan_server: requires_nix_shell
	cabal run $(GHC_FLAGS) vulcan-scripts:vulcan-server 

ghci_scripts: requires_nix_shell
	cabal v2-repl $(GHC_FLAGS) vulcan-scripts:vulcan-scripts

ghci_script_tests: requires_nix_shell
	cabal v2-repl $(GHC_FLAGS) vulcan-scripts:vulcan-smart-scripts-test


format: requires_nix_shell
	pre-commit run --all-files

# Execute CI
ci: 
	nix-build ./nix/ci.nix

NIX_SHELL_ONCHAIN = nix develop .\#onchain

shell:
	$(NIX_SHELL_ONCHAIN)

# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'nix develop' first" && false)



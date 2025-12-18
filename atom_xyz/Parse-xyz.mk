# Builds ALL xyz files in the data subdirectory into f90 modules
# Only need to run this script if sample set has change;
# The files that are produced are HUGE so use sparingly. 
# All files in the xyz folder are automatically deleted after the library is created to save space.

# Ocaml compiler / build tools
OCAMLFIND 	= ocamlfind
OC 			= $(OCAMLFIND) ocamlopt
OPAM 		= opam
REQUIRED 	= str

# check ocaml dependencies - install if needed
.PHONY: all 

all:
	@$(OC) -package str -linkpkg xyz_parse/xyz_parser.ml -o xyz_parser
	@./xyz_parser data/*.xyz
	@rm -rf xyz_parser xyz_parse/*.cmi xyz_parse/*.cmx xyz_parse/*.o
	@mkdir -p src/xyz_tmp
	@mv xyz_*.f90 src/xyz_tmp

## OCAML DEPENDENCY CHECKING ##

# check ocamlfind
check-ocaml:
	@which $(OCAMLFIND) > /dev/null 2>&1 || \
    (echo "ERROR: ocamlfind not found. Please install OCaml and opam first." && \
	echo "Visit: https://ocaml.org/docs/install.html" && \
	exit 1)

# Check if all required packages are installed
check-deps: check-ocaml
	@missing="" ; \
	for pkg in $(REQUIRED); do \
		if ! $(OCAMLFIND) query $$pkg > /dev/null 2>&1; then \
			missing="$$missing $$pkg" ; \
		fi ; \
	done ; \
	if [ -n "$$missing" ]; then \
		echo "Missing OCaml packages:$$missing" ; \
		echo "" ; \
		echo "Would you like to install them now? [y/N]" ; \
		read -r response ; \
		if [ "$$response" = "y" ] || [ "$$response" = "Y" ]; then \
			$(MAKE) install-deps PACKAGES="$$missing" ; \
		else \
			echo "Please install manually with: opam install$$missing" ; \
			exit 1 ; \
		fi ; \
	fi

# Install missing packages via opam
install-deps:
	@if [ -z "$(PACKAGES)" ]; then \
		echo "No packages specified to install." ; \
	else \
		echo "Installing OCaml packages:$(PACKAGES)" ; \
		$(OPAM) install -y $(PACKAGES) ; \
	fi

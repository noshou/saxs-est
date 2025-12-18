# compiler flags
FC     		= gfortran
CFLAGS 		= -O3 -march=native -mcmodel=large -fmax-array-constructor=500000 -pedantic -Wno-tabs

# build directory
BLD_DIR     := _build
MOD_DIR     := $(BLD_DIR)/mod
LIB_DIR     := $(BLD_DIR)/lib

# atom module files
ATM_SRC 	= src/atom.f90
ATM_OBJ 	= $(BLD_DIR)/atom.o
ATM_MOD 	= $(BLD_DIR)/atom.mod

# parsed xyz files
XYZ_SRC 	:= $(wildcard src/xyz_tmp/*.f90)
XYZ_OBJ := $(patsubst src/xyz_tmp/%.f90,$(BLD_DIR)/%.o,$(XYZ_SRC))
XYZ_MOD 	:= $(XYZ_SRC:.f90=.mod)

# library name
LIB_NME 	= $(LIB_DIR)/atom_xyz.a

# path of module files for form_fact
FFC_MOD 	:= $(abspath ../form_fact/_build/mod)

.PHONY: all build-dirs parse-xyz tabulate-xyz clean_up clean

all: clean check-ffc build-dirs parse-xyz tabulate-xyz $(LIB_NME) clean_up

# checks if form_fact is compiled/not corrupted; if not compiles it
check-ffc:
	@if [ ! -d $(abspath ../form_fact/_build) ]; then \
		$(MAKE) --no-print-directory -f $(abspath ../form_fact/Make-FormFact.mk); \
	elif [ ! -d $(abspath ../form_fact/_build/mod) ]; then \
		rm -rf $(abspath ../form_fact/_build); \
		$(MAKE) --no-print-directory -f $(abspath ../form_fact/Make-FormFact.mk); \
	fi
# make build dir
build-dirs:
	@rm -rf _build
	@mkdir $(BLD_DIR)
	@mkdir $(MOD_DIR)
	@mkdir $(LIB_DIR)

# parses all xyz files into .f90 files
parse-xyz:
	@rm -rf src/xyz_tmp
	@$(MAKE) --no-print-directory -f Parse-xyz.mk

# tabulates xyz modules
tabulate-xyz:
	@rm -f _build/xyz_modules.txt
	@for f in data/*.xyz; do \
		echo "xyz_$$(basename "$$f" .xyz)_mod.mod"; \
	done > _build/xyz_modules.txt

# compile files
$(ATM_OBJ): $(ATM_SRC) | $(BLD_DIR) $(MOD_DIR) $(FFC_MOD)
	@$(FC) $(CFLAGS) -I$(FFC_MOD) -J$(MOD_DIR) -c $< -o $@
$(BLD_DIR)/%.o: src/xyz_tmp/%.f90 | $(BLD_DIR) $(MOD_DIR)
	@$(FC) $(CFLAGS) -I$(MOD_DIR) -J$(MOD_DIR) -c $< -o $@

# create static library
$(LIB_NME): $(ATM_OBJ) $(XYZ_OBJ) | $(LIB_DIR)
	@ar rcs $@ $^

# remove stray obj files
clean_up:
	@rm -rf $(BLD_DIR)/*.o
	@rm -rf *.o
# clean
clean:
	@rm -rf $(BLD_DIR)
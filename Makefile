##################################################
# R C/CUDA extension Makefile 0.1.0              #
# -------------------------------                #
# Author: Mohamed Ishmael Diwan Belghazi (2015)  #
# Email:  ishmael.belghazi@gmail.com             #
# ################################################
#                                                #
# Usage:                                         #
# ------                                         #
#                                                #
# make all: to compile and link all C files      #
# defined in SOURCES to shared libs              #
# make clean: to clean all intermediate files    #
# make diagnostic: to print diagnostic screen    #
# make SOURCES=foo.c bar.c: to pass              #
# file names for compilation and linking         #
##################################################

## * Getting R configuration Variables
##,---------------
##| R build helper
##`---------------
RC=R --vanilla CMD SHLIB
R_COMMAND=R --no-save --slave
##,-----------------------------------
##| Defining R config Fetcher function
##`-----------------------------------
R_CONF=R --vanilla CMD config
R_getconf=$(shell $(R_CONF) $(1))
##,----------------------------
##| Getting pre-processor flags
##`----------------------------
R_INCLUDE_FLAG=$(call R_getconf, --cppflags)
##,---------------------
##| Getting Linker Flags
##`---------------------
R_LDFLAGS=$(call R_getconf, --ldflags)
##,-------------------------
##| Getting C compiler flags
##`-------------------------
CC=$(call R_getconf, CC)
CFLAGS=$(call R_getconf, CFLAGS)
CPICFLAGS=$(call R_getconf, CPICFLAGS)
C_SYS_INCLUDE_PATH?=/usr/local/include

##,-----------------------
##| Getting C linker flags
##`-----------------------
DYLIB_EXT=$(call R_getconf, DYLIB_EXT)
DYLIB_LD=$(call R_getconf, DYLIB_LD)
LDFLAGS=$(call R_getconf, LDFLAGS)
##,-------------------------------
##| Getting C shared objects flags
##`-------------------------------
SHLIB_EXT=$(call R_getconf, SHLIB_EXT)
SHLIB_LD=$(call R_getconf, SHLIB_LD)
SHLIB_LDFLAGS=$(call R_getconf, SHLIB_LDFLAGS)
SHLIB_CFLAGS=$(call R_getconf, SHLIB_CFLAGS)
##,-------------------
##| Getting BLAS flags
##`-------------------
BLAS_LIBS=$(call R_getconf, BLAS_LIBS)

# * Getting CUDA configuration variables

#########################
## miscellaneous flags ##
#########################

## * Defining Sources, Dependencies, and Targets
## ?= Defines if not already defined in the environement.
SOURCES?=
OBJECTS=$(SOURCES:.c=.o)
#OBJECTS=get_cond_prob.o logreg.o
TARGETS=$(SOURCES:.c=$(SHLIB_EXT))
TARGETS=
## * Defining Makefile
## ** Build processes
## Defining Build
all: build
build: $(TARGETS)
# @ is all files representing the targets.
# < is all files representing the dependencies:
$(TARGETS): $(OBJECTS)
	@printf "%s\n" "Linking ..."
	$(SHLIB_LD) $(SHLIB_LDFLAGS) $(LDFLAGS) $^ -o $@ $(R_LDFLAGS) $(BLAS_LIBS)
	@printf "%s\n" "... Linking completed."
#$(OBJECTS): $(SOURCES)
%.o: %.c
	@printf "%s\n" "Compiling ..."
	$(CC) $(R_INCLUDE_FLAG) -DNDEBUG -I$(C_SYS_INCLUDE_PATH) $(CPICFLAGS) $(CFLAGS) -c $< -o $@
	@printf "%s\n" "... Compilation completed."
## ** Environement variables diagnostic
## @ to avoid command echo. This a make feature not a shell one.
diagnostic:
	@printf '%s\n' "#####################################"
	@printf '%s\n' "## R config environement variables ##"
	@printf '%s\n' "#####################################"
	@printf '%s\n' "R configuration exec:"
	@printf '%s\n' "$(R_CONF)"
	@printf '%s\n' "R shared lib helper script:"
	@printf '%s\n' "$(RC)"
	@printf '%s\n' "R C shared lib compilation preprocessor flags:"
	@printf '%s\n' "$(R_INCLUDE_FLAG)"
	@printf '%s\n' "R C shared lib linking flags:"
	@printf '%s\n' "$(R_LDFLAGS)"
	@printf '%s\n' "#################################################"
	@printf '%s\n' "## C compilation config environement variables ##"
	@printf '%s\n' "#################################################"
	@printf '%s\n' "C compiler:"
	@printf '%s\n' "$(CC)"
	@printf '%s\n' "C compiler additional flags:"
	@printf '%s\n' "$(CFLAGS)"
	@printf '%s\n' "C compiler shared libs compilation flags:"
	@printf '%s\n' "$(CPICFLAGS)"
	@printf '%s\n' "C system include path:"
	@printf '%s\n' "$(C_SYS_INCLUDE_PATH)"
	@printf '%s\n' "###############################################################"
	@printf '%s\n' "## C dynamically loaded modules config environement variables #"
	@printf '%s\n' "###############################################################"
	@printf '%s\n' "Dynamically loaded modules system extension:"
	@printf '%s\n' "$(DYLIB_EXT)"
	@printf '%s\n' "Dynamically loaded modules library path:"
	@printf '%s\n' "$(DYLIB_LD)"
	@printf '%s\n' "Dynamically loaded modules additional flags:"
	@printf '%s\n' "$(LDFLAGS)"
	@printf '%s\n' "######################################################"
	@printf '%s\n' "## C shared libraries config environement variables ##"
	@printf '%s\n' "######################################################"
	@printf '%s\n' "Shared libraries system extension:"
	@printf '%s\n' "$(SHLIB_EXT)"
	@printf '%s\n' "Shared libraries library include directory:"
	@printf '%s\n' "$(SHLIB_LD)"
	@printf '%s\n' "Shared libraries library additional flags:"
	@printf '%s\n' "$(SHLIB_LDFLAGS)"
	@printf '%s\n' "Shared libraries C flags"
	@printf '%s\n' "$(SHLIB_CFLAGS)"
	@printf '%s\n' "###############################################"
	@printf '%s\n' "## BLAS/LAPACK config environement variables ##"
	@printf '%s\n' "###############################################"
	@printf '%s\n' "BLAS library configuration:"
	@printf '%s\n' "$(BLAS_LIBS)"
## ** Cleaning
clean:
	rm -f src/*.o
veryclean: clean
	rm -f src/*$(SHLIB_EXT)
## ** Building
build:
	@echo "devtools::build()" | $(R_COMMAND)
rebuild: veryclean build
reload:
	@echo "devtools::reload()" | $(R_COMMAND)
## ** Documentation update
document:
	@echo "devtools::document()" | $(R_COMMAND)
build/vignette:
	@echo "devtools::build_vignettes()" | $(R_COMMAND)
## ** Tests
test/custom:
	@printf "%s\n" "Running test..."
	@chmod +x ./test_custom
	@./test_custom
	@printf "%s\n" "... Test finished."
test/fast:
	@echo "devtools::test()" | $(R_COMMAND)
test: veryclean reload document test/fast
test/all: test test/custom
## ** Checks
check/docs: document
	@echo "devtools::check_doc()" | $(R_COMMAND)
check/examples: document
	@echo "devtools::run_examples()" | $(R_COMMAND)
check:
	@echo "devtools::check(check_dir='./checks/')" | $(R_COMMAND)
## ** Debug
valgrind:
	@R -d valgrind --vanilla < test_custom
cudamemcheck:
	@cuda-memcheck ./test_custom
## ** Code Coverage
coverage:
	@echo "library(covr);cov <- package_coverage();shine(cov)" | $(R_COMMAND)
## ** Lint
## *** R lint
lint/R:
	@echo "devtools::lint()"
## ** Benchmarks
bench:
	@./inst/benchmarks/run_bench
## ** Misc config
.PHONY: all clean rebuild test test/all diagnostic

module f0_mod
	use iso_fortran_env, only: int32, int64
	implicit none

	! Define hashtable for (Q,F0) pairs
	#define FFH_KEY_TYPE character(len=100)
	#define FFH_VAL_TYPE integer

	#include "ffhash_inc.f90"

	private

	! create hashtable with 
	type(ffh_t) :: f0_tbl 
	real(real64), allocatable :: f0_vals(:,:) 
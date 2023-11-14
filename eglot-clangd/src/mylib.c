#include <stdio.h>
#include <gcrypt.h>
#include "myproj.h"
#include <config.h>

void show_mpi (MPI a)
{
	gcry_error_t err = GPG_ERR_NO_ERROR;
	gcry_sexp_t data;
	char *buf;
	size_t size;
	err = gcry_sexp_build(&data, NULL, "%m", a);
	if (err)
		fprintf(stderr, "Error in %s.", __func__);
	size = gcry_sexp_sprint (data, GCRYSEXP_FMT_ADVANCED, NULL, 0);
	buf = (char *)malloc (size);
	gcry_sexp_sprint (data, GCRYSEXP_FMT_ADVANCED, buf, size);
	fflush(stdout);
	fprintf (stderr, "%s", buf);
	free (buf);
	gcry_sexp_release(data);
}

int my_lib_function() {
	MPI GPQHE_TWO;
	GPQHE_TWO = mpi_new(0);
	mpi_set_ui(GPQHE_TWO, 2);
	show_mpi(GPQHE_TWO);
	mpi_release(GPQHE_TWO);
	printf("Hello in mylib, " PACKAGE_STRING ".\n");
	return 0;
}

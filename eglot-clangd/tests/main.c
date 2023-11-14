#include <stdio.h>
#include "myproj.h"
#include <config.h>

#ifdef IMPORTANT_MACRO
int my_fcn() {
	printf("If def important macro, print me\n");
	return 0;
}
#endif

int main() {
	printf("Hello OChicken\n");
	printf("This is " PACKAGE_STRING ".\n");
	my_fcn();
	return my_lib_function();
}

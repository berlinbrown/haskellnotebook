/*
 *  Copyright (C) 2008-2009  Berlin Brown (Botnode.com).  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. The name of the author may not be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
 * --------------------
 * Author: Berlin Brown (contact at berlin.brown at gmail.com)
 * Short Description: Simple interpreter in C based on JScheme
 * (Code library also includes simple linked list implemenation, hash table
 *   implemenation)
 * Date: 12/27/2008
 * Compiled with: 
 * gcc (Ubuntu 4.3.2-1ubuntu11) 4.3.2
 * Tested On:
 * Linux houston 2.6.27-9-generic #1
 *  SMP Thu Nov 20 21:57:00 UTC 2008 i686 GNU/Linux
 * To Compile: type 'make' at the command line prompt
 * To Run:     type 'make run' to run the tests or ./octanec <lisp file>
 * Interesting Code:  
 * See octanec_lib.c:eval
 *     octanec_lib.c:read
 *     octanec_lib.c:next_token	
 *	   octanec_lib.c:load_scheme
 *     octane_obj.h: (object definitions)
 * --------------------
 *
 */
#include <stdio.h>

#include "octane_object.h"
#include "list_lib.h"
#include "octanec_lib.h"
#include "stack_lib.h"

/**
 * Entry point for the octanec interpreter
 */
int main(int argc, char **argv) {
	printf("Usage: octanec <source file>\n");
	if (argc != 2) {
		printf("Invalid arguments\n");
		exit(0);
	}
	install_built_in_funcs();
	load_scheme(argv[1]);
	hashtable_delete();
	return 0;
}
 
// End of File

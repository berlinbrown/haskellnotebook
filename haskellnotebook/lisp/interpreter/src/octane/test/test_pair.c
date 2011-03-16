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
 *
 */
#include <stdio.h>
#include <stdlib.h>

#include "../octane_object.h"
#include "../list_lib.h"
#include "../octanec_lib.h"
#include "../stack_lib.h"

#define MAX_TEST_CASES2        1

static int default_test_print(void *obj_data);
static int test_pair_1(void *obj_data);

static const struct OctObject_struct TEST_CASES [] = {
    { 1001, test_pair_1, default_test_print },
};

static int default_test_print(void *obj_data) {
    return (1);
}

static int test_pair_1(void *obj_data) {
    Buffer *buf = new_buffer();
    struct Object_struct *s  = new_str_object("+");
    struct Object_struct *s2 = new_str_object("1");
    struct Object_struct *s3 = new_str_object("2");

    struct Object_struct *pair1 = new_pair(s, new_pair(s2, new_pair(s3, NULL)));
    struct Object_struct *pair2 = new_pair(s, NULL);

    printf("pair test (1)\n");
    stringify(pair1, buf);
    printf("-->[%s]\n", buffer_to_string(buf));
    /*
    printf("pair test (2)\n");
    stringify_pair(pair2, buf);
    printf("pair test (3)\n");
    stringify_pair(pair1, buf);
    */
}

extern int test_pair_main(void) {
    List *test_list;
    test_list = build_test_cases(TEST_CASES, MAX_TEST_CASES2);
    execute_test_cases(test_list);
	return 0;
}

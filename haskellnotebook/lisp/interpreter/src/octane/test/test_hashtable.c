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
#include "../hashtable.h"

#define MAX_TEST_CASES3        1

static int default_test_print(void *obj_data);
static int test_hash_1(void *obj_data);

static const struct OctObject_struct TEST_CASES [] = {
    { 1001, test_hash_1, default_test_print },
};

static int default_test_print(void *obj_data) {
	printf("\n ** test_hashtable.c **\n");
	printf("************************\n");
    return (1);
}

static int test_hash_1(void *obj_data) {
	unsigned int h1 = 0;
    struct Object_struct *new_obj1;
    struct Object_struct *new_obj2;
    struct Object_struct *obj;
    int res = 0;

	hashtable_init();
	h1 = hash("My dog has chickens");
	printf("test: hash = %d\n", h1);

	h1 = hash("But what about Texas?");
	printf("test: hash = %d\n", h1);
    
    new_obj1 = new_str_object("Yes, I did it");
    new_obj2 = new_str_object("Well, what are you going to do?");
    
    // Build a hashtable
    res = hashtable_put("this_a1", new_obj1);
    printf("test: hashtable put=%d\n", res);
    res = hashtable_put("I_got_b1", new_obj2);
    printf("test: hashtable put=%d\n", res);

    obj = hashtable_get("this_a1");
    printf("test: hashtable get=%x\n", obj);
    printf("test: hashtable get=%s\n", obj->string_data);

    obj = hashtable_get("I_got_b1");
    printf("test: hashtable get=%s\n", obj->string_data);

    obj = hashtable_get("I_got_b1 ___x");
    printf("test: hashtable get=%x\n", obj);
	hashtable_delete();

}

extern int test_hash_main(void) {
    List *test_list;
    test_list = build_test_cases(TEST_CASES, MAX_TEST_CASES3);
    execute_test_cases(test_list);
	return 0;
}

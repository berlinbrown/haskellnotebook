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
 */
#include <stdio.h>
#include <stdlib.h>

#include "../octane_object.h"
#include "../list_lib.h"
#include "../octanec_lib.h"
#include "../stack_lib.h"

#define OBJECT_TYPE_TEST_ID   1001
#define MAX_TEST_CASES        7

static int default_test_print(void *obj_data);

static int test_execute_whitespace(void *obj_data);
static int test_execute_new_object(void *obj_data);
static int test_stack(void *obj_data);
static int test_next_token(void *obj_data);
static int test_buffer_to_string(void *obj_data);
static int test_read(void *obj_data);
static int test_eval(void *obj_data);

static const struct OctObject_struct TEST_CASES [] = {
    { 1001, test_execute_whitespace, default_test_print },  // Test 1 struct definition
    { 1002, test_execute_new_object, default_test_print },   // Test 1 struct definition
    { 1003, test_stack, default_test_print },
    { 1004, test_next_token, default_test_print },
    { 1005, test_buffer_to_string, default_test_print },
	{ 1006, test_read, default_test_print },
	{ 1007, test_eval, default_test_print },
};

/**
 * Test the is_whitespace function
 */
static int test_execute_whitespace(void *obj_data) {
    int i = 0;
    int res = 0;
	double dx;
    char t[5] = { 
        ' ', '\t', '\n', '\r', 'x'
    };
    printf("test(test_execute_whitespace): running\n");
    for (i = 0; i < 5; i++) {
        res = is_whitespace(t[i]);
        printf("is_whitespace() test: t(%d)=%d\n", i, res);
    }

	// Generic test
	dx = strtod("1.3", NULL);
	printf("Double, str to double: %f\n", dx);

    return (1);
}

/**
 * Test object creation
 */
static int test_execute_new_object(void *obj_data) {
    char *new_str;    
    printf("test(test_execute_new_object): running\n");
    struct Object_struct* obj = new_object(OBJ_STRING_TYPE, "(Test Data)\n", 0, 0);
    new_str = get_string(obj);
    printf(new_str);
    return (1);
}

static int test_stack(void *obj_data) {
    List *stack;
	int i = 0;
    struct Object_struct* obj1 = new_str_object("test_1.111111");
    struct Object_struct* obj2 = new_str_object("test_1.222222");
    struct Object_struct* obj2b = new_str_object("test_1.333333");
    struct Object_struct* obj3;
    char *buf;

    printf("test(test_stack): running\n");
	printf("-------------------------\n");
    stack = new_stack(obj1);
    stack_push(stack, obj2);
	//stack_push(stack, obj2b);

	i = sizeList(stack);
	printf("List Size ==>%d\n", i);

    obj3 = stack_pop(stack);
	buf = get_string(obj3);
	printf("Pop call(1): %s size=%d\n", buf, sizeList(stack));

    obj3 = stack_pop(stack);   
    buf = get_string(obj3);
    printf("Pop call(2): %s\n", buf);

	//obj3 = stack_pop(stack);

    return (1);
}

static int test_next_token(void *obj_data) {
    struct Object_struct *object;
    struct Octane_struct *scheme;

    printf("test(test_simple_octane): running\n");
    scheme = new_octane_scheme("test.lisp");    
    object = next_token(scheme);
	printf("test next token: object=%x type=%d %s\n", object, object->obj_type, object->string_data);

}

static int test_read(void *obj_data) {

    struct Object_struct *object;
    struct Octane_struct *scheme;

    printf("test(read): running\n");
    scheme = new_octane_scheme("test.lisp");    
    object = read(scheme);
	if (object != NULL) {
		printf("\ntest read: object=%x type=%d %s\n", object, object->obj_type, object->string_data);
		if (object->obj_type == OBJ_PAIR_TYPE) {
			printf("test read, pair=[%s]", stringify_str(object));
		}
		
	} else {
		printf("\ntest read: object is NULL\n");
	}

	// Test a full load
	//load_scheme("test.lisp");

}

static int test_buffer_to_string(void *obj_data) {
	Buffer *buffer = new_buffer();
	char *str;
	double dx;
	buffer_append(buffer, '1');
	buffer_append(buffer, '.');
	buffer_append(buffer, '0');
	buffer_append(buffer, '2');
	
	str = buffer_to_string(buffer);
	printf("Buffer String Test: %s\n", str);

	// Also, buffer to double
	dx = buffer_to_double(buffer);
	printf("Double String Test: %f\n", dx);

}

static int test_eval(void *obj_data) {

    struct Object_struct *object;
    struct Object_struct *res;
    struct Octane_struct *scheme;
    
    printf("test(eval): running\n");
    scheme = new_octane_scheme("test.lisp");    
    object = read(scheme);
	if (object != NULL) {
		printf("\ntest eval: object=%x type=%d %s\n", object, object->obj_type, object->string_data);
		if (object->obj_type == OBJ_PAIR_TYPE) {
			printf("test eval, pair=[%s]", stringify_str(object));
		} 
        res = eval(object);
		printf("\ntest eval [result after eval]: object=%x type=%d [%f]\n", res, res->obj_type, res->double_data);
	} else {
        printf("\ntest read: object is NULL\n");
	}
}

static int default_test_print(void *obj_data) {
	printf("\n******************************\n");
	printf("* NEW TEST\n");
	printf("******************************\n");
    return (1);
}

/**
 * Main entry point for the octane compiler
 * application test cases
 */
int main(int argc, char **argv) {
    List *test_list;
    install_built_in_funcs();
    test_list = build_test_cases(TEST_CASES, MAX_TEST_CASES);
    execute_test_cases(test_list);    
    test_pair_main();
	test_hash_main();
	return 0;
}

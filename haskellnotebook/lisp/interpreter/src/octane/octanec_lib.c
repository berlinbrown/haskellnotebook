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
#include <string.h>

#include "octanec_lib.h"
#include "octane_object.h"
#include "list_lib.h"
#include "stack_lib.h"
#include "hashtable.h"
#include "function_consts.h"

#define TOK_LEFT_PAREN   '('
#define TOK_RIGHT_PAREN  ')' 
#define TOK_SINGLE_QUOT  '\''
#define TOK_SEMICOLON    ';'
#define TOK_DOUBLE_QUOT  '"' 
#define TOK_COMMA        ','
#define TOK_BACK_QUOT    '`'

/**********************************************************
 * EVAL UTILITIES
 **********************************************************/

/**
 * Define the procedures for the lisp environment.
 */
extern void install_built_in_funcs(void)  {
    const int n = 99999;  

    hashtable_init();
    DEFINE_PROC("cons",     CONS,      2, 2);
    DEFINE_PROC("*",       	TIMES,     0, n);     
    DEFINE_PROC("+",       	PLUS,      0, n);
    DEFINE_PROC("-",       	MINUS,     1, n);
    DEFINE_PROC("/",       	DIVIDE,    1, n);
    DEFINE_PROC("caddr",    THIRD,     1, 1);
    DEFINE_PROC("cadr",     SECOND,    1, 1);	   	     	    
    DEFINE_PROC("car",     	CAR,       1, 1);
    DEFINE_PROC("cdr",     	CDR,       1, 1);   
    DEFINE_PROC("*",       	TIMES,     0, n);     
    DEFINE_PROC("+",       	PLUS,      0, n);
    DEFINE_PROC("-",       	MINUS,     1, n);
    DEFINE_PROC("/",       	DIVIDE,    1, n);
}	


struct Object_struct* eval(struct Object_struct *obj) {
    int obj_typ = obj->obj_type;
    struct Object_struct *fn;
    struct Object_struct *args;
    while(TRUE) {
        if (obj_typ == OBJ_STRING_TYPE) {

            // VARIABLE
            printf("trace: eval - instance of String: data=[%s]\n", obj->string_data);
            // Look up a variable or a procedure (built in function).
            return env_lookup(obj->string_data);
            
        } else if (obj_typ != OBJ_PAIR_TYPE) {
            
            // CONSTANT
            printf("trace: eval - instance of non-pair =>\n");
            return obj;

        } else {
            
            // PROCEDURE_CALL 
            printf("trace: eval[t1] - instance of procedure call");
            fn = first(obj);
            args = rest(obj);
            // The new 'fn' should be a procedure type.
            fn = eval(fn);
            return apply(fn, eval_list(args));
        }
    } // End of While
}

struct Object_struct* eval_list(struct Object_struct *list) {
    int obj_typ;
    struct Object_struct *first_obj;
    struct Object_struct *rest_obj;
    if (list == NULL) {
        return NULL;
    }
    
    obj_typ = list->obj_type;

    if (list == NULL) {
        return NULL;
    } else if (obj_typ != OBJ_PAIR_TYPE) {
        printf("Illegal arg list: ");
        return NULL;
    } else {
        first_obj = eval(first(list));
        rest_obj  = eval_list(rest(list));
        return cons(first_obj, rest_obj);
    }
}

/**********************************************************
 * SCHEME UTILITIES
 **********************************************************/

/**
 * cons(x, y) is the same as new Pair(x, y).
 * 
 * Cons presents and interesting function that is fundamental to lisp.
 * Here are some examples of cons usage (tested in common lisp).
 * <code>
 * (cons 1 2):
 * Pair pair = SchemeUtil.cons("1", "2");
 * assertEquals("" + pair, "(1 . 2)");
 * 
 * (cons 1 nil):
 * Pair pair = SchemeUtil.cons("1", null);
 * assertEquals("" + pair, "(1)");
 * 
 * (cons 1 (cons 2 nil)):
 * 
 * Pair pair = SchemeUtil.cons("1", SchemeUtil.cons("2", null));
 * assertEquals("" + pair, "(1 2)");
 *  
 * </code>
 */
struct Object_struct *cons(struct Object_struct *a,
                           struct Object_struct *b) {
	return new_pair(a, b);
}

/**
 * Creates a one element list.
 */
struct Object_struct *list(struct Object_struct *a) {
    return new_pair(a, NULL);
}

/**
 * Like Common Lisp first; car of a Pair, or null for anything else.
 */
struct Object_struct *first(struct Object_struct *x) {
    struct Pair_struct *pair;
    if ((x == NULL) || (x->obj_type != OBJ_PAIR_TYPE)) {
        return NULL;
    }
    pair = x->pair_data;
    return pair->first;
}

/**
 * Like Common Lisp rest; car of a Pair, or null for anything else.
 * "A cons cell is composed of two pointers; the car operation 
 * extracts the first pointer, and the cdr operation extracts the second."
 * 
 * "Thus, the expression (car (cons x y)) evaluates to x, 
 * and (cdr (cons x y)) evaluates to y."
 */
struct Object_struct *rest(struct Object_struct *x) {
    struct Pair_struct *pair;
    if ((x == NULL) || (x->obj_type != OBJ_PAIR_TYPE)) {
        return NULL;
    }
    pair = x->pair_data;
    return pair->rest;
}

struct Object_struct *third(struct Object_struct *x) {
    return first(rest(rest(x)));
}

/**
 * Like Common Lisp second.
 */
struct Object_struct *second(struct Object_struct *x) {
    return first(rest(x));
}	

/**
 * The length of a list, or zero for a non-list.
 * The input paramter must be of the pair type. 
 */
int length(struct Object_struct* x) {

    int obj_typ;
    struct Object_struct *obj = x;
    struct Pair_struct *pair;
    int len = 0;

    obj_typ = x->obj_type;
    pair = x->pair_data;
    while (obj_typ == OBJ_PAIR_TYPE) {
        len++;
        obj = pair->rest;
        if (obj != NULL) {
            pair = obj->pair_data;
            obj_typ = obj->obj_type;
        } else {
            obj_typ = -1;
        }
    }
    return len;
}

/**
 * Convert a Scheme object to its printed representation, as a java String
 * (not a Scheme string).
 */
void stringify(struct Object_struct *x, Buffer *buf) { 
    int    typ;
    double d;
    char   str_buf[500];
    char   *str;
    
    if (x == NULL) {
        buffer_append_str(buf, "()");
        return;
    }    
    typ = x->obj_type;
    if (typ == OBJ_DOUBLE_TYPE) {
        d = x->double_data;
        sprintf(str_buf, "%f", d);
        buffer_append_str(buf, str_buf);
    } else if (typ == OBJ_CHAR_TYPE) {
        buffer_append(buf, x->char_data);
    } else if (typ == OBJ_PAIR_TYPE) {
        stringify_pair(x, buf);
    } else if (typ == OBJ_STRING_TYPE) {
        str = x->string_data;
        buffer_append_str(buf, str);
    }
} // End of Method

char *stringify_str(struct Object_struct *object) {
	char *str;
	Buffer *buffer = new_buffer();
	stringify(object, buffer);
	str = buffer_to_string(buffer);
	return str;
}

/**********************************************************
 * SCHEME OBJECT UTILITIES
 **********************************************************/
struct Octane_struct *new_octane_scheme(const char *src_filename) {
    struct Octane_struct *new_scheme = (struct Octane_struct *) malloc(sizeof(struct Octane_struct));
    FILE *fp;
    
    new_scheme->token_stack = new_empty_stack();
    new_scheme->char_stack = new_empty_stack();

    fp = fopen(src_filename, "r");
    if (fp == NULL) {
        printf("ERR: Cannot open source file (%s).\n", src_filename);
        exit(1);
    }
    new_scheme->file_buffer = fp;
    return new_scheme;
}

/**********************************************************
 * PROCEDURE, ENVIRONMENT AND BUILTIN FUNCTIONS UTILITIES
 **********************************************************/

struct Object_struct *new_procedure(const char *name, int id, int minArgs, int maxArgs) {
    struct Object_struct *new_obj = (struct Object_struct *)    malloc(sizeof(struct Object_struct));    
    struct Procedure_struct *proc = (struct Procedure_struct *) malloc(sizeof(struct Procedure_struct));

    proc->id = id;
    proc->minArgs = minArgs;
    proc->maxArgs = maxArgs;

    new_obj->obj_type = OBJ_PROC_TYPE;
    new_obj->proc_data = proc;
    return new_obj;
}

/** 
 * Environments allow you to look up the value of a variable, given
 * its name.  Keep a list of variables and values, and a pointer to
 * the parent environment.
 * 
 * This code has changed significantly from Norvig's original code. Instead
 * of using a linked-list/cons-pair structure, we use a hashtable to look up
 * the variable definitions. 
 */
extern struct Object_struct* env_lookup(const char* symbol) {
     struct Object_struct* obj = hashtable_get(symbol);
     if (obj == NULL) {
         printf("Unbound variable: [%s]", symbol);
         return NULL;
     } else {
         return obj;
     }
}

extern struct Object_struct* env_define(char* symbol,  struct Object_struct* val) {
    int res = hashtable_put(symbol, val);
    int obj_type = val->obj_type;
    struct Procedure_struct *proc;
    
    if (res == FALSE) {
         printf("Unable to define variable: [%s]", symbol);
    }
    if (obj_type == OBJ_PROC_TYPE) {
        // Set the name if a procedure type
        proc = val->proc_data;
        proc->name = symbol;
    }
    return val;

}

void error(char *msg) {
    printf("ERR, exiting: %s\n", msg);
    exit(1);
}

/**
 * Apply a primitive function to a list of arguments.
 */
struct Object_struct* apply(struct Object_struct* proc, struct Object_struct* args) {

    // First make sure there are the right number of arguments. 
    int nArgs = 0;
    struct Object_struct *x;
    struct Object_struct *y;
    struct Procedure_struct *proc_obj;
    int maxArgs;
    int minArgs;
    int idNumber;
    double num;
    
    nArgs = length(args);
    proc_obj = proc->proc_data;
    maxArgs  = proc_obj->maxArgs;
    minArgs  = proc_obj->minArgs; 
    idNumber = proc_obj->id;
    
    if (nArgs < minArgs) {
        error("too few args");
        return NULL;
    } else if (nArgs > maxArgs) {
        error("too many args, ");
        return NULL;
    } // End of the If
    
    x = first(args);
    y = second(args);
    
    num = get_double(x);
    printf("at apply id= %d %f x==>%x %d\n", idNumber, num, x, x->obj_type);
    switch (idNumber) {
    case PLUS:
        printf("at plus %d\n", idNumber);
        return new_double_object(num_compute(args, '+', 0.0));
    case MINUS:
        return new_double_object(num_compute(rest(args), '-', num));
    case TIMES:
        return new_double_object(num_compute(args, '*', 1.0));
    case DIVIDE:
        return new_double_object(num_compute(rest(args), '/', num));			
    case THIRD:
        return third(x);
    case CONS:
        return cons(x, y);
    case CAR:
        return first(x);
    case CDR:
        return rest(x);
    default:
        error("internal error: unknown primitive: ");
        return NULL;
    }
} // End of the Apply

	

double num_compute(struct Object_struct *args_parm, char op, double result) {		

    int obj_type = 0;
    struct Object_struct *first_obj;
    struct Object_struct *args = args_parm;
    double x;
    double new_res=result;

    if (args == NULL) {
        switch (op) {
        case '-':
            return 0.0 - new_res;
        case '/':
            return 1.0 / new_res;
        default:
            return new_res;
        }
    } else {
        obj_type = args->obj_type;
        // Perform the operation against the linked list
        while (obj_type == OBJ_PAIR_TYPE) {
            first_obj = first(args); 
            args = rest(args);
            if (args != NULL) {
                obj_type = args->obj_type;
            } else {
                obj_type = -1;
            }
            double x = first_obj->double_data;

            // Traverse
            switch (op) {
            case '+':
                new_res += x;
                break;
            case '-':
                new_res -= x;
                break;
            case '*':
                new_res *= x;
                break;
            case '/':
                new_res /= x;
                break;
            default:
                error("Internal Error: unrecognized op: ");
                break;
            }

        } // End of while

        return new_res;
    } // End of if else

    return new_res;
} // End of num compute

/**********************************************************
 * OBJECT UTILITIES
 **********************************************************/
struct Object_struct *new_object(int obj_type, char *string_data, double double_data, int boolean_data) {

    struct Object_struct *new_obj = (struct Object_struct *) malloc(sizeof(struct Object_struct));    
    new_obj->obj_type = obj_type;
    if (new_obj->obj_type == OBJ_STRING_TYPE) {
        new_obj->string_data = string_data;
    } else if (new_obj->obj_type == OBJ_DOUBLE_TYPE) {
        new_obj->double_data = double_data;
    } else if (new_obj->obj_type == OBJ_BOOLEAN_TYPE) {
        new_obj->boolean_data = boolean_data;
    } // End of if-else
    return new_obj;
}

struct Object_struct *new_empty_object(int obj_type) {
    struct Object_struct *new_obj = (struct Object_struct *) malloc(sizeof(struct Object_struct));
    new_obj->obj_type = obj_type;
    return new_obj;
}

/***********************************************************
 * PAIR UTILITIES
 ***********************************************************/

struct Object_struct *new_pair(struct Object_struct *first,
                               struct Object_struct *rest) {
    
    // Create an instance of pair
    // Create the associated pair/object
    // and then set first and rest references.
    struct Pair_struct *pair = (struct Pair_struct *) malloc(sizeof(struct Pair_struct));
    struct Object_struct *new_pair_obj = new_empty_object(OBJ_PAIR_TYPE);    
    pair->first = first;
    pair->rest = rest;
    new_pair_obj->pair_data = pair;
    return new_pair_obj;
}
struct Object_struct *get_first(struct Object_struct *pair_obj) {
    struct Pair_struct *pair = pair_obj->pair_data;
    return pair->first;
}
struct Object_struct *get_rest(struct Object_struct *pair_obj) {
    struct Pair_struct *pair = pair_obj->pair_data;
    return pair->rest;
}

void stringify_pair(struct Object_struct *pair_obj, Buffer *buffer) {

    struct Pair_struct   *pair;
    struct Object_struct *tail;
    struct Pair_struct   *tail_pair_tmp;
    
    if (pair_obj == NULL) {
        return;
    }
    pair = pair_obj->pair_data;
    if (pair == NULL) {
        return;
    }    
    buffer_append(buffer, '(');
    stringify(pair->first, buffer);

    tail = pair->rest;
    while ((tail != NULL) && (tail->obj_type == OBJ_PAIR_TYPE)) {
        // Get the pair data from tail
        tail_pair_tmp = tail->pair_data;
        buffer_append(buffer, ' ');
        stringify(tail_pair_tmp->first, buffer);
        // Linked list operation.
        tail = tail_pair_tmp->rest;
    }
    if (tail != NULL) {
        buffer_append_str(buffer, " . ");
        stringify(tail, buffer);
    }
    buffer_append(buffer, ')');
}

/***********************************************************
 * END PAIR UTILITIES
 ***********************************************************/

struct Object_struct *new_char_object(const char char_data) {
    struct Object_struct *new_obj = new_empty_object(OBJ_CHAR_TYPE);    
    new_obj->char_data = char_data;
    return new_obj;
}
struct Object_struct *new_double_object(const double double_data) {
    struct Object_struct *new_obj = new_empty_object(OBJ_DOUBLE_TYPE);
    new_obj->double_data = double_data;
    return new_obj;
}

struct Object_struct *new_str_object(char *string_data) {
    struct Object_struct* obj = new_object(OBJ_STRING_TYPE, string_data, 0, 0);
    return obj;
}

char *get_string(struct Object_struct *obj) {
    char *str = NULL;
    if (obj->obj_type == OBJ_STRING_TYPE) {
        str = obj->string_data;
    }        
    return str;
}

int get_boolean(struct Object_struct *obj) {
    int bool_val = FALSE;
    if (obj->obj_type == OBJ_BOOLEAN_TYPE) {
        bool_val = obj->boolean_data;
    } // End of if-else
    return bool_val;

}
double get_double(struct Object_struct *obj) {
    double double_val;
    if (obj->obj_type == OBJ_DOUBLE_TYPE) {
        double_val = obj->double_data;
    }
    return double_val;
}

/**********************************************************
 * LISP UTILITIES
 **********************************************************/

/**********************************************************
 * INPUT READER UTILITIES
 **********************************************************/

extern char* str_duplicate(char *o) {

    int l    = strlen(o) + 1;
    char *ns = (char*) malloc(l * sizeof(char));

    strcpy(ns, o);
    if (ns == NULL) {
        return NULL;
    } else {
        return ns;
    }
}

/**
 * Read and return a Scheme expression, or EOF.
 */
struct Object_struct *read(struct Octane_struct *octane) {
    struct Object_struct *token;
    char *str;
	printf("trace: read()\n");

    token = next_token(octane);
    if (token == NULL) {
        return NULL;
    }
	if (token->obj_type == OBJ_STRING_TYPE) {
		str = token->string_data;
		if (strcmp("(", str) == 0) {
			return read_tail(octane);
		} else if (strcmp(")", str) == 0) {
			printf("WARN: Extra ')' ignored.");
			return read(octane);
		} else {
			return token;
		} // End of the if - else
	} else {
		return token;		
	} // End of the if - else
}

struct Object_struct *read_tail(struct Octane_struct *octane) {

	struct Object_struct *token         = next_token(octane);
	struct Object_struct *tok_read_tail = NULL;
	struct Object_struct *tok_read      = NULL;
    char *token_str      = token->string_data;
    Stack *token_stack   = octane->token_stack;
	struct Object_struct *result;
	char *err_msg = "";
    
    if (token == NULL) {
        return NULL;
    }
    
    if (token->obj_type == OBJ_STRING_TYPE) {
        if (strcmp(token_str, OCT_EOF) == 0) {
            err_msg = "ERROR: readTail() - EOF during read.";
            printf(err_msg);
            exit(1);
        } else if (strcmp(token_str, ")") == 0) {
            return NULL;
        }
    } // End of the if - else

    stack_push(token_stack, token);
    // return SchemeUtil.cons(read(), readTail());
    tok_read      = read(octane);
    tok_read_tail = read_tail(octane);		
    return cons(tok_read, tok_read_tail);
}

/**
 * Determines if the specified character is white space according to Java.
 */
int is_whitespace(const char c) {
    int ci = c;
    /*
     * Horizontal Tab, Newline, Vertical Tab, New Page
     * Carriage Return, File separator
     * Group separator, Record separator, Unit separator, Space
     */
    if ((ci == 9) || (ci == 10) || (ci == 11)
        || (ci == 12) || (ci == 13)
        || (ci == 28) || (ci == 29)
        || (ci == 30) || (ci == 31)
        || (ci == 32)) {
        return TRUE;
    } else {
        return FALSE;
    }
}

char input_reader_read(struct Octane_struct *octane) {
	char ch;
	// inputReader.read()
	ch = getc(octane->file_buffer);
    return ch;
}

/**
 * Collect the set of characters from the input stream until whitespace or
 * one of the language tokens is found.
 * 
 */
void build_generic_token(struct Octane_struct *octane, const char o_ch) {
	int ch = (char) o_ch;
	struct Object_struct *char_obj;
	Stack *char_stack  = octane->char_stack;
	do {
		// Build alpha numeric, atom/symbol characters/tokens into the buffer
		buffer_append(octane->buffer, ch);
		ch = (char) input_reader_read(octane);
	} while (!is_whitespace((char) ch)
			 && (ch != -1)
			 && (ch != TOK_LEFT_PAREN)  && (ch != TOK_RIGHT_PAREN) 
			 && (ch != TOK_SINGLE_QUOT) && (ch != TOK_SEMICOLON)
			 && (ch != TOK_DOUBLE_QUOT) && (ch != TOK_COMMA) 
			 && (ch != TOK_BACK_QUOT)); // End of do - while

	// Push a language token onto the character stack
	char_obj = new_char_object(ch);
	stack_push(char_stack, char_obj);
}

struct Object_struct *next_token(struct Octane_struct *octane) {

    struct Object_struct *object;
    Stack *token_stack   = octane->token_stack;
    Stack *char_stack    = octane->char_stack;
	char ch;
	int tmp_ci;


	// See if we should re-use a pushed char or token
	// Task 1: Pop the token and character stacks
	if ((stack_is_empty(token_stack) == FALSE) 
        && (stack_peek(token_stack) != NULL)) {
        return stack_pop(token_stack);
	} else if ((stack_is_empty(char_stack) == FALSE) 
               && (stack_peek(char_stack) != NULL)) {
		
        object = stack_pop(char_stack);
        ch = object->char_data;
	} else {
        // inputReader.read()
        ch = input_reader_read(octane);
	}

	while (is_whitespace(ch)) {
		ch = input_reader_read(octane);
	}

	// See what kind of non-white character we got
	// Task 3: Check if the character is of various token types.
	switch (ch) {
	case -1:
		return new_str_object(OCT_EOF);
	case TOK_LEFT_PAREN:
		return new_str_object("(");
	case TOK_RIGHT_PAREN:
		return new_str_object(")");
	case TOK_SINGLE_QUOT:
		return new_str_object("'");
	case TOK_BACK_QUOT:
		return new_str_object("`");
	case TOK_SEMICOLON:
		// Comment: skip to end of line and then read next token
		while ((ch != -1) && (ch != '\n') && (ch != '\r')) {
			ch = input_reader_read(octane);
		}
		return next_token(octane);
	default:
		reset_buffer(octane);
		tmp_ci = (int) ch;
		build_generic_token(octane, ch);
		// Try potential numbers, but catch any format errors.
		if (tmp_ci == '.' || tmp_ci == '-' || (tmp_ci >= '0' && tmp_ci <= '9')) {
            // Number type is currently in the buffer queue			
			return new_double_object(buffer_to_double(octane->buffer));
		} // End of If
		return new_str_object(lowercase_str(buffer_to_string(octane->buffer)));
	} // End of the Switch

	// Code is unreachable.
    return NULL;
}

extern void load_scheme(char *filename) {
	
	struct Object_struct *object;
    struct Octane_struct *scheme;
	struct Object_struct *res;
	
    scheme = new_octane_scheme(filename);
	for (;;) {
		object = read(scheme);
		if (feof(scheme->file_buffer)) {
			return;
		}
		if (object != NULL) {
			if (object->obj_type == OBJ_STRING_TYPE) {
				if (strcmp(object->string_data, OCT_EOF)) {
					return;
				}
			}
			res = eval(object);
			printf("\ntest eval [result after eval]: object=%x type=%d [%f]\n", res, res->obj_type, res->double_data);
		} // End of If

	}
}
 
// End of File

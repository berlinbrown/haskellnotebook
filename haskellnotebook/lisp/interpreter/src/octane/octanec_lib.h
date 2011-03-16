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
 */
#ifndef _OCTANE_LIB_H_
#define _OCTANE_LIB_H_

#include "octane_object.h"

#define OCT_EOF   "#!EOF"

#define FALSE     0
#define TRUE      1 

#define DEFINE_PROC(name, id, min, max)   env_define(name, new_procedure(name, id, min, max))

extern char* str_duplicate(char *o);

extern int is_whitespace(const char c);
extern struct Object_struct *next_token(struct Octane_struct *octane);
extern struct Object_struct *read_tail(struct Octane_struct *octane);

extern struct Object_struct *new_object(int obj_type, char *string_data, double double_data, int boolean_data);
extern struct Object_struct *new_pair(struct Object_struct *first,
                                      struct Object_struct *rest);

extern struct Object_struct *new_double_object(const double double_data);

struct Object_struct *cons(struct Object_struct *a,
                           struct Object_struct *b);

extern struct Object_struct *get_rest(struct Object_struct *pair_obj);
extern struct Object_struct *get_first(struct Object_struct *pair_obj);
extern double get_double(struct Object_struct *obj);

extern void stringify_pair(struct Object_struct *pair_obj, Buffer *buffer);
extern char *stringify_str(struct Object_struct *x);
extern char input_reader_read(struct Octane_struct *octane);

extern struct Object_struct* env_lookup(const char* symbol);
extern struct Object_struct* env_define(char* symbol,  struct Object_struct* val);

extern struct Object_struct *first(struct Object_struct *x);
extern struct Object_struct *rest(struct Object_struct *x);

extern struct Object_struct* eval(struct Object_struct *obj);
extern struct Object_struct* eval_list(struct Object_struct *list);

extern double num_compute(struct Object_struct *args_parm, char op, double result);
extern struct Object_struct* apply(struct Object_struct* proc, struct Object_struct* args);

#endif

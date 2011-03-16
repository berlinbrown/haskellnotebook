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
#include <stdio.h>
#include <stdlib.h>

#include "list_lib.h"
#include "stack_lib.h"

extern Stack* new_stack(struct Object_struct *obj) {
    Node *cur_node;
    Stack *stack;
    stack = createList();
    stack_push(stack, obj);    
    return stack;
}

Stack* new_empty_stack() {
    Stack *stack;
    stack = createList();
    return stack;
}

int stack_push(Stack *stack, struct Object_struct *obj) {
    Node *cur_node;
    cur_node = listCreateObjectNode(NULL, obj);
    listInsertNodeFront(stack, cur_node);
    return 0;
}

struct Object_struct *stack_pop(const Stack *stack) {
    Node *cur_node;
    struct Object_struct *object;
    cur_node = stack->head;
	if (cur_node == NULL) {
		return NULL;
	}
    object = cur_node->object;
    listSafeRemoveObject(stack, object);
    return object;
}

struct Object_struct *stack_peek(const Stack *stack) {

    Node *cur_node;
    struct Object_struct *object;
	if (stack == NULL) {
		return NULL;
	}

    cur_node = stack->head;
    if (cur_node == NULL) {
        return NULL;
    }
    object = cur_node->object;
    return object;
}

int stack_is_empty(const Stack *stack) {    
    return is_empty(stack);
}

/***********************************************************
 * BUFFER UTILITIES
 ***********************************************************/
extern Buffer* new_buffer() {
	return new_empty_stack();
}

extern void buffer_append(Buffer *buffer, const char ch) {
	struct Object_struct *char_obj;
	char_obj = new_char_object(ch);
	stack_push(buffer, char_obj);
}
extern void buffer_append_str(Buffer *buffer, const char *str) {
    int i = 0;
    if (str != NULL) {
        for (i = 0; i < strlen(str); i++) {
            buffer_append(buffer, str[i]);
        }
    }
}

/**
 * The buffer stack consists of a stack of character objects.
 */
extern char* buffer_to_string(Buffer *buffer) {
	int size = 0;
	int i = 0;
	char c;
	List *list = buffer;
	Node *current_ptr;
	char *new_str_buffer;
	struct Object_struct *object;
	
    if (is_empty(list)) {
        return NULL;
    } 
	
	size = sizeList(list);
	new_str_buffer = (char *) malloc((size+1) * sizeof(char));
	// Count the data first
    current_ptr = list->head; 
    while(current_ptr != NULL) { 
        object = current_ptr->object;
		c = object->char_data;
		new_str_buffer[i] = c;
        current_ptr = current_ptr->next;
		i++;
    } // end of while
    new_str_buffer[i] = '\0';
	return reverse_str(new_str_buffer, size);
}

/**
 * Clear the existing buffer and create another one
 */
extern Stack* reset_buffer(struct Octane_struct *octane) {
	if (octane->buffer != NULL) {
		RELEASE_OBJECT(octane->buffer);
		octane->buffer = new_buffer();
	} else {
		octane->buffer = new_buffer();
	}
	return octane->buffer;
}

/**
 * Convert buffer data to a double
 */
extern double buffer_to_double(Buffer* buffer) {
	char *str;
	double dx;
	str = buffer_to_string(buffer);  
	dx = strtod(str, NULL);
    printf("buffer to double %f\n", dx);
	return dx;
	
}

// End of File

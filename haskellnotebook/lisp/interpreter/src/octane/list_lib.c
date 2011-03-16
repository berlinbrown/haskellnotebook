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

#include "octanec_lib.h"
#include "list_lib.h"

char *reverse_str(char *s, int n) {
	int i=0;
	while (i<n/2) {		
		*(s+n) = *(s+i);
		*(s+i) = *(s + n - i -1);
		*(s+n-i-1) = *(s+n);
		i++;
	}
	*(s+n) = '\0';	
	return s;
}

char* lowercase_str(char *string) {
   int  i = 0;
   while (string[i]) {   
	   string[i] = tolower(string[i]);
	   i++;
   }
   return string;
}

int is_empty(const List *list) {
	if (list == NULL) {
		return TRUE;
	}
    if (list->head == NULL) {
        return TRUE;
    } else {
        return FALSE;
    }
} // end of the function 


Node *listCreateObjectNode(struct OctObject_struct *obj_data, 
                           struct Object_struct *object) {
    Node *h = (Node *) malloc(sizeof(Node));
    h->obj_data = obj_data;
    h->object = object;
    h->next = NULL;
    return h;    
} // end of the function 

Node *listCreateNode(struct OctObject_struct *obj_data) {
    Node *h = (Node *) malloc(sizeof(Node));
    h->obj_data = obj_data;
    h->next = NULL;
    return h;    
} // end of the function 

void listDestroyNode(Node *node) {
    RELEASE_OBJECT(node);
} // end of the function

List *createList() {    
    List *result = (List *) malloc(sizeof(List));
    result->head = NULL;
	return result;

} // end of the function 

void listDeleteNode(List *list, struct OctObject_struct *obj_data) {
    Node *current = list->head;
    Node *previous = NULL; 
    while (current != NULL ) {
        if (current->obj_data != obj_data) {
            previous = current;
            current = previous->next;
        } else {            
            if (previous != NULL) {                
                previous->next = current->next;
            } // end of the if
			RELEASE_OBJECT(current);
            break;
        } // end of the if - else

    } // end of the while 

} // end of the function

void listSafeRemoveObject(List *list, struct Object_struct *object) {
    Node *current = list->head;
    Node *previous = NULL; 
    while (current != NULL ) {
        if (current->object != object) {
            previous = current;
            current = previous->next;
        } else {            
            if (previous != NULL) {                
                previous->next = current->next;
            } // end of the if
			
			// Remove the object from the list but do not delete.
			list->head = current->next;
			
            break;
        } // end of the if - else

    } // end of the while 
	
	// TODO: fix, remove the last element by setting
	// the head to null.
	//if (sizeList(list) == 1) {
	//	list->head = NULL;
	//}

} // end of the function

void destroyList(List *list) {
    Node *pos, *next;
    pos = list->head;
    while(pos != NULL) {
        next = pos->next;
        //free(pos);
		RELEASE_OBJECT(pos);        
        pos = next;
    } // end of the while 
    free(list);
} // end of the function 

void listInsertFront(List *list, struct OctObject_struct *obj_data) {
	Node *new_node = NULL;
	new_node = listCreateNode(obj_data);
	if (is_empty(list)) {
  		list->head = new_node;
	} else { 		
		new_node->next = list->head;
		list->head = new_node;
	} // end if 

} // end of the function 

void listInsertNodeFront(List *list, Node *new_node) {
	if (is_empty(list)) {
  		list->head = new_node;
	} else { 		
		new_node->next = list->head;
		list->head = new_node;
	} // end if 

} // end of the function 


void printList(const List *list) {
    Node *current_ptr;
    if (is_empty(list)) {
        return;
    } 
    current_ptr = list->head; 
    while(current_ptr != NULL) { 
        current_ptr->obj_data->print(NULL);
        current_ptr = current_ptr->next;
    } // end of while
    
} // end of the function 

int sizeList(const List *list) {
	int size = 0;
    Node *current_ptr;
    if (is_empty(list)) {
        return 0;
    } 
    current_ptr = list->head; 
    while(current_ptr != NULL) { 
        current_ptr = current_ptr->next;
		size++;
    } // end of while
    return size;
} // end of the function 

void executeList(const List *list) {
    Node *current_ptr;
    if (is_empty(list)) {
        return;
    } 
    current_ptr = list->head; 
    while(current_ptr != NULL) { 
        current_ptr->obj_data->execute(current_ptr->obj_data);
        current_ptr = current_ptr->next;
    } // end of while
    
} // end of the function 

extern void executePrintList(const List *list) {
    Node *current_ptr;
    if (is_empty(list)) {
        return;
    } 
    current_ptr = list->head;
    while(current_ptr != NULL) { 
		current_ptr->obj_data->print(current_ptr->obj_data);
        current_ptr->obj_data->execute(current_ptr->obj_data);
        current_ptr = current_ptr->next;
    } // end of while
    
} // end of the function 


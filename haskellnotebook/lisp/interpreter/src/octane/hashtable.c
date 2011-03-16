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
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "octane_object.h"
#include "hashtable.h"

static struct HashNode_struct **hash_table;

void hashtable_init(void) {
	int i;
	hash_table = (struct HashNode_struct *) malloc(DEFAULT_HASHSIZE * sizeof(HashNode));
	for (i = 0; i < DEFAULT_HASHSIZE; i++) {
		hash_table[i] = (struct HashNode_struct *) NULL;
	}
}

unsigned int hash(char *str) {

	unsigned int h = 0;
	int i = 0;
	int len = strlen(str);

	for (i = 0; i < len; i++) {
		h = str[i] + h*31;
	}
	return (h % DEFAULT_HASHSIZE);
}

struct HashNode_struct *hashtable_lookup(char *n) {

	unsigned int hi = hash(n);
	struct HashNode_struct *np = hash_table[hi];
	for(; np != NULL; np = np->next) {
		if (!strcmp(np->hash_key, n)) {
			return np;
		}
	}
	return NULL;
}

static struct Object_struct* object_copy(struct Object_struct *o) {
    int l = sizeof(Object);
    struct Object_struct *obj = (struct Object_struct *) malloc(sizeof(Object));   
    memcpy(obj, o, l);
    if (obj == NULL) {
        return NULL;
    } else {
        return obj;
    } // End of the If - Else
}

struct Object_struct* hashtable_get(const char* name) {

	struct HashNode_struct* n = hashtable_lookup(name);	
	if (n == NULL) {
		return NULL;
	} else {
		return n->hash_object;
	}
}

int hashtable_put(char* key, struct Object_struct* val) {
	unsigned int hi;
	struct HashNode_struct* np;
	
	if ((np = hashtable_lookup(key)) == NULL) {
		hi = hash(key);
		np = (struct HashNode_struct *) malloc(sizeof(HashNode));
		if (np == NULL) {
            printf("hash table put, failed at lookup\n");
			return 0;
		}
		
		np->hash_key = object_copy(key);
		if (np->hash_key == NULL) {
            printf("hash table put, failed at duplicate\n");
			return 0;
		}
		
		np->next = hash_table[hi];
		hash_table[hi] = np;

	} else {
		RELEASE_OBJECT(np->hash_object);
	}
	
	np->hash_object = object_copy(val);
	if (np->hash_object == NULL) {
        printf("hash table put, failed at hash value duplicate\n");
		return 0;
	}	
	return 1;
}

void hashtable_delete(void) {

	int i;
	struct HashNode_struct *np, *t;

	for (i = 0; i < DEFAULT_HASHSIZE; i++) {
		
		if (hash_table[i] != NULL) {
			np = hash_table[i];
			while (np != NULL) {
				t = np->next;
				RELEASE_OBJECT(np->hash_key);
				RELEASE_OBJECT(np->hash_object);
				RELEASE_OBJECT(np);
				np = t;
			}
		}

	} // End of the for

	RELEASE_OBJECT(hash_table);

} // End of the Function


// End of File

/* 

Copyright (C) 2000 Matthew Danish

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#include<iostream.h>
#include"orglist.h"
#include"id.h"
#include"org.h"


OrganismListClass::OrganismListClass() {
  cur=start=end=new OrganismListStruct;
  cur->next = cur->prev = NULL;
}

OrganismListClass::~OrganismListClass() {
  while(start->next) {
    cur=start->next->next;
    delete start->next->o;
    delete start->next;
    start->next=cur;
  }
  delete start;
}

bool OrganismListClass::IsEmpty() {
  if(start == end) return true;
  else return false;
}

bool OrganismListClass::Append(OrganismClass *o) {
  while(cur->next) cur=cur->next;
  cur->next = new OrganismListStruct;

  if(!cur->next) return false;

  cur->next->prev = cur;
  cur = cur->next;
  cur->next = NULL;
  end = cur;
  cur->o = o;
  return true;
}

bool OrganismListClass::Prepend(OrganismClass *o) {
  cur=new OrganismListStruct;
  if(!cur) {
    cur=start;
    return false;
  }
  cur->o = o;
  cur->next = start->next;
  cur->prev = start;
  if(cur->next)
    cur->next->prev = cur;
  else
    end = cur;

  start->next = cur;
  return true;
}

OrganismClass *OrganismListClass::First() {
  if(IsEmpty()) return NULL;
  return start->next->o;
}

OrganismClass *OrganismListClass::Last() {
  if(IsEmpty()) return NULL;
  return(end->o);
}

OrganismClass *OrganismListClass::Next() {
  if(IsEmpty()) return NULL;
  if(!cur->next) return NULL;
  cur=cur->next;
  return(cur->o);
}

OrganismClass *OrganismListClass::Prev() {
  if(IsEmpty()) return NULL;
  if(!cur->prev) return NULL;
  if(cur->prev==start) return NULL;
  cur=cur->prev;
  return(cur->o);
}

bool OrganismListClass::Rewind() {
  cur=start;
  return true;
}

OrganismClass *OrganismListClass::N(int n) {
  int i=0;
  OrganismListStruct *tmp=start;

  while(tmp->next) {
    tmp=tmp->next;
    if(n==i++) {
      cur=tmp;
      return cur->o;
    }
  }
  return NULL;
}

OrganismClass *OrganismListClass::Cur() {
  if(!cur) return NULL;
  if(cur==start) return NULL;
  return(cur->o);
}

OrganismClass *OrganismListClass::Id(IdClass &id) {
  if(IsEmpty()) return NULL;
  
  OrganismListStruct *tmp=start;

  while(tmp->next) {
    tmp=tmp->next;
    if(id == tmp->o->Id()) {
      cur=tmp;
      return (cur->o);
    }
  }
  return NULL;
}

bool OrganismListClass::Pop() {
  if(IsEmpty()) return false;
  OrganismListStruct *tmp = start->next;
  start->next=start->next->next;
  if(start->next) {
    start->next->prev=start;
    if(tmp == cur) 
      cur=start->next;
  } else {
    cur = end = start;
  }
  delete tmp;
  return true;
}

bool OrganismListClass::Dequeue() {
  if(IsEmpty()) return false;
  end=end->prev;
  delete end->next;
  end->next=NULL;
  return true;
}

bool OrganismListClass::Remove(int n) {
  int i=0;
  OrganismListStruct *tmp=start;

  while(tmp->next) {
    tmp=tmp->next;
    if(n==i++) {
      if(tmp == end) end = tmp->prev;
      tmp->prev->next=tmp->next;
      if(tmp->next)
	tmp->next->prev=tmp->prev;
      cur=tmp->prev;
      delete tmp;
      return true;
    }
  }
  return false;
}

bool OrganismListClass::Remove(IdClass &id) {
  OrganismListStruct *tmp=start;
  
  while(tmp->next) {
    tmp=tmp->next;
    if(id == tmp->o->Id()) {
      if(tmp == end) end = tmp->prev;
      tmp->prev->next=tmp->next;
      if(tmp->next)
	tmp->next->prev=tmp->prev;
      cur=tmp->prev;
      delete tmp;
      return true;
    }
  }
  return false;
}

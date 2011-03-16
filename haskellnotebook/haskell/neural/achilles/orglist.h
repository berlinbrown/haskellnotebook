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

#ifndef ORGLIST_H_85445
#define ORGLIST_H_85445

class OrganismClass;
class IdClass;

struct OrganismListStruct {
  OrganismClass *o;
  OrganismListStruct *next,*prev;
};


class OrganismListClass {
private:
  OrganismListStruct *cur,*start,*end;
public:
  OrganismListClass();
  ~OrganismListClass();
  bool IsEmpty();
  bool Append(OrganismClass *);
  bool Prepend(OrganismClass *);
  OrganismClass *First();
  OrganismClass *Last();
  OrganismClass *Next();
  OrganismClass *Prev();
  bool Rewind();
  OrganismClass *N(int);
  OrganismClass *Cur();
  OrganismClass *Id(IdClass &);
  bool Pop(); // remove from start
  bool Dequeue(); // remove from end
  bool Remove(int);
  bool Remove(IdClass &);
};

#endif

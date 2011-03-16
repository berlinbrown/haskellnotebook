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

#include"id.h"
#include<stdio.h>

IdClass::IdClass(IdToken &token) :
  timestamp(token.timestamp),
  mod(token.mod)
{}

IdClass::~IdClass() {}

bool IdClass::String(char *s) {
  sprintf(s,"%ld.%d",timestamp,mod);
  return true;
}

bool IdClass::operator ==(IdClass &i) {
  if(i.timestamp == timestamp && i.mod == mod) return true;
  else return false;
}

bool IdClass::operator !=(IdClass &i) {
  return !(*this == i);
}


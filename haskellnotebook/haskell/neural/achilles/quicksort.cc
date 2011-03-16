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
#include<vector>
#include"quicksort.h"



void QuickSortOList(vector<OList> &list) {
  vector<OList> hilist,lowlist;
  OList tmp;
  int partition = 0,i,len = list.size();

  if(len == 2) {
    if(list[0].dist > list[1].dist) {
      tmp = list[0];
      list[0] = list[1];
      list[1] = tmp;
    }
    return;
  }
  if(len < 2) return;

  while(!hilist.size() || !lowlist.size()) {
    hilist.clear(); lowlist.clear();
    for(i=0;i<len;i++) {
      if(list[i].dist < list[partition].dist) 
	lowlist.push_back(list[i]);
      else
	hilist.push_back(list[i]);
    }
    if(++partition >= len) return;
  }
  QuickSortOList(lowlist);
  QuickSortOList(hilist);

  for(i=0;i<(signed int)lowlist.size();i++) {
    list[i]=lowlist[i];
  }
  for(i=0;i<(signed int)hilist.size();i++) {
    list[i+lowlist.size()]=hilist[i];
  }
}  

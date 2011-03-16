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

#include"color.h"

ColorClass::ColorClass() {
  r=g=b=0;
}

ColorClass::ColorClass(double _r,double _g,double _b) {
  R(_r);
  G(_g);
  B(_b);
}

ColorClass::~ColorClass() {}

double ColorClass::R() {
  return r;
}

double ColorClass::G() {
  return g;
}

double ColorClass::B() {
  return b;
}

double ColorClass::R(double _r) {
  if(_r>1) _r=1;
  return (r=_r);
}

double ColorClass::G(double _g) {
  if(_g>1) _g=1;
  return (g=_g);
}

double ColorClass::B(double _b) {
  if(_b>1) _b=1;
  return (b=_b);
}

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

#include"vector.h"
#include<math.h>
#include"defines.h"

VectorClass::VectorClass() :
  x(0),y(0),z(0)
{}

VectorClass::VectorClass(double _x,double _y,double _z) :
  x(_x),y(_y),z(_z)
{}

VectorClass::VectorClass(double mag,double angle) {
  x=mag*sin((MY_PI/180) * angle);
  y=0;
  z=mag*cos((MY_PI/180) * angle);
}

VectorClass::~VectorClass() {}

double VectorClass::X() {
  return x;
}

double VectorClass::Y() {
  return y;
}

double VectorClass::Z() {
  return z;
}

double VectorClass::X(double _x) {
  return (x=_x);
}

double VectorClass::Y(double _y) {
  return (y=_y);
}

double VectorClass::Z(double _z) {
  return (z=_z);
}

bool VectorClass::operator ==(VectorClass &v) {
  if(x == v.x && y == v.y && z == v.z) return true;
  else return false;
}

bool VectorClass::operator !=(VectorClass &v) {
  return !(*this == v);
}

VectorClass operator -(VectorClass &a, VectorClass &b) {
  VectorClass v(a.x - b.x, a.y - b.y, a.z - b.z);
  return v;
}

double VectorClass::Magnitude() {
  return sqrt(x*x+y*y+z*z);
}

VectorClass & VectorClass::operator +=(VectorClass &a) {
  x+=a.X();
  y+=a.Y();
  z+=a.Z();
  return *this;
}

double VectorClass::Dot(VectorClass &a) {
  return (a.X() * X() + a.Y() * Y() + a.Z() * Z());
}

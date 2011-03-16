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

#include"angle.h"
#include"vector.h"
#include<math.h>
#include"defines.h"

AngleClass::AngleClass() :
  angle(0)
{}

AngleClass::AngleClass(double _angle) {
  Angle(_angle);
}

AngleClass::AngleClass(VectorClass &v) {
  Angle((180/MY_PI) * double(atan2(v.X(),v.Z())));
}

AngleClass::~AngleClass() {}

double AngleClass::Angle() {
  return angle;
}

double AngleClass::Angle(double _angle) {
  angle=_angle;
  while(angle>=360) angle-=360;
  while(angle<0) angle+=360;
  return angle;
}

VectorClass AngleClass::Vector() {
  VectorClass v(sin(MY_PI/180*angle),0,cos(MY_PI/180*angle));
  return v;
}

bool AngleClass::operator ==(AngleClass &a) {
  if(angle == a.angle) return true;
  else return false;
}

bool AngleClass::operator !=(AngleClass &a) {
  return !(*this == a);
}

bool AngleClass::operator ==(double a) {
  if(angle == a) return true;
  else return false;
}

bool AngleClass::operator !=(double a) {
  return !(*this == a);
}

AngleClass operator +(AngleClass &a,AngleClass &b) {
  AngleClass c(a.angle + b.angle);
  return c;
}

AngleClass operator +(AngleClass &a,double b) {
  AngleClass c(a.angle + b);
  return c;
}

AngleClass operator -(AngleClass &a,AngleClass &b) {
  AngleClass c(a.angle - b.angle);
  return c;
}

AngleClass operator -(AngleClass &a,double b) {
  return (a + (-b));
}

AngleClass & AngleClass::operator +=(AngleClass &a) {
  Angle(Angle()+a.angle);
  return *this;
}

AngleClass & AngleClass::operator +=(double a) {
  Angle(Angle()+a);
  return *this;
}

AngleClass & AngleClass::operator -=(AngleClass &a) {
  Angle(Angle()-a.angle);
  return *this;
}

AngleClass & AngleClass::operator -=(double a) {
  Angle(Angle()-a);
  return *this;
}


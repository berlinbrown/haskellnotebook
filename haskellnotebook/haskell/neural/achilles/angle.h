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

#ifndef ANGLE_H_756834
#define ANGLE_H_756834

class VectorClass;

class AngleClass {
private:
  double angle;
public:
  AngleClass();
  AngleClass(double);
  AngleClass(VectorClass &);
  ~AngleClass();
  double Angle();
  double Angle(double);
  VectorClass Vector();
  bool operator ==(AngleClass &);
  bool operator !=(AngleClass &);
  bool operator ==(double);
  bool operator !=(double);
  friend AngleClass operator +(AngleClass &,AngleClass &);
  friend AngleClass operator +(AngleClass &,double);
  friend AngleClass operator -(AngleClass &,AngleClass &);
  friend AngleClass operator -(AngleClass &,double);
  AngleClass & operator +=(AngleClass &);
  AngleClass & operator +=(double);
  AngleClass & operator -=(AngleClass &);
  AngleClass & operator -=(double);
};

#endif


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

#ifndef VECTOR_H_46768
#define VECTOR_H_46768

class VectorClass {
private:
  double x,y,z;
public:
  VectorClass();
  VectorClass(double,double,double);
  VectorClass(double,double);
  ~VectorClass();
  double X();
  double Y();
  double Z();
  double X(double);
  double Y(double);
  double Z(double);
  double Magnitude();
  bool operator ==(VectorClass &);
  bool operator !=(VectorClass &);
  VectorClass & operator +=(VectorClass &);
  friend VectorClass operator -(VectorClass &,VectorClass &);
  double Dot(VectorClass &);
};

#endif

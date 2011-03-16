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

#ifndef ENERGY_H_3489564
#define ENERGY_H_3489564

#define ENERGY_PER_UNIT_VOLUME 1

class VectorClass;

class EnergyClass {
private:
  double food,health,fcap,hcap;
public:
  EnergyClass();
  EnergyClass(VectorClass &);
  ~EnergyClass();
  double Health();
  double Food();
  bool SetCap(VectorClass &);
  bool TakeDamage(double);
  bool UseEnergy(double);
  double Regen(double);
  bool EatFood(double);
  double HealthCap();
  double FoodCap();
};

#endif

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

#include"energy.h"
#include"vector.h"
#include"defines.h"
#include<stdio.h>
#include<iostream.h>

EnergyClass::EnergyClass() :
  food(0),
  health(0),
  fcap(0),
  hcap(0)
{}

EnergyClass::EnergyClass(VectorClass &s) :
  food(0),
  health(MIN_ECAP + (s.X() * s.Y() * s.Z() - MIN_VOLUME) * (MAX_ECAP - MIN_ECAP) / (MAX_VOLUME - MIN_VOLUME)),
  fcap(health),
  hcap(health)
{
#ifdef _DEBUG
  printf("HCAP: %f\n",hcap);
#endif
}

EnergyClass::~EnergyClass() {}

double EnergyClass::Food() {
  return food;
}

double EnergyClass::Health() {
  return health;
}

bool EnergyClass::SetCap(VectorClass &s) {
  hcap = fcap = MIN_ECAP + (s.X() * s.Y() * s.Z() - MIN_VOLUME) * (MAX_ECAP - MIN_ECAP) * (MAX_VOLUME - MIN_VOLUME);

  if(health > hcap) health = hcap;
  if(food > fcap) food = fcap;
  return true;
}

bool EnergyClass::TakeDamage(double dmg) {
  health-=dmg;
  if(health<=0) {
    health = 0;
    return false;  // dead!
  } else
    return true;
}

bool EnergyClass::UseEnergy(double amt) {
  if(food-amt < 0)
    return false;  // no can do..
  else {
    food-=amt;
    return true;
  }
}

double EnergyClass::Regen(double meta) {
  double amt = (hcap-health)/hcap * (fcap-food)/fcap * food * meta;
  food-=amt;
  health+=amt;
  if(health>hcap) health=hcap;
  if(food<0) food=0;
  return amt;
}

bool EnergyClass::EatFood(double f) {
  food+=f;
  if(f>fcap) f=fcap;
  if(f<0) f=0;
  return true;
}

double EnergyClass::HealthCap() {
  return hcap;
}

double EnergyClass::FoodCap() {
  return fcap;
}

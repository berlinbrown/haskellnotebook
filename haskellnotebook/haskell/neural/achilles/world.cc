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

#include"world.h"
#include"vector.h"
#include"defines.h"
#include<stdlib.h>

extern int FLOOR_BLOCKS_Z,FLOOR_BLOCKS_X,NUM_FOOD,NUM_ORGANISMS;

WorldClass::WorldClass(int argc, char *argv[])
{
  size.X(WORLD_X);
  size.Y(WORLD_Y);
  size.Z(WORLD_Z);
  position.Y(1);
  position.Z(5);
}

WorldClass::~WorldClass() {

}

VectorClass &WorldClass::Size() {
  return size;
}

VectorClass &WorldClass::Pos() {
  return position;
}

AngleClass &WorldClass::Heading() {
  return heading;
}

VectorClass * WorldClass::NewPosition() {
  VectorClass *v;

  //  v= new VectorClass(Size().X()/2-FLOOR_QUAD_SIZE/2,0,Size().Z()/2+FLOOR_QUAD_SIZE/2);
  //  v= new VectorClass(-Size().X()/2-FLOOR_QUAD_SIZE/2,0,Size().Z()/2+FLOOR_QUAD_SIZE/2);
  //  v= new VectorClass(Size().X()/2-FLOOR_QUAD_SIZE/2,0,-Size().Z()/2-FLOOR_QUAD_SIZE/2);
  //  v= new VectorClass(-Size().X()/2-FLOOR_QUAD_SIZE/2,0,-Size().Z()/2-FLOOR_QUAD_SIZE/2);
  v=new VectorClass(double(rand())/RAND_MAX * Size().X() - Size().X()/2-FLOOR_QUAD_SIZE/2,
		    0,
		    double(rand())/RAND_MAX * (Size().Z()+FLOOR_QUAD_SIZE) - Size().Z()/2-FLOOR_QUAD_SIZE/2);
  
  return v;
}

bool WorldClass::ChangePosition(VectorClass &p, VectorClass &d) {
  
  if(p.X() + d.X() > (FLOOR_BLOCKS_X/2)*FLOOR_QUAD_SIZE)
    d.X((FLOOR_BLOCKS_X/2)*FLOOR_QUAD_SIZE - p.X());
  if(p.X() + d.X() < (-FLOOR_BLOCKS_X/2-1)*FLOOR_QUAD_SIZE)
    d.X((-FLOOR_BLOCKS_X/2-1)*FLOOR_QUAD_SIZE - p.X());


  if(p.Z() + d.Z() > FLOOR_BLOCKS_Z/2*FLOOR_QUAD_SIZE)
    d.Z(FLOOR_BLOCKS_Z/2*FLOOR_QUAD_SIZE - p.Z());
  if(p.Z() + d.Z() < (-FLOOR_BLOCKS_Z/2-1)*FLOOR_QUAD_SIZE)
    d.Z((-FLOOR_BLOCKS_Z/2-1)*FLOOR_QUAD_SIZE - p.Z());
  
  p.X(p.X() + d.X());
  p.Y(p.Y() + d.Y());
  p.Z(p.Z() + d.Z());

  return true;
}


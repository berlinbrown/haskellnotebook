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

#ifndef ORG_H_86534
#define ORG_H_86534

#include"vector.h"
#include"angle.h"
#include"gene.h"
#include"color.h"

class NeuralNet;
class EnergyClass;
class IdClass;
struct IdToken;

class OrganismClass {
private:
  VectorClass size;
  NeuralNet *brain;
  EnergyClass *energy;
  IdClass *id;
  VectorClass position;
  AngleClass heading;
  GeneClass genes;
  ColorClass color;
  short type;
  long lifespan;
public:
  OrganismClass(IdToken *token,VectorClass *_position,AngleClass *_heading,GeneClass *_genes);
  ~OrganismClass();
  IdClass & Id();
  VectorClass & Pos();
  AngleClass & Heading();
  NeuralNet & Brain();
  EnergyClass & Energy();
  GeneClass & Genes();
  VectorClass & Size();
  ColorClass & Color();
  bool Lifetick();
  short Type();
  short Type(short);
};

#endif

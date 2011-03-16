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
#include"org.h"
#include"id.h"
#include"energy.h"
#include"hebbian.h"
#include"defines.h"

OrganismClass::OrganismClass(IdToken *token,
			     VectorClass *_position,
			     AngleClass *_heading,
			     GeneClass *_genes) :
  id(new IdClass(*token)),
  position(*_position),
  heading(*_heading),
  genes(*_genes)
{
  delete token;
  delete _position;
  delete _heading;
  delete _genes;
  size.X(genes.Size().X());
  size.Y(genes.Size().Y());
  size.Z(genes.Size().Z());
  lifespan = long(double(genes.Lifespan()) * (2 - genes.Metabolism()));
  energy = new EnergyClass(size);
  brain = new NeuralNet(genes.Brain().NumNeurodes(),
			genes.Brain().NumLayers(),
			genes.Brain().NumInputs(),
			genes.Brain().NumOutputs(),
			genes.Brain().TopoDist());
  Type(ORGANISM_LIVE);
}

OrganismClass::~OrganismClass() {
  delete id;
  delete energy;
  delete brain;
}

IdClass & OrganismClass::Id() {
  return *id;
}

VectorClass & OrganismClass::Pos() {
  return position;
}

AngleClass & OrganismClass::Heading() {
  return heading;
}

NeuralNet & OrganismClass::Brain() {
  return *brain;
}

EnergyClass & OrganismClass::Energy() {
  return *energy;
}

GeneClass & OrganismClass::Genes() {
  return genes;
}

VectorClass & OrganismClass::Size() {
  return size;
}

ColorClass & OrganismClass::Color() {
  return color;
}

short OrganismClass::Type() {
  return type;
}

short OrganismClass::Type(short t) {
  if(t==ORGANISM_LIVE) {
    color.R(0);
    color.G(0);
    color.B(genes.Color()*3/4 + 1/4);
  } else if(t==ORGANISM_FOOD) {
    color.R(1);
    color.G(1);
    color.B(0);
    lifespan = DECAY_SPAN;
  } else return type;
  type=t;
  return type;
}
  
bool OrganismClass::Lifetick() {

  if(type==ORGANISM_LIVE) {
    // Regenerate some health
    Energy().Regen(Genes().Metabolism());
    
    
    if(!--lifespan) {
      Type(ORGANISM_FOOD);
      Energy().TakeDamage(Energy().Health());
      return false;
    }
    if(Energy().TakeDamage(Genes().Metabolism() * ENERGY_LOSS_PER_TICK * Energy().HealthCap())==false) {
      Type(ORGANISM_FOOD);
      return false;
    }
  } else {
    if(--lifespan<=0) return false;
  }

  return true;
}

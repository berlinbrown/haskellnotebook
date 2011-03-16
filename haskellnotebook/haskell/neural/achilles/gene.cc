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

#include"gene.h"
#include"vector.h"
#include<vector>
#include"braininfo.h"
#include"defines.h"
#include<stdio.h>

GeneClass::GeneClass() { // randomly generate one
  int i;
  for(i=0;i<NUM_GENES;i++)
    DNA.push_back(long(rand()));
  MakeViable();
}

GeneClass::GeneClass(GeneClass &a,GeneClass &b) {
  int i;
  for(i=0;i<NUM_GENES;i++) {
    if(double(rand())/RAND_MAX*100.0 <= a.MutationRate())
      DNA.push_back(SCALE(rand(),0,LONG_MAX,GeneLimits[i].min,GeneLimits[i].max));
    else {
      if((int)((double)(rand())/RAND_MAX*2)) {
	DNA.push_back(a.DNA[i]);
      } else {
	DNA.push_back(b.DNA[i]);
      }

    }
  }
}

GeneClass::~GeneClass() {}

double GeneClass::MutationRate() {
  return (double(DNA[GENE_MUTATE_RATE])/MAX_MUTATE_RATE);
}

double GeneClass::Variance(GeneClass &a) {
  double abs_d(double);
  double average=0;

  for(int i=0;i<NUM_GENES;i++) 
    average+= double(DNA[i] - a.DNA[i])/double(GeneLimits[i].max - GeneLimits[i].min);
  average/=NUM_GENES;
#ifdef _DEBUG
  cout << "Variance: " << average << endl;
#endif
  return abs_d(average);
}

VectorClass GeneClass::Size() {
  VectorClass size((double)DNA[GENE_SIZE_X],
		   (double)DNA[GENE_SIZE_Y],
		   (double)DNA[GENE_SIZE_Z]);
  return size;
}

double GeneClass::LC() {
  return(double(DNA[GENE_LC])/MAX_LC);
}

BrainInfoClass GeneClass::Brain() {
  BrainInfoClass brain(DNA[GENE_NUM_NEURODES],
		       DNA[GENE_NUM_LAYERS],
		       DNA[GENE_NUM_INPUTS],
		       DNA[GENE_NUM_OUTPUTS],
		       double(DNA[GENE_TOPO_DIST])/MAX_TOPO_DIST);
  return brain;
}

bool GeneClass::IsViable() {
  int i;
  if(MIN_VOLUME > DNA[0]*DNA[1]*DNA[2] || 
     DNA[0]*DNA[1]*DNA[2] > MAX_VOLUME) return false;
  
  for(i=0;i<NUM_GENES;i++) {
    if(GeneLimits[i].min > DNA[i] || DNA[i] > GeneLimits[i].max)
      return false;
  }
  return true;
}

double GeneClass::Color() {
  return(double(DNA[GENE_COLOR]) / MAX_COLOR);
}

double GeneClass::MaxSpeed() {
  return(double(DNA[GENE_MAXSPEED]) / MAX_MAXSPEED);
}

double GeneClass::GetEnergyPcnt() {
  return(double(DNA[GENE_ENERGY_TO_OFFSPRING]) / MAX_ECAP);
}

double GeneClass::Metabolism() {
  return(double(DNA[GENE_METABOLISM]) / MAX_METABOLISM);
}

long GeneClass::Reach() {
  return(DNA[GENE_REACH]);
}

long GeneClass::Lifespan() {
  return(DNA[GENE_LIFESPAN]);
}

double GeneClass::GetStrength() {
  return(DNA[GENE_STRENGTH] * 2.0 / MAX_STRENGTH);
}

bool GeneClass::MakeViable() {
  int i,j;
  double tmp;

  do {
    j=0;
    for(i=0;i<3;i++) {
      if(DNA[i] < MIN_SIDE_LEN) DNA[i]=MIN_SIDE_LEN;
      if(DNA[i] > DNA[j]) j = i;
    }
    tmp = DNA[0] * DNA[1] * DNA[2];
  } while(tmp > MAX_VOLUME && (DNA[j]=long(double(DNA[j])*double(MAX_VOLUME/tmp))));


  for(i=0;i<NUM_GENES;i++)
    DNA[i]=SCALE(DNA[i],0,LONG_MAX,GeneLimits[i].min,GeneLimits[i].max);

  return true;
}



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
#include<stdio.h>
#include<math.h>
#include"universe.h"
#include"opengl.h"
#include"world.h"
#include"orglist.h"
#include"idserver.h"
#include"id.h"
#include"angle.h"
#include"vector.h"
#include"gene.h"
#include"org.h"
#include"unistd.h"
#include"energy.h"
#include"hebbian.h"
#include"defines.h"
#include"quicksort.h"

/*
 * universe.cc
 * 
 * Contains the UniverseClass member functions
 * UniverseClass contains all the objects that are 
 * related to the simulation
 *
 * Note: all code surrounded by #ifdef _DEBUG .... 
 * #endif is code capable of being turned on and 
 * off for debugging purposes 
 * (or any purposes for that matter)
 */

double abs_d(double);
float abs_f(float);
int SortOListFunc(OList *,OList *);

extern int FLOOR_BLOCKS_Z,FLOOR_BLOCKS_X,NUM_FOOD,NUM_ORGANISMS;

/*
 * Some Utility functions
 */

int SortOListFunc(const void *a, const void *b) {
  OList *ao = (OList *) a;
  OList *bo = (OList *) b;
  if(ao->dist < bo->dist) return -1;
  else if(ao->dist == bo->dist) return 0;
  else return 1;
}

double abs_d(double n) {
  return (n > 0 ? n : -n);
}

float abs_f(float n) {
  return (n > 0 ? n : -n);
}



UniverseClass::UniverseClass(int argc, char **argv) {
  done = 0;
  ogl = new OpenGLClass(argc,argv);
  world = new WorldClass(argc,argv);
  orglist = new OrganismListClass;
  idserver = new IdServerClass;
}


UniverseClass::~UniverseClass() {
  delete ogl;
  delete world;
  delete orglist;
  delete idserver;
}


bool UniverseClass::MainLoop() {
  OrganismClass *o;
  int i;

  IdToken *token;
  VectorClass *pos;
  AngleClass *heading;
  GeneClass *genes;

  /*
   * This section creates the initial organisms and food
   */

  for(i=0;i<NUM_ORGANISMS + NUM_FOOD;i++) {
    // Each Organism is represented by a unique Id
    token = idserver->GetToken();
    
    // Get a random position in the world
    pos = world->NewPosition();

    // Choose 0 degrees as the initial direction
    heading = new AngleClass(0);

    // Stir up some new genes!
    genes = new GeneClass;

    // Throw it all in the pot and cook up an organism
    o = new OrganismClass(token,pos,heading,genes);

    // Put it on the list to keep track of it
    orglist->Append(o);

    // If we've created all the organisms, do food instead
    if(i>=NUM_ORGANISMS)
      o->Type(ORGANISM_FOOD); // Make it into food instead!
    else // otherwise give it full energy to start
      o->Energy().EatFood(o->Energy().FoodCap()); 
  }
  
  int frames = 0;
  long start_time = time(NULL);
  long cur_time = start_time;

  while(!done) {

    Update();

    frames++;

    cur_time=time(NULL);
    if(cur_time-start_time >= 5) {
      printf("Frames per second: %f\n", double(double(frames)/double(cur_time-start_time)));
      start_time=cur_time;
      frames=0;
    }
  }
  return true;
}


bool UniverseClass::Update() {
  orglist->Rewind();

  OrganismClass *o;
  vector<OrganismClass *> foodlist,livelist;
  IdToken *token;
  VectorClass *pos;
  AngleClass *heading;
  GeneClass *genes;

  if(!pause) {
  while((o=orglist->Next())) { 

    // If the Organism has gone off to never-never land
    if(isnan(o->Pos().X()) || isnan(o->Pos().Z())) {
      orglist->Remove(o->Id()); // get rid of it
      continue;
    }


    if(o->Type()==ORGANISM_LIVE) livelist.push_back(o);
    
    if(o->Type()==ORGANISM_FOOD) {
      if(!o->Lifetick()) { // check if food has decayed
	orglist->Remove(o->Id());
      } else
	foodlist.push_back(o);
    }
  }

  // Check to see if the number of organisms have fallen below a certain
  // amount and if they have, to add a new organism randomly
  // (and print a silly message too!)
  if((signed int)livelist.size() < NUM_ORGANISMS) {
    cout << "New guy!" << endl;
    token = idserver->GetToken();
    pos = world->NewPosition();
    heading = new AngleClass(0);
    genes = new GeneClass;
    o = new OrganismClass(token,pos,heading,genes);
    orglist->Append(o);
    o->Energy().EatFood(o->Energy().FoodCap()/2);
  }

  // Same as above, but for food instead
  if((signed int)foodlist.size() < NUM_FOOD) {
    cout << "More food!" << endl;
    token = idserver->GetToken();
    pos = world->NewPosition();
    heading = new AngleClass(0);
    genes = new GeneClass;
    o = new OrganismClass(token,pos,heading,genes);
    orglist->Append(o);
    o->Type(ORGANISM_FOOD);
  }

  int i;
  for(i=0;i<(signed int)livelist.size();i++) {
    if(livelist[i]->Type()==ORGANISM_LIVE) {
      UpdateOrganism(livelist[i],foodlist,livelist.size());

      livelist[i]->Lifetick();
    }
  }
  livelist.clear();
  foodlist.clear();
  }

  UpdateDraw();

  return true;
}

bool UniverseClass::UpdateOrganism(OrganismClass *o,vector<OrganismClass *> foodlist,int org_count) {
  int i;
  vector<float> inputs,outputs;
  OrganismClass *tmp,*mate=NULL;
#ifdef OLD_LOOP
  float last_min=0,min_limit=0,dist=0;
#endif
  VectorClass distv,dims;
  ColorClass color;
  AngleClass heading;
  
  // Start by giving some inputs to the neural network
  inputs.push_back(float(rand())/RAND_MAX*2.0-1.0);
  inputs.push_back(o->Energy().Health());
  inputs.push_back(o->Energy().Food());


#ifndef OLD_LOOP
  // alternative to below

  orglist->Rewind();
  vector<OList> olist;
  VectorClass headingvect=o->Heading().Vector();
  while((tmp=orglist->Next())) {
    OList ol;
    if(tmp!=o) {
      ol.distv = (tmp->Pos() - o->Pos());
      if(ol.distv.Dot(headingvect) > 0) {
	ol.dist = ol.distv.Magnitude();
	ol.color = tmp->Color();
	ol.heading = ol.distv;
	ol.heading-=o->Heading();
	olist.push_back(ol);
      }
    }
  }
  long reallen = olist.size();
  QuickSortOList(olist);
  i=0;
  while(i<reallen && (signed int)(inputs.size()+5) < o->Brain().NumInputs()) {
    inputs.push_back((float)olist[i].dist);
    inputs.push_back((float)olist[i].heading.Angle());
    inputs.push_back((float)olist[i].color.R());
    inputs.push_back((float)olist[i].color.G());
    inputs.push_back((float)olist[i].color.B());
    i++;
  }

#else
  // This loop finds the closest organisms and gives their
  // position, direction, and color to the inputs of this guy's
  // neural network, until there are no more input spots left
  // (note: this _needs_ optimizing!)
  // (note: the above code optimizes this, this will be removed soon)

  while((signed int)(inputs.size() + 5) < o->Brain().NumInputs()) {
    color.R(0); color.G(0); color.B(0);
    heading=0;
    last_min=0;
    orglist->Rewind();
    while((tmp=orglist->Next())) {
      if(tmp == o) continue;
      distv=(tmp->Pos() - o->Pos());
      dist=distv.Magnitude();
      VectorClass headingvect=o->Heading().Vector();
      if((!last_min || dist<last_min) && dist>min_limit && distv.Dot(headingvect) > 0) {
	last_min=dist;
	color=tmp->Color();
	heading = distv;
	heading-=o->Heading();
      }
    }
    min_limit=(last_min>min_limit ? last_min : min_limit);
    inputs.push_back((float)last_min);
    inputs.push_back((float)heading.Angle());
    inputs.push_back((float)color.R());
    inputs.push_back((float)color.G());
    inputs.push_back((float)color.B());
  }
#endif

  // Any inputs left over? fill em with 0
#ifndef OLD_LOOP
  while((signed int)inputs.size() < o->Brain().NumInputs())
    inputs.push_back(0);
#else
  while((signed int)inputs.size()<o->Brain().NumInputs()) inputs.push_back(float(rand())/RAND_MAX*2.0-1.0);
#endif

#ifdef _NN_DEBUG  
  printf("Num Inputs: %d\n",inputs.size());
  for(i=0;i<(signed int)inputs.size();i++) 
    printf("%f ",inputs[i]);
  printf("\n");
  getchar();
#endif


  o->Brain().SetInputs(inputs);

  o->Brain().RunNet();

  o->Brain().GetOutputs(outputs);

#ifdef _NN_DEBUG  
  printf("Num Outputs: %d\n",inputs.size());
  for(i=0;i<(signed int)outputs.size();i++) 
    printf("%f ",outputs[i]);
  printf("\n");
  getchar();
#endif

  o->Brain().Learn(o->Genes().LC(),ALPHA_COEFFICIENT);

  // Change the heading according to the output of the neural network
  o->Heading()+=double(((int)outputs[1] % 20)/10 * (double)o->Genes().MaxSpeed());
  VectorClass dir((double)(o->Genes().MaxSpeed() * double((int)(outputs[0]) % 10)/10.0)/2,(double)outputs[1]);

#ifdef _DEBUG
  printf("%f %f %f\n",dir.X(),dir.Y(),dir.Z());
#endif

  // ditto for the position
  world->ChangePosition(o->Pos(),dir);


  // Set the Aggressive coloration
  o->Color().R(double(( abs_d(outputs[3]) > FIGHT_THRESHOLD ? FIGHT_THRESHOLD : abs_d(outputs[3]) ) / (FIGHT_THRESHOLD)));

  
  // Find out if this organism is near to any food, and whether or not
  // it needs that food
  for(i=0;i<(signed int)foodlist.size();i++) {
    if(o->Energy().Food() != o->Energy().FoodCap()) {  
      if(foodlist[i]) {
	distv=foodlist[i]->Pos() - o->Pos();
	long bounds = o->Genes().Reach();
	distv.X(abs_d(distv.X()) - bounds - o->Size().X());
	distv.Z(abs_d(distv.Z()) - bounds - o->Size().Z());
	if(distv.X() <= 0 && distv.Z() <= 0) {
	  double amt_food=(1-double(bounds/MAX_REACH)) * foodlist[i]->Energy().HealthCap() * (1+o->Genes().Metabolism());
#ifdef _DEBUG_FOOD
	  printf("Food Eaten: %f\n",amt_food);
#endif
	  EventStack es;
	  es.a = o->Pos();
	  es.b = foodlist[i]->Pos();
	  es.a.Y(o->Size().Y()/2);
	  es.b.Y(foodlist[i]->Size().Y()/2);
	  es.color.R(1);
	  es.color.G(1);
	  es.color.B(0);
	  foodstack.push(es);
	  o->Energy().EatFood(amt_food);
	  orglist->Remove(foodlist[i]->Id());
	  foodlist[i]=NULL;
	  break;
	}
      }
    } else break;
  }



  orglist->Rewind();
  // Find if there are any Organisms nearby this one
  while((mate=orglist->Next())) {
    if(mate==o) continue; // if its the same one
    distv = mate->Pos() - o->Pos(); // get the vector between them
    distv.X(abs_d(distv.X())-o->Size().X()-o->Genes().Reach());
    distv.Z(abs_d(distv.Z())-o->Size().Z()-o->Genes().Reach());
#ifdef _DEBUG
    char s[30];
    o->Id().String(s);
    printf("%s: %f %f %f\n",s,distv.X(),distv.Y(),distv.Z());
#endif
    if(distv.X() <= 0 &&  distv.Z() <= 0) { // if close enough
      vector<float> mate_outputs;
      mate->Brain().GetOutputs(mate_outputs);
      // Fight or mate, they decide!
#ifdef _DEBUG
      cout << "Fight: " << outputs[3] << " " << mate_outputs[3] << endl;
      cout << "Mate: " << outputs[2] << " " << mate_outputs[2] << endl;
#endif
      if(abs_f(mate_outputs[3]) * abs_f(outputs[3]) > FIGHT_THRESHOLD*FIGHT_THRESHOLD * NUM_ORGANISMS/org_count ) {
	Fight(o,mate);
      } else if (org_count < NUM_ORGANISMS+2*NUM_FOOD && abs_f(mate_outputs[2]) * abs_f(outputs[2]) > REPRODUCTION_THRESHOLD * org_count/NUM_ORGANISMS) {
	Mate(o,mate);
      }
      break;
    }
  }  

#ifdef _DEBUG
  printf("%f\n",(double)o->Genes().MaxSpeed());
#endif

  
  return true;
}

bool UniverseClass::Fight(OrganismClass *o1, OrganismClass *o2) {
  // fight yourself? maybe some other time...
  if(o1==o2) return false;

  // sorry, no playing with food
  if(o1->Type() != ORGANISM_LIVE || o2->Type() != ORGANISM_LIVE) return false;

  if(o1->Genes().Variance(o2->Genes()) < MISCEGENATION_RATE) { // yea its for mating but wahtever
    return false;
  }

  // calculate damage done and energy needed to do it
  double damage = DAMAGE_PER_HIT * o1->Genes().GetStrength() * (MAX_REACH - o1->Genes().Reach()) / MAX_REACH;
  double energy_used = damage * ENERGY_PER_DAMAGE * o1->Genes().Metabolism();

#ifdef _DEBUG_FIGHT
  char s1[30],s2[30];
  o1->Id().String(s1);
  o2->Id().String(s2);
  printf("%s does %f damage to %s\n",s1,damage,s2);
#endif

  // if ya don't got enough energy you can't do any damage...
  if(o1->Energy().Food() < energy_used) return false;
  
  // Use it if you got it
  o1->Energy().UseEnergy(energy_used);
  cout << "Fight!" << endl;

  // makes those little flashes between organisms :)
  EventStack es;
  es.a = o1->Pos();
  es.b = o2->Pos();
  es.a.Y(o1->Size().Y()/2);
  es.b.Y(o2->Size().Y()/2);
  es.color.R(1);
  es.color.G(0);
  es.color.B(0);
  fightstack.push(es);

  // Take that!
  if(!o2->Energy().TakeDamage(damage)) {
    o2->Type(ORGANISM_FOOD); // Uh oh, he died
    cout << "Kill!" << endl;
  }
#ifdef _DEBUG_FIGHT
  printf("%s has %f health left.\n",s2,o2->Energy().Health());
  fflush(stdout);
#endif
  return true;
}

bool UniverseClass::Mate(OrganismClass *o1, OrganismClass *o2) {
  if(o1==o2) return false; // no asexuals allowed

  // no necrophilia either..
  if(o1->Type() != ORGANISM_LIVE || o2->Type() != ORGANISM_LIVE) return false;

  // find the costs
  double o1_spend=o1->Energy().FoodCap() * o1->Genes().GetEnergyPcnt()/2;
  double o2_spend=o2->Energy().FoodCap() * o2->Genes().GetEnergyPcnt()/2;

  // if only humans could do this...
  if(o1_spend > o1->Energy().Food()) return false;
  if(o2_spend > o2->Energy().Food()) return false;

  // spend the energy
  o1->Energy().UseEnergy(o1_spend);
  o2->Energy().UseEnergy(o2_spend);

  // spread the word
  cout << "Mate." << endl;

  // show a flash
  EventStack es;
  es.a = o1->Pos();
  es.b = o2->Pos();
  es.a.Y(o1->Size().Y()/2);
  es.b.Y(o2->Size().Y()/2);
  es.color.R(0);
  es.color.G(0);
  es.color.B(1);
  matestack.push(es);

  // create an kiddie
  IdToken *token=idserver->GetToken();
  VectorClass *position=new VectorClass(double(rand())/RAND_MAX*2.0-1.0+o1->Pos().X(),0,o2->Pos().Z()+double(rand())/RAND_MAX*2.0-1.0);
  AngleClass *heading=new AngleClass(0);
  // with the parent's genes used
  GeneClass *genes=new GeneClass(o1->Genes(),o2->Genes());
  OrganismClass *o3 = new OrganismClass(token,
					position,
					heading,
					genes);
  orglist->Append(o3);

  // Give the new guy energy that mommy and daddy spent
  o3->Energy().EatFood(o1_spend + o2_spend);
  
  return true;
}

bool UniverseClass::UpdateDraw() {
  orglist->Rewind();
  OrganismClass *o;
  
  PrepareDraw();

  DrawLandscape();

  ogl->DrawStack(fightstack);
  ogl->DrawStack(matestack);
  ogl->DrawStack(foodstack);

  while((o=orglist->Next())) {
    DrawOrganism(o);
  }


  Draw();

  CheckKeys();

  return true;
}

bool UniverseClass::PrepareDraw() {
  // clear the screen and transform the world!
  ogl->Clear();
  return ogl->Transform(world->Pos().X(),world->Pos().Y(),world->Pos().Z(),world->Heading().Angle());

}

bool UniverseClass::DrawLandscape() {
  return ogl->DrawLandscape(world->Size().X(),world->Size().Y(),world->Size().Z());
}

bool UniverseClass::DrawOrganism(OrganismClass *o) {
  return ogl->DrawOrganism(o);
}

bool UniverseClass::Draw() {
  return ogl->SwapBuffers();
}

bool UniverseClass::CheckKeys() {
  double xpos=world->Pos().X();
  double ypos=world->Pos().Y();
  double zpos=world->Pos().Z();
  double yrot=world->Heading().Angle();

  bool retval=ogl->CheckInput(xpos,ypos,zpos,yrot,done,pause);
 
  world->Pos().X(xpos);
  world->Pos().Y(ypos);
  world->Pos().Z(zpos);
  world->Heading().Angle(yrot);

  return retval;
}



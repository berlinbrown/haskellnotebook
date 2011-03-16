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

#ifndef UNIVERSE_H_87634
#define UNIVERSE_H_87634
#include<vector>
#include<stack>
#include"vector.h"
#include"color.h"


class WorldClass;
class OpenGLClass;
class OrganismListClass;
class IdServerClass;
class OrganismClass;

struct EventStack {
  VectorClass a,b;
  ColorClass color;
};

class UniverseClass {
private:
  WorldClass *world;
  OpenGLClass *ogl;
  OrganismListClass *orglist;
  IdServerClass *idserver;
  int done,pause;
  stack<EventStack> fightstack,matestack,foodstack;
public:
  UniverseClass(int,char **);
  ~UniverseClass();
  bool MainLoop();
  bool Update();
  bool UpdateDraw();
  bool UpdateOrganism(OrganismClass *,vector<OrganismClass *>,int);
  bool DrawOrganism(OrganismClass *);
  bool PrepareDraw();
  bool DrawLandscape();
  bool CheckKeys();
  bool Draw();
  bool Mate(OrganismClass *,OrganismClass *);
  bool Fight(OrganismClass *,OrganismClass *);
};

#endif

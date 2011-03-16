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

#ifndef OPENGL_H_67548
#define OPENGL_H_67548
#include<stack>

#include"conf.h"
#ifdef HAVE_SDL_SDL_H
#include<SDL/SDL.h>
#else 
#include<SDL.h>
#endif

class OrganismClass;
struct EventStack;

class OpenGLClass {
private:
  SDL_Surface *surface;
  bool took_screenshot;
public:
  OpenGLClass(int,char **);
  ~OpenGLClass();
  bool InitGL(int,int);
  bool DrawLandscape(double,double,double);
  bool Transform(double,double,double,double);
  bool DrawOrganism(OrganismClass *);
  bool SwapBuffers();
  bool Clear();
  bool DrawStack(stack<EventStack> &);
  bool CheckInput(double &,double &,double &,double &,int &,int &);
  bool Screenshot(char *);
};

#endif

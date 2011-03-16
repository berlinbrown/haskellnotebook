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

#include"opengl.h"
#include<GL/gl.h>
#include<GL/glu.h>
//#include<SDL/SDL.h>
#include<math.h>
#include<iostream.h>
#include"org.h"
#include"vector.h"
#include"gene.h"
#include"defines.h"
#include"universe.h"


extern int FLOOR_BLOCKS_Z,FLOOR_BLOCKS_X,NUM_FOOD,NUM_ORGANISMS;

OpenGLClass::OpenGLClass(int argc, char **argv) {
 if ( SDL_Init(SDL_INIT_VIDEO) < 0 ) {
    fprintf(stderr, "Unable to initialize SDL: %s\n", SDL_GetError());
    exit(1);
  }
  if ( (surface=SDL_SetVideoMode(SCREEN_W, SCREEN_H, 0, SDL_OPENGL)) == NULL ) {
    fprintf(stderr, "Unable to create OpenGL screen: %s\n", SDL_GetError());
    SDL_Quit();
    exit(2);
  }

  SDL_WM_SetCaption("Achilles", NULL);


  InitGL(SCREEN_W,SCREEN_H);
}

bool OpenGLClass::InitGL(int W,int H) {
  GLfloat LightAmbient[]= { 0.2f, 0.2f, 0.2f, 1.0f };
  GLfloat LightDiffuse[]= { 0.5f, 0.5f, 0.5f, 1.0f };
  GLfloat LightPosition[]= { 0.0f, 0.0f, 2.0f, 1.0f };

  
  glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_DECAL);
  glEnable(GL_TEXTURE_2D);

  glViewport(0, 0, W, H);
  glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
  glClearDepth(1.0);
  glDepthFunc(GL_LESS);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_NORMALIZE);
  glShadeModel(GL_SMOOTH);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(45.0f,(GLfloat)W/(GLfloat)H,0.1f,100.0f);
  glMatrixMode(GL_MODELVIEW);
  glLightfv(GL_LIGHT1, GL_AMBIENT, LightAmbient);
  glLightfv(GL_LIGHT1, GL_DIFFUSE, LightDiffuse);
  glLightfv(GL_LIGHT1, GL_POSITION,LightPosition);
  glEnable(GL_LIGHT1);
  glEnable(GL_LIGHTING);
  glEnable(GL_COLOR_MATERIAL);

  return true;
}


OpenGLClass::~OpenGLClass() {
  SDL_Quit();
}


bool OpenGLClass::Clear() {
  glClear(GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT);
  return true;
}

bool OpenGLClass::SwapBuffers() {
  SDL_GL_SwapBuffers();
  return true;
}

bool OpenGLClass::Transform(double xpos,double ypos,double zpos,double yrot) {
  GLfloat transmatrix[16]={1,0,0,0,
                           0,1,0,0,
                           0,0,1,0,
                           -xpos,-ypos,-zpos,1};
  

  glLoadIdentity();
  glRotatef(360.0-yrot,0,1,0);
  glMultMatrixf(transmatrix);

  return true;
}

bool OpenGLClass::DrawLandscape(double xsize,double ysize,double zsize) {
  int u,v;

  glPushMatrix();
  glColor3f(0.0,0.5,0.0);
  glNormal3f(0.0,1.0,0.0);

  for(u=-FLOOR_BLOCKS_Z/2-1;u<=FLOOR_BLOCKS_Z/2;u++) {
    glBegin(GL_QUAD_STRIP);
    for(v=(int)-FLOOR_BLOCKS_X/2-1;v<=FLOOR_BLOCKS_X/2;v++) {
      glVertex3f(v*FLOOR_QUAD_SIZE,EVO_FLOOR_Y,(u+1)*FLOOR_QUAD_SIZE);
      glVertex3f(v*FLOOR_QUAD_SIZE,EVO_FLOOR_Y,(u)*FLOOR_QUAD_SIZE);
    }
    glEnd();
  }
  glPopMatrix();
  return true;
}

bool OpenGLClass::DrawOrganism(OrganismClass *o) {
  double w=o->Size().X(),h=o->Size().Y(),d=o->Size().Z();

#ifdef _DEBUG
  cout << "OGL Pos: " << o->Pos().X() << " ";
  cout << o->Pos().Y() << " ";
  cout << o->Pos().Z() << endl;
#endif

  glPushMatrix();
  glTranslated(o->Pos().X(),o->Pos().Y()+h/2,o->Pos().Z());
  glRotated(o->Heading().Angle(),0,1,0);
  

  glColor3d(o->Color().R(),o->Color().G(),o->Color().B());

  /*
  glBegin(GL_QUADS);

  // top
  glNormal3f(0,1,0);
  glTexCoord2f(0,0); glVertex3f( -w/2, h/2, d/2);
  glTexCoord2f(0,1); glVertex3f( -w/2, h/2, -d/2);
  glTexCoord2f(1,1); glVertex3f( w/2, h/2, -d/2);
  glTexCoord2f(1,0); glVertex3f( w/2, h/2, d/2);

  // front
  glNormal3f(0,0,1);
  glTexCoord2f(0,1); glVertex3f( -w/2, h/2, d/2);    
  glTexCoord2f(1,1); glVertex3f( w/2,h/2, d/2);      
  glTexCoord2f(1,0); glVertex3f(w/2,-h/2, d/2);
  glTexCoord2f(0,0); glVertex3f(-w/2,-h/2, d/2);

  // back
  glNormal3f(0,0,-1);
  glTexCoord2f(0,1); glVertex3f( w/2, h/2, -d/2);    
  glTexCoord2f(1,1); glVertex3f(-w/2,h/2,-d/2);              
  glTexCoord2f(1,0); glVertex3f(-w/2,-h/2, -d/2);             
  glTexCoord2f(0,0); glVertex3f(w/2,-h/2, -d/2);    

  // right
  glNormal3f(1,0,0);
  glTexCoord2f(0,1); glVertex3f(w/2,h/2, d/2);
  glTexCoord2f(1,1); glVertex3f(w/2,h/2,-d/2);       
  glTexCoord2f(1,0); glVertex3f(w/2,-h/2, -d/2);      
  glTexCoord2f(0,0); glVertex3f(w/2,-h/2, d/2);

  // left
  glNormal3f(-1,0,0);
  glTexCoord2f(0,1); glVertex3f(-w/2,h/2,-d/2);
  glTexCoord2f(1,1); glVertex3f(-w/2,h/2,d/2);
  glTexCoord2f(1,0); glVertex3f(-w/2,-h/2,d/2);
  glTexCoord2f(0,0); glVertex3f(-w/2,-h/2,-d/2);

  // bottom
  glNormal3f(0,-1,0);
  glTexCoord2f(0,1); glVertex3f(-w/2,-h/2,d/2);
  glTexCoord2f(1,1); glVertex3f(w/2,-h/2,d/2);
  glTexCoord2f(1,0); glVertex3f(w/2,-h/2,-d/2);
  glTexCoord2f(0,0); glVertex3f(-w/2,-h/2,-d/2);

  glEnd();*/

  glBegin(GL_QUAD_STRIP);
  
  glNormal3d(0.0,1.0,0.0);
  glVertex3d(-w/2,h/2,-d/2);
  glVertex3d(w/2,h/2,-d/2);

  glVertex3d(-w/2,h/2,d/2);
  glVertex3d(w/2,h/2,d/2);

  glNormal3d(0.0,0.0,1.0);
  glVertex3d(-w/2,-h/2,d/2);
  glVertex3d(w/2,-h/2,d/2);

  glNormal3d(0.0,-1.0,0.0);
  glVertex3d(-w/2,-h/2,-d/2);
  glVertex3d(w/2,-h/2,-d/2);
  
  glEnd();

  glBegin(GL_QUAD_STRIP);

  glNormal3d(1.0,0.0,0.0);
  glVertex3d(w/2,-h/2,d/2);
  glVertex3d(w/2,h/2,d/2);
  
  glVertex3d(w/2,-h/2,-d/2);
  glVertex3d(w/2,h/2,-d/2);

  glNormal3d(0.0,0.0,-1.0);
  glVertex3d(-w/2,-h/2,-d/2);
  glVertex3d(-w/2,h/2,-d/2);

  glNormal3d(-1.0,0.0,0.0);
  glVertex3d(-w/2,-h/2,d/2);
  glVertex3d(-w/2,h/2,d/2);

  glEnd();

  glPopMatrix();
  return true;
}

bool OpenGLClass::DrawStack(stack<EventStack> &s) {
  EventStack es;

  while(!s.empty()) {
    es=s.top();
    s.pop();
    glPushMatrix();
#ifdef _DEBUG
    cout << "Stack: " << es.a.X() << " ";
    cout << es.a.Y() << " ";
    cout << es.a.Z() << endl;
#endif
    glColor3d(es.color.R(),es.color.G(),es.color.B());    
    glTranslated(es.a.X(),es.a.Y(),es.a.Z());


    VectorClass distv = es.b - es.a;
    glBegin(GL_TRIANGLES);
    glNormal3d(distv.Z(),0,-distv.X());
    glVertex3d(0,-0.25,0);
    glVertex3d(distv.X(),distv.Y(),distv.Z());
    glVertex3d(0,0.25,0);
    glEnd();
    glPopMatrix();
  }
  return true;
}

bool OpenGLClass::CheckInput(double &xpos,double &ypos,double &zpos,double &yrot, int &done, int &pause) {
  Uint8 *keys;
  SDL_Event event;

  while ( SDL_PollEvent(&event) ) {
    if ( event.type == SDL_QUIT ) {
      done = 1;
    }
    if ( event.type == SDL_KEYDOWN ) {
      if ( event.key.keysym.sym == SDLK_ESCAPE ) {
        done = 1;
      }
    }
  }
  
  keys=SDL_GetKeyState(NULL);

#define piover180 MY_PI/180
  
  if ( keys[SDLK_UP] == SDL_PRESSED ) {
    xpos -= (double)sin(yrot*piover180) * USER_SPEED;
    zpos -= (double)cos(yrot*piover180) * USER_SPEED; 
  }
  if ( keys[SDLK_DOWN] == SDL_PRESSED ) {
    xpos += (double)sin(yrot*piover180) * USER_SPEED;
    zpos += (double)cos(yrot*piover180) * USER_SPEED;  
  }
  if ( keys[SDLK_LEFT] == SDL_PRESSED ) {
    yrot += 4.0;
  }
  if ( keys[SDLK_RIGHT] == SDL_PRESSED ) {
    yrot -= 4.0;
  }
  if ( keys[SDLK_PAGEUP] == SDL_PRESSED ) {
    ypos += USER_SPEED;
  }
  if ( keys[SDLK_PAGEDOWN] == SDL_PRESSED ) {
    ypos-= USER_SPEED;
  }
  if ( keys[SDLK_p] == SDL_PRESSED ) {
    if(pause == 0) pause = 2;
    if(pause == 1) pause = -1;
  }
  if ( keys[SDLK_p] != SDL_PRESSED) {
    if(pause == 2) pause = 1;
    if(pause == -1) pause = 0;
  }
  if ( !took_screenshot && keys[SDLK_s] == SDL_PRESSED) {
    took_screenshot=1;
    Screenshot("screenshot.png");
  }
  if ( took_screenshot && keys[SDLK_s] != SDL_PRESSED) {
    took_screenshot=0;
  }
  return true;
}


bool OpenGLClass::Screenshot(char *filename) {

  bool PNGScreenShot(char *);
  return PNGScreenShot(filename);

}


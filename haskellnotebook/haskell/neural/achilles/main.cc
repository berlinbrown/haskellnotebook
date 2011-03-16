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
#include"universe.h"
#include<time.h>
#include<stdlib.h>
#include"defines.h"

int FLOOR_BLOCKS_X=6;
int FLOOR_BLOCKS_Z=6;
int NUM_ORGANISMS=3;
int NUM_FOOD=3;

bool ShowLicense();

bool ShowUsage(char *);

bool ShowUsage(char *pn) {
  printf("Usage: %s [ -x <num of blocks in x axis> | -z <num blocks in z axis | -f <num of initial food> | <num of initial creatures> | -v | -h]\n",pn);
  printf("\n-v prints the version.\n-h prints this help.\nRest are self explanatory.\nKeys\n~~~~\nYou start off in the middle of the \"world\"\nThe arrow keys move you around, up and down are forward and back,\nand right and left will turn you with respect to the y axis.\nPageUp moves you up the y axis and PageDown moves you down it.\nESC quits.\n");
  printf("Defaults are: \n-x %d\n-z %d\n-f %d\nand initial creatures: %d\n",FLOOR_BLOCKS_X,FLOOR_BLOCKS_Z,NUM_FOOD,NUM_ORGANISMS);
  return true;
}

int main(int argc, char *argv[]) {
  srand(time(NULL));
  int i;

  ShowLicense();
  cout << endl << endl;

  for(i=1;i<argc;i++) {
    if(argv[i][0]=='-') {
      switch(argv[i][1]) {
      case 'h':
	ShowUsage(argv[0]);
	return 0;
      case 'v':
	printf("Achilles version %d\n",ACHILLES_VERSION);
	return 0;
      case 'x':
	i++;
	FLOOR_BLOCKS_X = atoi(argv[i]);
	break;
      case 'z':
	i++;
	FLOOR_BLOCKS_Z = atoi(argv[i]);
	break;
      case 'f':
	i++;
	NUM_FOOD = atoi(argv[i]);
	break;
      default:
	printf("That wasn't an option.  Use '-h' for help.\n");
	break;
      }
    } else {
      NUM_ORGANISMS=atoi(argv[i]);
    }
  }
  if(NUM_FOOD < 0) NUM_FOOD = 0;
  if(NUM_ORGANISMS < 0) NUM_ORGANISMS = 1;
  if(FLOOR_BLOCKS_X < 0) FLOOR_BLOCKS_X = 4;
  if(FLOOR_BLOCKS_Z < 0) FLOOR_BLOCKS_Z = 4;

  UniverseClass *univ=new UniverseClass(argc,argv);
  univ->MainLoop();

  delete univ;
  return 0;
}


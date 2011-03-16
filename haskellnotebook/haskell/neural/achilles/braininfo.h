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

#ifndef BRAININFO_H_787543
#define BRAININFO_H_787543

class BrainInfoClass {
private:
  long num_neurodes,num_layers,num_inputs,num_outputs;
  double td;
public:
  BrainInfoClass(long,long,long,long,double);
  ~BrainInfoClass();
  long NumNeurodes();
  long NumLayers();
  long NumInputs();
  long NumOutputs();
  double TopoDist();
};

#endif



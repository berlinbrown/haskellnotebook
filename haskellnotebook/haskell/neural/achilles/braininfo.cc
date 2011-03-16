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

#include"braininfo.h"

BrainInfoClass::BrainInfoClass(long _num_neurodes,
			       long _num_layers,
			       long _num_inputs,
			       long _num_outputs,
			       double _td) :
  num_neurodes(_num_neurodes),
  num_layers(_num_layers),
  num_inputs(_num_inputs),
  num_outputs(_num_outputs),
  td(_td)
{}

BrainInfoClass::~BrainInfoClass() {}

long BrainInfoClass::NumNeurodes() {
  return num_neurodes;
}

long BrainInfoClass::NumLayers() {
  return num_layers;
}

long BrainInfoClass::NumInputs() {
  return num_inputs;
}

long BrainInfoClass::NumOutputs() {
  return num_outputs;
}

double BrainInfoClass::TopoDist() {
  return td;
}

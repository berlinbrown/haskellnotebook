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

#ifndef HEBBIAN_H_678945
#define HEBBIAN_H_678945

#include<vector>

struct NeuralNetStruct;

struct Neurode {
  float *weights;
  int *inputs;
  int num_inputs;
  float output;
};

class NeuralNet {
private:
  Neurode *inner,*output;
  float *input,td;
  int layers,num_inner,num_out,num_in,num_per_layer;
public:
  NeuralNet(int,int,int,int,float);
  ~NeuralNet();
  int RunNet();
  float RunNeurode(int);
  int NumInputs();
  int SetInputs(vector<float> &);
  int NumOutputs();
  int GetOutputs(vector<float> &);
  int Learn(float,float);
  NeuralNetStruct *NewNeuralNetStruct();
};

#endif


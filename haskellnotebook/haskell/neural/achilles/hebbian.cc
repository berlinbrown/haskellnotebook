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

// Hebbian neural network simulator
// This is a stand-alone class btw :)

#include <iostream.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <vector.h>
#include <math.h>
#include "hebbian.h"



// Constructor for Class NeuralNet
// Creates a neural network with n inner neurodes, n_layers layers,
// ins number of inputs neurodes, and outs number of output neurodes
// td is the topological distortion factor, the chance that a given
// input to a given neurode will be randomly mapped
// if it is 1, all inputs will be randomly mapped, if it is 0, no inputs
// will be randomly mapped.

NeuralNet::NeuralNet(int n,int n_layers,int ins,int outs,float _td) {

  int remainder=n % n_layers;
  n+=(n_layers-remainder);
  inner=new Neurode[n];
  input=new float[ins];
  output=new Neurode[outs];
  layers=n_layers;
  num_inner=n;
  num_out=outs;
  num_in=ins;
  num_per_layer=n/n_layers;
  td=_td;
  int i,j;
  for(i=0;i<n;i++) {
    if(i<num_per_layer) inner[i].num_inputs=num_in;
    else inner[i].num_inputs=num_per_layer;
    inner[i].weights=new float[inner[i].num_inputs];
    inner[i].inputs=new int[inner[i].num_inputs];

    for(j=0;j<inner[i].num_inputs;j++) {
      inner[i].weights[j]=(float)rand()*(2.0/RAND_MAX)-1;
      if((float)rand()*(1.0/RAND_MAX)<=td)
	inner[i].inputs[j]=int(float(rand())/RAND_MAX*inner[i].num_inputs);
      else
	inner[i].inputs[j]=j;
    }
  }
  for(i=0;i<num_in;i++) 
    input[i]=0;
  for(i=0;i<num_out;i++) {
    output[i].num_inputs=num_per_layer;
    output[i].weights=new float[output[i].num_inputs];
    output[i].inputs=new int[output[i].num_inputs];

    for(j=0;j<output[i].num_inputs;j++) {
      output[i].weights[j]=(float)rand()*(2.0/RAND_MAX)-1;
      if((float)rand()*(1.0/RAND_MAX)<=td)
	output[i].inputs[j]=int(float(rand())/RAND_MAX*output[i].num_inputs);
      else
	output[i].inputs[j]=j;
    }
  }
}

NeuralNet::~NeuralNet() {
  delete [] inner;
  delete [] output;
  delete [] input;
}

int NeuralNet::RunNet() {
  int i,j;
  float sum;
  for(i=0;i<num_inner;i++) {
    inner[i].output=RunNeurode(i);
  }
  for(i=0;i<num_out;i++) {
    sum=0;
    for(j=0;j<num_per_layer;j++) 
      sum+=output[i].weights[j]*inner[output[i].inputs[j]].output;
    output[i].output=sum;
  }
  return 1;
}

float NeuralNet::RunNeurode(int n) {
  float sum=0;
  int i;

  if(n<num_per_layer) {
    for(i=0;i<inner[n].num_inputs;i++) 
      sum+=input[inner[n].inputs[i]]*inner[n].weights[i];
  } else {
    for(i=0;i<inner[n].num_inputs;i++)
      sum+=inner[n-num_per_layer+inner[n].inputs[i]].output*inner[n].weights[i];
  }
  return sum;
}
#define E 2.7182818285

// lc is the Hebbian learning constant
// alpha is a logistic coefficient.  seems to work best when <0

int NeuralNet::Learn(float lc,float alpha) {
  int i,j;
  for(i=0;i<num_inner;i++) {
    if(i<num_per_layer) {
      for(j=0;j<inner[i].num_inputs;j++)
	inner[i].weights[j]+=lc*(1/(1+pow(E,-alpha*inner[i].output))-0.5)*(input[inner[i].inputs[j]]-0.5); // Yaeger's model
    } else {
      for(j=0;j<inner[i].num_inputs;j++)
	inner[i].weights[j]+=lc*(1/(1+pow(E,-alpha*inner[i].output))-0.5)*(inner[i-num_per_layer+inner[i].inputs[j]].output-0.5); // Yaeger's model
      
    }
  }
  return 1;
}

int NeuralNet::NumInputs() {
  return num_in;
}

int NeuralNet::SetInputs(vector<float> &list) {
  if((signed int)list.size()!=num_in) return 0;
  for(int i=0;i<num_in;i++) 
    input[i]=list[i];
  return 1;
}

int NeuralNet::NumOutputs() {
  return num_out;
}

int NeuralNet::GetOutputs(vector<float> &list) {
  for(int i=0;i<num_out;i++)
    list.push_back(output[i].output);
  return 1;
}
/*
NeuralNetStruct *NeuralNet::NewNeuralNetStruct() {
  NeuralNetStruct *nn=new NeuralNetStruct;
  if(!nn) return NULL;

  nn->num_inner=num_inner;
  nn->num_layers=layers;
  nn->num_input=num_in;
  nn->td=td;
  return nn;
}
*/
/* TEST */
/*

int main(int argc, char *argv[]) {
  NeuralNet nn(10,3,3,3,1.0);
  float *tmp;
  int i,active=1;
  short random_inputs=0;

  while(active) {
    tmp=new float[nn.NumInputs()];
    for(i=0;i<nn.NumInputs();i++) {
      if(random_inputs) {
	tmp[i]=(float)rand()*(2.0/RAND_MAX)-1;
	cout << "Input #" << i << ": " << tmp[i] << endl;
      } else {
	cout << "Enter a number: ";
	cin >> tmp[i];
      }
      getchar();
    }
    if(!nn.SetInputs(tmp,nn.NumInputs())) {
      cout << "Error with SetInput()" << endl;
      return 1;
    }
    delete tmp;
    if(!nn.RunNet()) {
      cout << "Error with RunNet()" << endl;
      return 1;
    }
    tmp=new float[nn.NumOutputs()];
    if(!nn.GetOutputs(tmp,nn.NumOutputs())) {
      cout << "Error with GetOutputs()" << endl;
      return 1;
    }
    for(i=0;i<nn.NumOutputs();i++)
      cout << "Output #" << i << ": " << tmp[i] << endl;
    delete tmp;
    
    cout << "q to quit, enter to continue" << endl;
    if(getchar()=='q') active=0;
    
    nn.Learn(0.05,-1);
  }
  return 0;
}
*/

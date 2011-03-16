#ifndef AGENT_H
#define AGENT_H

// System
#include <algorithm>

// Local
#include "brain.h"
#include "debug.h"
#include "genome.h"

#include "misc.h"

// Forward declarations
class agent;

#define kPOVCellPad 2

//===========================================================================
// agent
//===========================================================================
class agent
{
public:
	static void agentinit();
    
    void grow(); 
    
    agent();
    ~agent();
    
    genome* Genes();
	brain* Brain();
    
    float Fight();
    float Strength();
    float Mate();
    long Age();
    long MaxAge();
    
    void dump(std::ostream& out);
    
protected:
	
    long fAgentNumber;
    
    genome* fGenome;
    brain* fBrain;
   
};


inline float agent::Fight() { return fBrain->Fight(); }
inline float agent::Strength() { return fGenome->Strength(); }
inline float agent::Mate() { return fBrain->Mate(); }
inline long agent::MaxAge() { return fGenome->Lifespan(); }

inline brain* agent::Brain() { return fBrain; }

#endif


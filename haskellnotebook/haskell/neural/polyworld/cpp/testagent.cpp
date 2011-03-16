/**
 * Berlin Brown
 * testagent.cp test
 */

#include <stdlib.h>

#include "brain.h"
#include "agent.h"
#include "error.h"

///////////////////////////////////////////////////////////

//---------------------------------------------------------------------------
// agent::agent
//---------------------------------------------------------------------------
agent::agent()
:
fGenome(NULL),
fBrain(NULL)
{
 
    fAgentNumber = 1;
    
	fGenome = new genome();    
	fBrain = new brain(this);
	    
}

//---------------------------------------------------------------------------
// agent::~agent
//---------------------------------------------------------------------------
agent::~agent()
{
    //	delete fPolygon;
	delete fGenome;
	delete fBrain;
}

//---------------------------------------------------------------------------
// agent::dump
//---------------------------------------------------------------------------
void agent::dump(ostream& out)
{
    fGenome->Dump(out);
    if (fBrain != NULL)
        fBrain->Dump(out);
    else
        error(1, "Attempted to dump a agent with no brain");
}


//---------------------------------------------------------------------------
// agent::grow
//---------------------------------------------------------------------------
void agent::grow()
{    
	// grow the brain from the genome's specifications
	fBrain->Grow(fGenome, fAgentNumber, false);
}    
    
///////////////////////////////////////////////////////////

int main(int argc, char *argv[]) {
    printf("Running\n");
    agent *a = new agent();
    a->dump(cout);
    a->grow();
    
    delete a;
    return 0;
}


// End of File //
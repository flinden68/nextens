#include "TimblAPI.h"

int main(){
  TimblAPI *My_Experiment = new TimblAPI( "-t cross_validate" );
  My_Experiment->Test( "cross_val.test" );  
  delete My_Experiment;
  exit(0);
}

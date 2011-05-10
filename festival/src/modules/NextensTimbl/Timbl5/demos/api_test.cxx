#include <iosfwd>
#include "TimblAPI.h"
int main(){
  TimblAPI My_Experiment( "-a IGTREE +vDI+DB", "test1" );
  My_Experiment.SetOptions( "-w2 -mM" );
  My_Experiment.SetOptions( "-w3 -vDB" );
  My_Experiment.ShowSettings( std::cout );
  My_Experiment.Learn( "dimin.train" );  
  My_Experiment.Test( "dimin.test", "my_first_test.out" );  
}

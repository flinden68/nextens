#include <iostream>
#include "TimblAPI.h"

int main(){
  TimblAPI *My_Experiment = new TimblAPI( "-a IB2 +vDI+DB" , 
                                          "test2" );
  My_Experiment->SetOptions( "-b100" );
  My_Experiment->ShowSettings( std::cout );
  My_Experiment->Learn( "dimin.train" );  
  My_Experiment->Test( "dimin.test", "my_second_test.out" );
  delete My_Experiment;
  exit(1);
}

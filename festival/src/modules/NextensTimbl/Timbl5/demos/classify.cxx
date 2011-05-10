/*
 *    classif.cc Timbl Classify Tester
 *
 *    A TimblAPI example program
 *
 * Copyright (c) 1998 - 2003
 * ILK  -  Tilburg University
 * CNTS -  University of Antwerp
 *
 * All rights Reserved.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * For questions and suggestions, see:
 *      http://ilk.uvt.nl/software.html
 * or send mail to:
 *      Timbl@uvt.nl
 */
 
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstring>
#include <ctime>
 
#include "TimblAPI.h"

using namespace std;

char inf[] = "./dimin.train";
char test_f[] = "./dimin.test";

int main(){
  string Bresult;
  double Distance;
  
  TimblAPI *Exp = new TimblAPI( "-a TRIBL" );
  Exp->SetOptions( "+vS +x -N30 -q2" );
  Exp->ShowOptions( cout );
  Exp->Learn( inf );
  ifstream testfile;
  string Buffer;
  testfile.open( test_f, ios::in );
  cout << "\nStart testing, using TRIBL" << endl;
  while ( getline( testfile, Buffer ) ){
    if ( Exp->Classify( Buffer, Bresult, Distance ) ){
      cout << Buffer << "\t --> " << Bresult << " " << Distance << endl;
    } 
    else
      cout << Buffer << "\t --> (nill)" << endl;
  }
  testfile.close();
  delete Exp;
  Exp = new TimblAPI( "-a IB1" );
  Exp->SetOptions( "-vS" );
  Exp->ShowOptions( cout );
  Exp->Learn( inf );
  testfile.clear();
  testfile.open( test_f, ios::in );
  cout << "\nStart testing, using IB" << endl;
  while ( getline( testfile, Buffer ) ){
    if ( Exp->Classify( Buffer, Bresult, Distance ) ){
      cout << Buffer << "\t --> " << Bresult << " " << Distance << endl;
    } 
    else
      cout << Buffer << "\t --> (nill)" << endl;
  }
  testfile.close();
  delete Exp;
  Exp = new TimblAPI( "-a IGTREE" );
  Exp->SetOptions( "-vS -N40" );
  Exp->ShowOptions( cout );
  Exp->Learn( inf );
  Exp->WriteInstanceBase( "dimin.tree" );
  Exp->SaveWeights( "dimin.wgt" );
  cout << "\nStart testing, using IGTree, first run" << endl;
  testfile.clear();
  testfile.open( test_f, ios::in );
  while ( getline( testfile, Buffer ) ){ 
    if ( Exp->Classify( Buffer, Bresult, Distance ) ){ 
      cout << Buffer << "\t --> " << Bresult << " " << Distance << endl;
    } 
    else
      cout << Buffer << "\t --> (nill)" << endl;
  }
  testfile.close();
  delete Exp;
  Exp = new TimblAPI( "-a IGTREE" );
  Exp->SetOptions( "-vS" );
  Exp->ShowOptions( cout );
  Exp->GetInstanceBase( "dimin.tree" );
  Exp->GetWeights( "dimin.wgt" );
  cout << "\nStart testing, using IGTree, second run, (retrieved Tree)" << endl;
  testfile.clear();
  testfile.open( test_f, ios::in );
  while ( getline( testfile, Buffer ) ){
    if ( Exp->Classify( Buffer, Bresult, Distance ) ){ 
      cout << Buffer << "\t --> " << Bresult << " " << Distance << endl;
    } 
    else
      cout << Buffer << "\t --> (nill)" << endl;
  }
  testfile.close();
  exit(1);
}

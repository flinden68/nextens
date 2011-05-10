/*
 * a tagging demo program
 *
 * Copyright (c) 1998 - 2005
 * ILK  -  Tilburg University
 * CNTS -  University of Antwerp
 *
 * All rights Reserved.
 *
 */

#include "MbtAPI.h"

using namespace std;

int main() {
  MbtAPI demo( "-s eindh.data.settings" );
  cerr << demo.Tag( "dit is een test" );
}


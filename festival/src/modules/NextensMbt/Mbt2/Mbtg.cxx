/*
 * Tagdemo.cc
 *
 *    The tagger main program
 *
 * Copyright (c) 1998-2005
 * ILK  -  Tilburg University
 * CNTS -  University of Antwerp
 *
 * All rights Reserved.
 *
 */

#include "MbtAPI.h"

int main(int argc, char *argv[]) {
  MbtAPI::GenerateTagger( argc, argv );   
  exit(1);
}


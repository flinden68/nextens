#ifndef MBT_API_H
#define MBT_API_H

#include "TimblAPI.h"
#include "Tagger.h"

using namespace Tagger;

const std::string MBT_VERSION = "Version 2.0";

class MbtAPI {
 public:
  static bool GenerateTagger( int, char** );
  static bool RunTagger( int, char** );
  MbtAPI( const std::string& );
  ~MbtAPI();
  std::string Tag( const std::string& );
 private:
  TaggerClass *tagger;
};

#endif

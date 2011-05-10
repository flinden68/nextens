#ifndef TAGGER_H
#define TAGGER_H

namespace Tagger {
  class TaggerClass;
  int MakeTagger( TimblOpts& );
  int RunTagger( TimblOpts& );
  TaggerClass *CreateTagger( TimblOpts& );
  void RemoveTagger( TaggerClass * );
  std::string Tag( TaggerClass*, const std::string& );
}

#endif
  

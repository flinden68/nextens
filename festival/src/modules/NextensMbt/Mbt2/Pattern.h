//*********************************************
//                                            *
//  Pattern.h                                 *
//                                            *
//                                            *
//*********************************************

class PatTemplate
{
public:
  std::string templatestring;
  std::string word_templatestring;
  int tlen;
  int numslots;
  int wordslots;
  int focuspos;
  int word_focuspos;
  int skipfocus;
  int numsuffix;
  int numprefix;
  int hyphen;
  int capital;
  int numeric;
  int compensation;
  int wordfocus;

  // todo:
  // int Cap1 : Cap1, CAPH, CAP1H
  // int non_alfanum
  // int num

  // int wordlength
  // 

  PatTemplate();
  ~PatTemplate(){;}

  int totalslots();
  int word_totalslots();
  bool set( const std::string& );
  int sprint( std::string& );
};

#ifndef MSGCLASS_H
#define MSGCLASS_H

/*
 * MsgClass.h
 *
 *    Helper Class to provide messaging.
 *
 * Copyright (c) 1998 - 2006
 * ILK  -  Tilburg University
 * CNTS -  University of Antwerp
 *
 * All rights Reserved.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * For questions and suggestions, see:
 *	http://ilk.uvt.nl/software.html
 * or send mail to:
 *	Timbl@uvt.nl
 */

namespace Messages {
  class MsgClass{
  public:
    MsgClass() {};
    virtual ~MsgClass() {};
    virtual void Info( const std::string&  );
    virtual void Warning( const std::string& );
    virtual void Error( const std::string& );
    virtual void FatalError( const std::string& );
  };

}
#endif

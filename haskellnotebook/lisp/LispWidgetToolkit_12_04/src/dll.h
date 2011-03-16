///
/// Berlin Brown
///
/// Date: 12/12/2004
///
/// $Id: dll.h,v 1.3 2004/12/15 06:44:40 bigbinc Exp $
/// 
/// dll.h
///

#ifndef _DLL_H_
#define _DLL_H_

#if BUILDING_DLL
# define DLLIMPORT __declspec (dllexport)
#else
# define DLLIMPORT __declspec (dllimport)
#endif


///
/// DLL Functions ;;
///
DLLIMPORT void TestMessageBox(void);
DLLIMPORT void TestMessageBoxStr(char *str);

#endif /// end of DLL_H_

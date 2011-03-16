///
/// Author: Berlin Brown
/// 
/// Date: 12/13/2004	
///
/// WidgetWin.h
///
/// $Id: WidgetWin.h,v 1.6 2004/12/21 08:01:56 bigbinc Exp $
///
/// Rewrite of the Windows Defines to LISP
///
#ifndef _WIDGET_WIN_H_
#define _WIDGET_WIN_H_

/// WNDCLASSEX
/// HWND
/// MSG
/// BOOL
/// HINSTANCE
/// HBRUSH
///
///
/// WINDOWS Defines:
///
///		typedef char CHAR;
///		typedef short SHORT;
///		typedef long LONG;
///		typedef char CCHAR, *PCCHAR;
///		typedef unsigned char UCHAR,*PUCHAR;
///		typedef unsigned short USHORT,*PUSHORT;
///		typedef unsigned long ULONG,*PULONG;
///		typedef char *PSZ;
/// 
/// Some Lisp Equivalents:
///
///		NIL			NIL				void
///		BOOLEAN		BOOLEAN			int	BOOLEAN	 
///		CHARACTER	CHARACTER		char
///		char		INTEGER			signed char	 	 
///		uchar		INTEGER			unsigned char	 	 
///		short		INTEGER			short	 	 
///		ushort		INTEGER			unsigned short	 	 
///		int			INTEGER			int	 	 
///		uint		INTEGER			unsigned int	 	 
///		long		INTEGER			long	 	 
///		ulong		INTEGER			unsigned long	 	 
///		uint8		(UNSIGNED-BYTE 8)	uint8	BYTE	 
///		sint8		(SIGNED-BYTE 8)	sint8	 	 
///		uint16		(UNSIGNED-BYTE 16)	uint16	SHORT CARDINAL	 
///		sint16		(SIGNED-BYTE 16)	sint16	SHORT INTEGER	 
///		uint32		(UNSIGNED-BYTE 32)	uint32	CARDINAL	 
///		sint32		(SIGNED-BYTE 32)	sint32	INTEGER	 
///		uint64		(UNSIGNED-BYTE 64)	uint64	LONG CARDINAL
///		sint64			(SIGNED-BYTE 64)	sint64	LONG INTEGER
///		SINGLE-FLOAT	SINGLE-FLOAT	float	 	 
///		DOUBLE-FLOAT	DOUBLE-FLOAT	double
///
/// for example:
///
///		(def-c-struct foo
///  		(a int)
///  		(b (c-array (c-ptr foo) 100)))
///
/// or  the typedef
///
/// 	(def-c-struct (div_t :typedef)
///  		(quot int)
///  		(rem int))
///
/// (declare (type foo f))
///	(foo-a (aref (foo-b f) 7))  ... or  ...
///	(slot-value (aref (slot-value f 'b) 7) 'a)
///
/// ;;;
///	;;; #S(WIDGET-TOOLKIT::FOO :A 5)
///	;;;
/// (defun test-s2 ()
///   (let ((px (make-foo :a 5)))
///     (print px)
///     (print (slot-value px 'a))
///     ))
///
/// This is known as WNDPROC function pointer
///
typedef long(CALLBACK *LISP_WNDPROC)(void*,unsigned int,unsigned int,long);

typedef struct __TEST_WND {
	
	unsigned int style;
	LISP_WNDPROC lpfnWndProc;
		
} TEST_WND;

///
/// Macro for deep copy of WND struct
///
/// isnt C a fun language
///
/// You may also use: a.lpfnWndProc   = (WNDPROC)WndProc; \
/// a.lpfnWndProc   = (WNDPROC)b->lpfnWndProc; \
///
#define HWND_DEEP_COPY(a,b,c)					\
			a.cbSize        = b->cbSize;		\
		  	a.style         = b->style;			\
			a.lpfnWndProc   = (WNDPROC)b->lpfnWndProc; \
   			a.cbClsExtra    = b->cbClsExtra;	\
  			a.cbWndExtra    = b->cbWndExtra;	\
  			a.hInstance     = c;				\
  			a.hIcon         = b->hIcon;			\
  			a.hCursor       = b->hCursor;		\
  			a.hbrBackground = b->hbrBackground;	\
  			a.lpszMenuName  = b->lpszMenuName;	\
  			a.lpszClassName = b->lpszClassName;	\
  			a.hIconSm       = b->hIconSm;

///
/// Description:
///
/// This Object is used to pass a HWND created in LISP to C
///
typedef struct __LISP_HWND {
	void *_hwnd;		
} LISP_HWND;

typedef struct __LISP_PTRTCHAR {
	char *_tchar;
} LISP_PTRTCHAR;


///
/// Description:
///
/// This struct should match the struct
/// defined in the lisp toolkit
///
typedef struct __LISP_WNDCLASSEX {
							
	unsigned int cbSize;
  	unsigned int style;
	LISP_WNDPROC lpfnWndProc;
  	int cbClsExtra;
  	int cbWndExtra;
  	void *hInstance;
  	void *hIcon;
  	void *hCursor;
	void *hbrBackground;
 	char *lpszMenuName;
  	char *lpszClassName;
  	void *hIconSm;
  	
} LISP_WNDCLASSEX;

///
/// Dialog Add Data Macros
/// 	note: from VIM improved GUI code
///
#define DIALOG_SET_LONG(x)	*((unsigned long *)ptr)++ = (x)
#define DIALOG_SET_BYTE(x)	*((unsigned char *)ptr)++ = (x)
#define DIALOG_SET_WORD(x)	*((unsigned short *)ptr)++ = (x)


#endif /// widget_win


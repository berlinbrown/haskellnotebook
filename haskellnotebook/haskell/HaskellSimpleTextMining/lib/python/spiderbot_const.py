"""
File: spiderdb.py

Copyright (c) 2007, Botnode.com (Berlin Brown)
http://www.opensource.org/licenses/bsd-license.php

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice, 
    this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, 
    this list of conditions and the following disclaimer in the documentation 
    and/or other materials provided with the distribution.
    * Neither the name of the Newspiritcompany.com (Berlin Brown) nor 
    the names of its contributors may be used to endorse or promote 
    products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
'AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Description:

Save spider database format in big endian format (network format).

"""

__author__ = "Berlin Brown"
__copyright__ = "Copyright (c) 2006-2008 Berlin Brown"
__license__ = "New BSD"

MAX_LEN_EXTRACT = 10000

SPIDER_CSV_DELIM = "::|"

KEY_HTML_TAGS = [
	"a",
	"b",
	"blockquote",
	"div",
	"h1",
	"h2",
	"i",
	"img",
	"p",
	"span",
	"strong",
	"table"
]
HTML_TAG_MAP =  {
    	"a": "Anchor Tag",
	"b": "Bold Tag",
	"blockquote": "Block Quote",
	"div": "Div Tag",
	"h1": "Header1 Tag",
	"h2": "Header2 Tag",
	"i": "Italic Tag",
	"img": "Image Tag",
	"p": "Paragraph Tag",
	"span": "Span Tag",
	"strong": "Strong Tag",
	"table": "Table Tag"
}

ENTITY_IGNORE_LIST = [
 "&#160;" , # no-break space
 "&#161;" , # inverted exclamation mark 
 "&#162;" , # cent sign 
 "&#163;" , # pound sterling sign 
 "&#164;" , # general currency sign 
 "&#165;" , # yen sign 
 "&#166;" , # broken (vertical) bar 
 "&#167;" , # section sign 
 "&#168;" , # umlaut (dieresis) 
 "&#169;" , # copyright sign 
 "&#170;" , # ordinal indicator, feminine 
 "#171;"  , # angle quotation mark, left 
 "&#172;" , # not sign 
 "&#173;" , # soft hyphen 
 "&#174;" , # registered sign 
 "&#175;" , # macron 
 "&#176;" , # degree sign 
 "&#177;" , # plus-or-minus sign 
 "&#178;" , # superscript two 
 "&#179;" , # superscript three 
 "&#180;" , # acute accent 
 "&#181;" , # micro sign 
 "&#182;" , # pilcrow (paragraph sign) 
 "&#183;" , # middle dot 
 "&#184;" , # cedilla 
 "&#185;" , # superscript one 
 "&#186;" , # ordinal indicator, masculine 
 "&#187;" , # angle quotation mark, right 
 "&#188;" , # fraction one-quarter 
 "&#189;" , # fraction one-half 
 "&#190;" , # fraction three-quarters 
 "&#191;" , # inverted question mark 
 "&#192;" , # capital A, grave accent 
 "&#193;" , # capital A, acute accent 
 "&#194;" , # capital A, circumflex accent 
 "&#195;" , # capital A, tilde 
 "&#196;" , # capital A, dieresis or umlaut mark 
 "&#197;" , # capital A, ring 
 "&#198;" , # capital AE diphthong (ligature) 
 "&#199;" , # capital C, cedilla 
 "&#200;" , # capital E, grave accent 
 "&#201;" , # capital E, acute accent 
 "&#202;" , # capital E, circumflex accent 
 "&#203;" , # capital E, dieresis or umlaut mark 
 "&#204;" , # capital I, grave accent 
 "&#205;" , # capital I, acute accent 
 "&#206;" , # capital I, circumflex accent 
 "&#207;" , # capital I, dieresis or umlaut mark 
 "&#208;" , # capital Eth, Icelandic 
 "&#209;" , # capital N, tilde 
 "&#210;" , # capital O, grave accent 
 "&#211;" , # capital O, acute accent 
 "&#212;" , # capital O, circumflex accent 
 "&#213;" , # capital O, tilde 
 "&#214;" , # capital O, dieresis or umlaut mark 
 "&#215;" , # multiply sign 
 "&#216;" , # capital O, slash 
 "&#217;" , # capital U, grave accent 
 "&#218;" , # capital U, acute accent 
 "&#219;" , # capital U, circumflex accent 
 "&#220;" , # capital U, dieresis or umlaut mark 
 "&#221;" , # capital Y, acute accent 
 "&#222;" , # capital THORN, Icelandic 
 "&#223;" , # small sharp s, German (sz ligature) 
 "&#224;" , # small a, grave accent 
 "&#225;" , # small a, acute accent 
 "&#226;" , # small a, circumflex accent 
 "&#227;" , # small a, tilde 
 "&#228;" , # small a, dieresis or umlaut mark 
 "&#229;" , # small a, ring 
 "&#230;" , # small ae diphthong (ligature) 
 "&#231;" , # small c, cedilla 
 "&#232;" , # small e, grave accent 
 "&#233;" , # small e, acute accent 
 "&#234;" , # small e, circumflex accent 
 "&#235;" , # small e, dieresis or umlaut mark 
 "&#236;" , # small i, grave accent 
 "&#237;" , # small i, acute accent 
 "&#238;" , # small i, circumflex accent 
 "&#239;" , # small i, dieresis or umlaut mark 
 "&#240;" , # small eth, Icelandic 
 "&#241;" , # small n, tilde 
 "&#242;" , # small o, grave accent 
 "&#243;" , # small o, acute accent 
 "&#244;" , # small o, circumflex accent 
 "&#245;" , # small o, tilde 
 "&#246;" , # small o, dieresis or umlaut mark 
 "&#247;" , # divide sign 
 "&#248;" , # small o, slash 
 "&#249;" , # small u, grave accent 
 "&#250;" , # small u, acute accent 
 "&#251;" , # small u, circumflex accent 
 "&#252;" , # small u, dieresis or umlaut mark 
 "&#253;" , # small y, acute accent 
 "&#254;" , # small thorn, Icelandic 
 "&#255;"  # small y, dieresis or umlaut mark
]

//************************************************
//* Copyright (c) 2007 (botnode.com).  All Rights Reserved
//* 
//* Created On: 11/6/2007
//* 
//* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
//* A PARTICULAR PURPOSE ARE DISCLAIMED.
//* 
//* (see /LICENSE for more details)
//************************************************
/*
 data SpiderDatabase =  SpiderDatabase { 
      magicNumberA :: Word16,
      magicNumberB :: Word16,
      majorVers :: Word16,
      minorVers :: Word16,
      headerTag :: Word16,
      poolLen :: Word16,
      spiderpool :: [URLSet]
    }
data URLSet = URLSet {
      urlinfo :: URLInfo,
      titleinfo :: TitleInfo,
      descrinfo :: DescrInfo,
      keywordsinfo :: KeywordsInfo
}
data URLInfo = URLInfo {
      tag :: Word8,
      urlid :: Word16,
      urllen :: Word16,
      url :: ByteString
}
data TitleInfo = TitleInfo {
      titletag :: Word8,      
      titlelen :: Word16,
      title :: ByteString
}
data DescrInfo = DescrInfo {
      descrtag :: Word8,      
      descrlen :: Word16,
      descr :: ByteString
}
data KeywordsInfo = KeywordsInfo {
      keywordstag :: Word8,      
      keywordslen :: Word16,
      keywords :: ByteString
}
*/

package org.botlist.dbreader

import scala.Console
import scala.io.Source
import java.io._

object DbReader {
  
  def main(args: Array[String]): Unit = {
	Console.println("INFO: Indexing Document Data <standby> ...")
	val timeStart = System.currentTimeMillis()	   

	val fis = new FileInputStream( args(0) )
	val bis = new BufferedInputStream( fis, 4096 )
    val inputstream = new DataInputStream( bis )
	
	val magicNumberA = inputstream.readUnsignedShort()
	val magicNumberB = inputstream.readUnsignedShort()	
	val versMajor = inputstream.readUnsignedShort()
	val versMinor = inputstream.readUnsignedShort()
	val headerTag = inputstream.readUnsignedShort()
	val poolct = inputstream.readUnsignedShort()
    Console.printf("Magic Number A: 0x{0} 0x{1}\n", 
				   Integer.toHexString(magicNumberA), 
				   Integer.toHexString(magicNumberB)) 
    Console.printf("Version: 0x{0} 0x{1}\n", 
				   Integer.toHexString(versMajor), 
				   Integer.toHexString(versMinor))	
	Console.printf("Header Tag: 0x{0}\n", 
				   Integer.toHexString(headerTag))
	Console.printf("Pool Len Count: {0,number}\n", poolct)
	
	// Extract the URL content, first URL
	val tag = inputstream.readUnsignedByte()
	val urlid = inputstream.readUnsignedShort()
	val urllen = inputstream.readUnsignedShort()

	Console.printf("URL Info: 0x{0}, {1}, {2}\n", 
				   Integer.toHexString(tag),
				   urlid, urllen)
	val bytestrdata = new Array[byte](urllen)
	inputstream.readFully(bytestrdata)
	val urlstr = new String(bytestrdata)
	Console.printf("URL: [{0}]\n", urlstr)

	// Read the title info struct
	val titletag = inputstream.readUnsignedByte()
	val titlelen = inputstream.readUnsignedShort()
	Console.printf("Title Info: 0x{0}, {1}\n",
				   Integer.toHexString(titletag), titlelen)
	
	// Print the title string data
	val bytestrdata2 = new Array[byte](titlelen)
	inputstream.readFully(bytestrdata2)
	val titlestr = new String(bytestrdata2)
	Console.printf("Title: [{0}]\n", titlestr)
	
	inputstream.close()
    val timeEnd = System.currentTimeMillis()
    Console.println("Done...")
    Console.println("Completed processing in " + (timeEnd - timeStart) + " ms.")
  }
}

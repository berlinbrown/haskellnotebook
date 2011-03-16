/*
 * File Read Writer Utilites for Log file analysis.
 * Berlin Brown
 * Created on Oct 8, 2007
 *
 * FileUtilReaderWriter.java
 *
 */

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.List;

/**
 * Reader Writer Interface that can be used as a callback class.
 */
public interface FileUtilReaderWriter {

	public boolean filterAllow(String line);

	/**
	 * The read method handles the line by line operation for parsing the log file.
	 */
	public void read(BufferedReader bufInput, List resultList) throws Exception;
	public void shutdown();

	public BufferedWriter getWriter();
	public void writeln(final String str) throws IOException;
	public void write(final String str) throws IOException;

	public FileUtilReaderWriter setOutfilename(final String filename);
	public FileUtilReaderWriter setOutAppend(final boolean append);
	public void init() throws IOException;
}

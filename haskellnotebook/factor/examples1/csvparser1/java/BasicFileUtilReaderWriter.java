/*
 * File Read Writer Utilites for Log file analysis.
 * Berlin Brown
 * Created on Oct 8, 2007
 *
 * BasicFileUtilReaderWriter.java
 */

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

/**
 * Basic implementation of the ReaderWriter Utility; skeleton code to implement
 * the file writer routines.
 *
 * Example usage includes, make sure that the output file name and append file flag
 * have been set.
 * <code>
 * readerWriter.init();
 * reader.read(in, lData);
 * reader.shutdown();
 * </code>
 */
public abstract class BasicFileUtilReaderWriter implements FileUtilReaderWriter {

	private BufferedWriter bufWriteOut = null;
	private String outFilename = null;
	private boolean outAppend = false;

	public FileUtilReaderWriter setOutfilename(final String filename) {
		outFilename = filename;
		return this;
	}
	public FileUtilReaderWriter setOutAppend(final boolean append) {
		outAppend = append;
		return this;
	}
	public void init() throws IOException {
		bufWriteOut = new BufferedWriter(new FileWriter(outFilename, outAppend));
	}
	public void shutdown() {
		if (bufWriteOut != null) {
			try {
				bufWriteOut.flush();
				bufWriteOut.close();
			} catch (IOException e) {
			}
		}
	}
	public BufferedWriter getWriter() {
		return bufWriteOut;
	}
	public void writeln(final String str) throws IOException {
		if (getWriter() != null) {
			String nl = (System.getProperty("line.separator") == null) ? "\n" : System.getProperty("line.separator");
			getWriter().write(str + nl);
		}
	}
	public void write(final String str) throws IOException {
		if (getWriter() != null) {
			getWriter().write(str);
		}
	}
}

/*
 * File Read Writer Utilites for Log file analysis.
 * Berlin Brown
 * Created on Oct 8, 2007
 *
 * FileUtil.java
 */

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class FileUtil {

	/**
	 * Instantiate reader and ehandler through anonymous subclasses of <code>ReaderWriter</code> and
	 * <code>ExceptionHandler</code>.
	 *
	 * <code>
	 * // Example reader code.
	 *		while ((feed = in.readLine()) != null) {
	 *			feed = feed.trim();
	 *			if ((feed != null) && (!feed.startsWith("#")) && (feed.length() > 2)) {
	 *				lData.add(feed);
	 *			}
	 *		} // End of the While
	 * </code>
	 *
	 * @param filename
	 * @param reader
	 * @return
	 */
	public static Object[] loadFile(final String filename,
									FileUtilReaderWriter reader,
									FileUtilExceptionHandler eHandler) {
		List lData = new ArrayList();
		String feed = null;
		BufferedReader in = null;
		try {
			in = new BufferedReader(new FileReader(filename));
			reader.init();
			reader.read(in, lData);
		} catch (Exception e) {
			if (eHandler != null) {
				eHandler.handleException(e);
			} else {
				e.printStackTrace();
			}
		} finally {
			reader.shutdown();
			if (in != null) {
				try {
					in.close();
				} catch (IOException e) {
				}
			}
		}
		return lData.toArray();
	}
}

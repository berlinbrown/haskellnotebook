/*
 * File Read Writer Utilites for Log file analysis.
 * Berlin Brown
 * Created on Oct 8, 2007
 *
 * AnalyzeLogsHtml.java
 */

import java.io.BufferedReader;
import java.util.List;


public class AnalyzeURLFiles {

	public static final int MIN_LINE_LEN = 5;

	public static void runURLFiles(final String filename, final String outFilename, final boolean append) {
		FileUtil.loadFile(filename,
				(new BasicFileUtilReaderWriter() {
					public boolean filterAllow(String linePreFilter) {
						boolean filterAllowFlag = false;
						int parseAllowFSM = 0;
						final int PARSE_ALLOW_VALID = 1;
						if (linePreFilter != null) {
							String line = linePreFilter.toLowerCase();
							if (line.length() > MIN_LINE_LEN)
								parseAllowFSM++;
							// After parsing all filter flags, enable allow flag
							if (parseAllowFSM == PARSE_ALLOW_VALID)
								filterAllowFlag = true;
						} // end of null check
						return filterAllowFlag;
					} // End of Filter Allow

					public void read(BufferedReader bufInput, List resultList) throws Exception {
						System.out.println("Reading file=" + filename);
						String feed = "";
						long lines = 0;
						while ((feed = bufInput.readLine()) != null) {
							feed = feed.trim();
							if (this.filterAllow(feed)) {
								lines++;
							}
						} // End of the While
						System.out.println("Total Lines:" + lines);
					}
					// Note, set init parameters through method chaining.
				}).setOutfilename(outFilename).setOutAppend(append),
				new FileUtilExceptionHandler() {
					public void handleException(Exception e) {
						System.out.println("ERROR processing file=" + filename);
						System.out.println("ERROR: " + e.getMessage());
					}
				});
	}

	public static void main(String [] args) {

		if (args.length != 1) {
			System.out.println("Usage: AnalyzeLogsHtml file-to-analyze");
			System.exit(-1);
		}

		long tstart = System.currentTimeMillis();
		System.out.println("Analyzing Files");
		runURLFiles(args[0], "empty.txt", false);

		long tend = System.currentTimeMillis();
		long diff = tend - tstart;
		System.out.println("Completed in=" + diff + " ms");
	}

}

/*---------------------------
 * Berlin Brown
 * Created on Jul 18, 2007
 * @see http://www.lipsum.com/
 */
import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.net.Socket;
import java.util.Date;

/**
 * Simple Server Client Thread Handler
 */
public class SimpleHTTPServerThread implements Runnable {
	private Socket client;
	private boolean running = false;
	private DataInputStream in;
	private PrintStream out;

	public SimpleHTTPServerThread(Socket client) {
		this.client = client;
		try {
			System.out.println("communicating with server=" + client);
			in = new DataInputStream(client.getInputStream());
			out = new PrintStream(client.getOutputStream());
		} catch (IOException e) {
			try {
				client.close();
			} catch (IOException e2) { ; }
			System.err.println("Exception while opening socket streams: " + e);
			return;
		}
	}

	/**
	 * @see java.lang.Runnable#run()
	 */
	public void run() {
		running = true;
		String line;
		try {
			BufferedReader bufReader = new BufferedReader(new InputStreamReader(in));
			while(running) {
				// read in a line from the client
				line = bufReader.readLine();
				if (line == null)
					break;
				// and write out the reversed line
				//System.out.println("[server/" + line.length() + "]" + line);
				if (line.length() == 0)
					break;
			}
			// Write a html response back
			StringBuffer buf = new StringBuffer();
			buf.append("HTTP/1.1 200 Ok\r\n");
			buf.append("Server: Apache-Test\r\n");
			buf.append("Connection: close\r\n");
			buf.append("Content-Type: text/html\r\n");
			buf.append("\r\n");

			buf.append("<html>");
			buf.append("<body>");
			buf.append("" + new Date() + " / " + this.client);
			buf.append("<p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Vestibulum nec nibh vitae dolor adipiscing mattis. Mauris porta pulvinar felis. Curabitur sit amet tortor at erat viverra commodo. Nulla dolor sapien, luctus aliquam, fermentum sed, vestibulum vitae, nibh. Nunc vitae elit sed ante sagittis porttitor. Integer semper nibh eget leo. Nullam tincidunt. Nullam nec dui non leo laoreet tincidunt. Proin ac elit et elit facilisis lobortis. Aliquam consequat nisl vel nibh. Suspendisse convallis eleifend nibh. Ut sapien. Phasellus congue blandit sem. Suspendisse laoreet auctor mi. Duis laoreet luctus quam. Vestibulum vel urna non nunc dapibus volutpat. Integer est.\n</p>");
            buf.append("<p>Aenean tellus lorem, tempor at, semper at, imperdiet et, turpis. Quisque diam sapien, tempor ac, adipiscing quis, euismod vel, neque. Donec aliquam mauris quis augue. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Aliquam tempor purus in est. Suspendisse nec mi. Cras dictum, nisi eget feugiat condimentum, metus nulla molestie purus, vel elementum quam quam eget urna. Integer tincidunt odio a leo. Mauris augue velit, eleifend quis, volutpat vitae, tincidunt a, urna. Cras suscipit venenatis ipsum. Donec adipiscing, ligula et pretium vulputate, mi ligula posuere nisi, quis convallis augue velit sed tellus. Mauris posuere ante et augue. Vestibulum nibh mauris, ultricies at, facilisis id, fringilla nec, magna. Integer pede. Mauris rutrum vestibulum purus. Fusce rutrum vulputate leo. Nunc gravida. Lorem ipsum dolor sit amet, consectetuer adipiscing elit.\n");
			buf.append("</body>");
			buf.append("</html>");
			out.println(buf);
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			try {
				if (out != null) out.close();
				if (in != null) in.close();
				client.close();
			} catch (IOException e2) {;}
			System.out.println("[server] closing connection");
		} // End of Try - Catch - Finally

	}
}
// End of File
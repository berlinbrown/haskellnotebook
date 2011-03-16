/*
 * Berlin Brown
 * Created on Jul 18, 2007
 * 
 */
import java.io.BufferedReader;

import java.io.DataInputStream;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.net.Socket;
import java.util.Date;

/**
 * Simple Server Client Thread Handler to mimic the AMQP server. 
 */
public class SimpleServerThread implements Runnable {	
	private Socket client;
	private boolean running = false;
	private DataInputStream in;	
	private PrintStream out;
	
	public SimpleServerThread(Socket client) {
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
		    DataInputStream inputstream = 
		    		new DataInputStream(new BufferedInputStream(in, 4096 ));
			while(running) {												
				// and write out the reversed line								
				byte [] bytestrdata = new byte[4];
				inputstream.readFully(bytestrdata);
				
				byte [] byteheader_rest = new byte[4];
				inputstream.readFully(byteheader_rest);
				
				// Extract the string
				String amq = new String(bytestrdata);
				System.out.println("[server/" + amq + "]");
				System.out.printf("[server/ [%x %x %x %x] \n",  
							byteheader_rest[0], byteheader_rest[1], byteheader_rest[2], byteheader_rest[3]);
				
				running = false;
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
			buf.append("</body>");
			buf.append("</html>");
			out.println(buf);
			out.flush();
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